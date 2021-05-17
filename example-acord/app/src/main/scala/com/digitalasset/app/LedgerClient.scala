// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app

import java.time.{Clock, Instant, ZoneOffset}
import java.util.Collections.singletonMap
import java.util.{Collections, Optional, UUID}

import com.daml.ledger.javaapi.data.CreatedEvent
import com.daml.ledger.javaapi.data.{Command, Filter, FiltersByParty, Identifier, InclusiveFilter, SubmitCommandsRequest, TransactionFilter}
import com.daml.ledger.rxjava.DamlLedgerClient
import com.daml.ledger.rxjava.components.{Bot, LedgerViewFlowable}
import com.daml.ledger.rxjava.components.helpers.{CommandsAndPendingSet, CreatedContract}
import com.google.protobuf.Timestamp
import io.grpc.ManagedChannelBuilder
import org.pcollections.{HashTreePMap, HashTreePSet}
import io.reactivex.Flowable
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._
import com.daml.ledger.api.v1.testing.TimeServiceGrpc
import com.daml.ledger.api.v1.testing.TimeServiceOuterClass.{GetTimeRequest, SetTimeRequest}

case class Config
  (
    appId: String,
    hostIp: String,
    hostPort: Integer,
    maxRecordOffset: Integer
  )

trait Bot[T] {
  def name: String
  def templateFilter: Set[String]
  def transform (getTemplateId: String => Identifier, c: CreatedContract): T
  def run(getTemplateId: String => Identifier, ledgerView: LedgerViewFlowable.LedgerView[T], clock: Clock): List[Command]
  def logger: Logger = LoggerFactory.getLogger(name)
}

class LedgerClient(config: Config) {
  private val client = DamlLedgerClient.newBuilder(config.hostIp, config.hostPort).build()
  client.connect()
  private val templateName2id = loadTemplates()

  // Time client
  private val channel = ManagedChannelBuilder.forAddress(config.hostIp, config.hostPort).usePlaintext.build
  private val timeClient = TimeServiceGrpc.newBlockingStub(channel)
  private val ledgerId = client.getLedgerId

  // Get template id by name
  def getTemplateId(name: String) = templateName2id(name)

  // Get current ledger time
  def getTime(): Instant = {
    val getRequest = GetTimeRequest.newBuilder()
      .setLedgerId(ledgerId)
      .build()
    val time = timeClient.getTime(getRequest).next.getCurrentTime
    Instant.ofEpochSecond(time.getSeconds, time.getNanos)
  }

  // Set current ledger time
  def setTime(newTime: Instant): Unit = {
    val currentTime = getTime()
    if (currentTime.isBefore(newTime)) {
      val currentTimestamp = Timestamp.newBuilder().setSeconds(currentTime.getEpochSecond).setNanos(currentTime.getNano).build
      val newTimestamp = Timestamp.newBuilder().setSeconds(newTime.getEpochSecond).setNanos(newTime.getNano).build

      val setRequest = SetTimeRequest.newBuilder()
        .setLedgerId(ledgerId)
        .setCurrentTime(currentTimestamp)
        .setNewTime(newTimestamp)
        .build()

      timeClient.setTime(setRequest); ()
    }
  }

  // Send a list of commands
  def sendCommands(wfId: String, party: String, commands: List[Command]): Unit = {
    val currentTime = getTime()
    val maxRecordTime = currentTime.plusSeconds(30)
    client.getCommandClient.submitAndWait(
      wfId,
      config.appId,
      UUID.randomUUID().toString,
      party,
      commands.asJava
    )
  }

  // Wire new bot
  def wireBot[T](party: String, bot: Bot[T]): Unit = {

    def runWithErrorHandling(ledgerView: LedgerViewFlowable.LedgerView[T]): Flowable[CommandsAndPendingSet] = {
      val ledgerTime = getTime()

      val clock = Clock.fixed(ledgerTime, ZoneOffset.UTC)
      try {
        bot.run(getTemplateId, ledgerView, clock) match {
          case l if l.nonEmpty => Flowable.just(createCommandsAndPendingSet(clock.instant, party, l))
          case _ => Flowable.just(CommandsAndPendingSet.empty)
        }
      } catch {
        case e: Throwable =>
          bot.logger.error("failed", e)
          Flowable.error(e)
      }
    }

    def transactionFilter: FiltersByParty = {
      val inclusiveFilter: Filter = new InclusiveFilter(bot.templateFilter.map(getTemplateId).asJava)
      val filter = singletonMap(party, inclusiveFilter)
      new FiltersByParty(filter)
    }

    def transform(c: CreatedContract): T = bot.transform(getTemplateId, c)

    Bot.wire(config.appId, client, transactionFilter, runWithErrorHandling, transform)
  }

  private def createCommandsAndPendingSet(time: Instant, party: String, commands: List[Command]): CommandsAndPendingSet = {
    val cId = UUID.randomUUID().toString + ":" + commands.map(_.asExerciseCommand()).filter(_.isPresent).map(c => c.get.getContractId).mkString(";")
    val cmds = createSubmitCommandsRequest(cId, time, party, commands)

    val pendingSet = commands
      .map(_.asExerciseCommand())
      .filter(_.isPresent)
      .map(c => (c.get.getTemplateId, c.get.getContractId))
      .groupBy(_._1)
      .mapValues(x => HashTreePSet.from(x.map(_._2).asJava))

    new CommandsAndPendingSet(cmds, HashTreePMap.from(pendingSet.asJava))
  }

  private def createSubmitCommandsRequest(cId: String, time: Instant, party: String, commands: List[Command]): SubmitCommandsRequest = {
    val maxRecordTime = time.plusSeconds(30)
    new SubmitCommandsRequest(
      UUID.randomUUID().toString,
      config.appId,
      cId,
      party,
      Optional.empty(),
      Optional.empty(),
      Optional.empty(),
      commands.asJava
    )
  }

  // load templates, taking care not to download packages
  // TODO clean-up this mapping
  private def loadTemplates(): Map[String, Identifier] = {

    // find template identifier, assume only two packages in sandbox, the first of which is the stdlib
    // FIXME I cannot inspect the metadata of a package without downloading it, which blows grpc limits
    val pkgId = client.getPackageClient().listPackages().blockingFirst()
        Map( "RequestClearingEvent" -> new Identifier(pkgId, "Role.ClearingMember", "RequestClearingEvent"),
             "ClearingConfirmedEvent" -> new Identifier(pkgId, "Role.ClearingMember", "ClearingConfirmedEvent"),
             "AcknowledgementEvent" -> new Identifier(pkgId, "Role.ClearingMember", "AcknowledgementEvent"),
             "ClearingHouseRole" -> new Identifier(pkgId, "Role.ClearingHouse", "ClearingHouseRole"),
             "ClearingMemberRole" -> new Identifier(pkgId, "Role.ClearingMember", "ClearingMemberRole"),
             "ClientRole" -> new Identifier(pkgId, "Role.Client", "ClientRole")
        )
  }

  def getActiveContracts(templateId: Identifier, party: String): List[CreatedEvent] =
    client.getActiveContractSetClient()
               .getActiveContracts(filterFor(templateId, party), true)
               .blockingIterable().asScala
               .flatMap(r => r.getCreatedEvents().asScala)
               .toList
               .sortBy(_.getContractId)

  private def filterFor(templateId: Identifier, party: String): TransactionFilter = {
        val inclusiveFilter = new InclusiveFilter(Collections.singleton(templateId))
        new FiltersByParty(singletonMap(party, inclusiveFilter))
  }

}
