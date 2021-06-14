// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app

import com.daml.ledger.javaapi.data.{Command, CreatedEvent, FiltersByParty, Identifier, InclusiveFilter, TransactionFilter}
import com.daml.ledger.rxjava.DamlLedgerClient
import com.daml.ledger.rxjava.components.helpers.CreatedContract
import com.daml.ledger.rxjava.components.{LedgerViewFlowable}
import org.slf4j.{Logger, LoggerFactory}

import java.time.Clock
import java.util.Collections.singletonMap
import java.util.{Collections, UUID}
import scala.collection.JavaConverters._

case class Config
  (
    appId: String,
    hostIp: String,
    hostPort: Int,
    maxRecordOffset: Int
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

  // Send a list of commands
  def sendCommands(wfId: String, party: String, commands: List[Command]): Unit = {
    client.getCommandClient.submitAndWait(
      wfId,
      config.appId,
      UUID.randomUUID().toString,
      party,
      commands.asJava
    )
  }

  def getActiveContracts(templateId: Identifier, party: String): List[CreatedEvent] =
    client.getActiveContractSetClient
               .getActiveContracts(filterFor(templateId, party), true)
               .blockingIterable().asScala
               .flatMap(r => r.getCreatedEvents.asScala)
               .toList
               .sortBy(_.getContractId)

  private def filterFor(templateId: Identifier, party: String): TransactionFilter = {
        val inclusiveFilter = new InclusiveFilter(Collections.singleton(templateId))
        new FiltersByParty(singletonMap(party, inclusiveFilter))
  }

}
