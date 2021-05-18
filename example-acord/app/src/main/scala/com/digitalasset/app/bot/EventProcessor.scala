// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app.bot

import com.daml.ledger.javaapi.data.Record.Field
import com.daml.ledger.javaapi.data.{Unit => _, _}
import com.daml.ledger.rxjava.components.LedgerViewFlowable
import com.daml.ledger.rxjava.components.helpers.CreatedContract
import com.digitalasset.app.Bot
import com.digitalasset.app.encoders.FpMLEncoder
import com.digitalasset.app.utils.Record._
import com.digitalasset.integration.internal.codec.metadata.XsdMetadataReader
import com.digitalasset.integration.protocols.classpath.Handler

import java.net.URL
import java.time.Clock
import scala.collection.JavaConverters._

class EventProcessor(clearingHouseTid: Identifier,
                     clients: Map[String, ContractId],
                     members: Map[String, ContractId]) extends Bot[Record] {

  private val metadata = XsdMetadataReader.fromJSON(mkClasspathURL("classpath:fpml/metadata/v510/Confirmation.json"))

  def mkClasspathURL(path: String) : URL =
    new URL(null, path, new Handler())

  def templateFilter: Set[String] = Set("RequestClearingEvent", "ClearingConfirmedEvent", "AcknowledgementEvent")

  def name: String = "EventProcessor " + clearingHouseTid.getEntityName

  def transform(getTid: String => Identifier, c: CreatedContract): Record = c.getCreateArguments

  def run(getTid: String => Identifier, ledgerView: LedgerViewFlowable.LedgerView[Record], clock: Clock): List[Command] = {
    val eventTid = getTid("RequestClearingEvent")
    val events  = ledgerView.getContracts(eventTid).asScala.toList
    val cmds = events.flatMap {
      case (cId, r) =>
        logger.info("Handling request clearing event " + cId)
        val context = makeEventContext(r.get("partyInfo"))
        val arg = new Record(List(
                  new Field("event", r),
                  new Field("context", context)
                ).asJava)
        Some(new ExerciseCommand(eventTid, cId, "DispatchRequestClearing", arg))
    }

    showEvents(ledgerView, getTid("AcknowledgementEvent"), "clearingAcknowledgement", "ack")
    showEvents(ledgerView, getTid("ClearingConfirmedEvent"), "clearingConfirmed", "message")

    cmds
  }

  private def showEvents(ledgerView: LedgerViewFlowable.LedgerView[Record], tid: Identifier, rootElemName: String, payloadField: String ): Unit = {
    val encoder = new FpMLEncoder(rootElemName, metadata)
    val events = ledgerView.getContracts(tid).asScala.toList
    events.flatMap {
      case (_, r) =>
        logger.debug(encoder.encodePretty(r.get(payloadField)))
        None
    }
    ()
  }

  def makeEventContext(partyInfo: Record): Record =
    new Record(List(
      new Field("clientCid1", clients(partyInfo.get[Text]("party1Name").getValue)),
      new Field("clientCid2", clients(partyInfo.get[Text]("party2Name").getValue)),
      new Field("memberCid1", members(partyInfo.get[Text]("member1Name").getValue)),
      new Field("memberCid2", members(partyInfo.get[Text]("member2Name").getValue))
    ).asJava)

}
