// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app.bot

import java.net.URL
import java.time.Clock

import com.daml.ledger.javaapi.data.DamlRecord.Field
import com.daml.ledger.javaapi.data._
import com.daml.ledger.rxjava.components.LedgerViewFlowable
import com.daml.ledger.rxjava.components.helpers.CreatedContract
import com.digitalasset.app.Bot
import com.digitalasset.app.encoders.FpMLEncoder
import com.digitalasset.app.utils.Record._
import com.digitalasset.integration.internal.codec.metadata.XsdMetadataReader
import com.digitalasset.integration.protocols.classpath.Handler

import scala.collection.JavaConverters._

class EventProcessor(clearingHouseTid: Identifier,
                     clients: Map[String, ContractId],
                     members: Map[String, ContractId]) extends Bot[DamlRecord] {

  val metadata = XsdMetadataReader.fromJSON(mkClasspathURL("classpath:fpml/metadata/v510/Confirmation.json"))

  def mkClasspathURL(path: String) : URL =
    new URL(null, path, new Handler())

  def templateFilter: Set[String] = Set("RequestClearingEvent", "ClearingConfirmedEvent", "AcknowledgementEvent")

  def name: String = "EventProcessor " + clearingHouseTid.getEntityName

  def transform(getTid: String => Identifier, c: CreatedContract): DamlRecord = c.getCreateArguments

  def run(getTid: String => Identifier, ledgerView: LedgerViewFlowable.LedgerView[DamlRecord], clock: Clock): List[Command] = {
    val eventTid = getTid("RequestClearingEvent")
    val events  = ledgerView.getContracts(eventTid).asScala.toList
    val cmds = events.flatMap {
      case (cId, r) =>
        logger.info("Handling request clearing event " + cId)
        val context = makeEventContext(r.getFieldsMap.get("partyInfo").asRecord().get())
        val arg = new DamlRecord(List(
                  new Field("event", r),
                  new Field("context", context)
                ).asJava)
        Some(new ExerciseCommand(eventTid, cId, "DispatchRequestClearing", arg))
    }

    showEvents(ledgerView, getTid("AcknowledgementEvent"), "clearingAcknowledgement", "ack")
    showEvents(ledgerView, getTid("ClearingConfirmedEvent"), "clearingConfirmed", "message")

    cmds
  }

  def showEvents(ledgerView: LedgerViewFlowable.LedgerView[DamlRecord], tid: Identifier, rootElemName: String, payloadField: String ) = {
    val encoder = new FpMLEncoder(rootElemName, metadata, null)
    val events = ledgerView.getContracts(tid).asScala.toList
    events.flatMap {
      case (_, r) => {
        logger.debug(encoder.encodePretty(r.getFieldsMap().get(payloadField)))
        None
      }
    }
    ()
  }

  def makeEventContext(partyInfo: DamlRecord): DamlRecord =
    new DamlRecord(List(
      new Field("clientCid1", clients(partyInfo.getFieldsMap.get("party1Name").toString())),
      new Field("clientCid2", clients(partyInfo.getFieldsMap.get("party2Name").toString())),
      new Field("memberCid1", members(partyInfo.getFieldsMap.get("member1Name").toString())),
      new Field("memberCid2", members(partyInfo.getFieldsMap.get("member2Name").toString()))
    ).asJava)

}
