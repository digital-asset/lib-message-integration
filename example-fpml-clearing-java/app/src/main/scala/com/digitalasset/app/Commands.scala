// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app

import java.net.URL
import java.time.Instant

import org.slf4j.Logger
import com.daml.ledger.javaapi.data.Identifier
import com.digitalasset.app.decoders.RequestClearingDecoder
import com.digitalasset.app.encoders.FpMLEncoder
import com.digitalasset.integration.internal.codec.metadata.XsdMetadataReader
import com.digitalasset.integration.protocols.classpath.Handler
import com.typesafe.config.ConfigFactory
import io.grpc.internal.IoUtils
import org.slf4j.LoggerFactory

object Commands {
  val config = ConfigFactory.load()
  val client = initClient()

  val operatorName  = "OPERATOR"
  val houseName     = "CLEARCO"

  var houseContract: Option[House] = None
  val metadata = XsdMetadataReader.fromJSON(mkClasspathURL("classpath:fpml/metadata/v510/Confirmation.json"))
  val schema = mkClasspathURL("classpath:fpml/confirmation/fpml-main-5-10.xsd")
  val requestClearing = mkClasspathURL("classpath:fpml/confirmation/business-processes/clearing/msg-ex07-clearingRequested-from-sef.xml")

  private val logger: Logger = LoggerFactory.getLogger("Commands")

  case class House(tid: Identifier, cid: String)

  private def initClient(): LedgerClient = {
    new LedgerClient(
      Config(
        config.getString("id"),
        config.getString("platform.host"),
        config.getInt("platform.port"),
        config.getInt("platform.maxRecordOffset")
      )
    )
  }

  def init() : Unit = {
    val houseTid = client.getTemplateId("ClearingHouseRole")
    val houseCid = client.getActiveContracts(houseTid, operatorName).head.getContractId
    logger.info("Got ClearingHouse role contract: " + houseCid)
    houseContract = Some(House(houseTid, houseCid))
  }

  // Flows
  def send(example: URL): Unit = {

    val decoder = new RequestClearingDecoder(houseContract.get.tid, houseContract.get.cid, metadata, schema)
    // val encoder = new FpMLEncoder("clearingAcknowledgement", metadata, schema)

    val cmd = decoder.decode("example", IoUtils.toByteArray(example.openStream()))
    client.sendCommands("RequestClearingFlow", operatorName, List(cmd))
  }

  def getTime() = {client.getTime()}

  def setTime(timeString: String): Unit = {
    val time = Instant.parse(timeString)
    client.setTime(time)
  }

  // TODO why does installing a protocol handler via sysprop not work?
  def mkClasspathURL(path: String) : URL =
    new URL(null, path, new Handler())

  def fetchUrl(url: URL): String =
    new String(IoUtils.toByteArray(url.openStream))

}
