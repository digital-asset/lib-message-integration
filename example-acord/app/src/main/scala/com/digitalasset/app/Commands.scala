// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app

import acord.AcordDecoder
import com.digitalasset.app.decoders.RequestClearingDecoder
import com.digitalasset.integration.internal.codec.metadata.XsdMetadataReader
import com.digitalasset.integration.protocols.classpath.Handler
import com.google.common.io.ByteStreams
import com.typesafe.config.ConfigFactory
import org.slf4j.{Logger, LoggerFactory}
import role.operator.OperatorRole

import java.net.URL

object Commands {
  private val config = ConfigFactory.load()
  private val client = initClient()

  private val operator = "Operator"

  private var operatorRole: Option[OperatorRole.Contract] = None
  private val metadata = XsdMetadataReader.fromJSON(mkClasspathURL("classpath:acord/metadata/TXLifeJ/TXLife.json"))

  private val logger: Logger = LoggerFactory.getLogger("Commands")

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

  def init(): Unit = {
    val roleContractEvent = client.getActiveContracts(OperatorRole.TEMPLATE_ID, operator).head
    val contract = AcordDecoder.fromCreatedEvent(roleContractEvent)
    operatorRole = Some(contract.asInstanceOf[OperatorRole.Contract])
    logger.info("Got operator role contract: " + operatorRole.get.id)
  }

  def send(example: URL): Unit = {
    val decoder = new RequestClearingDecoder(OperatorRole.TEMPLATE_ID, operatorRole.get.id.contractId, metadata)
    val cmd = decoder.decode("example", ByteStreams.toByteArray(example.openStream()))
    client.sendCommands("Acord - SubmitTXLife", operator, List(cmd))
  }

  // TODO why does installing a protocol handler via sysprop not work?
  def mkClasspathURL(path: String): URL =
    new URL(null, path, new Handler())

}
