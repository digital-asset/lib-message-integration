// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app

import com.daml.ledger.javaapi.data.{ContractId, Party}
import com.digitalasset.app.utils.Record._
import com.typesafe.config.ConfigFactory

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

object REPL extends App {
  val settings = new Settings {
    usejavacp.value = true
  }
  settings.embeddedDefaults[Commands.type]
  new sys.SystemProperties += (
    "scala.repl.autoruncode" -> "repl.init",
    "scala.repl.maxprintstring" -> "64000",
    "java.protocol.handler.pkgs" -> "com.digitalasset.integration.protocols"
    )
  Commands.init()
  new SampleILoop().process(settings)
}

class SampleILoop extends ILoop {
  override def prompt = "DA $ "

  override def printWelcome() {
    echo("\nWelcome to the FpML Clearing Demo Scala REPL\n")
  }
}

object Bots extends App {
  val config = ConfigFactory.load()
  val client =
    new LedgerClient(
      Config(
        config.getString("id"),
        config.getString("platform.host"),
        config.getInt("platform.port"),
        config.getInt("platform.maxRecordOffset")
      )
    )

  val houseName = "CLEARCO"
  val operatorName = "OPERATOR"

  // TODO this is needed by commands too, can we share it?
  val houseTid = client.getTemplateId("ClearingHouseRole")
  val houseCid = client.getActiveContracts(houseTid, operatorName).head.getContractId
  //

  val memberTid = client.getTemplateId("ClearingMemberRole")
  val clientTid = client.getTemplateId("ClientRole")
  val members = client.getActiveContracts(memberTid, houseName)
    .map(e => (e.getArguments.get[Party]("member").getValue, new ContractId(e.getContractId))).toMap
  val clients = client.getActiveContracts(clientTid, houseName)
    .map(e => (e.getArguments.get[Party]("client").getValue, new ContractId(e.getContractId))).toMap

  client.wireBot(houseName, new bot.EventProcessor(houseTid, clients, members))
  Thread.currentThread().join()
}
