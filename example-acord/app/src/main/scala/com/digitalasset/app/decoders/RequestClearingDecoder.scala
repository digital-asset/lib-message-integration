// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app.decoders

import java.net.URL

import com.daml.ledger.javaapi.data.Record.Field
import com.daml.ledger.javaapi.data.{Command, ExerciseCommand, Identifier, Record}
import com.digitalasset.integration.api.codec.Decoder
import com.digitalasset.integration.internal.codec.metadata.{XsdMetadata}
import com.digitalasset.integration.internal.codec.strategies.DomXsdDecodeStrategy
import com.digitalasset.integration.internal.xml.QueryableXml

import scala.collection.JavaConverters._

class RequestClearingDecoder(templateId: Identifier,
                             contractId: String,
                             metadata: XsdMetadata,
                             schema: URL
                            ) extends Decoder {

  val loader = getClass().getClassLoader()
  val rootName = "requestClearing"
  val choiceName = "SubmitRequestClearing"

  override def decode(s: String, bytes: Array[Byte]): Command = {
        val xml = QueryableXml.parse(bytes)
        //val xml = QueryableXml.parse(bytes, schema)

        // check the root element name
        assert(rootName == xml.getRootName)

        val rootType = metadata.getElementType(rootName).get()
        val decodingStrategy = new DomXsdDecodeStrategy(metadata)

        val choiceArgument = new Record(List(
          new Field(decodingStrategy.decode(xml, rootType))).asJava)

        new ExerciseCommand(templateId, contractId, choiceName, choiceArgument)
    }
}
