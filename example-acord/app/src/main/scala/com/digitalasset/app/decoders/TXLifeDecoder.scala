// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app.decoders

import com.daml.ledger.javaapi.data.Record.Field
import com.daml.ledger.javaapi.data.{Command, ExerciseCommand, Identifier, Record}
import com.digitalasset.integration.api.codec.Decoder
import com.digitalasset.integration.internal.codec.metadata.XsdMetadata
import com.digitalasset.integration.internal.codec.strategies.DomXsdDecodeStrategy
import com.digitalasset.integration.internal.xml.QueryableXml

import scala.collection.JavaConverters._

class TXLifeDecoder(templateId: Identifier,
                    contractId: String,
                    metadata: XsdMetadata) extends Decoder {

  val rootName = "TXLife"
  val choiceName = "SubmitTXLife"

  override def decode(s: String, bytes: Array[Byte]): Command = {
        val xml = QueryableXml.parse(bytes)

        // check the root element name
        assert(rootName == xml.getRootName)

        val rootType = metadata.getElementType(rootName).get()
        val decodingStrategy = new DomXsdDecodeStrategy(metadata)

        val choiceArgument = new Record(List(
          new Field(decodingStrategy.decode(xml, rootType))).asJava)

        new ExerciseCommand(templateId, contractId, choiceName, choiceArgument)
    }
}
