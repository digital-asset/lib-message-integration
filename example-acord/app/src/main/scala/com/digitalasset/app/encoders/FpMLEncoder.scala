// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.app.encoders

import java.net.URL

import com.daml.ledger.javaapi.data.Value
import com.digitalasset.integration.api.codec.Encoder
import com.digitalasset.integration.internal.codec.metadata.XsdMetadata
import com.digitalasset.integration.internal.codec.strategies.DomXsdEncodeStrategy

class FpMLEncoder(rootElemName: String, metadata: XsdMetadata, schema: URL) extends Encoder {

  private val rootType = metadata.getElementType(rootElemName).get()

  override def encode(value: Value): Array[Byte] = {
    val encodingStrategy = new DomXsdEncodeStrategy(rootElemName, metadata)
    encodingStrategy.encode(value, rootType).toBytes
  }

  def encodePretty(value: Value): String = {
    val encodingStrategy = new DomXsdEncodeStrategy(rootElemName, metadata)
    encodingStrategy.encode(value, rootType).toPrettyString
  }
}
