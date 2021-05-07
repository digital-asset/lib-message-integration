// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.examples.codec.acord;

import com.daml.ledger.javaapi.data.Value;
import com.digitalasset.integration.api.codec.exceptions.CodecException;
import com.digitalasset.integration.api.codec.Encoder;
import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;
import com.digitalasset.integration.internal.codec.metadata.XsdMetadata;
import com.digitalasset.integration.internal.codec.metadata.XsdMetadataReader;
import com.digitalasset.integration.internal.codec.strategies.DomXsdEncodeStrategy;

import java.io.IOException;
import java.util.Optional;

/**
 * An example ACORD encoder.
 */
public class ExampleACORDEncoder implements Encoder {

    private final String rootElemName;
    private final DamlType rootType;
    private final XsdMetadata metadata;

    public ExampleACORDEncoder(String choiceName) throws IOException {
        this.rootElemName = choiceNameToRootName(choiceName);
        this.metadata = XsdMetadataReader.fromJSON(getClass().getResource("/acord/metadata/TXLifeJ/TXLife.json"));
        Optional<DamlType> rootType = metadata.getElementType(rootElemName);
        if(!rootType.isPresent()) {
            throw new IllegalArgumentException("Choice type not found for element: "+rootElemName);
        }
        this.rootType = rootType.get();
    }

    @Override
    public byte[] encode(Value value) throws CodecException {
        DomXsdEncodeStrategy encodingStrategy = new DomXsdEncodeStrategy(rootElemName, metadata);
        return encodingStrategy.encode(value, rootType).toBytes();
    }

    public String encodePretty(Value value) throws CodecException {
        DomXsdEncodeStrategy encodingStrategy = new DomXsdEncodeStrategy(rootElemName, metadata);
        return encodingStrategy.encode(value, rootType).toPrettyString();
    }

    // NOTE this works for ACORD but not for any XML element name
    private static String choiceNameToRootName(String choiceName) {
        return choiceName.substring(0,1).toLowerCase() + choiceName.substring(1);
    }
}
