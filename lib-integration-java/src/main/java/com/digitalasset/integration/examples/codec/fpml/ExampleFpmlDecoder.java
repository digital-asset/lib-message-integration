// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.examples.codec.fpml;

import com.daml.ledger.javaapi.data.Command;
import com.daml.ledger.javaapi.data.ExerciseCommand;
import com.daml.ledger.javaapi.data.Identifier;
import com.daml.ledger.javaapi.data.DamlRecord;
import com.daml.ledger.javaapi.data.Value;

import com.digitalasset.integration.api.codec.exceptions.CodecException;
import com.digitalasset.integration.api.codec.Decoder;
import com.digitalasset.integration.api.codec.exceptions.SchemaValidationException;
import com.digitalasset.integration.api.codec.exceptions.UnsupportedMessageException;
import com.digitalasset.integration.internal.codec.metadata.*;
import com.digitalasset.integration.internal.codec.strategies.DomXsdDecodeStrategy;
import com.digitalasset.integration.internal.xml.QueryableXml;

import java.io.IOException;
import java.net.URL;

import static java.util.Arrays.asList;

/**
 * An example FpML decoder.
 * Expects a template with a non-consuming choice that corresponds to the root element name and a
 * single parameter with the corresponding message type.
 */
public class ExampleFpmlDecoder implements Decoder {

    private final Identifier templateId;
    private final String contractId;
    private final XsdMetadata metadata;
    private final URL schema;

    public ExampleFpmlDecoder(Identifier templateId, String contractId) throws IOException {
        this.templateId = templateId;
        this.contractId = contractId;
        this.metadata = XsdMetadataReader.fromJSON(getClass().getResource("/fpml/metadata/v510/Confirmation.json"));
        this.schema = getClass().getResource("/fpml/confirmation/fpml-main-5-10.xsd");
    }

    @Override
    public Command decode(String uniqueId, byte[] messageContents) throws UnsupportedMessageException, SchemaValidationException, CodecException {

        QueryableXml xml = QueryableXml.parse(messageContents, schema);

        // the rootname determines the choice
        String rootName = xml.getRootName();
        DamlTypes.DamlType rootType = metadata.getElementType(rootName).get();

        DomXsdDecodeStrategy decodingStrategy = new DomXsdDecodeStrategy(metadata);

        Value choiceArgument = new DamlRecord(
                asList(new DamlRecord.Field(decodingStrategy.decode(xml, rootType))));

        return new ExerciseCommand(templateId, contractId, rootNameToChoiceName(rootName), choiceArgument);
    }

    // NOTE this works for FpML but not for any XML element name
    private static String rootNameToChoiceName(String rootName) {
        return rootName.substring(0,1).toUpperCase() + rootName.substring(1);
    }
}
