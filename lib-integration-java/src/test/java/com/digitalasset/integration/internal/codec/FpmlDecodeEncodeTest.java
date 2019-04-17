// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec;

import com.digitalasset.integration.api.codec.Decoder;
import com.digitalasset.integration.examples.codec.fpml.ExampleFpmlDecoder;
import com.digitalasset.integration.examples.codec.fpml.ExampleFpmlEncoder;
import com.digitalasset.integration.internal.xml.QueryableXml;
import org.junit.Before;
import org.junit.Test;

import static com.digitalasset.integration.internal.codec.NodeCount.getNodeCounts;
import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import com.daml.ledger.javaapi.data.*;


public class FpmlDecodeEncodeTest {

    private URL schema = getClass().getResource("/fpml/confirmation/fpml-main-5-10.xsd");

    private Map<String, ExampleFpmlEncoder> encoders = new HashMap<>();
    private Decoder decoder;

    @Before
    public void setUp() throws Exception {
        decoder = new ExampleFpmlDecoder(
                    new Identifier("packageId", "Template.Dummy", "Dummy"),
                    "contractId");
    }

    @Test
    public void testDecodeEncode() throws IOException, URISyntaxException {
        Properties props = System.getProperties();
        props.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "INFO");

        decodeEncodeWith(Paths.get(getClass().getResource("/fpml/confirmation/business-processes").toURI()));
        decodeEncodeWith(Paths.get(getClass().getResource("/fpml/confirmation/products").toURI()));
    }

    private void decodeEncodeWith(Path root) throws IOException {
        for (Path fp : Files.walk(root)
                .filter(Files::isRegularFile)
                .filter(f -> f.getFileName().toString().endsWith("xml"))
                .collect(Collectors.toList())) {

            System.out.println("#### Trying " + fp.toString());
            byte[] bytes = Files.readAllBytes(fp);
            Command cmd = decoder.decode(fp.toString(), bytes);

            String choiceName = cmd.asExerciseCommand().get().getChoice();
            ExampleFpmlEncoder encoder = getEncoderForChoice(choiceName);

            Value value = cmd.asExerciseCommand().get().getChoiceArgument().asRecord().get().getFields().get(0).getValue();
            //System.out.println(value);
            String doc = encoder.encodePretty(value);
            System.out.println(doc);

            // parse and validate output, compare node counts
            QueryableXml xmlIn = QueryableXml.parse(bytes, schema);
            QueryableXml xmlOut = QueryableXml.parse(doc.getBytes("UTF-8"), schema);
            Map<String, Integer> nodeCountIn = getNodeCounts(xmlIn.getRoot());
            Map<String, Integer> nodeCountOut = getNodeCounts(xmlOut.getRoot());
            assertEquals("Node count mismatch", nodeCountIn, nodeCountOut);
        }
    }

    private ExampleFpmlEncoder getEncoderForChoice(String choiceName) throws IOException {
        ExampleFpmlEncoder encoder = encoders.get(choiceName);
        if(encoder==null) {
            encoder = new ExampleFpmlEncoder(choiceName);
            encoders.put(choiceName, encoder);
        }
        return encoder;
    }
}
