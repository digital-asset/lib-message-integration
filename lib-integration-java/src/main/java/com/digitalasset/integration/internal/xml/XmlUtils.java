// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.xml;

import com.digitalasset.integration.api.codec.exceptions.CodecException;

import org.w3c.dom.Node;

import java.io.*;
import java.net.URL;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import com.digitalasset.integration.api.codec.exceptions.SchemaValidationException;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.SchemaFactory;

public class XmlUtils {

    private static final TransformerFactory TRANSFORMER_FACTORY = TransformerFactory.newInstance();

    public static void writeTo(Node node, OutputStream outputStream) throws TransformerException {
        Transformer transformer = TRANSFORMER_FACTORY.newTransformer();
        transformer.setOutputProperty ( "encoding" , "UTF-8" ) ;
        transformer.transform(new DOMSource(node), new StreamResult(outputStream));
    }

    public static void writeTo(Node document, Writer writer) throws TransformerException {
        Transformer transformer = TRANSFORMER_FACTORY.newTransformer();
        transformer.setOutputProperty ( "encoding" , "UTF-8" ) ;
        transformer.transform(new DOMSource(document), new StreamResult(writer));
    }

    public static byte[] toBytes(Node node) {
        try {
            ByteArrayOutputStream xmlByteStream = new ByteArrayOutputStream();
            XmlUtils.writeTo(node, xmlByteStream);
            return xmlByteStream.toByteArray();
        } catch (TransformerException e) {
            throw new CodecException("unable to write XML to bytes", e);
        }
    }

    public static void validate(byte[] bytes, URL schema) throws SchemaValidationException {
        try (InputStream xmlStream = new ByteArrayInputStream(bytes)) {
            StreamSource contents = new StreamSource(xmlStream);
            SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
                    .newSchema(schema)
                    .newValidator()
                    .validate(contents);
        } catch (SAXParseException ex) {
            throw new SchemaValidationException(ex.getMessage());
        } catch (SAXException | IOException ex) {
            throw new IllegalStateException("Unexpected error occurred during XML schema validation: " + ex.getMessage());
        }
    }
}
