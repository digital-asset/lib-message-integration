// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.xml;

import static com.google.common.base.Strings.emptyToNull;

import com.digitalasset.integration.api.codec.exceptions.CodecException;

import com.google.common.base.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.InputSource;

import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

/**
 * Encapsulates an XML document, allowing for finding nodes, XPath queries etc.
 */
public class QueryableXml {

    private static final Logger logger = LoggerFactory.getLogger(QueryableXml.class);

    private final Document document;
    private final Node root;
    private final XPath xPath;

    public QueryableXml(Document document) {
        this.document = document;
        this.root = this.document.getDocumentElement();
        this.root.normalize();
        this.xPath = XPathFactory.newInstance().newXPath();
    }

    /**
     *  Parse and validate.
     */
    public static QueryableXml parse(byte[] data, URL schema) {
        XmlUtils.validate(data, schema);
        return parse(data);
    }

    /**
     *  Parse but do not validate.
     */
    public static QueryableXml parse(byte[] data) {
        DocumentBuilderFactory factory = createDocumentBuilderFactory();
        // FIXME: This needs to be set; however, as this can change behaviour it should be done as its own task early (https://digitalasset.atlassian.net/browse/DEL-3093)
        // in a release cycle.
        // factory.setNamespaceAware(true);
        try(Reader reader = new InputStreamReader(new ByteArrayInputStream(data), StandardCharsets.UTF_8)) {
            DocumentBuilder builder = factory.newDocumentBuilder();
            return new QueryableXml(builder.parse(new InputSource(reader)));
        } catch (Exception e) {
            throw new IllegalArgumentException("Failed to parse XML data: " + e.getMessage(), e);
        }
    }

    public Element getRoot() {
        return (Element) root;
    }

    /**
     * @return root element name with namespace prefix removed, if present
     */
    public String getRootName() {
        String name = getRoot().getTagName();
        int nsIndex = name.indexOf(':');
        return nsIndex == -1 ? name : name.substring(nsIndex + 1);
    }

    public Optional<Node> xPathNode(String expression) {
        try {
            return Optional.ofNullable((Node) xPath.evaluate(expression, document, XPathConstants.NODE));
        } catch (XPathExpressionException e) {
            throw new IllegalArgumentException("Unable to execute XPath expression: " + expression, e);
        }
    }

    public Optional<NodeList> xPathNodeList(String expression) {
        try {
            return Optional.ofNullable((NodeList) xPath.evaluate(expression, document, XPathConstants.NODESET));
        } catch (XPathExpressionException e) {
            throw new IllegalArgumentException("Unable to execute XPath expression: " + expression, e);
        }
    }

    public Optional<Element> xPathElement(String expression) {
        return xPathNode(expression).filter(Element.class::isInstance).map(Element.class::cast);
    }

    public Optional<String> xPathString(String expression) {
        try {
            return Optional.ofNullable(emptyToNull((String) xPath.evaluate(expression, document, XPathConstants.STRING)));
        } catch (XPathExpressionException e) {
            throw new IllegalArgumentException("Unable to execute XPath expression: " + expression, e);
        }
    }

    public Optional<String> xPathNodeValueAsString(String expression) {
        return xPathNodeAsString(expression + "/*");
    }

    public Optional<String> xPathNodeAsString(String expression) {
        return xPathNode(expression).map(QueryableXml::nodeToString);
    }

    /**
     * @param expression XPath to execute to find element
     * @param attributeName attribute name to retrieve from the element
     * @return attribute value as found by {@code expression} (pointing to an element) and {@code attributeName}
     * (pointing to an attribute of the element), or empty {@link Optional} otherwise
     */
    public Optional<String> xPathAttribute(String expression, String attributeName) {
        return xPathElement(expression)
                .map(element -> element.getAttribute(attributeName))
                .map(Strings::emptyToNull);
    }

    public String toXmlString() {
        try {
            StringWriter writer = new StringWriter();
            XmlUtils.writeTo(document, writer);
            return writer.toString();
        } catch (TransformerException ex) {
            throw new CodecException("Error while streaming XML: " + ex.getMessage(), ex);
        }
    }

    public String toPrettyString() {
          try {
            DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();
            DOMImplementationLS impl = (DOMImplementationLS) registry.getDOMImplementation("LS");
            LSSerializer writer = impl.createLSSerializer();
            LSOutput lsOutput =  impl.createLSOutput();
            lsOutput.setEncoding("UTF-8");
            StringWriter stringWriter = new StringWriter();
            lsOutput.setCharacterStream(stringWriter);
            writer.getDomConfig().setParameter("format-pretty-print", true);
            writer.getDomConfig().setParameter("xml-declaration", true);
            writer.write(this.document, lsOutput);
            return stringWriter.toString();
        } catch (Exception ex) {
            throw new CodecException("Unexpected error while creating the XML document: " + ex.getMessage(), ex);
        }
    }

    public byte[] toBytes() {
        return XmlUtils.toBytes(this.document);
    }

    public static String nodeToString(Node node) {
        try (StringWriter sw = new StringWriter()) {
            Transformer t = TransformerFactory.newInstance().newTransformer();
            t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            t.transform(new DOMSource(node), new StreamResult(sw));
            return sw.toString();
        } catch (Exception e) {
            throw new CodecException("Could not get contents of XML node as string", e);
        }
    }

    @Override
    public String toString() {
        return "XML with root: <" + document.getDocumentElement().getTagName() + ">";
    }

    private static DocumentBuilderFactory createDocumentBuilderFactory() {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

            // PROD-9157 disable entity expansion to prevent XXE attacks
            factory.setXIncludeAware(false);
            factory.setExpandEntityReferences(false);
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            factory.setFeature("http://xml.org/sax/features/external-general-entities", false);

            return factory;
        } catch (ParserConfigurationException e) {
            throw new CodecException("Unable to create DocumentBuilderFactory", e);
        }
    }
}
