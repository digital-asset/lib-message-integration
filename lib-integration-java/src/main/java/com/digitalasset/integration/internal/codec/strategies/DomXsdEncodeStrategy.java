// Copyright (c) 2019-2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.strategies;

import com.daml.ledger.javaapi.data.DamlOptional;
import com.daml.ledger.javaapi.data.DamlRecord;
import com.daml.ledger.javaapi.data.Value;
import com.daml.ledger.javaapi.data.Variant;
import com.digitalasset.integration.api.codec.exceptions.CodecException;
import com.digitalasset.integration.api.codec.strategies.EncodeStrategy;
import com.digitalasset.integration.internal.codec.metadata.*;
import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;
import com.digitalasset.integration.internal.xml.QueryableXml;
import com.digitalasset.integration.internal.time.InstantFormatter;

import io.vavr.Tuple;
import io.vavr.Tuple2;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Consumer;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import static java.lang.String.format;

/**
 * Render XML using the provided Daml-LF Value and XSD metadata.
 */
public class DomXsdEncodeStrategy implements EncodeStrategy<QueryableXml> {

    private static final Logger logger = LoggerFactory.getLogger(DomXsdEncodeStrategy.class);

    private final XsdMetadata metadata;
    private final String rootElementName;

    public DomXsdEncodeStrategy(String rootElementName, XsdMetadata metadata) {
        this.metadata = metadata;
        this.rootElementName = rootElementName;
    }

    @Override
    public QueryableXml encode(Value messageData, DamlType damlType) {
        try {
            Document doc = populateDocument(messageData, damlType);
            return new QueryableXml(doc);
        } catch (ParserConfigurationException | RuntimeException ex) {
            throw new CodecException("Unexpected error while creating the XML document: " + ex.getMessage(), ex);
        }
    }

    private Document populateDocument(Value messageData, DamlType damlType) throws ParserConfigurationException {
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();

        Document doc = docBuilder.newDocument();
        Element root = createRoot(doc, damlType, rootElementName);
        doc.appendChild(root);

        // Unless verbose streaming is enabled, gRPC does not send labels for values.
        // It is purely based on the ordering of the fields.
        DamlRecord messageContent = messageData.asRecord().get();
        populateElement(doc, root, damlType, messageContent);

        return doc;
    }

    private void populateElement(Document doc,
                                 Element element,
                                 DamlType damlType,
                                 Value damlValue) {
        if (damlType.isPrimitive()) {
            encodePrimitive(doc, element, damlType.asPrimitive(), damlValue);
        } else if (damlType.isOptional()) {
            encodeOptional(doc, element, damlType.asOptional(), damlValue.asOptional().get());
        } else if(damlType.isRecord()) {
            encodeRecord(doc, element, damlType, damlValue.asRecord().get());
        } else if(damlType.isVariant()) {
            Variant variant = damlValue.asVariant().get();
            if(metadata.isBaseType(damlType)) { // non-abstract base type
                encodeBaseType(doc, element, damlType, variant);
            } else if(metadata.isEnumValue(damlType)) {
                encodeEnum(doc, element, damlType, variant);
            } else if(metadata.isAbstractType(damlType)) {
                // assume we have a subtype instance and need to emit an "xsi:type" attribute
                encodeSubType(doc, element, damlType, variant);
            } else {
                encodeVariant(doc, element, damlType, variant);
            }
        } else if(damlType.isList()) {
            // It does not make sense to populate a list into a single element
            // List is a placeholder that says create zero or more elements under the parent and is taken care of
            // in the buildChildElements() method.
            throw new CodecException("List should never be passed to populateElement()");
        } else {
            throw new CodecException("Unsupported Daml Value type: " + damlType.toString());
        }
    }

    private void encodeField(Document doc,
                             Element parent,
                             FieldMetadata<XmlFieldMeta> field,
                             Value fieldValue) {
        XmlFieldMeta meta = field.getSrcMeta();
        if(meta instanceof XmlFieldMeta.XmlElement) {
            final String elemName = ((XmlFieldMeta.XmlElement)meta).name;
            logger.trace("[element {}] : {}", elemName, field.getType());
            applyCardinality(t -> appendElement(doc, parent, elemName, t._1, t._2),
                    field.getType().getElementType(), fieldValue, field.getCardinality());
        } else if (meta instanceof XmlFieldMeta.XmlElementRef) {
            String elemName = ((XmlFieldMeta.XmlElementRef) meta).name;
            logger.trace("[element-ref {}] : {}", elemName, field.getType());
            applyCardinality(t -> appendSubstitution(doc, parent, elemName, t._1, t._2),
                    field.getType().getElementType(), fieldValue, field.getCardinality());
        } else if (meta instanceof XmlFieldMeta.XmlAttribute) {
            String attrName = ((XmlFieldMeta.XmlAttribute)meta).name;
            logger.trace("[attribute {}] : {}", attrName, field.getType());
            appendAttribute(doc, parent, attrName, field.getType(), fieldValue);
        } else if (meta instanceof XmlFieldMeta.XmlChoice) {
            logger.trace("[choice] : {}", field.getType());
            applyCardinality(t -> appendChoice(doc, parent, t._1, t._2),
                    field.getType().getElementType(), fieldValue, field.getCardinality());
        } else if (meta instanceof XmlFieldMeta.XmlSequence) {
            logger.trace("[sequence] : {}", field.getType());
            applyCardinality(t -> appendSequence(doc, parent, t._1, t._2),
                    field.getType().getElementType(), fieldValue, field.getCardinality());
        } else if (meta instanceof XmlFieldMeta.XmlContent) {
            logger.trace("[content] : {}", field.getType());
            if(field.getType().isPrimitive() || field.getType().isVariant()) {
                String rawValue = printSimpleType(field.getType(), fieldValue);
                parent.appendChild(doc.createTextNode(rawValue));
            } else {
                throw new CodecException(
                        format("Unexpected simple content '%s' for field %s.",
                                fieldValue.toString(), field.toString()));
            }
        } else if (meta instanceof XmlFieldMeta.XmlAny) { // any element whatsoever! encoded as text
            logger.trace("[any] : {}", field.getType());
            String text = "<root>" + fieldValue.asText().get().getValue() + "</root>";
            NodeList elems = QueryableXml.parse(text.getBytes()).getRoot().getChildNodes();
            for(int i=0; i<elems.getLength(); i++) {
                parent.appendChild(doc.importNode(elems.item(i), true));
            }
        } else {
            throw new CodecException("Unexpected metadata for field: "+field.toString());
        }
    }

    private void appendElement(Document doc,
                               Element parent,
                               String elemName,
                               DamlType fieldType,
                               Value fieldValue) {
        final Element childElement = createChildElement(doc, fieldType, elemName);
        populateElement(doc, childElement, fieldType, fieldValue);
        parent.appendChild(childElement);
    }

    private void appendAttribute(Document doc,
                                 Element parent,
                                 String attrName,
                                 DamlType fieldType,
                                 Value fieldValue) {
        if (fieldType.isList()) {
            throw new CodecException("Unexpected list type: "+fieldType.toString());
        } else if(fieldType.isOptional()) {
            if (fieldValue.asOptional().isPresent()) {
                DamlOptional damlOpt = fieldValue.asOptional().get();
                if (!damlOpt.isEmpty() && damlOpt.getValue().isPresent()) {
                    String text = printSimpleType(fieldType.asOptional().getNestedType(), damlOpt.getValue().get());
                    parent.setAttribute(attrName, text);
                }
            } else {
                throw new CodecException(
                        format("Unexpected value %s for type %s.",fieldValue,fieldType.toString()));
            }
        } else {
            String text = printSimpleType(fieldType, fieldValue);
            parent.setAttribute(attrName, text);
        }
    }

    private void appendSubstitution(Document doc,
                                    Element parent,
                                    String expectedElemName,
                                    DamlType elemType,
                                    Value fieldValue) {
          // trivial case, no substitution groups
        if(!fieldValue.asVariant().isPresent()) {
            appendElement(doc, parent, expectedElemName, elemType, fieldValue);
        } else {
            Variant variant = fieldValue.asVariant().get();
            FieldMetadata<XmlFieldMeta> variantField = lookupVariantField(
                    variant.getConstructor(), elemType);
            String subsElemName = ((XmlFieldMeta.XmlElement)variantField.getSrcMeta()).name;
            appendElement(doc, parent, subsElemName, variantField.getType(), variant.getValue());
        }
    }

    private void appendChoice(Document doc,
                              Element parent,
                              DamlType fieldType,
                              Value fieldValue) {
        Variant variant = fieldValue.asVariant().get();
        FieldMetadata<XmlFieldMeta> choice = lookupVariantField(
                    variant.getConstructor(),
                    fieldType.getElementType());
        encodeField(doc, parent, choice, variant.getValue());
    }

    private void appendSequence(Document doc,
                                Element parent,
                                DamlType fieldType,
                                Value fieldValue) {
        DamlRecord record = fieldValue.asRecord().get();
        encodeRecord(doc, parent, fieldType, record);
    }

    private void applyCardinality(
            Consumer<Tuple2<DamlType, Value>> encoder,
            DamlType elemType,
            Value value,
            FieldMetadata.Cardinality cardinality) {
        if (cardinality.equals(FieldMetadata.Cardinality.MANY)) {
            List<Value> values = value.asList().get().getValues();
            for (Value elemValue : values) {
                encoder.accept(Tuple.of(elemType, elemValue));
            }
        } else if(cardinality.equals(FieldMetadata.Cardinality.MANY1)) {
            List<Value> values = value.asList().get().getValues();
            for (Value elemValue : values) {
                encoder.accept(Tuple.of(elemType, elemValue));
            }
        } else if (cardinality.equals(FieldMetadata.Cardinality.OPTIONAL)) {
            DamlOptional damlOpt = value.asOptional().get();
            if(!damlOpt.isEmpty() && damlOpt.getValue().isPresent()) {
                encoder.accept(Tuple.of(elemType, damlOpt.getValue().get()));
            }
        } else {
            encoder.accept(Tuple.of(elemType, value));
        }
    }

    private void encodeRecord(Document doc,
                              Element parent,
                              DamlType damlType,
                              DamlRecord damlValue) {
        Iterator<FieldMetadata<XmlFieldMeta>> metadataIterator = metadata.getFields(damlType).iterator();
        for (DamlRecord.Field field : damlValue.getFields()) {
            final FieldMetadata<XmlFieldMeta> fieldMetadata = metadataIterator.next();
            encodeField(doc, parent, fieldMetadata, field.getValue());
        }
    }

    private void encodeVariant(Document doc,
                              Element parent,
                              DamlType damlType,
                              Variant variant) {
        FieldMetadata<XmlFieldMeta> variantField = lookupVariantField(
                variant.getConstructor(), damlType);
        encodeField(doc, parent, variantField, variant.getValue());
    }

    // special case for a non-abstract base type with an associated "contents" type
    private void encodeBaseType(Document doc,
                               Element parent,
                               DamlType damlType,
                               Variant variant) {
        FieldMetadata<XmlFieldMeta> variantField = lookupVariantField(
                variant.getConstructor()+"Contents", damlType);
        Optional<DamlRecord> record = variant.getValue().asRecord();
        if(record.isPresent()) {
            encodeRecord(doc, parent, variantField.getType(), record.get());
        } else {
            throw new CodecException(
                    "Expected a record value for the contents of non-abstract base type: "+variant.getValue().toString());
        }
    }

    private void encodeSubType(Document doc,
                               Element parent,
                               DamlType damlType,
                               Variant variant) {
        FieldMetadata<XmlFieldMeta> variantField = lookupVariantField(
                variant.getConstructor(), damlType);
        Optional<DamlRecord> record = variant.getValue().asRecord();
        if(record.isPresent()) {
            encodeRecord(doc, parent, variantField.getType(), record.get());
            parent.setAttribute("xsi:type", variantField.getType().getName());
        } else {
            throw new CodecException(
                    "Expected a record value for an (extended) subtype: "+variant.getValue().toString());
        }
    }

    private void encodeEnum(Document doc,
                            Element parent,
                            DamlType damlType,
                            Variant damlValue) {
        parent.appendChild(doc.createTextNode(printEnum(damlType, damlValue)));
    }

    private void encodeOptional(Document doc, Element element, DamlTypes.Optional damlType, DamlOptional damlValue) {
        if (!damlValue.isEmpty() && damlValue.getValue().isPresent()) {
            populateElement(doc, element, damlType.asOptional().getNestedType(), damlValue.getValue().get());
        }
    }

    private void encodePrimitive(Document doc, Element parent, DamlTypes.Primitive primType, Value damlValue) {
        String text = printPrimitive(primType, damlValue);
        parent.appendChild(doc.createTextNode(text));
    }

    private void encodeXmlSignature(Document doc, Element parent) {
        parent.appendChild(doc.createElementNS("http://www.w3.org/2000/09/xmldsig#", "auto-generated_for_wildcard"));
    }

    private String printPrimitive(DamlTypes.Primitive primType, Value damlValue) {
        if (primType==DamlTypes.TEXT) {
            return damlValue.asText().get().getValue();
        } else if (primType==DamlTypes.BOOL) {
            return damlValue.asBool().get().isValue() ? "true" : "false";
        } else if(primType==DamlTypes.INT64) {
            return String.valueOf(damlValue.asInt64().get().getValue());
        } else if(primType==DamlTypes.DECIMAL) {
            return damlValue.asNumeric().get().getValue().stripTrailingZeros().toPlainString();
        } else if(primType==DamlTypes.TIME) {
            Instant instant = damlValue.asTimestamp().get().getValue();
            return InstantFormatter.asZulu().formatAsDateTime(instant);
        } else if(primType==DamlTypes.DATE) {
            return damlValue.asDate().get().getValue().format(DateTimeFormatter.ISO_DATE);
        } else {
            throw new CodecException("Unable to encode primitive value of type: " + primType.toString());
        }
    }

    // Simple types can be primitives, enums or a variant expressing a choice
    // between primitive or enums
    private String printSimpleType(DamlType type, Value value) {
        if(type.isPrimitive()) {
            return printPrimitive(type.asPrimitive(), value);
        } else if(type.isVariant()) {
            Variant variant = value.asVariant().get();
            if (metadata.isEnumValue(type)) {
                return printEnum(type, variant);
            } else { // unwrap the variant down a layer
                FieldMetadata<XmlFieldMeta> field = lookupVariantField(variant.getConstructor(), type);
                return printSimpleType(field.getType(), variant.getValue());
            }
        } else {
            throw new CodecException("Unexpected simple type: "+type);
        }
    }

    private String printEnum(DamlType damlType, Variant damlValue) {
        String constructorName = damlValue.getConstructor();
        XmlFieldMeta xmlFieldMeta = metadata.getFields(damlType).stream()
                .filter(f -> constructorName.equals(f.getName()))
                .findFirst()
                .map(FieldMetadata::getSrcMeta)
                .orElseThrow(() -> new CodecException(
                        format("Could not find enum field %s for type %s.", constructorName, damlType.toString())));
        return ((XmlFieldMeta.XmlEnum) xmlFieldMeta).name;
    }

    private FieldMetadata<XmlFieldMeta> lookupVariantField(String constructor, DamlType variant) {
        Optional<FieldMetadata<XmlFieldMeta>> field =
                metadata.getFields(variant)
                        .stream()
                        .filter(f -> f.getName().equals(constructor))
                        .findAny();
        if(field.isPresent()) {
            return field.get();
        }
        throw new CodecException("Field not present: "+constructor+" : "+variant.toString());
    }

    private Element createChildElement(Document doc, DamlType childType, String elementName) {
        if (isNamespacedElement(elementName)) {
            return createElement(doc, childType, elementName);
        } else {
            return doc.createElement(elementName);
        }
    }

    private boolean isNamespacedElement(String fieldName) {
        return metadata.isOfTopLevelSchema(fieldName);
    }

    private Element createElement(Document doc, DamlType damlType, String elementName) {
        String xmlns = metadata.getNamespace();
        Optional<String> schemaLocation = metadata.getSchemaLocation();

        Element element = StringUtils.isBlank(xmlns)
                ? doc.createElement(elementName)
                : doc.createElementNS(xmlns, elementName);

        if (schemaLocation.isPresent() && StringUtils.isNotBlank(schemaLocation.get())) {
            element.setAttribute("xsi:schemaLocation", schemaLocation.get());
        }
        return element;
    }

    private Element createRoot(Document doc, DamlType damlType, String elementName) {
        Element element = createElement(doc, damlType, elementName);
        element.setAttribute("xmlns:xsi", XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI);
        return element;
    }

    private boolean isSome(Variant variant) {
        return "Some".equals(variant.getConstructor()) ||
               "Just".equals(variant.getConstructor());
    }
}
