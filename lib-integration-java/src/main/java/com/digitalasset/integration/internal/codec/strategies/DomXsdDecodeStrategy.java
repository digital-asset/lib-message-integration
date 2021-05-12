// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.strategies;

import com.daml.ledger.javaapi.data.Bool;
import com.daml.ledger.javaapi.data.DamlList;
import com.daml.ledger.javaapi.data.Date;
import com.daml.ledger.javaapi.data.Int64;
import com.daml.ledger.javaapi.data.Record;
import com.daml.ledger.javaapi.data.Text;
import com.daml.ledger.javaapi.data.Timestamp;
import com.daml.ledger.javaapi.data.Unit;
import com.daml.ledger.javaapi.data.Value;
import com.daml.ledger.javaapi.data.Variant;
import com.digitalasset.integration.api.codec.exceptions.CodecException;
import com.digitalasset.integration.api.codec.strategies.DecodeStrategy;
import com.digitalasset.integration.internal.codec.metadata.*;
import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;
import com.digitalasset.integration.internal.xml.QueryableXml;
import com.google.common.base.Function;
import io.vavr.*;
import io.vavr.collection.Stream;
import io.vavr.control.Either;
import io.vavr.control.Option;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.annotation.Nullable;
import java.math.BigDecimal;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.IntStream;

import static com.google.common.base.Strings.emptyToNull;
import static java.lang.String.format;
import static java.time.temporal.ChronoField.EPOCH_DAY;

/**
 * A simple DOM-based XML decoder which uses Metadata derived directly from the XSD.
 */
public final class DomXsdDecodeStrategy implements DecodeStrategy<QueryableXml> {

    private static final Logger logger = LoggerFactory.getLogger(DomXsdDecodeStrategy.class);

    private final XsdMetadata metadata;

    public DomXsdDecodeStrategy(XsdMetadata metadata) {
        this.metadata = metadata;
    }

    @Override
    public Value decode(QueryableXml xml, DamlType damlType) {
        String rootPath = "/" + xml.getRootName();
        Element rootNode = xml.getRoot();
        return getRecordTypeValue(rootNode, damlType, rootPath);
    }

    // try to decode a field using the provided XML schema metadata
    private Either<Set<ParseFailure>, Result> decodeField(
            Element parentElem,
            Stream<Node> cursor,
            FieldMetadata<XmlFieldMeta> field,
            String path) {

        XmlFieldMeta meta = field.getSrcMeta();
        if(meta instanceof XmlFieldMeta.XmlElement) {
            String elemName = ((XmlFieldMeta.XmlElement) meta).name;
            logger.trace("[element: {}] : {} ({})", elemName, field.getType(), path);
            return applyCardinality((c, ty) -> decodeElement(c, elemName, ty, path),
                    cursor, field.getType().getElementType(), field.getCardinality());
        } else if (meta instanceof XmlFieldMeta.XmlElementRef) {
            String expectedElemName = ((XmlFieldMeta.XmlElementRef) meta).name;
            logger.trace("[element-ref {}] : {} ({})", expectedElemName, field.getType(), path);
            return applyCardinality((c, ty) -> decodeSubstitution(expectedElemName, c, ty, path),
                    cursor, field.getType().getElementType(), field.getCardinality());
        } else if(meta instanceof XmlFieldMeta.XmlAttribute) {
            String attrName = ((XmlFieldMeta.XmlAttribute) meta).name;
            logger.trace("[attribute {}] : {} ({})", attrName, field.getType(), path);
            return decodeAttribute(parentElem, attrName, field.getType(), path)
                    .map(v -> new Result(v, cursor));
        } else if(meta instanceof XmlFieldMeta.XmlChoice) {
            return applyCardinality((c, ty) -> decodeChoice(parentElem, c, ty, path),
                    cursor, field.getType().getElementType(), field.getCardinality());
        } else if(meta instanceof XmlFieldMeta.XmlSequence) {
            return applyCardinality((c, ty) -> decodeSequence(parentElem, c, ty, path),
                    cursor, field.getType().getElementType(), field.getCardinality());
        } else if(meta instanceof XmlFieldMeta.XmlContent) {
            logger.trace("[content] : {} ({})", field.getType(), path);
            Either<String, Value> content = getSimpleTypeValue(parentElem, field.getType(), path);
            if (content.isRight()) {
                return Either.right(new Result(content.get(), cursor));
            }
            throw new CodecException(
                    format("Unexpected simple content for field %s at path %s: %s", field.toString(), path, content.getLeft()));
        } else if (meta instanceof XmlFieldMeta.XmlAny) { // any element whatsoever! we encode as text
            logger.trace("[any] : {} ({})", field.getType(), path);
            String text = cursorToString(cursor);
            return Either.right(new Result(new Text(text), Stream.empty()));
        } else {
            throw new CodecException(
                    format("Unexpected metadata for field %s at path %s.", field.toString(), path));
        }
    }

    // try to decode an element
    private Either<Set<ParseFailure>, Result> decodeElement(Stream<Node> cursor, String elemName, DamlType fieldType, String parentPath) {
        Option<Node> node = cursor.headOption();
        if (!node.isEmpty()) {
            Node fieldNode = node.get();
            if (fieldNode.getNodeType()==Element.ELEMENT_NODE &&
                    compareNameWithoutNS(fieldNode, elemName)) {
                String path = parentPath + "/" + elemName;
                Element elem = (Element)fieldNode;
                // FIXME use hasAttributeNodeNS/getAttributeNodeNS
                if(elem.hasAttribute("xsi:type")) {
                    DamlType subType = DamlTypes.record(elem.getAttribute("xsi:type"));
                    Value value = liftValue(getNodeValue(fieldNode, subType, path), subType, fieldType);
                    return Either.right(new Result(value, cursor.tail()));
                } else {
                    Value value = getNodeValue(fieldNode, fieldType, path);
                    return Either.right(new Result(value, cursor.tail()));
                }
            }
        }
        return Either.left(Collections.singleton(new MissingElement(elemName, parentPath)));
    }

    // try to decode an attribute
    private Either<Set<ParseFailure>, Value> decodeAttribute(Element parentElem, String attrName, DamlType fieldType, String parentPath) {
        Optional<Node> fieldNode = Optional.ofNullable(parentElem.getAttributeNode(attrName));
        if (fieldType.isList()) {
            throw new UnsupportedOperationException();
        } else if (fieldType.isOptional()) {
            Optional<Value> x = fieldNode.map(n -> getNodeValue(n, fieldType.asOptional().getNestedType(), parentPath));
            if (x.isPresent()) {
                return Either.right(DamlTypes.Optional.some(x.get()));
            } else {
                return Either.right(DamlTypes.Optional.NONE);
            }
        } else {
            Optional<Value> x = fieldNode.map(n -> getNodeValue(n, fieldType, parentPath));
            if (x.isPresent()) {
                return Either.right(x.get());
            } else {
                return Either.left(Collections.singleton(new MissingAttribute(attrName, parentPath)));
            }
        }
    }

    // decode a choice of elements
    // NOTE: that some parses are ambiguous, e.g. a choice between two possibly empty lists. If both elements are absent
    // in the document, we have to pick an arbitrary empty value for the sum type representing the choice.
    private Either<Set<ParseFailure>, Result> decodeChoice(Element parentElem, Stream<Node> cursor, DamlType fieldType, String path) {
        logger.trace("[choice] : {} ({})", fieldType, path);
        Set<ParseFailure> errs = new HashSet<>();
        List<Value> emptyValues = new ArrayList<>();
        for (FieldMetadata<XmlFieldMeta> childField : metadata.getFields(fieldType)) {
            Either<Set<ParseFailure>, Result> res = decodeField(parentElem, cursor, childField, path);
            if (res.isRight() && !cursor.headOption().equals(res.get().cursor.headOption())) {
                return res.map(r -> r.mapValue(v -> new Variant(childField.getName(), v)));
            } else if (res.isRight()) {
                emptyValues.add(new Variant(childField.getName(), res.get().getValue()));
            } else {
                errs.addAll(res.getLeft());
            }
        }
        if(emptyValues.isEmpty()) {
            return Either.left(errs);
        } else {
            // arbitrarily pick the first empty value
            return Either.right(new Result(emptyValues.get(0),cursor));
        }
    }

    // decode a sequence of elements
    private Either<Set<ParseFailure>, Result> decodeSequence(Element parentElem, Stream<Node> startCursor, DamlType fieldType, String path) {
        logger.trace("[sequence] : {} ({})", fieldType, path);
        List<Record.Field> fields = new ArrayList<>();
        Stream<Node> cursor = startCursor;
        for (FieldMetadata<XmlFieldMeta> childField : metadata.getFields(fieldType)) {
           Either<Set<ParseFailure>, Result> res = decodeField(parentElem, cursor, childField, path);
           if(res.isRight()) {
               fields.add(new Record.Field(res.get().getValue()));
               cursor = res.get().getCursor();
           } else {
               return res;
           }
        }
        return Either.right(new Result(new Record(fields), cursor));
    }

    // decode an element that is potentially a member of a substitution group
    private Either<Set<ParseFailure>, Result> decodeSubstitution(String expectedElemName, Stream<Node> cursor, DamlType elemType, String path) {
        Map<String, DamlType> groups = metadata.getSubstitutionGroups(expectedElemName);
        Option<Node> nextNode = cursor.headOption();
        if(!nextNode.isEmpty() && nextNode.get() instanceof Element) {
            Element nextElem = (Element) nextNode.get();
            Optional<DamlType> nextType = Optional.ofNullable(groups.get(nextElem.getTagName()));
            if(nextType.isPresent()) {
                // type of next element is a substituted element
                Either<Set<ParseFailure>, Result> res = decodeElement(cursor, nextElem.getTagName(), nextType.get(), path);
                if (res.isRight()) {
                    return res.map(r -> r.mapValue(v ->
                            subsValue(v, nextElem.getTagName(), elemType)));
                }
                return res;
            } else if (expectedElemName.equals(nextElem.getTagName())) {
                // trivial case, not a substitution
                return decodeElement(cursor, expectedElemName, elemType, path);
            }
        }
        return Either.left(Collections.singleton(new MissingElement(expectedElemName, path)));
    }

    // For substituted elements, we need to substitute a value (possibly a "subtype")
    // into the variant "basetype" by wrapping it in the appropriate constructor.
    // Uses the XML element metadata to determine the correct alternative.
    private Value subsValue(Value v, String elemName, DamlType baseType) {
        if(!baseType.isVariant()) {
            throw new CodecException("Supplied base type is not a variant: " + baseType.toString());
        }
        Optional<FieldMetadata<XmlFieldMeta>> fieldOpt = metadata.getFields(baseType).stream()
                .filter(f -> elemName.equals(((XmlFieldMeta.XmlElement)f.getSrcMeta()).name))
                .findFirst();
        if (fieldOpt.isPresent()) {
            FieldMetadata<XmlFieldMeta> field = fieldOpt.get();
            return new Variant(field.getName(), v);
        }
        throw new CodecException(
                format("Could not find matching constructor in %s for element %s.", baseType.toString(), elemName));
    }

    // For substituted types, e.g. elements with an "xsi:type" attribute, we need to lift a value
    // of a subtype into the required basetype by finding a suitable constructor, based on the type.
    private Value liftValue(Value v, DamlType subType, DamlType baseType) {
        Optional<FieldMetadata<XmlFieldMeta>> fieldOpt = metadata.getFields(baseType).stream()
                .filter(f -> f.getType().equals(subType))
                .findFirst();
        if (fieldOpt.isPresent()) {
            FieldMetadata<XmlFieldMeta> field = fieldOpt.get();
            return new Variant(field.getName(), v);
        }
        throw new CodecException(
                "Could not find matching constructor in " + baseType.toString() + " for " + subType.toString());
    }

    private Either<Set<ParseFailure>, Result> applyCardinality(
            Function2<Stream<Node>, DamlType, Either<Set<ParseFailure>, Result>> decoder,
            Stream<Node> startCursor,
            DamlType elementType,
            FieldMetadata.Cardinality cardinality) {

        if (cardinality.equals(FieldMetadata.Cardinality.MANY) ||
                cardinality.equals(FieldMetadata.Cardinality.MANY1)) {
            Stream<Node> cursor = startCursor;
            Stream<Node> prevCursor = startCursor;
            List<Value> values = new ArrayList<>();
            Set<ParseFailure> errs = new HashSet<>();
            @Nullable Either<Set<ParseFailure>, Result> res;
            do {
                res = decoder.apply(cursor, elementType);
                if (res.isRight()) {
                    prevCursor = cursor;
                    cursor = res.get().getCursor();
                    if (cardinality.equals(FieldMetadata.Cardinality.MANY)) {
                        // Only add element if we have made progress.
                        // For MANY, don't add empty elements, prefer empty lists.
                        if (!cursor.equals(prevCursor)) {
                            values.add(res.get().getValue());
                        }
                    } else {
                        // for MANY1, we allow a singleton of an empty item
                        values.add(res.get().getValue());
                    }
                } else {
                    errs.addAll(res.getLeft());
                }
            } while (res.isRight() && !cursor.equals(prevCursor));

            if (values.isEmpty() && cardinality.equals(FieldMetadata.Cardinality.MANY1)) {
                return Either.left(errs);
            } else {
                return Either.right(new Result(new DamlList(values), cursor));
            }

        } else if (cardinality.equals(FieldMetadata.Cardinality.OPTIONAL)) {
            Either<Set<ParseFailure>, Result> res =
                    decoder.apply(startCursor, elementType);
            if(res.isRight()) {
                return res.map(t -> t.mapValue(DamlTypes.Optional::some));
            }
            return Either.right(new Result(DamlTypes.Optional.NONE, startCursor));
        }
        return decoder.apply(startCursor, elementType);
    }

    private Stream<Node> nodeListToStream(NodeList nodes) {
        return Stream.ofAll(IntStream.range(0, nodes.getLength()).mapToObj(nodes::item));
    }

    private Value getNodeValue(Node node, DamlType fieldType, String path) {
        if (fieldType.isRecord()) {
            return getRecordTypeValue(node, fieldType, path);
        } else if (fieldType.isVariant()) {
            if (metadata.isBaseType(fieldType)) { // non-abstract base type
                return getBaseTypeValue(node, fieldType, path);
            } else if (metadata.isEnumValue(fieldType)) {
                return getEnumValue(node.getTextContent(), fieldType)
                        .getOrElseThrow(s -> new CodecException(s));
            } else if (!nodeHasChildElements(node)){
                return getVariantSimpleTypeValue(node, fieldType, path);
            } else {
                return getVariantTypeValue(node, fieldType, path);
            }
        } else if (fieldType.isPrimitive()) {
            return getPrimitiveTypeValue(node, fieldType.asPrimitive())
                    .getOrElseThrow(s -> new CodecException(s));
        } else if (fieldType.isList()) {
            throw new CodecException("Field type 'List' should be handled outside here: we only expect a nested type. Path: " + path);
        } else if (fieldType.isOptional()) {
            throw new CodecException("Field type 'Maybe' should be handled outside here: we only expect a nested type. Path: " + path);
        }
        throw new CodecException(
                format("Unsupported XML field [path=%s, type=%s]", path, fieldType.toString()));
    }

    private Value getRecordTypeValue(Node node, DamlType damlType, String path) {
        if (node.getNodeType() != Node.ELEMENT_NODE) {
            throw new CodecException(
                format("XML node %s for record type %s was expected to be an element", path, damlType.toString()));
        }
        Element elem = (Element) node;
        Stream<Node> cursor = nodeListToStream(elem.getChildNodes())
                .filter(n -> n.getNodeType() == Node.ELEMENT_NODE);
        return decodeSequence(elem, cursor, damlType, path)
                .getOrElseThrow(s -> new CodecException(
                        "Expected one of: "+s.toString())).getValue();
    }

    private Value getVariantTypeValue(Node node, DamlType damlType, String path) {
        if (node.getNodeType() != Node.ELEMENT_NODE) {
            throw new CodecException(
                format("XML node %s for variant type %s was expected to be an element", path, damlType.toString()));
        }
        Element elem = (Element) node;
        Stream<Node> cursor = nodeListToStream(elem.getChildNodes())
                .filter(n -> n.getNodeType() == Node.ELEMENT_NODE);
        return decodeChoice(elem, cursor, damlType, path)
                .getOrElseThrow(s -> new CodecException("Expected one of " + s.toString())).getValue();

    }

    // special case for a non-abstract base type with an associated "contents" type
    private Value getBaseTypeValue(Node node, DamlType fieldType, String path) {
        if (node.getNodeType() != Node.ELEMENT_NODE) {
            throw new CodecException(
                format("Variant XML node not element [path={},type={}]", path, fieldType));
        }
        Element elem = (Element)node;
        Optional<FieldMetadata<XmlFieldMeta>> fieldOpt = metadata.getFields(fieldType).stream()
                .filter(f -> f.getType().equals(DamlTypes.record(fieldType.getName()+"Contents")))
                .findFirst();
        if (fieldOpt.isPresent()) {
            FieldMetadata<XmlFieldMeta> field = fieldOpt.get();
            return getRecordTypeValue(elem, field.getType(), path);
        }
        throw new CodecException(
                "Could not find base type contents constructor in " + fieldType.toString());
    }

    // Simple types can be primitives, enums or a variant expressing a choice
    // between primitives or enums
    private Either<String,Value> getSimpleTypeValue(Node node, DamlType type, String path) {
        if(type.isPrimitive()) {
            return getPrimitiveTypeValue(node, type.asPrimitive());
        } else if(type.isVariant()) {
            if (metadata.isEnumValue(type)) {
                return getEnumValue(node.getTextContent(), type);
            } else {
                return Either.right(getVariantSimpleTypeValue(node, type, path));
            }
        }
        return Either.left("Unexpected simple type: "+type);
    }

    // A simple type value that is a variant consisting of a primitive, enum or a mixture of both.
    private Value getVariantSimpleTypeValue(Node node, DamlType type, String path) {
        Set<String> errs = new HashSet<>();
        for (FieldMetadata<XmlFieldMeta> field : metadata.getFields(type)) {
            Either<String, Value> val = getSimpleTypeValue(node, field.getType(),path);
            if(val.isLeft()) {
                errs.add(val.getLeft());
            } else {
                return liftValue(val.get(), field.getType(), type);
            }
        }
        throw new CodecException(
                format("Could not parse simple type value at %s. Expected one of %s.", path, errs.toString()));
    }

    private Either<String,Value> getEnumValue(String text, DamlType fieldType) {
        for (FieldMetadata<XmlFieldMeta> childField : metadata.getFields(fieldType)) {
            String enumName = ((XmlFieldMeta.XmlEnum) childField.getSrcMeta()).name;
            if (enumName.equals(text)) {
                return Either.right(new Variant(childField.getName(), Unit.getInstance()));
            }
        }
        return Either.left(format("Unable to match enum value of %s with type %s.", text, fieldType.toString()));
    }

    private Either<String,Value> getPrimitiveTypeValue(Node node, DamlTypes.Primitive type) {
        // for a Maybe field it is legal to get an explicit nil tag
        // eg <ASX_CODE_NEXT_AVAILABLE_DATE xsi:nil="true"/>
        if (isNil(node)) {
            return Either.right(Unit.getInstance());
        } else {
            return nodeToStringMaybe(node)
                    .map(str -> parsePrimitive(type, str))
                    .orElseGet(() -> Either.right(type.defaultDamlValue()));
        }
    }

    // NOTE: These are XML specific parsers.
    // We cannot switch over primType because Java is lacking (no sum types, only enums).
    private Either<String,Value> parsePrimitive(DamlTypes.Primitive primType, String rawValue) {
        if(primType==DamlTypes.TEXT) {
            return Either.right(new Text(rawValue));
        } else if(primType==DamlTypes.BOOL) {
            return Either.right(new Bool(Boolean.valueOf(rawValue)));
        } else if(primType==DamlTypes.INT64) {
            return Either.right(new Int64(Long.valueOf(rawValue)));
        } else if(primType==DamlTypes.DECIMAL) {
            return Either.right(new com.daml.ledger.javaapi.data.Decimal(new BigDecimal(rawValue)));
        } else if(primType==DamlTypes.TIME) {
            TemporalAccessor time = DateTimeFormatter.ISO_DATE_TIME.parseBest(rawValue,
                    ZonedDateTime::from, LocalDateTime::from);
            if (time instanceof ZonedDateTime) {
                return Either.right(new Timestamp(((ZonedDateTime) time).toInstant().getEpochSecond()));
            }
            if (time instanceof LocalDateTime) {
                ZoneId timezone = ZoneId.systemDefault();
                return Either.right(new Timestamp(((LocalDateTime) time).atZone(timezone).toInstant().getEpochSecond()));
            }
            return Either.left("Can't parse time: " + rawValue);
        } else if(primType==DamlTypes.DATE) {
            // NOTE CME FIXML uses an 8-digit non-standard format
            if(rawValue.length()==8) {
                return Either.right(new Date((int)
                        LocalDate.parse(rawValue, DateTimeFormatter.ofPattern( "yyyyMMdd" )).getLong(EPOCH_DAY)));
            }
            return Either.right(new Date((int)
                    LocalDate.parse(rawValue.replaceAll("Z","")).getLong(EPOCH_DAY)));
        } else if(primType==DamlTypes.UNIT) {
            return Either.right(com.daml.ledger.javaapi.data.Unit.getInstance());
        } else {
            return Either.left("Unable to decode primitive value of type: " + primType.toString());
        }
    }

    private boolean isNil(Node node) {
        if (node.getNodeType() != Node.ELEMENT_NODE) {
            return false;
        } else {
            Element elem = (Element)node;
            // FIXME: This is the correct way to extract the nil attribute, but requires a namespace aware XML factory.
            // return Optional.ofNullable(elem.getAttributeNodeNS(XSI_NS, "nil"))
            return Optional.ofNullable(elem.getAttributeNode("xsi:nil"))
                .map(attr -> attr.getValue().equalsIgnoreCase("true"))
                .orElse(false);
        }
    }

    private Optional<String> nodeToStringMaybe(Node elem) {
        return Optional.ofNullable(emptyToNull(elem.getTextContent()));
    }

    private boolean compareNameWithoutNS(Node node, String name) {
        final String COLON_DELIMITED_PREFIX_RE = "[^:]*:";

        String baseName = node.getNodeName().replaceFirst(COLON_DELIMITED_PREFIX_RE, "");
        return baseName.equals(name);
    }

    // used to identify simple types
    private boolean nodeHasChildElements(Node node) {
        return !nodeListToStream(node.getChildNodes())
                   .filter(n -> n.getNodeType()==Node.ELEMENT_NODE)
                   .isEmpty();
    }

     // used to handle XML Any
    private static String cursorToString(Stream<Node> cursors) {
        StringBuilder result = new StringBuilder();
        for(Node node : cursors.toJavaList()) {
            result.append(QueryableXml.nodeToString(node));
        }
        return result.toString();
    }

    private static class Result {
        private final Value value;
        private final Stream<Node> cursor;
        private Result(Value value, Stream<Node> cursor) {
            this.value = value;
            this.cursor = cursor;
        }
        public Value getValue() {
            return value;
        }
        public Stream<Node> getCursor() {
            return cursor;
        }
        public Result mapValue(Function<Value,Value> f) {
            return new Result(f.apply(value), cursor);
        }
    }
    private static abstract class ParseFailure {
        protected final String name;
        protected final String path;
        public ParseFailure(String name, String path) {
            this.name = name;
            this.path = path;
        }
        public String getName() {
            return name;
        }
        public String getPath() {
            return path;
        }
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ParseFailure that = (ParseFailure) o;
            return Objects.equals(name, that.name) &&
                    Objects.equals(path, that.path);
        }
        @Override
        public int hashCode() {
            return Objects.hash(name, path);
        }
    }
    private static class MissingElement extends ParseFailure {
        public MissingElement(String name, String path) {
            super(name, path);
        }
        @Override
        public String toString() {
            return "element(" + name + ") at " + path;
        }
    }
    private static class MissingAttribute extends ParseFailure {
        public MissingAttribute(String name, String path) {
            super(name, path);
        }
        @Override
        public String toString() {
            return "attribute("+name+") at " + path;
        }
    }
}
