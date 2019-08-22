// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.integration.internal.codec.strategies;

import com.daml.ledger.javaapi.data.*;
import com.daml.ledger.javaapi.data.Value;
import com.digitalasset.integration.api.codec.strategies.DecodeStrategy;
import com.digitalasset.integration.internal.codec.metadata.*;
import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;
import com.digitalasset.integration.internal.codec.metadata.cdm.CdmFieldMeta;

import com.google.common.base.CaseFormat;
import com.google.common.base.Converter;
import io.vavr.*;
import io.vavr.control.Either;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.time.LocalDateTime;
import java.util.*;

import com.google.gson.JsonElement;

import static com.google.common.base.Strings.emptyToNull;
import static java.lang.String.format;
import static java.time.temporal.ChronoField.EPOCH_DAY;

import java.util.ArrayList;

/**
 * A simple JSON decoder for CDM.
 */
public class JsonCdmDecodeStrategy implements DecodeStrategy<JsonElement> {

    private static final Logger logger = LoggerFactory.getLogger(JsonCdmDecodeStrategy.class);

    private final Metadata<CdmFieldMeta,Void> metadata;

    public JsonCdmDecodeStrategy(Metadata<CdmFieldMeta,Void> metadata) {
        this.metadata = metadata;
    }

    @Override
    public Value decode(JsonElement messageContents, DamlType damlType) {
        return decodeRecord(messageContents, damlType, "");
    }

    private Record decodeRecord(JsonElement elem, DamlType damlType, String path) {
        List<Record.Field> fields = new ArrayList<>();
        for (FieldMetadata<CdmFieldMeta> childField : metadata.getFields(damlType)) {
            Either<String, Value> res = decodeField(elem, childField, path);
            if(res.isRight()) {
                fields.add(new Record.Field(res.get()));
            } else {
                throw new IllegalStateException(res.getLeft());
            }
        }
        return new Record(fields);
    }

     // try to decode a field using the provided CDM schema metadata
    private Either<String, Value> decodeField(
            JsonElement parentElem,
            FieldMetadata<CdmFieldMeta> field,
            String parentPath) {

        CdmFieldMeta meta = field.getSrcMeta();
        if(meta instanceof CdmFieldMeta.CdmField) {
            String elemName = ((CdmFieldMeta.CdmField) meta).name;
            String path = parentPath + "/" + elemName;
            logger.trace("[field: {}] : {} ({})", elemName, field.getType(), path);
            Optional<JsonElement> fieldValue = Optional.ofNullable(parentElem.getAsJsonObject().get(elemName));
            return applyCardinality((el, ty) -> decodeElement(el, ty, path),
                    fieldValue, field.getType().getElementType(), field.getCardinality(), path);
        } else {
            throw new IllegalStateException(
                    format("Unexpected metadata for field %s at path %s.", field.toString(), parentPath));
        }
    }

    private Either<String, Value> decodeElement(JsonElement elem, DamlType ty, String path) {
        if (ty.isRecord() && ty.getName().equals("ZonedDateTime")) {
            return Either.right(decodeZonedDateTime(elem));
        } else if (ty.isRecord()) {
            return Either.right(decodeRecord(elem, ty, path));
        } else if (ty.isVariant()) {
            if (metadata.isEnumValue(ty)) {
                return Either.right(decodeEnum(elem, ty, path));
            } else { // we are only expecting enums as variants currently
                return Either.left("Unexpected variant at "+path);
            }
        } else if (ty.isPrimitive()) {
            return Either.right(parsePrimitive(ty.asPrimitive(), elem));
        } else if (ty.isList()) {
            throw new IllegalStateException("Field type 'List' should be handled outside here: we only expect a nested type. Path: " + path);
        } else if (ty.isOptional()) {
            throw new IllegalStateException("Field type 'Optional' should be handled outside here: we only expect a nested type. Path: " + path);
        }
        throw new IllegalStateException(
                format("Unsupported field [path=%s, type=%s]", path, ty.toString()));
    }

     // NOTE: enums seem to be specified using camelcase, but sometimes appear in uppercase underscored
     // format. For now, we try both.
     private Value decodeEnum(JsonElement elem, DamlType ty, String path) {
        String text = decodeEnumText(elem.getAsString());
        Converter<String, String> converter = CaseFormat.UPPER_UNDERSCORE.converterTo(CaseFormat.UPPER_CAMEL);
        for (FieldMetadata<CdmFieldMeta> childField : metadata.getFields(ty)) {
            String enumName = ((CdmFieldMeta.CdmEnum) childField.getSrcMeta()).name;
            if (enumName.equals(text) || enumName.equals(converter.convert(text))) {
                    return new com.daml.ledger.javaapi.data.DamlEnum(childField.getName());
            }
        }
        throw new IllegalStateException(
                format("Unable to match enum value of '%s' with type %s at %s.", text, ty.toString(), path));
    }

    private String decodeEnumText(String text) {
        // return text.replaceAll("[ :/\\\\\\.\\((\\)]","_");
        return text;
    }

    private Value decodeZonedDateTime(JsonElement elem) {
        String rawValue = elem.getAsString();
        TemporalAccessor time = DateTimeFormatter.ISO_DATE_TIME.parseBest(rawValue,
                ZonedDateTime::from, LocalDateTime::from);
        if (time instanceof ZonedDateTime) {
            List<Record.Field> fields = new ArrayList<>();
            fields.add(0, new Record.Field("dateTime", new Timestamp(((ZonedDateTime) time).toInstant().getEpochSecond()))); // FIXME:
            fields.add(0, new Record.Field("timezone", new Text(((ZonedDateTime) time).getZone().getId())));
            return new Record(fields);
        }
        if (time instanceof LocalDateTime) {
            List<Record.Field> fields = new ArrayList<>();
            fields.add(0, new Record.Field("dateTime", new Timestamp(((ZonedDateTime) time).toInstant().getEpochSecond()))); // FIXME:
            fields.add(0, new Record.Field("timezone", new Text("UTC")));
            return new Record(fields);
        }
        throw new IllegalArgumentException("Can't parse datetime: " + rawValue);
    }

    private Either<String, Value> applyCardinality(
            Function2<JsonElement, DamlType, Either<String, Value>> decoder,
            Optional<JsonElement> parentElem,
            DamlType elemType,
            FieldMetadata.Cardinality cardinality,
            String path) {

        if (cardinality.equals(FieldMetadata.Cardinality.MANY) ||
            cardinality.equals(FieldMetadata.Cardinality.MANY1)) {
            List<Value> values = new ArrayList<>();
            Iterable<JsonElement> iterable =
                    parentElem.isPresent()?parentElem.get().getAsJsonArray():Collections.emptyList();
            for(JsonElement elem : iterable) {
               Either<String, Value> res = decoder.apply(elem, elemType);
               if(res.isRight()) {
                   values.add(res.get());
               } else {
                   return res;
               }
            }
            if(cardinality.equals(FieldMetadata.Cardinality.MANY1) && values.isEmpty()) {
                return Either.left("Unexpected empty array at " + path);
            } else {
                return Either.right(new DamlList(values));
            }

        } else if (cardinality.equals(FieldMetadata.Cardinality.OPTIONAL)) {
            if (parentElem.isPresent()) {
                return decoder.apply(parentElem.get(), elemType)
                        .map(DamlTypes.Optional::some);
            } else {
                return Either.right(DamlTypes.Optional.NONE);
            }
        } else if(parentElem.isPresent()) {
            return decoder.apply(parentElem.get(), elemType);
        } else {
            return Either.left("Missing value at "+path);
        }
    }

    // NOTE: These are CDM JSON specific parsers.
    // We cannot switch over primType because Java is lacking (no sum types, only enums).
    private Value parsePrimitive(DamlTypes.Primitive primType, JsonElement elem) {
        if(primType==DamlTypes.TEXT) {
            return new Text(emptyToNull(elem.getAsString()));
        } else if (primType==DamlTypes.PARTY) {
            return new Party(elem.getAsString());
        } else if(primType==DamlTypes.BOOL) {
            return new Bool(elem.getAsBoolean());
        } else if(primType==DamlTypes.INT64) {
            return new Int64(elem.getAsLong());
        } else if(primType==DamlTypes.DECIMAL) {
            return new com.daml.ledger.javaapi.data.Decimal(elem.getAsBigDecimal());
        } else if(primType==DamlTypes.TIME) {
            String rawValue = elem.getAsString();
            TemporalAccessor time = DateTimeFormatter.ISO_DATE_TIME.parseBest(rawValue,
                    ZonedDateTime::from, LocalDateTime::from);
            if (time instanceof ZonedDateTime) {
                return new Timestamp(((ZonedDateTime) time).toInstant().getEpochSecond());
            }
            if (time instanceof LocalDateTime) {
                ZoneId timezone = ZoneId.systemDefault();
                return new Timestamp(((LocalDateTime) time).atZone(timezone).toInstant().getEpochSecond());
            }
            throw new IllegalArgumentException("Can't parse datetime: " + rawValue);
        } else if(primType==DamlTypes.DATE) {
            return new com.daml.ledger.javaapi.data.Date((int)
                    LocalDate.parse(elem.getAsString().replaceAll("Z","")).getLong(EPOCH_DAY)); // TODO
        } else if(primType==DamlTypes.UNIT) {
            return com.daml.ledger.javaapi.data.Unit.getInstance();
        } else {
            throw new IllegalStateException("Unable to decode primitive value of type: " + primType.toString());
        }
    }
}
