// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata;

import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.vavr.collection.Stream;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.digitalasset.integration.internal.codec.metadata.FieldMetadata.field;
import static com.digitalasset.integration.internal.codec.metadata.TypeDeclarationMetadata.record;
import static com.digitalasset.integration.internal.codec.metadata.TypeDeclarationMetadata.variant;

/**
 * Used by the encoding/decoding strategies.
 */
public abstract class MetadataReader<A, B> {

    public Metadata<A, B> fromJSON(byte[] bytes) {
        JsonElement json = new JsonParser().parse(new String(bytes));
        JsonObject meta = json.getAsJsonObject();
        String schemaName = meta.get("schema").getAsString();
        Map<String,TypeDeclarationMetadata<A>> typeDeclarations =
                Stream.ofAll(meta.get("declarations").getAsJsonArray())
                        .map(this::parseTypeDecl)
                        .collect(Collectors.toMap(TypeDeclarationMetadata::getName, Function.identity()));
        B topLevelMeta = parseTopLevelMeta(meta.get("top_level_meta"));
        return new Metadata<>(schemaName, typeDeclarations, topLevelMeta);
    }

    protected TypeDeclarationMetadata<A> parseTypeDecl(JsonElement elem) {
        JsonObject decl = elem.getAsJsonObject();
        String kind = decl.get("kind").getAsString();
        String name = decl.get("name").getAsString();
        List<FieldMetadata<A>> fields =
                Stream.ofAll(decl.get("fields").getAsJsonArray())
                      .map(this::parseField)
                      .collect(Collectors.toList());
        switch(kind) {
            case "record"  : return record(name, fields);
            case "variant" : return variant(name, fields);
        }
        throw new IllegalStateException("Could not parse metadata declaration kind: "+kind);
    }

    protected FieldMetadata<A> parseField(JsonElement elem) {
        JsonObject decl = elem.getAsJsonObject();
        String name = decl.get("name").getAsString();
        FieldMetadata.Cardinality card = parseCardinality(decl.get("cardinality"));
        DamlType type = applyCardinality(parseDamlType(decl.get("type")), card);
        A meta = parseFieldMeta(decl.get("meta").getAsJsonObject());
        return field(name, type, card, meta);
    }

    private DamlType applyCardinality(DamlType type, FieldMetadata.Cardinality card) {
        // adjust the Daml type to match the cardinality
        switch (card) {
            case MANY : return DamlTypes.list(type);
            case MANY1 : return DamlTypes.list(type);
            case SINGLE: return type;
            case OPTIONAL: return DamlTypes.optional(type);
        }
        throw new IllegalStateException("Unrecognised metadata cardinality: "+card.toString());
    }

    protected DamlType parseDamlType(JsonElement elem) {
        JsonObject type = elem.getAsJsonObject();
        String name = type.get("name").getAsString();
        String kind = type.get("kind").getAsString();
        switch(kind) {
            case "record":  return DamlTypes.record(name);
            case "variant": return DamlTypes.variant(name);
            case "prim":    return parsePrimType(name);
        }
        throw new IllegalStateException("Could not parse metadata type kind: "+kind);
    }

    protected DamlType parsePrimType(String name) {
        switch (name) {
            case "TEXT": return DamlTypes.TEXT;
            case "BOOL": return DamlTypes.BOOL;
            case "INT64": return DamlTypes.INT64;
            case "DECIMAL": return DamlTypes.DECIMAL;
            case "TIME": return DamlTypes.TIME;
            case "DATE": return DamlTypes.DATE;
            case "UNIT": return DamlTypes.UNIT;
            case "PARTY": return DamlTypes.PARTY;
        }
        throw new IllegalStateException("Could not parse metadata primitive type name: "+name);
    }

    protected FieldMetadata.Cardinality parseCardinality(JsonElement elem) {
        String card = elem.getAsString();
        switch(card) {
            case "MANY"     : return FieldMetadata.Cardinality.MANY;
            case "MANY1"    : return FieldMetadata.Cardinality.MANY1;
            case "OPTIONAL" : return FieldMetadata.Cardinality.OPTIONAL;
            case "SINGLE"   : return FieldMetadata.Cardinality.SINGLE;
        }
        throw new IllegalStateException("Could not parse metadata cardinality: "+card);
    }

    protected abstract A parseFieldMeta(JsonElement element);
    protected abstract B parseTopLevelMeta(JsonElement element);

}
