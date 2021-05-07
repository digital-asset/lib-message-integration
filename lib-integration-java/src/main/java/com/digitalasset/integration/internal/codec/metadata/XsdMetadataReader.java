// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
//import io.grpc.internal.IoUtils;
import com.google.common.io.ByteStreams;
import io.vavr.collection.Stream;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Used by the XSD encoding/decoding strategies.
 */
public class XsdMetadataReader extends MetadataReader<XmlFieldMeta, XmlTopLevelMeta> {

    public static XsdMetadata fromJSON(URL url) throws IOException {
        URLConnection urlConnection = url.openConnection();
        InputStream is = urlConnection.getInputStream();
        if (is != null) {
           //return new XsdMetadata(new XsdMetadataReader().fromJSON(IoUtils.toByteArray(is)));
           return new XsdMetadata(new XsdMetadataReader().fromJSON(ByteStreams.toByteArray(is)));
        }
        throw new IllegalStateException("Could not load resource from url: "+url);
    }

    @Override
    protected XmlFieldMeta parseFieldMeta(JsonElement elem) {
        JsonObject meta = elem.getAsJsonObject();
        String src = meta.get("src").getAsString();
        switch(src) {
            case "element": return new XmlFieldMeta.XmlElement(meta.get("name").getAsString());
            case "element-ref": return new XmlFieldMeta.XmlElementRef(meta.get("name").getAsString());
            case "attribute": return new XmlFieldMeta.XmlAttribute(meta.get("name").getAsString());
            case "enum": return new XmlFieldMeta.XmlEnum(meta.get("name").getAsString());
            case "choice": return XmlFieldMeta.XmlChoice.INSTANCE;
            case "sequence": return XmlFieldMeta.XmlSequence.INSTANCE;
            case "content": return XmlFieldMeta.XmlContent.INSTANCE;
            case "any": return XmlFieldMeta.XmlAny.INSTANCE;
        }
        throw new IllegalStateException("Could not parse XML field metadata src: "+src);
    }

    @Override
    protected XmlTopLevelMeta parseTopLevelMeta(JsonElement elem) {
        JsonObject meta = elem.getAsJsonObject();
        Map<String, DamlTypes.DamlType> topElementTypes = parseTopElementTypes(meta.get("top_level_elements"));
        Map<String, List<String>> topElementSubs = parseTopElementSubs(meta.get("top_level_substs"));
        Set<DamlTypes.DamlType> baseTypes = parseSetOfTypes(meta.get("base_types"));
        Set<DamlTypes.DamlType> abstractTypes = parseSetOfTypes(meta.get("abstract_types"));
        String namespace = meta.get("namespace").getAsString();
        Optional<String> schemaVersion = getAsOptionalString(meta.get("schema_version"));
        Optional<String> schemaLocation = getAsOptionalString(meta.get("schema_location"));
        return new XmlTopLevelMeta(topElementTypes,topElementSubs,baseTypes,abstractTypes,namespace,schemaVersion,schemaLocation);
    }

    protected Optional<String> getAsOptionalString(JsonElement e) {
        return e.isJsonNull()?Optional.empty():Optional.of(e.getAsString());
    }

    protected Map<String, DamlTypes.DamlType> parseTopElementTypes(JsonElement elem) {
        return elem.getAsJsonObject().entrySet().stream()
                .collect(Collectors.toMap(e -> e.getKey(), e -> parseDamlType(e.getValue())));
    }

    protected Map<String, List<String>> parseTopElementSubs(JsonElement elem) {
        return elem.getAsJsonObject().entrySet().stream()
                .collect(Collectors.toMap(e -> e.getKey(),
                        e -> Stream.ofAll(e.getValue().getAsJsonArray())
                                .map(JsonElement::getAsString)
                                .collect(Collectors.toList())));
    }

    protected Set<DamlTypes.DamlType> parseSetOfTypes(JsonElement elem) {
        return Stream.ofAll(elem.getAsJsonArray())
                .map(this::parseDamlType)
                .collect(Collectors.toSet());
    }
}
