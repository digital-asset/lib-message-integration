// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata;

import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;

import java.util.*;

public class Metadata<A, B> {

    private final String schemaName;
    private final Map<String, TypeDeclarationMetadata<A>> typeDeclarations;
    private final B topLevelMetadata;

    public Metadata(String schemaName, Map<String, TypeDeclarationMetadata<A>> typeDeclarations, B topLevelMetadata) {
        this.schemaName = schemaName;
        this.typeDeclarations = typeDeclarations;
        this.topLevelMetadata = topLevelMetadata;
    }

    public String getSchemaName() {
        return schemaName;
    }

    public Map<String, TypeDeclarationMetadata<A>> getTypeDeclarations() {
        return typeDeclarations;
    }

    public B getTopLevelMetadata() {
        return topLevelMetadata;
    }

    public boolean isEnumValue(DamlType type) {
        return type.isVariant() && getFields(type).stream().map(FieldMetadata::getType).allMatch(DamlTypes.Unit::isUnit);
    }

    public List<FieldMetadata<A>> getFields(DamlType type) {
        if (type.isVariant() || type.isRecord()) {
            return Optional
                    .ofNullable(typeDeclarations.get(type.getName()))
                    .map(TypeDeclarationMetadata::getFields)
                    .orElseThrow(() -> new IllegalArgumentException("Unknown type: " + type));
        } else {
            return Collections.emptyList();
        }
    }

}
