// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata;

import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;

import javax.annotation.concurrent.Immutable;

@Immutable
public class FieldMetadata<SrcMeta> {

    private final String name;
    private final DamlType type;
    private final Cardinality cardinality;
    private final SrcMeta srcMeta;

    // Both many and many1 map to lists, so we also track cardinality here, not
    // just in the DAML types.
    public enum Cardinality {
        OPTIONAL, SINGLE, MANY, MANY1
    }

    FieldMetadata(String name, DamlType type, Cardinality cardinality, SrcMeta srcMeta) {
        this.name = name;
        this.type = type;
        this.cardinality = cardinality;
        this.srcMeta = srcMeta;
    }

    public static <SrcMeta> FieldMetadata<SrcMeta> field(String name, DamlType type, Cardinality cardinality, SrcMeta srcMeta) {
        return new FieldMetadata(name, type, cardinality, srcMeta);
    }

    public String getName() {
        return name;
    }

    public DamlType getType() {
        return type;
    }

    public Cardinality getCardinality() {
        return cardinality;
    }

    public SrcMeta getSrcMeta() {
        return srcMeta;
    }

    @Override
    public String toString() {
        return "FieldMetadata{" +
                "name='" + name + '\'' +
                ", type=" + type + '\'' +
                ", cardinality=" + cardinality + '\'' +
                ", srcMeta=" + srcMeta +
                '}';
    }
}
