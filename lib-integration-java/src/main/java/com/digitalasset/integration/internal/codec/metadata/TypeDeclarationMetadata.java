// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata;

import java.util.List;
import java.util.Objects;
import javax.annotation.Nonnull;
import javax.annotation.concurrent.Immutable;

@Immutable
public class TypeDeclarationMetadata<SrcMeta> {

    public enum Kind {RECORD, VARIANT}

    private final String name;
    private final Kind kind;
    private final List<FieldMetadata<SrcMeta>> fields;

    private TypeDeclarationMetadata(String name, Kind kind, List<FieldMetadata<SrcMeta>> fields) {
        this.name = name;
        this.kind = kind;
        this.fields = fields;
    }

    public static <SrcMeta> TypeDeclarationMetadata<SrcMeta> record(String name, List<FieldMetadata<SrcMeta>> fields) {
        return new TypeDeclarationMetadata(name, Kind.RECORD, fields);
    }

    public static <SrcMeta> TypeDeclarationMetadata<SrcMeta> variant(String name, List<FieldMetadata<SrcMeta>> fields) {
        return new TypeDeclarationMetadata(name, Kind.VARIANT, fields);
    }

    public String getName() {
        return name;
    }

    @Nonnull
    public Kind getKind() {
        return kind;
    }

    public List<FieldMetadata<SrcMeta>> getFields() {
        return fields;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        TypeDeclarationMetadata<SrcMeta> that = (TypeDeclarationMetadata<SrcMeta>) other;
        return Objects.equals(name, that.name) && Objects.equals(kind, that.kind) && Objects.equals(fields, that.fields);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, kind, fields);
    }

    @Override
    public String toString() {
        return "TypeDeclarationMetadata{" +
                "name='" + name + '\'' +
                ", kind='" + kind + '\'' +
                ", fields=" + fields +
                '}';
    }
}
