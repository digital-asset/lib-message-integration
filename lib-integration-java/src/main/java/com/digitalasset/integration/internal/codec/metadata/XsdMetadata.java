// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata;

import java.util.ArrayList;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Implemented by metagen XSD output.
 */
public class XsdMetadata extends Metadata<XmlFieldMeta,XmlTopLevelMeta> {

    public XsdMetadata(Metadata<XmlFieldMeta, XmlTopLevelMeta> meta) {
        super(meta.getSchemaName(), meta.getTypeDeclarations(), meta.getTopLevelMetadata());
    }

    public Optional<DamlTypes.DamlType> getElementType(String name) {
        return Optional.ofNullable(getTopLevelMetadata().getTopElementTypes().get(name));
    }

    public Map<String, DamlTypes.DamlType> getSubstitutionGroups(String elemName) {
        return getTopLevelMetadata().getTopElementSubs().getOrDefault(elemName, new ArrayList<>()).stream()
                .collect(Collectors.toMap(
                        Function.identity(),
                        sub -> getTopLevelMetadata().getTopElementTypes().get(sub)));
    }

    public boolean isOfTopLevelSchema(String fieldName) {
        return getTopLevelMetadata().getTopElementTypes().containsKey(fieldName);
    }

    public boolean isBaseType(DamlTypes.DamlType type) {
        return getTopLevelMetadata().getBaseTypes().contains(type);
    }

    public boolean isAbstractType(DamlTypes.DamlType type) {
        return getTopLevelMetadata().getAbstractTypes().contains(type);
    }

    public String getNamespace() {
        return getTopLevelMetadata().getNamespace();
    }

    public Optional<String> getSchemaVersion() {
        return getTopLevelMetadata().getSchemaVersion();
    }

    public Optional<String> getSchemaLocation() {
        return getTopLevelMetadata().getSchemaLocation();
    }
}
