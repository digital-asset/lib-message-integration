// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.integration.internal.codec.metadata.xsd;

import com.digitalasset.integration.internal.codec.metadata.DamlTypes;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class XmlTopLevelMeta {

    private final Map<String, DamlTypes.DamlType> topElementTypes;
    private final Map<String, List<String>> topElementSubs;
    private final Set<DamlTypes.DamlType> baseTypes;
    private final Set<DamlTypes.DamlType> abstractTypes;
    private final String namespace;
    private final Optional<String> schemaVersion;
    private final Optional<String> schemaLocation;

    public XmlTopLevelMeta(Map<String, DamlTypes.DamlType> topElementTypes, Map<String, List<String>> topElementSubs, Set<DamlTypes.DamlType> baseTypes, Set<DamlTypes.DamlType> abstractTypes, String namespace, Optional<String> schemaVersion, Optional<String> schemaLocation) {
        this.topElementTypes = topElementTypes;
        this.topElementSubs = topElementSubs;
        this.baseTypes = baseTypes;
        this.abstractTypes = abstractTypes;
        this.namespace = namespace;
        this.schemaVersion = schemaVersion;
        this.schemaLocation = schemaLocation;
    }

    public Map<String, DamlTypes.DamlType> getTopElementTypes() {
        return topElementTypes;
    }

    public Map<String, List<String>> getTopElementSubs() {
        return topElementSubs;
    }

    public Set<DamlTypes.DamlType> getBaseTypes() {
        return baseTypes;
    }

    public Set<DamlTypes.DamlType> getAbstractTypes() {
        return abstractTypes;
    }

    public String getNamespace() {
        return namespace;
    }

    public Optional<String> getSchemaVersion() {
        return schemaVersion;
    }

    public Optional<String> getSchemaLocation() {
        return schemaLocation;
    }
}
