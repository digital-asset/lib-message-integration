// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata.cdm;

import com.digitalasset.integration.internal.codec.metadata.Metadata;
import com.digitalasset.integration.internal.codec.metadata.MetadataReader;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

/**
 * Used by the XSD encoding/decoding strategies.
 */
public class CdmMetadataReader extends MetadataReader<CdmFieldMeta, Void> {

    public static Metadata<CdmFieldMeta, Void> fromJSON(URL url) throws IOException {
        URLConnection urlConnection = url.openConnection();
        InputStream is = urlConnection.getInputStream();
        if (is != null) {
           return new CdmMetadataReader().fromJSON(IOUtils.toByteArray(is));
        }
        throw new IllegalStateException("Could not load resource from url: "+url);
    }

    @Override
    protected CdmFieldMeta parseFieldMeta(JsonElement elem) {
        JsonObject meta = elem.getAsJsonObject();
        String src = meta.get("src").getAsString();
        switch(src) {
            case "field": return new CdmFieldMeta.CdmField(meta.get("name").getAsString());
            case "enum": return new CdmFieldMeta.CdmEnum(meta.get("name").getAsString());
        }
        throw new IllegalStateException("Could not parse CDM field metadata src: "+src);
    }

    @Override
    protected Void parseTopLevelMeta(JsonElement elem) {
        return null;
    }

}
