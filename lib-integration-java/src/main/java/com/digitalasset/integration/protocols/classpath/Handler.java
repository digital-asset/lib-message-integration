// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.integration.protocols.classpath;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/**
 * A {@link URLStreamHandler} that handles resources on the classpath.
 *
 * To install the handler, set the system property:
 *    -Djava.protocol.handler.pkgs=com.digitalasset.integration.protocols
 *
 * To manually construct URLs that will be handled with this handler, use:
 *
 * new URL(null, "classpath:some/package/resource.extension",
 *    new com.digitalasset.integration.protocols.classpath(ClassLoader.getSystemClassLoader()))
 *
 * */
public class Handler extends URLStreamHandler {
    /** The classloader to find resources from. */
    private final ClassLoader classLoader;

    public Handler() {
        this.classLoader = getClass().getClassLoader();
    }

    public Handler(ClassLoader classLoader) {
        this.classLoader = classLoader;
    }

    @Override
    protected URLConnection openConnection(URL u) throws IOException {
        final URL resourceUrl = classLoader.getResource(u.getPath());
        return resourceUrl.openConnection();
    }
}