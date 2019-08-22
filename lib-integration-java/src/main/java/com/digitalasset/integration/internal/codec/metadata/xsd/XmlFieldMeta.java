// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata.xsd;

import java.util.Objects;

public abstract class XmlFieldMeta {

    public static final class XmlElement extends XmlFieldMeta {
        public final String name;

        public XmlElement(String name) {
            this.name = name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            XmlElement that = (XmlElement) o;
            return Objects.equals(name, that.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }
    }

    public static final class XmlElementRef extends XmlFieldMeta {
        public final String name;

        public XmlElementRef(String name) {
            this.name = name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            XmlElementRef that = (XmlElementRef) o;
            return Objects.equals(name, that.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }
    }

    public static final class XmlAttribute extends XmlFieldMeta {
        public final String name;

        public XmlAttribute(String name) {
            this.name = name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            XmlAttribute that = (XmlAttribute) o;
            return Objects.equals(name, that.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }
    }

    public static final class XmlEnum extends XmlFieldMeta {
        public final String name;

        public XmlEnum(String name) {
            this.name = name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            XmlEnum that = (XmlEnum) o;
            return Objects.equals(name, that.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }
    }

    public static final class XmlChoice extends XmlFieldMeta {
        public static XmlChoice INSTANCE = new XmlChoice();
    }

    public static final class XmlSequence extends XmlFieldMeta {
        public static XmlSequence INSTANCE = new XmlSequence();
    }

    public static final class XmlContent extends XmlFieldMeta {
        public static XmlContent INSTANCE = new XmlContent();
    }

    public static final class XmlAny extends XmlFieldMeta {
        public static XmlAny INSTANCE = new XmlAny();
    }
}
