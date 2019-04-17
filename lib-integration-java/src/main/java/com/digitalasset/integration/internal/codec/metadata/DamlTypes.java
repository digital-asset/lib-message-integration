// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec.metadata;

import static com.google.common.base.Preconditions.checkNotNull;

import com.daml.ledger.javaapi.data.*;

import java.math.BigDecimal;
import java.time.*;
import java.util.Objects;
import java.util.Optional;

public final class DamlTypes {

    private static final long NUMBER_LONG = -1;

    private DamlTypes() {
    }

    public static final Text TEXT = Text.INSTANCE;
    public static final Bool BOOL = Bool.INSTANCE;
    public static final Integer INT64 = Integer.INSTANCE;
    public static final Decimal DECIMAL = Decimal.INSTANCE;
    public static final Time TIME = Time.INSTANCE;
    public static final Date DATE = Date.INSTANCE;
    public static final Unit UNIT = Unit.INSTANCE;
    public static final Party PARTY = Party.INSTANCE;
    public static final Unsupported UNSUPPORTED = Unsupported.INSTANCE;

    public static List list(DamlType elementType) {
        return new List(elementType);
    }

    public static Optional optional(DamlType elementType) {
         return new Optional(elementType);
    }

    public static Variant variant(String name) {
        return new Variant(name);
    }

    public static Record record(String name) {
        return new Record(name);
    }

    public static abstract class DamlType {
        private final String name;

        private DamlType(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public boolean isPrimitive() {
            return is(Primitive.class);
        }

        public boolean isList() {
            return is(List.class);
        }

        public boolean isOptional() {
            return is(Optional.class);
        }

        public boolean isVariant() {
            return is(Variant.class);
        }

        public boolean isRecord() {
            return is(Record.class);
        }

        public boolean isSupported() {
            return !is(Unsupported.class);
        }

        public boolean is(Class<? extends DamlType> iaType) {
            return iaType.isInstance(this);
        }

        public Primitive asPrimitive() {
            return as(Primitive.class);
        }

        public List asList() {
            return as(List.class);
        }

        public Optional asOptional() {
            return as(Optional.class);
        }

        public DamlType getElementType() {
            return this;
        }

        public <T extends DamlType> T as(Class<T> iaType) {
            return iaType.cast(this);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof DamlType)) {
                return false;
            }
            DamlType that = (DamlType) o;
            return Objects.equals(name, that.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }

        @Override
        public String toString() {
            return name;
        }
    }

    public static abstract class Primitive extends DamlType {

        final Value defaultValue;

        private Primitive(String name,
                          Value defaultValue) {
            super(name);
            this.defaultValue = defaultValue;
        }

        public Value defaultDamlValue() {
            return defaultValue;
        }
    }

    public static final class Text extends Primitive {
        private static final Text INSTANCE = new Text();

        private Text() {
            super("Text", new com.daml.ledger.javaapi.data.Text(""));
        }
    }

    public static final class Bool extends Primitive {
        private static final Bool INSTANCE = new Bool();

        private Bool() {
            super("Bool", new com.daml.ledger.javaapi.data.Bool(Boolean.FALSE));
        }
    }

    public static final class Integer extends Primitive {
        private static final Integer INSTANCE = new Integer();

        private Integer() {
            super("Integer", new Int64(Long.valueOf(NUMBER_LONG)));
        }
    }

    public static final class Decimal extends Primitive {
        private static final Decimal INSTANCE = new Decimal();

        private Decimal() {
            super("Decimal",
                    new com.daml.ledger.javaapi.data.Decimal(new BigDecimal(NUMBER_LONG)));
        }
    }

    public static final class Time extends Primitive {
        private static final Time INSTANCE = new Time();

        private Time() {
            super( "Time",
                    new Timestamp(Instant.EPOCH.getEpochSecond()));
        }
    }

    public static final class Date extends Primitive {
        private static final Date INSTANCE = new Date();

        private Date() {
            super("Date",
                    new com.daml.ledger.javaapi.data.Date(0));
        }
    }

    public static final class Unit extends Primitive {
        private static final Unit INSTANCE = new Unit();

        static boolean isUnit(DamlType type) {
            return type instanceof Unit;
        }

        private Unit() {
            super("Unit", com.daml.ledger.javaapi.data.Unit.getInstance());
        }
    }

     public static final class Party extends Primitive {
        private static final Party INSTANCE = new Party();

        private Party() {
            super("Party",
                    new com.daml.ledger.javaapi.data.Party(""));
        }
    }

    public static final class List extends DamlType {
        private final DamlType nestedType;

        private List(DamlType nestedType) {
            super("List");
            this.nestedType = checkNotNull(nestedType);
        }

        public DamlType getNestedType() {
            return nestedType;
        }

        @Override
        public DamlType getElementType() {
            return nestedType.getElementType();
        }

        @Override
        public String toString() {
            return "List<"+getNestedType().toString()+">";
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            if (!super.equals(o)) return false;
            List list = (List) o;
            return Objects.equals(nestedType, list.nestedType);
        }

        @Override
        public int hashCode() {
            return Objects.hash(super.hashCode(), nestedType);
        }
    }

    public static final class Optional extends DamlType {
        private final DamlType nestedType;

        public static final Value NONE = new com.daml.ledger.javaapi.data.DamlOptional(java.util.Optional.empty());

        private Optional(DamlType nestedType) {
            super("Optional");
            this.nestedType = checkNotNull(nestedType);
        }

        public DamlType getNestedType() {
            return nestedType;
        }

        @Override
        public DamlType getElementType() {
            return nestedType.getElementType();
        }

        public static Value some(Value val) {
            return new com.daml.ledger.javaapi.data.DamlOptional(java.util.Optional.of(val));
        }

        @Override
        public String toString() {
            return "Optional<"+getNestedType().toString()+">";
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            if (!super.equals(o)) return false;
            Optional optional = (Optional) o;
            return Objects.equals(nestedType, optional.nestedType);
        }

        @Override
        public int hashCode() {
            return Objects.hash(super.hashCode(), nestedType);
        }
    }

    public static final class Variant extends DamlType {
        private Variant(String name) {
            super(name);
        }
    }

    public static final class Record extends DamlType {
        private Record(String name) {
            super(name);
        }
    }

    public static final class Unsupported extends DamlType {
        private static final Unsupported INSTANCE = new Unsupported();

        private Unsupported() {
            super("Unsupported");
        }
    }
}
