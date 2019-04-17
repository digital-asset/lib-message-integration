// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.time;

import static java.time.format.DateTimeFormatter.ISO_DATE;
import static java.time.format.DateTimeFormatter.ISO_DATE_TIME;
import static java.time.temporal.ChronoUnit.MILLIS;
import static java.time.temporal.ChronoUnit.SECONDS;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Formatter capable of displaying an {@link Instant} in Zulu or offset-less mode (considering the JVM's default
 * timezone as the local one) preserving full information (datetime) or truncating it to date.
 */
public class InstantFormatter {

    private static final String ZULU_SUFFIX = "Z";
    private static final String EMPTY_SUFFIX = "";

    private final ZoneId timezone;
    private final String tzSuffix;

    private final DateTimeFormatter zeroMillisFormatter;

    private InstantFormatter(final ZoneId timezone) {
        this.timezone = timezone;
        this.tzSuffix = (timezone == ZoneOffset.UTC ? ZULU_SUFFIX : EMPTY_SUFFIX);
        this.zeroMillisFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS");
    }

    public static InstantFormatter asZulu() {
        return new InstantFormatter(ZoneOffset.UTC);
    }

    public static InstantFormatter asLocal() {
        return new InstantFormatter(ZoneId.systemDefault());
    }

    /**
     * Renders supplied {@link Instant} as date depending on which factory method it was created with. Example:
     * <ul>
     * <li>{@link #asZulu()}: 2018-01-01Z</li>
     * <li>{@link #asLocal()}: 2018-01-01</li>
     * </ul>
     *
     * @param time the {@link Instant} to render
     * @return instant representation as date
     */
    public String formatAsDate(final Instant time) {
        return LocalDateTime.ofInstant(time, timezone)
                .format(ISO_DATE) + tzSuffix;
    }

    /**
     * Renders supplied {@link Instant} as datetime depending on which factory method it was created with. Example:
     * <ul>
     * <li>{@link #asZulu()}: 2018-01-01T01:23:45Z</li>
     * <li>{@link #asLocal()}: 2018-01-01T01:23:45</li>
     * </ul>
     *
     * @param time the {@link Instant} to render
     * @return instant representation as datetime
     */
    public String formatAsDateTime(final Instant time) {
        return LocalDateTime.ofInstant(time, timezone)
                .truncatedTo(SECONDS)
                .format(ISO_DATE_TIME) + tzSuffix;
    }

    /**
     * Renders supplied {@link Instant} as datetime depending on which factory method it was created with.
     * A 3 digit millisecond will always be present even if the milliseconds is 0
     * Example:
     * <ul>
     * <li>{@link #asZulu()}: 2018-01-01T01:23:45.000Z</li>
     * <li>{@link #asZulu()}: 2018-01-01T01:23:45.321Z</li>
     * <li>{@link #asLocal()}: 2018-01-01T01:23:45.123</li>
     * </ul>
     *
     * @param time the {@link Instant} to render
     * @return instant representation as datetime
     */
    public String formatAsDateTimeWithMillis(final Instant time) {
        return zeroMillisFormatter.format(ZonedDateTime.ofInstant(time, timezone).truncatedTo(MILLIS)) + tzSuffix;
    }
}
