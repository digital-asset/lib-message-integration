// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.time;

import static java.time.temporal.ChronoField.DAY_OF_MONTH;
import static java.time.temporal.ChronoField.HOUR_OF_DAY;
import static java.time.temporal.ChronoField.MINUTE_OF_HOUR;
import static java.time.temporal.ChronoField.MONTH_OF_YEAR;
import static java.time.temporal.ChronoField.SECOND_OF_MINUTE;
import static java.time.temporal.ChronoField.YEAR;
import static java.time.temporal.ChronoUnit.SECONDS;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.TemporalQuery;
import java.util.function.Function;

/**
 * Date/datetime parser capable of parsing the supplied text as {@link Instant} according to the rule:
 * <ol>
 * <li>if input is Zulu-based (for instance, <strong>2018-01-01Z</strong> or <strong>2018-01-01T01:23:45Z</strong>)
 * then uses GMT timezone</li>
 * <li>otherwise (for instance, <strong>2018-01-01</strong> or <strong>2018-01-01T01:23:45</strong>) uses the JVM's
 * default timezone</li>
 * </ol>
 */
public final class DateTimeParser {

    private static final char DATE_TIME_SEPARATOR = 'T';
    private static final char DATE_PARTS_SEPARATOR = '-';
    private static final char TIME_PARTS_SEPARATOR = ':';
    private static final char ZULU_QUALIFIER = 'Z';

    private static final Function<ZoneId, TemporalQuery<Instant>> DATETIME_TO_INSTANT =
            timezone -> temporal -> LocalDateTime.from(temporal).atZone(timezone).toInstant();

    private static final DateTimeFormatter NORMALIZED_DATETIME_FORMATTER = new DateTimeFormatterBuilder()
            .parseCaseInsensitive()
            .appendValue(YEAR, 4)
            .appendLiteral(DATE_PARTS_SEPARATOR)
            .appendValue(MONTH_OF_YEAR, 2)
            .appendLiteral(DATE_PARTS_SEPARATOR)
            .appendValue(DAY_OF_MONTH, 2)
            .optionalStart()
            .appendLiteral(DATE_TIME_SEPARATOR)
            .appendValue(HOUR_OF_DAY, 2)
            .appendLiteral(TIME_PARTS_SEPARATOR)
            .appendValue(MINUTE_OF_HOUR, 2)
            .appendLiteral(TIME_PARTS_SEPARATOR)
            .appendValue(SECOND_OF_MINUTE, 2)
            .optionalEnd()
            .optionalStart()
            .appendLiteral(ZULU_QUALIFIER)
            .optionalEnd()
            .parseDefaulting(SECOND_OF_MINUTE, 0)
            .parseDefaulting(MINUTE_OF_HOUR, 0)
            .parseDefaulting(HOUR_OF_DAY, 0)
            .toFormatter();

    private DateTimeParser() {
    }

    /**
     * Parses the supplied text as {@link Instant}.
     *
     * @param text the textual representation of date/datetime to parse
     * @return the exact {@link Instant} representing input datetime, or midnight {@link Instant} if date is supplied
     */
    public static Instant parse(final String text) {
        return parse(text, deriveTimezone(text));
    }

    private static Instant parse(final String text, final ZoneId zone) {
        final TemporalQuery<Instant> zonedQuery = DATETIME_TO_INSTANT.apply(zone);
        return NORMALIZED_DATETIME_FORMATTER.parse(text, zonedQuery).truncatedTo(SECONDS);
    }

    private static ZoneId deriveTimezone(final String text) {
        return isInZuluFormat(text) ? ZoneOffset.UTC : ZoneId.systemDefault();
    }

    private static boolean isInZuluFormat(final String text) {
        return text.endsWith(String.valueOf(ZULU_QUALIFIER));
    }
}
