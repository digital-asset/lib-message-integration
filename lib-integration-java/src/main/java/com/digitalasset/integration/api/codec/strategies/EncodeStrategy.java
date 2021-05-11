// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2018 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.api.codec.strategies;

import com.daml.ledger.javaapi.data.Value;
import com.digitalasset.integration.internal.codec.metadata.DamlTypes.DamlType;

/**
 * Encode a Daml-LF value into a message.
 */
public interface EncodeStrategy<T> {
    T encode(Value messageData, DamlType type);
}
