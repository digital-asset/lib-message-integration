// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2017 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.api.codec;

import com.daml.ledger.javaapi.data.Value;
import com.digitalasset.integration.api.codec.exceptions.CodecException;

/**
 * Encoder of DA ledger events into external business message protocols (data formats).
 */
public interface Encoder {

    /**
     * Encode {@code event} into an external business format supported by this encoder.
     *
     * @param event event to encode
     * @return business message contents
     * @throws CodecException if encoding fails for any reason
     */
    byte[] encode(Value event) throws CodecException;

}
