// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2017 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.api.codec;

import com.daml.ledger.javaapi.data.Command;
import com.digitalasset.integration.api.codec.exceptions.CodecException;
import com.digitalasset.integration.api.codec.exceptions.SchemaValidationException;
import com.digitalasset.integration.api.codec.exceptions.UnsupportedMessageException;

/**
 * Acts as decoder of external business message protocols (data formats) into DA contract choice executions.
 */
public interface Decoder {

    /**
     * Decode external business message contents into a DA contract choice exercise command.
     *
     * @param uniqueId unique id associated to the message contents
     * @param messageContents business message contents to decode
     * @return Command to exercise a choice on a contract that will deliver the given ingress message
     * @throws UnsupportedMessageException if decoding fails because no ingress master contract choice can be mapped
     * @throws SchemaValidationException if decoding fails because of schema non-compliance
     * @throws CodecException if decoding fails for any other reason
     */
    Command decode(String uniqueId, byte[] messageContents)
            throws UnsupportedMessageException, SchemaValidationException, CodecException;
}
