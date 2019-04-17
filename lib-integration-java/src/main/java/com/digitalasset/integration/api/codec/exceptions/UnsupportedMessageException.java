// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2017 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.api.codec.exceptions;

/**
 * Raised when a decode operation fails to match the message contents to any of well-known ingress master contract
 * choices.
 */
public class UnsupportedMessageException extends RuntimeException {

    public UnsupportedMessageException(String message) {
        super(message);
    }

}
