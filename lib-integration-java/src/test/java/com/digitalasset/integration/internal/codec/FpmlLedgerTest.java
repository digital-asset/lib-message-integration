// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2013-2019 Digital Asset Holdings, LLC.
 * Confidential, Proprietary, and/or the subject matter herein may be
 * protected under Patent law.  All rights reserved.
 */
package com.digitalasset.integration.internal.codec;

import com.digitalasset.integration.examples.codec.fpml.ExampleFpmlDecoder;
import com.daml.ledger.rxjava.DamlLedgerClient;
import com.digitalasset.integration.api.codec.Decoder;
import com.google.common.collect.Lists;
import io.reactivex.Flowable;
import org.junit.Test;
import org.junit.Ignore;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.*;

import com.daml.ledger.javaapi.data.Record;
import com.daml.ledger.javaapi.data.*;

import static java.util.Arrays.asList;

public class FpmlLedgerTest {

    // application id used for sending commands
    public static final String APP_ID = "FpmlLedgerTest";
    public static final String SUBMITTER = "Scrooge_McDuck";
    public static final String TEMPLATE_MODULE = "Main";
    public static final String TEMPLATE_ENTITY = "TestTemplate";

    // TODO command line arguments
    private final String HOST = "localhost";
    private final int PORT = 7600;

    @Ignore("requires a running sandbox")
    @Test
    public void setup() {
        Properties props = System.getProperties();
        props.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "INFO");

        // create a client object to access services on the ledger
        DamlLedgerClient client = DamlLedgerClient.forHostWithLedgerIdDiscovery(HOST, PORT, Optional.empty());

        // Connects to the ledger and runs initial validation
        client.connect();
        client.getTimeClient().setTime(
                client.getTimeClient().getTime().blockingFirst(),
                Instant.now());

        // find template identifier, assume only one package in sandbox
        String pkgId = client.getPackageClient().listPackages().blockingFirst();
        Identifier templateId = new Identifier(pkgId, TEMPLATE_MODULE, TEMPLATE_ENTITY);

        // create the initial contract
        Record args = new Record(asList(
                new Record.Field(new Party(SUBMITTER)),
                new Record.Field(new DamlList(Collections.EMPTY_LIST))
        ));
        Command createCmd = new CreateCommand(templateId, args);
        client.getCommandClient().submitAndWait(
                "Create",
                APP_ID,
                UUID.randomUUID().toString(),
                SUBMITTER,
                Optional.of(Instant.now()),
                Optional.of(Duration.ofSeconds(25)),
                Optional.of(Duration.ofSeconds(25)),
                Collections.singletonList(createCmd)).blockingGet();

        // show transactions
        /*for ( Transaction tx : client.getTransactionsClient().getTransactions(
                LedgerOffset.LedgerBegin.getInstance(),filterFor(templateId, SUBMITTER),true).blockingIterable()) {
            System.out.println(tx.toString());
        }*/

        // show active contracts
        Flowable<GetActiveContractsResponse> acs = client.getActiveContractSetClient().getActiveContracts(
                filterFor(templateId, SUBMITTER), true);
        for(GetActiveContractsResponse resp : acs.blockingIterable()) {
            System.out.println( resp.toString() );
        }
    }

    @Ignore("requires a running sandbox")
    @Test
    public void testExercise() throws IOException, URISyntaxException {
        Properties props = System.getProperties();
        props.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "INFO");

        ClassLoader loader = getClass().getClassLoader();

          // create a client object to access services on the ledger
        DamlLedgerClient client = DamlLedgerClient.forHostWithLedgerIdDiscovery(HOST, PORT, Optional.empty());

        // Connects to the ledger and runs initial validation
        client.connect();
        client.getTimeClient().setTime(
                client.getTimeClient().getTime().blockingFirst(),
                Instant.now());

        // find template identifier, assume only one package in sandbox
        String pkgId = client.getPackageClient().listPackages().blockingFirst();
        Identifier templateId = new Identifier(pkgId, TEMPLATE_MODULE, TEMPLATE_ENTITY);

        // find latest active contract
        String contractId = Lists.newArrayList(client
                .getActiveContractSetClient()
                .getActiveContracts(filterFor(templateId, SUBMITTER), true)
                .blockingIterable())
                .stream()
                .flatMap(r -> r.getCreatedEvents().stream())
                .sorted(Comparator.comparing(CreatedEvent::getContractId).reversed())
                .findFirst()
                .get()
                .getContractId();

        System.out.println("Found contractId: "+contractId);

        Decoder decoder = new ExampleFpmlDecoder(
                templateId,
                contractId);

        // load and decode message
        Path fp = Paths.get(loader.getResource("fpml/confirmation/business-processes/clearing/msg-ex50-request-clearing-trade-package.xml").toURI());
        byte[] bytes = Files.readAllBytes(fp);
        Command msgCmd = decoder.decode(fp.toString(), bytes);

        client.getCommandClient().submitAndWait(
                "Exercise",
                APP_ID,
                UUID.randomUUID().toString(),
                SUBMITTER,
                Optional.of(Instant.now()),
                Optional.of(Duration.ofSeconds(25)),
                Optional.of(Duration.ofSeconds(25)),
                Collections.singletonList(msgCmd));

         // show active contracts
        Flowable<GetActiveContractsResponse> acs = client.getActiveContractSetClient().getActiveContracts(
                filterFor(templateId, SUBMITTER), true);
        for(GetActiveContractsResponse resp : acs.blockingIterable()) {
            System.out.println( resp.toString() );
        }
    }

    private static TransactionFilter filterFor(Identifier templateId, String party) {
        InclusiveFilter inclusiveFilter = new InclusiveFilter(Collections.singleton(templateId));
        Map<String, Filter> filter = Collections.singletonMap(party, inclusiveFilter);
        return new FiltersByParty(filter);
    }

}
