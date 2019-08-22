package com.digitalasset.integration.internal.codec;

import com.digitalasset.integration.internal.codec.metadata.DamlTypes;
import com.digitalasset.integration.internal.codec.metadata.Metadata;
import com.digitalasset.integration.internal.codec.metadata.cdm.CdmFieldMeta;
import com.digitalasset.integration.internal.codec.metadata.cdm.CdmMetadataReader;
import com.digitalasset.integration.internal.codec.strategies.JsonCdmDecodeStrategy;

import com.daml.ledger.javaapi.data.Command;
import com.daml.ledger.javaapi.data.CreateCommand;
import com.daml.ledger.javaapi.data.DamlList;
import com.daml.ledger.javaapi.data.Identifier;
import com.daml.ledger.javaapi.data.Party;
import com.daml.ledger.javaapi.data.Record;
import com.daml.ledger.javaapi.data.Value;
import com.daml.ledger.rxjava.DamlLedgerClient;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

public class CdmLedgerTest {
    public static final String APP_ID = "CdmLedgerTest";
    public static final String SUBMITTER = "Tester";
    public static final String TEMPLATE_MODULE = "Main";
    public static final String TEMPLATE_ENTITY = "ContractInstance";

    private final String HOST = "localhost";
    private final int PORT = 6865;

    private void testDecode() throws IOException, URISyntaxException {
        Metadata<CdmFieldMeta,Void> metadata = CdmMetadataReader.fromJSON(
            getClass().getResource("/cdm_v2_0_136/metadata/CDM.json"));

        DamlLedgerClient client = DamlLedgerClient.forHostWithLedgerIdDiscovery(HOST, PORT, Optional.empty());
        client.connect();

        Path root = Paths.get(getClass().getResource("/cdm_v2_0_136/examples/").toURI());
        for (Path fp : Files.walk(root)
                .filter(Files::isRegularFile)
                .filter(f -> f.getFileName().toString().endsWith("json"))
                .collect(Collectors.toList())) {

            System.out.println("#### Trying " + fp.toString());
            byte[] bytes = Files.readAllBytes(fp);
            JsonCdmDecodeStrategy decoder = new JsonCdmDecodeStrategy(metadata);
            JsonElement json = new JsonParser().parse(new String(bytes));
            Value val = decoder.decode(json, DamlTypes.record("Contract"));

            String pkgId = client.getPackageClient().listPackages().blockingFirst();
            Identifier idt = new Identifier(pkgId, TEMPLATE_MODULE, TEMPLATE_ENTITY);

            List<Record.Field> fields = new ArrayList<>();
            fields.add(0, new Record.Field("d", val));
            fields.add(0, new Record.Field("ps", new DamlList(new Party(SUBMITTER))));
            Record arg = new Record(fields);

            Command cmd = new CreateCommand(idt, arg);

            client.getCommandClient().submitAndWaitForTransaction(
                    "Create",
                    APP_ID,
                    UUID.randomUUID().toString(),
                    SUBMITTER,
                    Instant.now(),
                    Instant.now().plusSeconds(25),
                    Collections.singletonList(cmd)).blockingGet();
            System.out.println("Sent");
        }
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        new CdmLedgerTest().testDecode();

        System.out.println("Finished");
    }
}
