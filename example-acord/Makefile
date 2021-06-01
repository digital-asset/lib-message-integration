# Make is used as a task runner. SBT could be utilized for this as well, however this seemed simpler at the time.

LEDGER_HOST := "localhost"
LEDGER_PORT := 6865
ACORD_MODELS_DAR := "acord-models/.daml/dist/acord-models-0.0.1.dar"
ACORD_LEDGER_SETUP_DAR := "ledger-setup/.daml/dist/acord-ledger-setup-0.0.1.dar"
INIT_SCRIPT_NAME := "Main:init"

.PHONY: generateSources startSandbox initializeLedger

# TODO: This should make more sense in SBT.
MANAGED_SOURCES = "app/target/scala-2.12/src_managed/main"
generateSources:
	daml codegen java \
	--output-directory $(MANAGED_SOURCES) \
	acord-models/.daml/dist/acord-models-0.0.1.dar
	daml codegen java \
	--output-directory $(MANAGED_SOURCES) \
	--decoderClass acord.AcordDecoder \
	ledger-setup/.daml/dist/acord-ledger-setup-0.0.1.dar

startSandbox:
	daml sandbox \
	--address=$(LEDGER_HOST) \
	--port=$(LEDGER_PORT) \
	$(ACORD_MODELS_DAR) \
	$(ACORD_LEDGER_SETUP_DAR)

initializeLedger:
	daml script \
	--ledger-host=$(LEDGER_HOST) \
	--ledger-port=$(LEDGER_PORT) \
	--dar=$(ACORD_LEDGER_SETUP_DAR) \
	--script-name=$(INIT_SCRIPT_NAME)
