# Make is used as a task runner. SBT could be utilized for this as well, however this seemed simpler at the time.

.DEFAULT_GOAL := build

LEDGER_HOST := "localhost"
LEDGER_PORT := 6865
ACORD_MODELS_DAR := "acord-models/.daml/dist/acord-models-0.0.1.dar"
ACORD_LEDGER_SETUP_DAR := "ledger-setup/.daml/dist/acord-ledger-setup-0.0.1.dar"

PROJECTS := acord-models ledger-setup

.PHONY: generateSources startSandbox $(PROJECTS) build

$(PROJECTS):
	$(MAKE) -C $@ $(TARGET)

build: TARGET = build
build: $(PROJECTS)

clean: TARGET = clean
clean: $(PROJECTS)

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
	DAML_PROJECT=ledger-setup daml start --json-api-port=none
