# Make is used as a task runner. SBT could be utilized for this as well, however this seemed simpler at the time.

.DEFAULT_GOAL := generateSources

LEDGER_HOST := "localhost"
LEDGER_PORT := 6865
ACORD_MODELS_DAR := "acord-models/.daml/dist/acord-models-0.0.1.dar"

.PHONY: generateSources startSandbox acord-models build

acord-models:
	$(MAKE) -C $@ $(TARGET)

build: TARGET = build
build: acord-models

clean: TARGET = clean
clean: acord-models

# TODO: This should make more sense in SBT.
MANAGED_SOURCES = "app/target/scala-2.12/src_managed/main"
generateSources: build
	daml codegen java \
	--output-directory $(MANAGED_SOURCES) \
	--decoderClass acord.AcordDecoder \
	acord-models/.daml/dist/acord-models-0.0.1.dar

startSandbox:
	DAML_PROJECT=acord-models daml start --json-api-port=none
