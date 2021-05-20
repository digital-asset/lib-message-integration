# Make is used as a task runner. SBT could be utilized for this as well, however this seemed simpler at the time.

LEDGER_HOST ?= "localhost"
LEDGER_PORT ?= 6865
ACORD_MODELS_DAR ?= "models/.daml/dist/acord-models-0.0.1.dar"
ACORD_LEDGER_SETUP_DAR ?= "ledger-setup/.daml/dist/acord-ledger-setup-0.0.1.dar"
INIT_SCRIPT_NAME ?= "Main:init"

.PHONY: startSandbox initializeLedger

startSandbox:
	daml sandbox \
	--address=$(LEDGER_HOST) \
	--port=$(LEDGER_PORT) \
	$(ACORD_MODELS_DAR)

initializeLedger:
	daml script \
	--ledger-host=$(LEDGER_HOST) \
	--ledger-port=$(LEDGER_PORT) \
	--dar=$(ACORD_LEDGER_SETUP_DAR) \
	--script-name=$(INIT_SCRIPT_NAME)
