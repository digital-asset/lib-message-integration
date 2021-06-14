# Make is used as a task runner. SBT could be utilized for this as well, however this seemed simpler at the time.

.PHONY: build start

build:
	$(MAKE) -C acord-models

start:
	DAML_PROJECT=acord-models daml start --json-api-port=none
