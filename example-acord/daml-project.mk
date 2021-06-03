ifndef NAME
$(error 'NAME' is undefined)
endif

ifndef VERSION
$(error 'VERSION' is undefined)
endif

DAML_YAML := daml.yaml
DAML_SRC := $(shell find . -name '*.daml')

DAR_FILE := .daml/dist/$(NAME)-$(VERSION).dar
$(DAR_FILE): $(DAML_YAML) $(DAML_SRC) $(DEPENDENCIES) $(DATA_DEPENDENCIES)
	daml build --output $@

.PHONY: clean build

build: $(DAR_FILE)

clean:
	daml clean
