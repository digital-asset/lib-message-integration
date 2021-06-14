
DAR:= .daml/dist/acord-models-0.0.1.dar
MANAGED_SOURCES:= ../app/target/scala-2.12/src_managed/main

.PHONY: codegen

codegen: $(DAR)
	daml codegen java \
		--output-directory $(MANAGED_SOURCES) \
		--decoderClass acord.AcordDecoder \
		$(DAR)

$(DAR):
	daml build
