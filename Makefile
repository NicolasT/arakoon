OCP_BUILD ?= ocp-build

default: arakoon

arakoon:
	@rm -f $@
	@$(OCP_BUILD) $@
	@ln -s _obuild/$@/$@.asm $@

.PHONY: arakoon

clean:
	@rm -f arakoon
	@$(OCP_BUILD) clean

.PHONY: clean
