include ../support/include.mk

all: $(EBIN_FILES_NO_DOCS)

doc: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
	rm -rf ../tests_ebin