HMAKE		= ghc --make
HCFLAGS		= -package HaXml

.SUFFIXES:
.PHONY:		all dbmorph

all:	dbmorph

dbmorph:
	$(HMAKE) $(HCFLAGS) -o $@ $@.hs

clean:
	@find . -name '*.hi' -exec rm {} \;
	@find . -name '*.o' -exec rm {} \;
	@rm -f dbmorph
