### INCLUDES
include app.mk

###-----------------------------------------------------------------------------
### APPLICATION LAYOUT
###-----------------------------------------------------------------------------
APPSRC = $(patsubst src/%.app.src,%.app.src,$(wildcard src/*.app.src))
APP = $(APPSRC:.app.src=.app)
APPNAME = $(basename $(APP))
ERLS = $(patsubst src/%.erl,%.erl,$(wildcard src/*.erl))
BEAMS = $(ERLS:.erl=.beam)
MODS = $(subst $(space),$(comma)$(space),$(ERLS:.erl=))
DOCS = $(patsubst doc/%.ndoc,%.ndoc,$(wildcard doc/*.ndoc))
MANS = $(DOCS:.ndoc=.3)
HTMS = $(DOCS:.ndoc=.html)
TEXS = $(DOCS:.ndoc=.tex)
PDFS = $(DOCS:.ndoc=.pdf)

### DEPENDENCY SEARCH PATHS
VPATH = src:include:ebin:doc

.PHONY: all clean doc test
.SUFFIXES: .erl .hrl .beam .app.src .app .rel .ndoc

###-----------------------------------------------------------------------------
### TARGETS
###-----------------------------------------------------------------------------
all: compile

man: $(MANS)
	@$(MV) doc/man/$(APPNAME)_overview.3 doc/man/$(APPNAME).1

html: $(HTMS)
	@$(MV) doc/html/$(APPNAME)_overview.html doc/html/index.html

pdf: $(PDFS)

doc: man html pdf

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

realclean: clean cleanvo
	@$(RM) -Rf doc/html
	@$(RM) -Rf doc/man
	@$(RM) -Rf doc/pdf

test: compile
	@$(REBAR) ct --sname nbully --spec test/conf/test.spec --cover --readable true

dialyze:
	$(REBAR) dialyzer

beams:
	@$(ERL) -make

### RULES
%.3: %.ndoc
	@$(CD) doc; $(SED) "s|%MODULES%|`echo $(MODS)`|g" ../$^ | \
	$(SED) "s|%VSN%|$(VSN)|g" | $(SED) "s|%APPLICATION%|$(APPNAME)|g" | \
	$(ERLDOC) -i - -t man --no-toc --erl -o man/$@; $(CD) ..

%.html: %.ndoc
	@$(CD) doc; $(SED) "s|%MODULES%|`echo $(MODS)`|g" ../$^ | \
	$(SED) "s|%VSN%|$(VSN)|g" | $(SED) "s|%APPLICATION%|$(APPNAME)|g" | \
	$(ERLDOC) -i - -t xhtml --erl -o html/$@; $(CD) ..

%.pdf: %.ndoc
	@$(CD) doc; $(SED) "s|%MODULES%|`echo $(MODS)`|g" ../$^ | \
	$(SED) "s|%VSN%|$(VSN)|g" | $(SED) "s|%APPLICATION%|$(APPNAME)|g" | \
	$(ERLDOC) -i - -t pdf --no-toc --erl -o pdf/$@; $(CD) ..

$(APP): $(APPSRC)
	@$(SED) "s|%MODULES%|`$(LS)`|g" $^ | \
	$(SED) "s|%VSN%|$(VSN)|g" | $(SED) "s|%APPLICATION%|$(APPNAME)|g" > ebin/$@
