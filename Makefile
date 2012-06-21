#
# Targets
#

REBAR=`which rebar`

.PHONY: build

all: build

clean:
	rm -rf ebin
	$(REBAR) skip_deps=true clean

build:
	$(REBAR) compile
	$(MAKE) xref

test: build
	rm -rf .eunit
	$(REBAR) skip_deps=true eunit


#
# Run
#

ERL=exec erl -pa ebin -sname stetson

.PHONY: boot noboot

boot: build
	$(ERL) -s stetson

noboot: build
	$(ERL)

#
# Analysis
#

PLT=./plt/R15B.plt

WARNINGS=-Werror_handling \
  -Wrace_conditions \
  -Wunderspecs \
  -Wunmatched_returns

APPS=kernel stdlib sasl erts ssl \
  tools os_mon runtime_tools crypto \
  inets xmerl webtool snmp public_key \
  mnesia eunit syntax_tools compiler

build-plt: all
	dialyzer --build_plt --output_plt $(PLT) \
	  --apps $(APPS)

dialyzer: build
	dialyzer ebin --plt $(PLT) $(WARNINGS)

xref:
	$(REBAR) skip_deps=true xref

typer:
	typer --annotate --plt $(PLT) -I deps/ -r .
