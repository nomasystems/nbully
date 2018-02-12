VSN = 1.0.0
VO_HRLS =

### Special characters
comma := ,
empty :=
space := $(empty) $(empty)

### Erlang compiler
ERL = erl
ERLC = erlc
DIALYZER = dialyzer
RUN_TEST = ct_run

### Flags
ifeq ($(DEBUG_INFO), true)
	EFLAGS = -Wall -I include -I ..  -o ebin +debug_info +bin_opt_info
else
	EFLAGS = -Wall -I include -I ..  -o ebin
endif
DFLAGS = -I include -I .. --src --verbose -c
STUB_EFLAGS = -W0 -o stubs

### Default apps
CD = cd
CP = cp -vf
ECHO = echo
ERLDOC = ndoc
MKDIR = mkdir
MV = mv -vf
RM = rm -vf
SED = sed
VOGENERATE = otpVOGenerate
VOCLEAN = otpVOClean
LS = otpLs
REBAR = rebar
