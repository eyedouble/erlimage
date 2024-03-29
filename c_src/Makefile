
#
# Define OS
#
OS ?= $(shell uname -s)

ifneq (,$(findstring /cygdrive/,$(PATH)))
	OSNAME := Cygwin
else
	ifneq (,$(findstring Windows_NT,$(OS)))
		OSNAME := Windows
	else
		ifneq (,$(findstring mingw32,$(MAKE)))
			OSNAME := Windows
		else
			ifneq (,$(findstring MINGW32,$(shell uname -s)))
				OSNAME = Windows
			else
				OSNAME := $(shell uname -s)
			endif
		endif
	endif
endif

#
# Set Current Dir
#
ifeq ($(OSNAME), Windows)	
	CURDIR := $(shell echo %cd%)
else
	CURDIR := $(shell pwd)
endif

BASEDIR := $(abspath $(CURDIR)/..)
PRIVDIR := $(abspath $(CURDIR)/../priv)
PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

#
# OS SPECIFIc
# 
ifeq ($(OSNAME), Windows)
	SLEXT := .dll
	LIBNAME := erlimage_nif$(SLEXT)
	COPY := copy ".\freeimage_win\x64\FreeImage$(SLEXT)" ".\..\priv"
	COPY2 := copy "$(LIBNAME)" ".\..\priv"
	RM := del /f
else 
	SLEXT := .so
	LIBNAME := erlimage_nif$(SLEXT)
	COPY := cp FreeImage/Dist/libfreeimage.a ../priv
	COPY2 := cp $(LIBNAME) ../priv
	RM := rm -f
endif

#
# OPTIONS
#
CC       ?= gcc
CXX      ?= g++
CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
CXXFLAGS ?= -O3 -finline-functions -Wall

#
#	Compilation
#
ifeq ($(OSNAME), Windows)
	CFLAGS += -I "$(ERTS_INCLUDE_DIR)" -I "$(ERL_INTERFACE_INCLUDE_DIR)"
	CXXFLAGS += -I "$(ERTS_INCLUDE_DIR)" -I "$(ERL_INTERFACE_INCLUDE_DIR)" -I "FreeImage/Source" "freeimage_win/x64/FreeImage.dll"
else 
	CFLAGS += -I "$(ERTS_INCLUDE_DIR)" -I "$(ERL_INTERFACE_INCLUDE_DIR)"
	CXXFLAGS += -I "$(ERTS_INCLUDE_DIR)" -I "$(ERL_INTERFACE_INCLUDE_DIR)" -I "FreeImage/Source" "$(PRIVDIR)/libfreeimage.a"
endif


LDLIBS += -L "$(ERL_INTERFACE_LIB_DIR)" -lerl_interface -lei
LDFLAGS += -shared

$(info ${OS})
$(info ${OSNAME})
$(info ${CURDIR})
$(info ${PRIVDIR})
# $(info ${ERTS_INCLUDE_DIR})
# $(info ${ERL_INTERFACE_INCLUDE_DIR})
# $(info ${ERL_INTERFACE_LIB_DIR})

ifeq (,$(wildcard ./$(LIBNAME))) 
all: copy1 compile copy2 
else
all: compile
endif	

compile:
	$(shell g++ -o $(LIBNAME) -fpic -shared erlimage_nif.cpp $(CXXFLAGS))

copy1:
	$(COPY)

copy2:
	$(COPY2)

clean:
	$(RM) $(LIBNAME)

.PHONY: copy1 copy2