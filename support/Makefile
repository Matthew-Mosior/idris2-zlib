IDRIS := idris2

TARGET = zlib-idris
TARGET_VERSION ?= 0.1.0

INSTALLDIR = `${IDRIS} --libdir`/zlib-${TARGET_VERSION}
LIBDIR = "$(INSTALLDIR)/lib"

LDFLAGS =
CFLAGS = -O2 -g -Wall -Wextra -fPIC

CC_VERSION = $(shell $(CC) --version)
MACHINE := $(shell $(CC) -dumpmachine)

ifneq (,$(findstring cygwin,$(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring mingw,$(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring windows,$(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring darwin,$(MACHINE)))
	OS := darwin
	SHLIB_SUFFIX := .dylib
else ifneq (,$(findstring bsd,$(MACHINE)))
	OS := bsd
	SHLIB_SUFFIX := .so
else
	OS := linux
	SHLIB_SUFFIX := .so
endif

ifeq ($(findstring clang,$(CC_VERSION)),clang)
	LDFLAGS +=
else
	LDFLAGS += -fuse-ld=lld
endif

SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)

all: $(TARGET)$(SHLIB_SUFFIX)

$(TARGET)$(SHLIB_SUFFIX): $(OBJS)
	$(CC) $(CFLAGS) -shared $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.d: %.c
	@$(CPP) $(CFLAGS) $< -MM -MT $(@:.d=.o) > $@

.PHONY: clean

clean:
	rm -f $(OBJS) $(DEPS) $(TARGET)$(SHLIB_SUFFIX)

.PHONY: cleandep

cleandep: clean

.PHONY: install

install:
	@if ! [ -d $(LIBDIR) ]; then mkdir -p $(LIBDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(LIBDIR)
