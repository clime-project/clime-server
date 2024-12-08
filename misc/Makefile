TARGET     = climesrv
LISP       ?= sbcl --noinform --no-sysinit
OPTIONS    ?=
LOADLIST   = --eval '(require :asdf)' \
	--eval '(asdf:load-asd (truename "../clime.asd"))' \
	--load make-executable.lisp
TARGET_DEF = --eval "(defconstant +OUTPUT-FILENAME+ \"$(TARGET)$(TARGET_SUFFIX)\")"

# COMSPEC 環境変数が定義されていれば Windows(cygwin) 環境とみなす
ifdef COMSPEC
	TARGET_SUFFIX = .exe
else
	TARGET_SUFFIX =
endif

all: $(TARGET)$(TARGET_SUFFIX)

$(TARGET)$(TARGET_SUFFIX): make-executable.lisp
	$(LISP) $(OPTIONS) $(TARGET_DEF) $(LOADLIST)

clean:
	$(RM) $(TARGET)$(TARGET_SUFFIX)

.PHONY: all clean
