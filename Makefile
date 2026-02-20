SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin
BIN = $(BIN_DIR)/hs2lazy
LAZY_DIR = lazy

SRC = Lexer.hs Parser.hs Syntax.hs PPrint.hs Static.hs SCC.hs Type.hs PatComp.hs Compiler.hs Optimizer.hs Builtin.hs Main.hs
SRC_HS = $(addprefix $(SRC_DIR)/,$(SRC))
OBJ = $(patsubst $(SRC_DIR)/%.hs,$(OBJ_DIR)/%.o,$(SRC_HS))

GHC = ghc
GHCFLAGS =
OPTFLAGS = -O
#PROFFLAGS = -prof -auto-all

EXAMPLES_SRC = echo.hs even_lines.hs hello.hs tarai.hs reverse_lines.hs fizzbuzz.hs lisp.hs
EXAMPLES = $(EXAMPLES_SRC:.hs=.lazy)

all: $(BIN) examples

$(BIN): $(SRC_HS)
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	$(GHC) $(GHCFLAGS) $(OPTFLAGS) $(PROFFLAGS) \
	    -i$(SRC_DIR) \
	    -outputdir $(OBJ_DIR) \
	    -o $@ --make $(SRC_DIR)/Main.hs

$(SRC_DIR)/Lexer.hs: $(SRC_DIR)/Lexer.x
	alex $<

examples: $(addprefix $(LAZY_DIR)/,$(EXAMPLES))

$(LAZY_DIR)/%.lazy: examples/%.hs $(BIN)
	@mkdir -p $(LAZY_DIR)
	$(BIN) examples/hs2lazy-prelude.hs $< > $@

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR) $(LAZY_DIR)

.PHONY: all examples clean

fmt:
	stack exec -- ormolu --mode inplace hs/**/*.hs

fmt-check:
	stack exec -- ormolu --mode check hs/**/*.hs

lint:
	stack exec -- hlint hs

lint-fix:
	stack exec -- hlint hs --refactor --refactor-options="--inplace"

build:
	stack build

test:
	stack test
