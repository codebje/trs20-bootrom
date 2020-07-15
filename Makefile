SRC_DIR:=src
BIN_DIR:=bin
SRC_FILES:=$(wildcard $(SRC_DIR)/*.asm)

all: $(BIN_DIR)/bootrom.bin

$(BIN_DIR)/bootrom.bin: $(SRC_FILES)
	@mkdir -p $(BIN_DIR)
	zasm -uy --z180 $(SRC_DIR)/bootrom.asm -l $(BIN_DIR)/bootrom.lst -o $(BIN_DIR)/bootrom.bin

clean:
	rm -f $(BIN_DIR)/bootrom.{lst,bin}
