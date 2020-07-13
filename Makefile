SRC_DIR:=src
BIN_DIR:=bin

all: $(BIN_DIR)/bootrom.bin

$(BIN_DIR)/bootrom.bin: $(SRC_DIR)/bootrom.asm
	@mkdir -p $(BIN_DIR)
	zasm -uy --z180 $^ -l $(BIN_DIR)/bootrom.lst -o $(BIN_DIR)/bootrom.bin

clean:
	rm -f $(BIN_DIR)/bootrom.{lst,bin}
