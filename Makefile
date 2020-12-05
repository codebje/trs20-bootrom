SRC_DIR:=src
BIN_DIR:=bin
SRC_FILES:=$(wildcard $(SRC_DIR)/*.asm)

all: $(BIN_DIR)/bootrom.bin

$(BIN_DIR)/bootrom.bin: $(SRC_FILES)
	@mkdir -p $(BIN_DIR)
	zasm -uwy --z180 $(SRC_DIR)/bootrom.asm -l $(BIN_DIR)/bootrom.lst -o $(BIN_DIR)/bootrom.bin

$(BIN_DIR)/romdisk.img: src/romdisk.txt diskdefs
	@LANG=en_US.US-ASCII tr '\000' '\345' < /dev/zero | dd of=$@ bs=1024 count=32
	@for file in $(shell cat src/romdisk.txt) ; do cpmcp -f rom_32kb $@ $${file} 0: ; done

$(BIN_DIR)/bootdisk.bin: $(BIN_DIR)/bootrom.bin $(BIN_DIR)/romdisk.img
	@cat $^ > $@

clean:
	rm -f $(BIN_DIR)/bootrom.{lst,bin} $(BIN_DIR)/romdisk.img
