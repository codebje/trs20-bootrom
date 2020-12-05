.z180

#target rom

#include	"src/z180registers.asm"

; Section definitions
#code		PAGE0,$0000,$0100	; Page zero: vectors etc
#code		BOOT,$0100		; BOOT code
#code		BIOSCODE		; BIOS code follows the boot code
#code		BIOSROM			; BIOS code in ROM for banking
#code		OSDATA			; the OS image is inserted into the binary
#data		BIOSDATA,BIOS_DATA

; Constants and definitions
STACK		equ	CCP		; point the stack at the end of SRAM
UPLOAD		equ	$2000		; uploaded code beyond CPM

; Macros

#code		PAGE0

		.org	$0000

		;; The first instruction does an absolute jump to $0103.
		;; When executed from ROM, this will skip down to _start.
		;; When executed by CP/M as a command, this will only skip
		;; to the second instruction here.
reset:		jp	_start

		;.phase	$0103

		;; Most of the setup has already been done by now
		;; Just set up the MMU, copy the BIOS, and invoke the BIOS.
		;; Problem to solve: the BIOS wants to copy the OS from ROM
		;; Need to copy the OS to somewhere the RAM disk won't
		;; obliterate it and patch the BIOS to copy the ROM from there.

		; Set up the MMU:	logical		physical
		;   Common Area 1	e000-ffff	0e000-0ffff
		;   Bank Area		8000-dfff	08000-0dfff
		;   Common Area 0	0000-7fff	00000-07fff
		ld	a, $e8
		out0	(CBAR), a	; CA1 base = $f000, BA base = $8000
		xor	a
		out0	(BBR), a	; the banked area also points to SRAM bank 0
		out0	(CBR), a	; don't add anything to CA1 accesses

		;; Patch bootrom and BIOS to use bank 1 in SRAM instead of bank 8 in ROM
		ld	a, 1
		ld	(biospage+$100), a
		ld	(ospage+$100), a

		;; copy the BIOS+OS into bank 1: dest $10000, source $000100
		xor	a
		out0	(DAR0L), a
		out0	(DAR0H), a
		out0	(SAR0L), a
		out0	(SAR0B), a
		inc	a
		out0	(SAR0H), a
		out0	(DAR0B), a

		ld	bc, $2000		; copy the boot ROM, the BIOS, and the OS
		out0	(BCR0L),c
		out0	(BCR0H),b

		ld	bc, 0110000000000010b	; MEM+ -> MEM+, burst mode, ch. 0 enable
		out0	(DMODE),c
		out0	(DSTAT),b		; burst mode will halt CPU until complete

		;; let the BIOS take over now
		jp	copybios+$100

		;.dephase

		; include a visual marker of the boot rom version
version:	defm	'TRS-20 boot ROM v20201205.01', 13, 10, 0

#code		BOOT

		.db	0, 0, 0

_start:		xor	a
		out0	(RCR), a	; disable the DRAM refresh
		out0	(ITC), a	; disable INT0 (and others)

		; Increase clock speed to 18.432MHz
		ld	a, $80
		out0	(CCR), a	; boost up to 18.432MHz

		; Prepare ASCI0
		ld	a, 01100100b		; RE, TE, 8N1
		out0	(CNTLA0), a

		; System clock is 18.432MHz.
		;	bit 5, PS=0 selects /10
		;	bit 3, DR=0 selects /16
		;	bis 2-0, SS=001 selects /2
		;	18432000 / 10 / 16 / 2 = 57600 baud
		ld	a, 00000001b
		out0	(CNTLB0), a

		; The ROM masking is still in effect - SRAM is unreachable. Set up the MMU to allow a jump to a
		; high physical address, which will disable ROM masking.

		; Set up the MMU:	logical		physical
		;   Common Area 1	e000-ffff	0e000-0ffff
		;   Bank Area		8000-dfff	08000-0dfff
		;   Common Area 0	0000-7fff	00000-07fff
		ld	a, $e8
		out0	(CBAR), a	; CA1 base = $e000, BA base = $8000
		xor	a
		out0	(BBR), a	; the banked area also points to SRAM bank 0

		ld	a, $80 - $0e
		out0	(CBR), a	; CA1 now maps e000-ffff to 80000-81fff
		jp	$+3+$e000	; jump to the next instruction, but in ROM

		; SRAM should now be unmasked. Copy the boot ROM into RAM, then jump back.

		xor	a		; destination address 00100
		out0	(DAR0L), a
		out0	(DAR0B), a
		out0	(SAR0L), a	; source address 80100
		inc	a
		out0	(SAR0H), a
		out0	(DAR0H), a
		ld	a, $08
		out0	(SAR0B), a

		ld	bc, BOOT_size
		out0	(BCR0L),c
		out0	(BCR0H),b

		; DMODE specifies memory mode and increment/decrement
		;  7-6 unused
		;  5-4 DM1:0  destination mode - 00 for memory, incrementing
		;  3-2 SM1:0  source mode - 00 for memory, incrementing
		;  1   MMOD   BURST (1) or CYCLE STEAL (0) memory mode
		; DSTAT enables DMA transfers and interrupts
		;  7   DE1    Enables DMA channel 1
		;  6   DE0    Enables DMA channel 0
		;  5   /DWE1  Mask to control writing DE1
		;  4   /DWE0  Mask to control writing DE0
		;  3   DIE1   DMA channel 1 interrupt enable
		;  2   DIE1   DMA channel 0 interrupt enable
		;  1   unused
		;  0   DME    DMA enabled - set via DE0/DE1 only
		ld	bc, 0110000000000010b
		out0	(DMODE),c
		out0	(DSTAT),b	; burst mode will halt CPU until complete

		;; jump back to low memory
		jp	$+3

		;; restore CA1 as SRAM
		xor	a
		out0	(CBR), a

		; Set up PRT0 with a 100Hz timer
		ld	hl, 18432000/20/100 - 1
		out0	(RLDR0L), l
		out0	(RLDR0H), h
		ld	a, 00000001b	; enable PRT0, no interrupts
		out0	(TCR), a

		ld	sp, STACK	; Set a writable stack location

		;; Erase the RAM disk at $20000 with the fill byte
		xor	a
		out0	(DAR0L), a
		out0	(DAR0H), a
		out0	(SAR0B), a
		ld	a, 2
		out0	(DAR0B), a
		ld	bc, fill_byte
		out0	(SAR0L), c
		out0	(SAR0H), b

		ld	bc, 1
		out0	(BCR0L),c
		out0	(BCR0H),b

		; copy the fill byte to $20000
		ld	hl, 0110000000000010b
		out0	(DMODE),l
		out0	(DSTAT),h	; burst mode will halt CPU until complete

		; then replicate that byte throughout the RAMDISK
		xor	a
		out0	(SAR0L), a
		out0	(SAR0H), a
		ld	a, 2
		out0	(SAR0B), a

		; first time around, copy 65535 times - BCR0 will then be zero, copying 65536 bytes next time
		ld	a, $ff
		out0	(BCR0L), a
		out0	(BCR0H), a

		ld	b, 6
filler:		out0	(DSTAT), h
		djnz	filler

copybios:
		;; copy the BIOS from ROM into $F600
		xor	a
		out0	(DAR0B), a
		out0	(DAR0L), a
		ld	a, $f6
		out0	(DAR0H), a

		ld	bc, BIOSCODE
		out0	(SAR0L), c
		out0	(SAR0H), b
		ld	a, $08
biospage:	equ	$ - 1
		out0	(SAR0B), a

		ld	bc, BIOSCODE_size
		out0	(BCR0L),c
		out0	(BCR0H),b

		ld	bc, 0110000000000010b	; MEM+ -> MEM+, burst mode, ch. 0 enable
		out0	(DMODE),c
		out0	(DSTAT),b		; burst mode will halt CPU until complete

		;; let the BIOS take over now
		jp	REBOOT

fill_byte:	.db	$e5

#include	"src/cbios.asm"

;; It's not good if the boot image overruns the OS image
#assert		BIOSCODE_end <= OS_IMAGE

#code		OSDATA
		org	OS_IMAGE
#insert		"zsystem.bin"

#end
