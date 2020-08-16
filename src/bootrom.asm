.z180

#target rom

#include "z180registers.asm"

; Section definitions
#code		RESTARTS,$0000,$0100	; RESTART vector table
#code		BOOT,$0100		; BOOT code
#code		CPM			; The CPM segment is straight after the boot code
#code		ROMDISK,$2000,$8000	; the ROM disk is 32kb and must be aligned to 256 bytes
#data		DATA			; DATA starts after the code
#data		BIOSDATA,$DD00,$300	; BIOS data region

; Constants and definitions
STACK		equ	BIOSDATA	; point the stack at the end of SRAM
UPLOAD		equ	$2000		; uploaded code beyond CPM

; Macros

#local
		macro	asci0_send &data

		; wait for TDRE=1 to indicate transmit register is empty
		ld	b, &data
tdre		defl	$
		in0	a, (STAT0)
		tst	00000010b		; bit 1: TDRE
		jr	z, tdre
		out0	(TDR0), b

		endm
#endlocal

#code		RESTARTS

		.org	$0000
reset:		jp	_start

#code		BOOT

		; include a visual marker of the boot rom version
version:	defm	13, 10, 'TRS-20 boot rom v20200816.01', 13, 10, 0

_start:		xor	a
		out0	(RCR), a	; disable the DRAM refresh
		out0	(ITC), a	; disable INT0 (and others)

		; Increase clock speed to 18.432MHz
		ld	a, $80
		out0	(CCR), a	; boost up to 18.432MHz

		; Prepare ASCI0 very early, to allow status bytes to be sent during the boot-up sequence
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
		;   Common Area 1	f000-ffff	80000-80fff
		;   Bank Area		0000-efff	00000-0efff
		;   Common Area 1	0000-0000	00000-00000
		xor	a
		out0	(BBR), a
		ld	a, $80 - $0F
		out0	(CBR), a
		ld	a, $F0
		out0	(CBAR), a

		jp	$+3+$f000

		; SRAM should now be unmasked. Copy ROM into RAM, then jump back.

		xor	a		; destination address 00000
		out0	(DAR0L), a
		out0	(DAR0H), a
		out0	(DAR0B), a

		out0	(SAR0L), a	; source address 80000
		out0	(SAR0H), a
		ld	a, $08
		out0	(SAR0B), a

		ld	bc, CPM_end	; copy all the code segments
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

		jp	$+3

		; Set up PRT0 with a 100Hz timer
		ld	hl, 18432000/20/100 - 1
		out0	(RLDR0L), l
		out0	(RLDR0H), h
		ld	a, 00000001b	; enable PRT0, no interrupts
		out0	(TCR), a

		ld	sp, STACK	; Set a writable stack location

booting:	call	boot_monitor	; get an input char

		ld	hl, cmdtab
		ld	b, a
keys:		ld	a, (hl)
		inc	hl
		or	a
		jr	z, unknown
		cp	a, b
		jr	z, match
		inc	hl
		inc	hl
		jr	keys

match:		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		call	jump
		jr	booting
jump:		jp	(hl)

unknown:	ld	hl, badcmd
		call	asci0_xmit
		jr	booting

badcmd:		.text	'Unknown command, try "?" for help'
		.db	13,10,0

cmdtab:		.db	'?'
		.dw	boot_help
		.db	'y'
		.dw	ymodem_upload
		.db	'j'
		.dw	boot_execute
		.db	'c'
		.dw	cpm_execute
		.db	0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boot_help
#local
boot_help::	push	hl
		ld	hl, message
		call	asci0_xmit
		pop	hl
		ret
message		.text	'Boot ROM menu:',13,10
		.text	'  ? - this help',13,10
		.text	'  j - jump to uploaded code',13,10
		.text	'  y - ymodem receive',13,10
		.text	'  c - Start CP/M 2.2',13,10,0
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boot_execute
#local
boot_execute::	jp	UPLOAD
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cpm_execute
#local
fill_byte:	.db	$e5
cpm_execute::	xor	a		; reset MMU to flat SRAM for all 64k
		out0	(BBR), a	; Bank Area starts at physical 00000
		out0	(CBR), a	; Common Area 1 starts at physical 0000
		ld	a, $F0		; Bank Area starts at logical 0000
		out0	(CBAR), a	; Common Area 1 starts at logical F000

		xor	a		; destination address 20000
		out0	(DAR0L), a	; source address FILL_BYTE in bank zero
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

		; copy CPM into place
		xor	a			; destination address 0E000
		out0	(DAR0B), a
		out0	(DAR0L), a
		ld	a, $e0
		out0	(DAR0H), a

		ld	bc, CPM			; source address is the CPM segment ...
		out0	(SAR0L), c
		out0	(SAR0H), b
		ld	a, $08			; ... in ROM
		out0	(SAR0B), a

		ld	bc, $2000 - CPM
		out0	(BCR0L),c
		out0	(BCR0H),b

		ld	bc, 0110000000000010b	; MEM+ -> MEM+, burst mode, ch. 0 enable
		out0	(DMODE),c
		out0	(DSTAT),b		; burst mode will halt CPU until complete

		jp	CPM_ENTRY

#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bin_to_hex - convert an 8-bit binary value to hex digits
;;
;; https://stackoverflow.com/questions/22838444/convert-an-8bit-number-to-hex-in-z80-assembler
;;
;; in:		a	value
;; out:		de	hex digits
#local
bin_to_hex::	push	af
		push	bc
		ld	c, a
		call	shift
		ld	e, a
		ld	a, c
		call	convert
		ld	d, a
		pop	bc
		pop	af
		ret

shift:		rra		; shift higher nibble to lower
		rra
		rra
		rra
convert:	or	a, $f0
		daa		; I've no idea if this will work on a Z180...
		add	a, $a0
		adc	a, $40
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boot_monitor
;;
;; Wait for user input on ASCI0, periodically transmitting a "hello"
;; message.
;;
;; in:		none
;; out:		the user input in A
#local
boot_monitor::	push	bc
usrwait:	ld	hl, version
		call	asci0_xmit	; shout into the void

		ld	hl, input
		ld	bc, 1
		ld	de, 3000	; 3000x100Hz = 30 second timeout
		call	recv_wait

		jr	z, usrwait	; on timeout, go back and shout again

get_char:	ld	a, (input)
		pop	bc
		ret

input		.ds	1

#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; send_byte
;;
;; Sends one byte via ASCI0
;;
;; in:		a	the byte to send
;; out:		none
#local
send_byte::	push	af
		push	bc
		asci0_send a
		pop	bc
		pop	af
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recv_byte
;;
;; Receives one byte via ASCI0, with a timeout
;;
;; in:		de	the timeout measured in 100Hz ticks
;; out:		a	the byte that was received
;;		zf	zf is SET if a timeout occurred
#local
recv_byte::	push	bc
		push	de
		push	hl

		ld	hl, byte+1	; write to the ld operand
		ld	bc, 1
		call	recv_wait
byte:		ld	a, 0

		pop	hl
		pop	de
		pop	bc
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recv_wait
;;
;; Receives BC bytes from ASCI0 into HL, with a timeout of DEx100Hz.
;;
;; in:		bc	count of bytes to read
;;		hl	destination buffer
;;		de	timeout, in units of 100Hz
;; out:		zf	zf is SET if a timeout occurred
;;		bc	the count of bytes left unread
;;		de	the original timeout value
;;		hl	one past the last byte read
;;		a	undefined
#local
recv_wait::	push	de		; preserve the timeout value
input_loop:	pop	de
		push	de

		; check for input on ASCI0 in a loop
timeout:	in0	a, (STAT0)
		tst	01110000b	; check for errors
		jr	nz, error
		tst	10000000b	; RDRF in bit 7 is set when a byte is ready to read
		jr	nz, get_char

		; check if PRT0 has fired
		in0	a, (TCR)
		tst	01000000b	; TIF0 in bit 6
		jr	z, timeout	; not set: check for input again
		in0	a, (TMDR0L)	; reset TIF0 by reading TMDR0
		in0	a, (TMDR0H)	; must read both halves in order!  
		dec	de		; track how many times PRT0 has fired
		ld	a,d
		or	a,e
		jr	nz, timeout

		; failed to receive a byte: abort with Z set
		pop	de
		ret

error:		ld	a, 01100100b	; Reset errors (bit 4=0)
		out0	(CNTLA0), a
		jr	timeout

get_char:	in0	a, (RDR0)
		ld	(hl), a
		inc	hl
		dec	bc
		ld	a, b
		or	a, c
		jr	nz, input_loop

		or	a, 1		; clear Z
return:		pop	de
		ret

#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asci0_xmit 
;;
;; Transmit an ASCIIZ message synchronously over ASCI0
;;
;; in:		hl	the message to send
;; out:		none
#local
asci0_xmit::	push	af
		push	bc
		push	hl

xmit:		ld	a, (hl)

		; check for NUL terminator
		or	a
		jr	z, done

		; wait for TDRE=1 to indicate transmit register is empty
		ld	b, a
tdre:		in0	a, (STAT0)
		tst	00000010b		; bit 1: TDRE
		jr	z, tdre
		out0	(TDR0), b

		inc	hl
		jr	xmit

done:		pop	hl
		pop	bc
		pop	af
		ret
#endlocal

#include 	"ymodem.asm"

#code		CPM
#include	"cbios.asm"

		org ROMDISK
#code		ROMDISK
#insert		"../bin/romdisk.img"

#data		DATA

#assert		CPM_size <= $2000

#end
