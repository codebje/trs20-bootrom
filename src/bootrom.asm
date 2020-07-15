.z180

#target rom

#include "z180registers.asm"

; Section definitions
#code		RESTARTS,$0000,$0100	; RESTART vector table
#code		BOOT,$0100		; BOOT code
#data		DATA,$1000		; DATA starts after the code
#data		UPLOAD,$2000		; uploaded file contents go here

; Constants and definitions
STACK		equ	$F000		; point the stack at the end of SRAM

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
four:		.db	'4'

#code		BOOT

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
		;	bit 5, PS=1 selects /30
		;	bit 3, DR=0 selects /16
		;	bis 2-0, SS=000 selects /1
		;	18432000 / 30 / 16 / 1 = 38400 baud
		ld	a, 00100000b
		out0	(CNTLB0), a

		; Transmit a '1' to indicate we got this far
		asci0_send '1'

		; Copy 4k of ROM into RAM. Programming the ROM requires that code is not being executed out of ROM.
		xor	a		; destination address 00000
		out0	(DAR0L), a
		out0	(DAR0H), a
		out0	(DAR0B), a

		out0	(SAR0L), a	; source address 80000
		out0	(SAR0H), a
		ld	a, $08
		out0	(SAR0B), a

		ld	bc, BOOT_end	; copy all the code segments
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

		asci0_send '2'

		; A19 went high during the DMA transfer, and the ROM overlay is no longer
		; in effect. Code is now executing from SRAM - so set up Common Area 1 to
		; the first page of the ROM again.

		; Set up the MMU:
		;   Common Area 1	f000-ffff	80000-80fff
		;   Bank Area		0000-efff	00000-0efff
		;   Common Area 1	0000-0000	00000-00000
		ld	a, $80
		out0	(CBR), a

		; confirm that MMU programming didn't fault
		asci0_send '3'

		; read a byte from high-ROM to verify MMU is programmed for it
		ld	hl, four + $f000
		asci0_send (hl)

		; attempt to modify a byte in what should now be SRAM
		ld	a, '5'
		ld	(five+1),a
five:		ld	a, '!'
		asci0_send a

		; Set up PRT0 with a 100Hz timer
		ld	hl, 18432000/20/100 - 1
		out0	(RLDR0L), l
		out0	(RLDR0H), h
		ld	a, 00000001b	; enable PRT0, no interrupts
		out0	(TCR), a

		; Verify the PRT0 programming didn't fault
		asci0_send '6'

		ld	sp, STACK	; Set a writable stack location

		; Verify the stack is writable
		ld	b, '7'
		push	bc
		pop	de
		asci0_send d

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
		.db	'b'
		.dw	boot_rom
		;.db	'w'
		;.dw	burn_rom
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
		.text	'  b - boot ROM+4k',13,10
		.text	'  y - ymodem receive',13,10,0
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boot_rom
#local
boot_rom::	ret
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
usrwait:	ld	hl, message
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

message		.db	13,10
		.text	'TRS-20 online and ready'
		.db	13,10,0
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

#include "ymodem.asm"

#data		DATA
#end
