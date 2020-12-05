;; TRS-20 I/O ports
FPGA_CTRL		equ	$f0		; FPGA control (write)
SPI_CTRL		equ	$f1		; SPI control
SPI_DATA		equ	$f2		; SPI data
FPGA_VERSION		equ	$f3		; FPGA version (read)

SPI_CTRL_ENABLE		equ	$01		; Enable SPI: set bit 1
SPI_CTRL_FLASHROM	equ	$00		; Flash ROM: clear bit 2
SPI_CTRL_SDCARD		equ	$02		; SD card: set bit 2
SPI_CTRL_BULKREAD	equ	$08		; Bulk read mode
SPI_CTRL_50MHZ		equ	$00		; 50MHz clock: clear clock bits
SPI_CTRL_25MHZ		equ	$10		; 25MHz clock: 001
SPI_CTRL_12MHZ		equ	$20		; 12MHz clock: 010
SPI_CTRL_6MHZ		equ	$30		; 6.25MHz SPI clock: 011
SPI_CTRL_400KHZ		equ	$70		; 390kHz SPI clock: 111
SPI_CTRL_BUSY		equ	$80		; Busy flag (read-only)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sdtransfer - write a byte, read a byte
;;
;; in:		a	the byte to send
;; out:		a	the byte received
#local
sdtransfer::
		out0	(SPI_DATA), a
		call	busy
		in0	a, (SPI_DATA)
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sd_begin - begin an SD SPI exchange
;;
;; sd_begin will send a dummy byte, lower /CS, then send dummy bytes until
;; $ff is received back.
;;
;; in:		none
;; out:		a	undefined
;;		b	0 if $ff was never received
;;		flags	ZF set on success, reset on timeout
#local
sd_begin::
		ld	a, $ff
		call	sdtransfer

		in0	a, (SPI_CTRL)
		or	a, SPI_CTRL_ENABLE
		out0	(SPI_CTRL), a

		ld	b, 50
waitready:
		ld	a, $ff
		call	sdtransfer
		cp	a, $ff
		ret	z
		djnz	waitready
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sd_end - end an SD SPI exchange
;;
;; sd_end will send a dummy byte, raise /CS, and send another dummy byte.
;;
;; in:		none
;; out:		a	undefined
#local
sd_end::
		ld	a, $ff
		call	sdtransfer

		in0	a, (SPI_CTRL)
		and	a, ~SPI_CTRL_ENABLE
		out0	(SPI_CTRL), a

		ld	a, $ff
		call	sdtransfer
		ld	a, $ff
		jp	sdtransfer
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sdcommand - send a command and receive a response
;;
;; sdcommand will activate the SPI slave select line, then send 0xff until
;; $ff is received. It will then send six bytes from (HL), receive an R1
;; response in A, then receive an R3 or R7 response in BC/DE. The SPI slave
;; line will then be deactivated.
;;
;; in:		hl	pointer to command to send
;;		b	0 for R1 responses
;;			1 for R3/R7 responses
;; out:		a	R1 response code - bit 7 is set on timeout
;;		bc	response word 31:16
;;		de	response word 15:0
#local
sdcommand::
		push	bc
		call	sd_begin
		jr	nz, abort
		call	sendcmd
		call	getr1
		pop	bc
		ld	h, a
		jr	nz, abort

		djnz	abort

		ld	a, $ff
		call	sdtransfer
		ld	b, a
		ld	a, $ff
		call	sdtransfer
		ld	c, a
		ld	a, $ff
		call	sdtransfer
		ld	d, a
		ld	a, $ff
		call	sdtransfer
		ld	e, a

abort:
		call	sd_end
		ld	a, h
		ret
#endlocal

;; sendcmd: send a 6-byte command sequence from hl to (SPI_DATA)
#local
sendcmd::	ld	b, 6

byteloop:	ld	a, (hl)
		call	sdtransfer
		inc	hl
		djnz	byteloop

		ret
#endlocal

;; wait for SPI to be done
#local
busy::		in0	a, (SPI_CTRL)
		tst	SPI_CTRL_BUSY
		jr	nz, busy
		ret
#endlocal

;; get an R1 response - ZF is reset on exit on timeout
#local
getr1::		ld	b, 12

readloop:	ld	a, $ff
		call	sdtransfer
		tst	$80
		ret	z

		djnz	readloop

		ld	a, $ff
		ret
#endlocal

;; get a word response into (hl)
#local
getword::
		ld	bc, $04 << 8 | SPI_DATA
		ld	d, $ff
readloop:	out0	(SPI_DATA), d
		call	busy
		ini
		jr	nz, readloop
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getdata - read a data block
;;
;; in:		hl	the data storage area
;;		bc	the number of bytes to read (excl. CRC)
;; out:		flags	ZF reset on timeout
#local
getdata::
		push	bc

		ld	b, 0
tokenloop:	
		ld	a, $ff
		out0	(SPI_DATA), a
		call	busy
		in0	a, (SPI_DATA)
		cp	$fe
		jr	z, responded
		djnz	tokenloop

		pop	bc
		or	1			; reset ZF to signal timeout
		ret

responded:	
		pop	bc
bytes:		ld	a, $ff
		out0	(SPI_DATA), a
		call	busy
		in0	a, (SPI_DATA)
		ld	(hl), a
		inc	hl

		dec	bc
		ld	a, b
		or	c
		jr	nz, bytes

		;; skip two CRC bytes
		ld	a, $ff
		out0	(SPI_DATA), a
		call	busy
		ld	a, $ff
		out0	(SPI_DATA), a
		call	busy

		;; set ZF
		xor	a

		ret

#endlocal

cmd0:		.db	$40, $00, $00, $00, $00, $95		; puts the card into SPI mode
cmd8:		.db	$48, $00, $00, $01, $aa, $87		; sets voltage to 01 = 2.7v-3.6v
cmd17:		.db	$51, $00, $00, $00, $00, $01		; read block
cmd24:		.db	$58, $00, $00, $00, $00, $01		; write block
cmd55:		.db	$77, $00, $00, $00, $00, $01		; application command follows
cmd58:		.db	$7a, $00, $00, $00, $00, $01		; read OCR

acmd41:		.db	$69, $40, $00, $00, $00, $01		; initialise card
