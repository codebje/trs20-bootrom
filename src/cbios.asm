
BIOS		equ	$F600			; BIOS loads here
CCP		equ	BIOS - $1600		; $E000 - CCP entry point
BDOS		equ	BIOS - $0E00		; $E800 - BDOS entry point
FBASE		equ	BDOS + 6
IOBYTE		equ	$0003			; IO control byte location
USERDRV		equ	$0004			; Current drive control byte
ROMDISK		equ	$2000			; ROM disk location (plus $80000)
OS_IMAGE	equ	$0A00			; offset into ROM of the OS
OS_SIZE		equ	$1600			; Total size of the operating system

#code		BIOSCODE
		.phase	BIOS

;**************************************************************
;*
;*        B I O S   J U M P   T A B L E
;*
;**************************************************************

REBOOT:	JP	BIOS_REBOOT			; COLD BOOT
WBOOT:	JP	BIOS_WBOOT			; WARM BOOT
CONST:	JP	BIOS_CONST			; CONSOLE STATUS
CONIN:	JP	BIOS_CONIN			; CONSOLE CHARACTER IN
CONOUT:	JP	BIOS_CONOUT			; CONSOLE CHARACTER OUT
LIST:	JP	BIOS_LIST			; LIST CHARACTER OUT
PUNCH:	JP	BIOS_PUNCH			; PUNCH CHARACTER OUT
READER:	JP	BIOS_READER			; READER CHARACTER OUT
HOME:	JP	BIOS_HOME			; MOVE HEAD TO HOME POSITION
SELDSK:	JP	BIOS_SELDSK			; SELECT DISK
SETTRK:	JP	BIOS_SETTRK			; SET TRACK NUMBER
SETSEC:	JP	BIOS_SETSEC			; SET SECTOR NUMBER
SETDMA:	JP	BIOS_SETDMA			; SET DMA ADDRESS
READ:	JP	BIOS_READ			; READ DISK
WRITE:	JP	BIOS_WRITE			; WRITE DISK
PRSTAT:	JP	BIOS_PRSTAT			; RETURN LIST STATUS
SECTRN:	JP	BIOS_SECTRN			; SECTOR TRANSLATE

DISKPARAMS:	
		dw	0, 0			; A: ROM disk
		dw	0, 0
		dw	DIRBF, DPBLK32K
		dw	0, ALL01

		dw	0, 0			; B: RAM disk
		dw	0, 0
		dw	DIRBF, DPBLK384K
		dw	0, ALL00

		dw	0, 0			; C: SD card
		dw	0, 0
		dw	DIRBF, DPBLKSDCARD
		dw	SDCHECK, SDALLOC

MAXDISKS	equ	($ - DISKPARAMS) / 16

;; Construct a disk parameter block with label &NAME
;; Total disk size &KB (in kb), log2(block size/128) in &BITS, sectors per track in &SPT,
;; and the number of blocks to use for directory entries in &DIRENT; the disk is fixed if
;; &FIXED is a true value.
DPB:		macro	&NAME, &KB, &BITS, &SPT, &DIRENT, &FIXED
&NAME:		dw	&SPT			; SPT: sectors per track
		db	&BITS			; BSH: block shift
		db	#(1<<&BITS)-1		; BLM: block mask
		db	#1<<(&BITS - (&KB>>(&BITS-3) > 256 ? 4 : 3)) - 1 ; EXM: extent mask
		dw	#(&KB>>(&BITS-3)) - 1	; DSM: max. block number
		dw	#(1<<(&BITS+2))*&DIRENT - 1 ; DRM: max. directory entries
		db	hi($ffff << (16-&DIRENT)) ; AL0: allocation reservation
		db	lo($ffff << (16-&DIRENT)) ; AL1: allocation reservation
		dw	&FIXED ? 0 : (&DIRENT<<&BITS) ; CKS
		dw	0			; OFF: track offset
		endm

		DPB	DPBLK384K, 384, 4, 16, 2, 1		; 384k RAM disk
		DPB	DPBLK32K, 32, 3, 16, 1, 1		; 32k ROM disk
		DPB	DPBLKSDCARD, 16*1024, 7, 256, 1, 0	; 16M SD card

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_EXTFN - BIOS extended functions (RST $30)
;;
;; Functions:	0	raw ASCI0 input; byte in A, ZF set if none ready
;;		1	raw ASCI0 output; reads from E
;;
;; in:		c	ext. function
;;		de	arguments
;; out:		varies, nothing preserved
#local
BIOS_EXTFN::
		ld	(params), de
		ld	a, c
		cp	MAX_EXTFN
		ret	nc
		ld	hl, extfns
		ld	b, 0
		add	hl, bc
		add	hl, bc
		ld	de, (hl)
		ex	de, hl
		ld	de, (params)
		jp	(hl)

params:		dw	0
extfns:		dw	BIOS_RAWIN
		dw	BIOS_RAWOUT
MAX_EXTFN	equ	($ - extfns) / 2
#endlocal

#local
BIOS_RAWIN::
		ld	hl, asci0_rx_write
		ld	a, (asci0_rx_read)	; CONSOLE CHARACTER IN
		inc	a
		cp	(hl)
		ret	z			; read+1 == write, buffer empty
		ld	h, hi(asci0_rx)
		ld	l, a
		ld	a, (hl)
		push	af
		ld	a, l
		ld	(asci0_rx_read), a	; must read before updating cursor
		pop	af
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_REBOOT - cold boot
;;
;; The BOOT entry point gets control from the cold start loader and is
;; responsible for basic system initialization, including sending a sign-on
;; message. If the IOBYTE function is implemented, it must be set at this
;; point. The various system parameters that are set by the WBOOT entry point
;; must be initialized, and control is transferred to the CCP at 3400 + b for
;; further processing. Note that register C must be set to zero to select
;; drive A.
;;
#local
bootmsg:	defm	13,10,'ZSDOS+ZCPR',13,10
		defm	'TRS-20 BIOS v20201205.01',13,10,0

BIOS_REBOOT::					; COLD BOOT
		xor	a			; set up initial IOBYTE and USERDRV
		ld	(IOBYTE), a
		ld	(USERDRV), a

		ld	hl, bootmsg
		call	BIOS_PRINT

		; carry on into warm boot code
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_WBOOT - warm boot
;;
;; The WBOOT entry point gets control when a warm start occurs. A warm start
;; is performed whenever a user program branches to location 0000H, or when
;; the CPU is reset with a non-maskable interrupt. The CP/M system must be
;; loaded from the first two tracks of drive A up to, but not including, the
;; BIOS. System parameters must be initialized as follows:
;; 
;; location 0,1,2	Set to JMP WBOOT (000H: JMP 4A03H + b)
;; location 3		Set initial value of IOBYTE, if implemented
;; location 4		High nibble = current user, low nibble = current drive
;; location 5,6,7	Set to JMP FBASE, which is the primary entry point to
;;			CP/M for transient programs. (0005H: JMP 3C06H + b)
;;
;; Upon completion of the initialization, the WBOOT program must branch to
;; the CCP at 3400H + b to restart the system. Upon entry to the CCP,
;; register C is set to the drive to select after system initialization.
;; The WBOOT routine should read location 4 in memory, verify that is a
;; legal drive, and pass it to the CCP in register C.
;;
#local
BIOS_WBOOT::	di				; WARM BOOT

		ld	sp, STACK		; reset the stack

		ld	a, $c3			; set up JMPs to WBOOT, FBASE, EXT.BIOS
		ld	(0), a
		ld	(5), a
		ld	($30), a
		ld	bc, BIOS_WBOOT
		ld	(1), bc
		ld	bc, FBASE
		ld	(6), bc
		ld	bc, BIOS_EXTFN
		ld	($31), bc

		;; Copy the OS from ROM
		xor	a			; destination address 0E000
		out0	(DAR0B), a
		out0	(DAR0L), a
		ld	a, $e0
		out0	(DAR0H), a

		ld	bc, OS_IMAGE		; source address is the CPM segment ...
		out0	(SAR0L), c
		out0	(SAR0H), b
		ld	a, $08			; ... in ROM
ospage::	equ	$$-1
		out0	(SAR0B), a

		ld	bc, OS_SIZE
		out0	(BCR0L),c
		out0	(BCR0H),b

		ld	bc, 0110000000000010b	; MEM+ -> MEM+, burst mode, ch. 0 enable
		out0	(DMODE),c
		out0	(DSTAT),b		; burst mode will halt CPU until complete

		di				; no interrupts for a moment please

		ld	bc, $80
		ld	(DMAAD), bc

		xor	a			; reset RX/TX buffers
		ld	(asci0_rx_write), a
		ld	(asci0_tx_write), a
		dec	a
		ld	(asci0_rx_read), a
		ld	(asci0_tx_read), a

		;; enable ASCI0 RX interrupts
		ld	a, 00001000b
		out0	(STAT0), a

		;; set up interrupt vectors
		ld	a, hi(ivec)
		ld	i, a
		ld	a, lo(ivec)
		out0	(IL), a
		xor	a
		out0	(ITC), a

		ei				; safe to get interrupts now

		ld	a, (USERDRV)
		ld	c, a

		; check the drive - if it's invalid, reset to A:
		call	BIOS_SELDSK
		ld	a, h
		or	l
		jr	nz, loadcpm
		ld	(USERDRV), a

loadcpm:	ld	a, (USERDRV)
		ld	c, a

		jp	CCP+3

#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_CONST - console status
;;
;; Sample the status of the currently assigned console device and return 0FFH
;; in register A if a character is ready to read and 00H in register A if no
;; console characters are ready.
;;
;; returns:	A	00H if no character ready, FFH if character is ready
;;
#local
BIOS_CONST::	ld	a, (asci0_rx_read)	; CONSOLE STATUS
		inc	a
		ld	hl, asci0_rx_write
		cp	(hl)			; if read+1 == write, buffer is empty
		ld	a, 0
		jr	z, done
		dec	a
done:		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_CONIN - console input
;;
;; The next console character is read into register A, and the parity bit is
;; set, high-order bit, to zero. If no console character is ready, wait until
;; a character is typed before returning.
;;
;; If interrupts are disabled, this may never return.
;;
;; returns:	A	the input character
;;
#local
BIOS_CONIN::	
wait:		call	BIOS_RAWIN
		jr	z, wait
		and	$7f			; 7f masks parity bit
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_CONOUT - console output
;;
;; The character is sent from register C to the console output device. The
;; character is in ASCII, with high-order parity bit set to zero. Control
;; characters that may affect the console device may be filtered out.
;;
;; input:	C	output character value
;;
#local
BIOS_CONOUT::
		ld	a, c
		and	$7f
		ld	e, a
		;; fall through into BIOS_RAWOUT

#endlocal

#local
BIOS_RAWOUT::

wait:		in0	a, (STAT0)
		tst	00000010b		; check for TDRE=1
		jr	z, wait
		out0	(TDR0), e
		ret

#endlocal

#local
BIOS_PRINT::
wait:		in0	a, (STAT0)
		tst	00000010b
		jr	z, wait
		ld	a, (hl)
		or	a
		ret	z
		out0	(TDR0), a
		inc	hl
		jr	wait
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_LIST - list device output
;;
;; The character is sent from register C to the currently assigned listing
;; device. The character is in ASCII with zero parity bit.
;;
;; input:	C	output character value
;;
#local
BIOS_LIST::					; LIST CHARACTER OUT
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_PUNCH - punch device output
;;
;; The character is sent from register C to the currently assigned punch
;; device. The character is in ASCII with zero parity.
;;
;; input:	C	output character value
;;
#local
BIOS_PUNCH::					; PUNCH CHARACTER OUT
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_READER - reader character input
;;
;; The next character is read from the currently assigned reader device into
;; register A with zero parity (high-order bit must be zero); an end-of-file
;; condition is reported by returning an ASCII CTRL-Z(1AH).
;;
;; returns:	A	the character read, or 1AH for end-of-file
;;
#local
BIOS_READER::					; READER CHARACTER IN
		ld	a, $1A			; end-of-file
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_HOME - move head to home position
;;
;; The disk head of the currently selected disk (initially disk A) is moved
;; to the track 00 position. If the controller allows access to the track 0
;; flag from the drive, the head is stepped until the track 0 flag is
;; detected. If the controller does not support this feature, the HOME call
;; is translated into a call to SETTRK with a parameter of 0.
;;
#local
BIOS_HOME::					; MOVE HEAD TO HOME POSITION
		ld	c, 0
		jp	BIOS_SETTRK
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_SELDSK - select a disk
;;
;; The disk drive given by register C is selected for further operations,
;; where register C contains 0 for drive A, 1 for drive B, and so on up to 15
;; for drive P (the standard CP/M distribution version supports four drives).
;; On each disk select, SELDSK must return in HL the base address of a
;; 16-byte area, called the Disk Parameter Header, described in Section 6.10
;; of the CP/M Operating Manual.
;;
;; If there is an attempt to select a nonexistent drive, SELDSK returns
;; HL = 0000H as an error indicator. Although SELDSK must return the header
;; address on each call, it is advisable to postpone the physical disk select
;; operation until an I/O function (seek, read, or write) is actually
;; performed, because disk selects often occur without ultimately performing
;; any disk I/O, and many controllers unload the head of the current disk
;; before selecting the new drive. This causes an excessive amount of noise
;; and disk wear. The least significant bit of register E is zero if this is
;; the first occurrence of the drive select since the last cold or warm start.
;;
;; input:	C	the disk to select
;;		E	bit 0 is zero for first drive select since last boot
;; returns:	HL	the Disk Parameter Header location, or 0000H on error
;;
#local
BIOS_SELDSK::					; SELECT DISK
		ld	hl, 0			; error: will cause a reboot
		ld	a, c
		ld	(DISKNO), a
		cp	3
		ret	nc			; carry flag set if A < 3
		ld	h, 0
		ld	l, a
		add	hl, hl			; *2^4 = *16, size of disk param table
		add	hl, hl
		add	hl, hl
		add	hl, hl
		ld	bc, DISKPARAMS
		add	hl, bc
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_SETTRK - set track number
;;
;; Register BC contains the track number for subsequent disk accesses on the
;; currently selected drive. The sector number in BC is the same as the
;; number returned from the SECTRAN entry point. You can choose to seek the
;; selected track at this time or delay the seek until the next read or write
;; actually occurs. Register BC can take on values in the range 0-76
;; corresponding to valid track numbers for standard floppy disk drives and
;; 0-65535 for nonstandard disk subsystems.
;;
;; input:	BC	the track number
;;
#local
BIOS_SETTRK::					; SET TRACK NUMBER
		ld	(TRACK), bc
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_SETSEC - set sector number
;;
;; Register BC contains the sector number, 1 through 26, for subsequent disk
;; accesses on the currently selected drive. The sector number in BC is the
;; same as the number returned from the SECTRAN entry point. You can choose
;; to send this information to the controller at this point or delay sector
;; selection until a read or write operation occurs.
;;
;; input:	BC	the sector number
;;
#local
BIOS_SETSEC::					; SET SECTOR NUMBER
		ld	(SECTOR), bc
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_SETDMA - set DMA address
;;
;; Register BC contains the DMA (Disk Memory Access) address for subsequent
;; read or write operations. For example, if B = 00H and C = 80H when SETDMA
;; is called, all subsequent read operations read their data into 80H through
;; 0FFH and all subsequent write operations get their data from 80H through
;; 0FFH, until the next call to SETDMA occurs. The initial DMA address is
;; assumed to be 80H. The controller need not actually support Direct Memory
;; Access. If, for example, all data transfers are through I/O ports, the
;; CBIOS that is constructed uses the 128 byte area starting at the selected
;; DMA address for the memory buffer during the subsequent read or write
;; operations.
;;
;; input	BC	the DMA address
;;
#local
BIOS_SETDMA::					; SET DMA ADDRESS
		ld	(DMAAD), bc
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memdisk_addr - compute memory disk address
;;
;; The disk track and sector values are combined with the memory's base
;; address to produce the 20-bit absolute memory address of the sector.
;;
;; in:		BC	the memory's base address, A[19:8]
;; out:		H	A[19:16]
;;		L	A[15:8]
;;		A	A[7:0]
#local
memdisk_addr::	push	de

		; the sector address is the track times 16 plus the sector
		ld	hl, (TRACK)
		add	hl, hl			; 2^4 = 16
		add	hl, hl
		add	hl, hl
		add	hl, hl
		ld	de, (SECTOR)
		add	hl, de			; hl = track*16 + sector, 128-byte units
		add	hl, bc
		add	hl, bc

		;; source address is now hl*128
		xor	a			; clears A and CF
		rr	h			; h = h >> 1, CF = h & 1
		rr	l			; l = (h & 1) << 7 | l >> 1, CF = l & 1
		rr	a			; a = (l & 1) << 7

		pop	de
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_READ - read a disk sector
;;
;; Assuming the drive has been selected, the track has been set, and the DMA
;; address has been specified, the READ subroutine attempts to read one
;; sector based upon these parameters and returns the following error codes
;; in register A:
;;
;; 0	no errors occurred
;; 1	nonrecoverable error condition occurred
;;
;; Currently, CP/M responds only to a zero or nonzero value as the return
;; code. That is, if the value in register A is 0, CP/M assumes that the disk
;; operation was completed properly. If an error occurs the CBIOS should
;; attempt at least 10 retries to see if the error is recoverable. When an
;; error is reported the BDOS prints the message BDOS ERR ON x: BAD SECTOR.
;; The operator then has the option of pressing a carriage return to ignore
;; the error, or CTRL-C to abort.
;;
;; returns:	A	0 for no error, 1 for error
;;
#local
BIOS_READ::					; READ DISK
		ld	a, (DISKNO)
		or	a			; check for RAM disk
		jr	z, rom_read
		dec	a
		jr	z, ram_read
		dec	a
		jr	z, sd_read
		ld	a, 1
		ret

ram_read:	ld	bc, $0200		; RAM disk A[19:8]
		jr	mem_read

rom_read:	ld	bc, $0800 + hi(ROMDISK)	; ROM disk A[19:8]
		; fall through to mem_read

mem_read:	call	memdisk_addr

		out0	(SAR0L), a
		out0	(SAR0H), l
		out0	(SAR0B), h

		xor	a			; destination address in SRAM bank 0
		out0	(DAR0B), a
		ld	bc, (DMAAD)
		out0	(DAR0L), c
		out0	(DAR0H), b

		ld	bc, 128
		out0	(BCR0L),c
		out0	(BCR0H),b

		ld	bc, 0110000000000010b
		out0	(DMODE),c
		out0	(DSTAT),b		; burst mode will halt CPU until complete

		xor	a
		ret

sd_read:	call	sd_prep
		ret	nz

		;; card is ready, work out the sector address
		ld	hl, (TRACK)		; sd-sector = TRACK*256 + SECTOR
		ld	de, 8			; + 2048 sectors to first partition
		add	hl, de
		ld	a, h
		ld	h, l
		ld	l, a
		ld	(cmd17+2), hl
		ld	a, (SECTOR)
		ld	(cmd17+4), a

		call	sd_begin		; activate /CS, transmit 0xFF until 0xFF received
		ld	a, 1
		jr	nz, finish
		ld	hl, cmd17
		call	sendcmd			; send read sector command
		call	getr1			; check response
		tst	$3f
		ld	a, 1
		jr	nz, finish

		ld	hl, (DMAAD)		; read straight to DMA area
		ld	bc, 128			; read just the BDOS sector data
		call	getdata
		ld	a, 1
		jr	nz, finish

		;; consume the remaining 514-130=384 bytes
		ld	bc, $8002
eatloop:	ld	a, $ff
		call	sdtransfer
		djnz	eatloop
		dec	c
		jr	nz, eatloop

		xor	a			; signal happy success

finish:
		push	af
		call	sd_end
		pop	af
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sd_prep - check the SD card is valid and initialise it if needed
;;
;; This will read the card's operating condition status register and verify
;; that the card is ready (bit 31). If the card is not ready, it will attempt
;; to re-initialise the card. If any operation fails the routine will set A
;; to 1 and return with the Z flag set.
;;
;; in:		none
;; out:		A	1 on error, 0 otherwise
;;		FLAGS	ZF reset on error
#local
sd_prep::
		;; set up the SPI lines for low-speed access
		ld	a, SPI_CTRL_SDCARD | SPI_CTRL_400KHZ
		out0	(SPI_CTRL), a

		ld	a, 10
		ld	(ctr), a

ocrcheck:
		ld	hl, cmd58
		ld	b, 1
		call	sdcommand
		tst	$80			; if bit 7 is set there's no SD card
		jr	nz, nocard
		or	a			; if errors or IDLE, try a re-init
		jr	nz, reinit

		;; The OCR should be C0 FF 80 00 for an SDHC card. Variants will have
		;; extra bits set, but haven't been tested and should instead fail
		ld	a, $c0			; OCR bits 31:24 should be 11000000
		cp	b
		jr	nz, reinit
		ld	a, c
		inc	a
		jr	nz, nocard		; voltages (24:16) not supported
		ld	a, $80
		cp	d
		jr	nz, nocard		; voltages (15:15) not supported
		ld	a, e
		or	a
		jr	nz, nocard		; last byte should be zero

		;; set up the SPI lines for high-speed access
		ld	a, SPI_CTRL_SDCARD | SPI_CTRL_6MHZ
		out0	(SPI_CTRL), a

		xor	a			; A=0, ZF set
		ret

nocard:
		xor	a
		or	a, 1			; reset ZF and set A=1
		ret

		;; (re-)initialise the SD card, if possible
reinit:
		;; check that the init counter isn't clear
		ld	a, (ctr)
		or	a
		jr	z, nocard

		;; begin with 80 clock pulses, /CS inactive
		ld	b, 10
pulses:		ld	a, $ff
		call	sdtransfer
		djnz	pulses

		;; CMD0 to put the card into IDLE mode
		ld	hl, cmd0
		ld	b, 0
		call	sdcommand
		dec	a			; R1 should be $01 after CMD0
		jr	nz, nocard

		;; CMD8 to set voltage and indicate SDHC support
		ld	hl, cmd8
		ld	b, 1
		call	sdcommand
		dec	a			; R1 should still be $01
		jr	nz, nocard

		;; validate voltage support and bit pattern: DE=$01AA
		dec	d
		jr	nz, nocard		; voltage invalid
		ld	a, $aa
		cp	e
		jr	nz, nocard		; bit pattern mismatch

		ld	a, 10			; try ACMD41 up to 10 times
		ld	(ctr), a
initloop:
		ld	hl, cmd55
		ld	b, 0
		call	sdcommand
		tst	%11111110		; IDLE is ok, all else is not
		jr	nz, nocard

		ld	hl, acmd41
		ld	b, 0
		call	sdcommand
		or	a
		jr	z, ready		; good news, everyone
		tst	%11111110		; any errors?
		jr	nz, nocard

		ld	bc, 2			; 20ms delay
		call	delayms

		ld	hl, ctr
		dec	(hl)
		jr	nz, initloop

		jr	nocard			; oh well, we tried

ready:
		; re-do the OCR check, but don't try init again
		xor	a
		ld	(ctr), a
		jp	ocrcheck

ctr:		.db	0
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_WRITE - write a disk sector
;;
;; Data is written from the currently selected DMA address to the currently
;; selected drive, track, and sector. For floppy disks, the data should be
;; marked as nondeleted data to maintain compatibility with other CP/M
;; systems. The error codes given in the READ command are returned in
;; register A, with error recovery attempts as described for BIOS_READ.
;;
;; returns:	A	0 for no error, 1 for error
;;
#local
BIOS_WRITE::					; WRITE DISK
		ld	a, (DISKNO)
		dec	a
		jr	z, ram_write		; write RAM disk
		dec	a
		jr	z, sd_write		; or SD card
		ld	a, 1
		ret

ram_write:	ld	bc, $0200

		call	memdisk_addr

		out0	(DAR0L), a
		out0	(DAR0H), l
		out0	(DAR0B), h

		xor	a			; source address in SRAM bank 0
		out0	(SAR0B), a
		ld	bc, (DMAAD)
		out0	(SAR0L), c
		out0	(SAR0H), b

		ld	bc, 128
		out0	(BCR0L),c
		out0	(BCR0H),b

		; increment both src and dst memory addresses, burst mode, DMA0 enable
		ld	bc, 0110000000000010b
		out0	(DMODE),c
		out0	(DSTAT),b		; burst mode will halt CPU until complete

		xor	a
		ret

sd_write:	call	sd_prep
		ret	nz

		;; card is ready, work out the sector address
		ld	hl, (TRACK)		; sd-sector = TRACK*256 + SECTOR
		ld	de, 8			; + 2048 sectors to first partition
		add	hl, de
		ld	a, h
		ld	h, l
		ld	l, a
		ld	(cmd24+2), hl
		ld	a, (SECTOR)
		ld	(cmd24+4), a

		call	sd_begin		; activate /CS, transmit 0xFF until 0xFF received
		ld	a, 1
		jr	nz, finish
		ld	hl, cmd24
		call	sendcmd			; send write sector command
		call	getr1			; check response
		tst	$3f
		ld	a, 1
		jr	nz, finish

		ld	a, 0xfe			; data start token
		call	sdtransfer

		ld	hl, (DMAAD)		; write straight from DMA area
		ld	b, 128			; write just the BDOS sector data
writeloop:	ld	a, (hl)
		inc	hl
		call	sdtransfer
		djnz	writeloop

		;; write the remaining 514-128=386 bytes
		ld	bc, $8202
stuffloop:	ld	a, $ff
		call	sdtransfer
		djnz	stuffloop
		dec	c
		jr	nz, stuffloop

		;; try 25 times to get a response
		ld	b, 25
acceptloop:	ld	a, $ff
		call	sdtransfer
		cp	a, $ff
		jr	nz, accepted
		push	bc
		ld	bc, 1
		call	delayms
		pop	bc
		djnz	acceptloop
		ld	a, 1
		jr	finish

accepted:	and	a, $1f
		cp	a, $5
		ld	a, 1
		jr	nz, finish

		;; try 25 times to let the card do its work
		ld	b, 25
busyloop:	ld	a, $ff
		call	sdtransfer
		or	a
		jr	nz, written
		push	bc
		ld	bc, 1
		call	delayms
		pop	bc
		djnz	busyloop
		ld	a, 1
		jr	finish

written:
		xor	a			; signal happy success

finish:
		push	af
		call	sd_end
		pop	af
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_PRSTAT - list device status
;;
;; Return the ready status of the list device used by the DESPOOL program to
;; improve console response during its operation. The value 00 is returned in
;; A if the list device is not ready to accept a character and 0FFH if a
;; character can be sent to the printer. A 00 value should be returned if
;; LIST status is not implemented.
;;
;; returns	A	00H if not ready, FFH if ready
;;
#local
BIOS_PRSTAT::					; RETURN LIST STATUS
		xor	a
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIOS_SECTRN - sector translation
;;
;; Logical-to-physical sector translation is performed to improve the overall
;; response of CP/M. Standard CP/M systems are shipped with a skew factor of
;; 6, where six physical sectors are skipped between each logical read
;; operation. This skew factor allows enough time between sectors for most
;; programs to load their buffers without missing the next sector. In
;; particular computer systems that use fast processors, memory, and disk
;; subsystems, the skew factor might be changed to improve overall response.
;; However, the user should maintain a single-density IBM-compatible version
;; of CP/M for information transfer into and out of the computer system,
;; using a skew factor of 6.
;;
;; In general, SECTRAN receives a logical sector number relative to zero in
;; BC and a translate table address in DE. The sector number is used as an
;; index into the translate table, with the resulting physical sector number
;; in HL. For standard systems, the table and indexing code is provided in
;; the CBIOS and need not be changed.
;;
;; input:	BC	logical sector number
;;		DE	sector translate table
;; returns:	HL	physical sector number
;;
#local
BIOS_SECTRN::					; SECTOR TRANSLATE
		ld	hl, bc
		ret
#endlocal

TRACK:		DS	2			;TWO BYTES FOR EXPANSION
SECTOR:		DS	2			;TWO BYTES FOR EXPANSION
DMAAD:		DS	2			;DIRECT MEMORY ADDRESS
DISKNO:		DS	1			;DISK NUMBER 0-15

DIRBF:		DS	128			;SCRATCH DIRECTORY AREA
ALL00:		DS	24			;ALLOCATION VECTOR 0	DSM=191, vector size = 192/8 = 24 bytes
ALL01:		DS	4			;ALLOCATION VECTOR 1	DSM=31

SDCHECK:	DS	128			; 128 bytes for SD card checksum vector
SDALLOC:	DS	256			; 128 bytes for SD card allocation vector, x2

#local
irq_int1::
irq_int2::
irq_prt1::
irq_dma0::
irq_dma1::
irq_csio::
irq_asci1::
		ei
		reti
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delayms - delay for a number of milliseconds
;;
;; delayms relies on PRT0 which is running at 100Hz, and therefore will delay
;; for a given number of 10ms intervals.
;;
;; in:		bc	the number of 10ms intervals to to delay
#local
delayms::
		push	af

		; reset PRT0 counter flag
		in0	a, (TCR)
		in0	a, (TMDR0L)
		in0	a, (TMDR0H)

waitloop:
		in0	a, (TCR)		; check if timer has fired
		tst	%01000000		; test TIF0
		jr	z, waitloop
		in0	a, (TMDR0L)		; reset TIF0 by reading TMDR0
		in0	a, (TMDR0H)
		dec	bc
		ld	a, b
		or	c
		jr	nz, waitloop

		pop	af
		ret

#endlocal

#local
irq_prt0::	; todo: store a 'systick' timer variable
		ei
		reti
#endlocal

#local
irq_asci0::	push	af
		push	bc
		push	hl

		; read until STAT0 stops indicating bytes available
reads:		in0	a, (STAT0)
		tst	01110000b		; check for errors
		jr	nz, error
		tst	10000000b		; RDRF in bit 7 is set when a byte is ready to read
		jr	z, writes
		in0	b, (RDR0)

		ld	a, (asci0_rx_write)	; load the write cursor
		ld	hl, asci0_rx_read	; compare to the read cursor
		cp	(hl)			; if the cursors are equal the buffer is full
		jr	z, reads		; ... drop the byte, but keep draining ASCI buffer

		ld	h, hi(asci0_rx)		; write the received byte at the write cursor
		ld	l, a
		ld	(hl), b

		inc	a			; increment the write cursor
		ld	(asci0_rx_write), a

		jr	reads			; read any more pending bytes

error:		ld	(asci0_rx_error), a	; store the error bits
		ld	a, 01100100b		; Reset errors (bit 4=0)
		out0	(CNTLA0), a
		jr	reads

		;; write one byte, if ready
writes:		tst	00000010b		; TDRE in bit 1 is set when a byte can be sent
		jr	z, done

		; read: if read cursor + 1 = write cursor, the buffer is empty

		ld	a, (asci0_tx_read)	; load the read cursor, and increment
		inc	a

		ld	hl, asci0_tx_write	; compare to write cursor
		cp	a, (hl)

		jr	z, txempty		; caught up to write cursor, nothing left to send

		ld	h, hi(asci0_tx)		; read from buffer, write to ASCI
		ld	l, a
		ld	b, (hl)
		out0	(TDR0), b

		ld	(asci0_tx_read), a	; update read cursor
		jr	done

txempty:	in0	a, (STAT0)
		and	a, 11111110b		; disable TX interrupt while buffer is empty
		out0	(STAT0), a

done:		pop	hl
		pop	bc
		pop	af
		ei
		reti
#endlocal

#include	"sdcard.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASCI transmit/receive buffers
;;
;; The ASCI transmit and receive buffers are 256 byte ring buffers. For each
;; buffer there is a read cursor and a write cursor. Reading increments the
;; read cursor, then compares to the write cursor. If they are equal, the
;; buffer is empty. If not, the byte at the read cursor is consumed, and the
;; incremented read cursor is stored. Writing compares the write cursor to
;; the read cursor. If they are equal, the buffer is full. Otherwise, a byte
;; is written at the write cursor location, and the write cursor is
;; incremented. With one producer and one consumer, locks are not needed.
;;
asci0_rx_read	ds	1
asci0_rx_write	ds	1
asci0_tx_read	ds	1
asci0_tx_write	ds	1
asci0_rx_error	ds	1

		.align	$20
ivec:		dw	irq_int1
		dw	irq_int2
		dw	irq_prt0
		dw	irq_prt1
		dw	irq_dma0
		dw	irq_dma1
		dw	irq_csio
		dw	irq_asci0
		dw	irq_asci1

		.align	$100
BIOS_DATA:	equ	$

#data		BIOSDATA
asci0_rx	ds	256
asci0_tx	ds	256

;; The BIOSROM segment contains code that needs to be mapped into the bank area at logical $8000.
#code		BIOSROM

		.dephase
		.phase	$8000+$

		.dephase


