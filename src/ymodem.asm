YM_SOH		equ	$01		; start of a 128-byte packet
YM_STX		equ	$02		; start of a 1024-byte packet
YM_EOT		equ	$04		; end of transmission
YM_ACK		equ	$06		; received ok
YM_NAK		equ	$15		; receive error
YM_CAN		equ	$18		; cancel transmission
YM_CRC		equ	'C'		; request CRC-16 mode

#data		UPLOAD
upload_file	equ	$
#code		BOOT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ymodem_upload
;;
;; Writes files received in sequence starting from (upload_file).
#local
ymodem_upload::	push	af
		push	bc
		push	de
		push	hl

		ld	hl, upload_file
		push	hl

		ld	hl, ready
		call	asci0_xmit

receive_file:	ld	c, YM_CRC
		pop	hl
		push	hl
		call	recv_packet
		jr	z, abort

		cp	a, YM_EOT
		jr	z, ack_eot

		; a NUL filename indicates all done
		ld	a, (hl)
		or	a
		jr	z, complete

		; could parse out size or filename, but not necessary

		ld	a, YM_ACK
		call	send_byte
		ld	a, YM_CRC
		call	send_byte

		ld	a, 1
		ld	(block_nr), a
receive_data:	pop	hl
		push	hl
		call	recv_packet
		cp	a, YM_EOT	; EOT indicates file transfer complete
		jr	z, ack_eot
		ld	b, a
		ld	a, (block_nr)
		cp	a, c
		jr	nz, block_error
		inc	a
		ld	(block_nr), a	; update block number
		ld	a, b
		ld	bc, 128		; SOH means 128-byte block
		cp	a, YM_SOH
		jr	z, $+5
		ld	bc, 1024	; STX means 1024-byte block
		pop	hl
		add	hl, bc
		push	hl
		ld	a, YM_ACK
		call	send_byte
		jr	receive_data	; go back for next block

abort: 		ld	hl, cancan
		call	asci0_xmit
		call	flush_rx
		ld	hl, msg_aborted
		jr	print

ack_eot:	ld	a, YM_ACK
		call	send_byte
		ld	a, YM_CRC
		call	send_byte
		jr	receive_file

block_error:	dec	a		; might be last block re-sent?
		cp	a, c
		jr	nz, abort	; alas no, give up
		ld	a, YM_ACK	; if yes, re-ACK it
		call	send_byte
		jr	receive_data

complete:	ld	a, YM_ACK
		call	send_byte
		call	flush_rx
		ld	hl, msg_complete

print:		call	asci0_xmit

		pop	hl
		pop	hl
		pop	de
		pop	bc
		pop	af
		ret

cancan:		.db	YM_CAN, YM_CAN, 0
ready:		.text	'YModem upload ready... ', 0
msg_aborted:	.text	'aborted', 13, 10, 0
msg_complete:	.text	'complete', 13, 10, 0

#data		DATA
cmd		.ds	1		; the received command byte
block_nr	.ds	1		; the expected block number
#code		BOOT

#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recv_packet
;;
;; Receive a Y-Modem packet, retrying up to ten times.
;;
;; in:		hl	destination of packet (1026 bytes required)
;; 		c	the byte to send in case of timeout
;; out:		zf	set on error
;;		a	the command code received
;;		c	the packet sequence number
#local
recv_packet::	push	de
		push	hl

		ld	b, 10		; retry ten times
		jr	read_cmd

metadata:	ld	a, c
		call	send_byte

read_cmd:	ld	de, 5*100
		call	recv_byte
		jr	z, retry

		cp	a, YM_CAN	; Cancel?
		jr	z, cancel

		cp	a, YM_EOT	; End of transmission?
		jr	z, eot

		cp	a, YM_SOH
		jr	z, recv_body

		cp	a, YM_STX
		jr	z, recv_body

		call	flush_rx	; anything else, clear it out and retry

retry:		djnz	metadata

done:		pop	hl
		pop	de
		ret

cancel:		ld	de, 1*100
		call	recv_byte
		jr	z, retry
		cp	a, YM_CAN
		jr	z, done
		jr	retry

eot:		or	a		; clear zf - a contains a non-zero value
		jr	done

recv_body:	ld	(cmd), a
		ld	de, 1*100
		call	recv_byte
		jr	z, retry
		ld	c, a
		call	recv_byte
		jr	z, retry
		cpl
		cp	a, c
		jr	nz, retry
		ld	(seq), a

		push	hl
		push	bc

		ld	bc, 130
		ld	a, (cmd)
		cp	a, YM_SOH
		jr	z, $+5
		ld	bc, 1026

		ld	de, 1*100
		call	recv_wait

		pop	bc
		pop	hl

		jr	z, retry
		call	ym_crc		; CRC should be zero
		ld	a, d
		or	e
		jr	nz, retry
		ld	a, (seq)
		ld	c, a
		ld	a, (cmd)
		or	a		; clear zf
		jr	done
cmd:		.db	0
seq:		.db	0

#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flush_rx
;;
;; Flush the RX pipeline
;;
;; in:		none
;; out:		none
#local
flush_rx::	push	af
		push	de
		ld	de, 10		; brief timeout only
loop:		call	recv_byte
		jr	nz, loop
		pop	de
		pop	af
		ret
#endlocal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ym_crc
;;
;; Compute a Y-Modem CRC over the data at HL of size BC
;;
;; in:		hl	the data to checksum
;;		bc	the size of the data
;; out:		de	the checksum of the data
#local
ym_crc::	push	hl
		push	bc
		push	af

		ld	de,0
		ld	(crc),de

bytloop:	ld	a, (crc+1)	; pos = (crc >> 8) ^ data[i]
		xor	(hl)
		ld	(pos), a

		ld	a, (crc)	; crc = crc << 8
		ld	(crc+1), a
		xor	a
		ld	(crc), a

		push	hl
		ld	hl, pos
		xor	a
		rrd			; a = pos & 0xf, pos = pos >> 4
		sla	a
		sla	(hl)
		ld	d, (hl)

		ld	h, hi(ym_crc_tab)
		add	a, lo(ym_crc_tab)
		ld	l, a

		ld	a, (hl)		; crc = crc ^ ym_crc_tab[pos & 0xf]
		inc	hl
		ld	e, (hl)
		ld	hl, crc
		xor	a, (hl)
		ld	(hl), a
		inc	hl
		ld	a, (hl)
		xor	e
		ld	(hl), a

		ld	h, hi(ym_crc_tab+32)
		ld	a, d
		add	a, lo(ym_crc_tab+32)
		ld	l, a

		ld	a, (hl)		; crc = crc ^ ym_crc_tab[(pos >> 4) + 16]
		inc	hl
		ld	e, (hl)
		ld	hl, crc
		xor	a, (hl)
		ld	(hl), a
		inc	hl
		ld	a, (hl)
		xor	e
		ld	(hl), a

		pop	hl
		inc	hl
		dec	bc
		ld	a,b
		or	c
		jr	nz, bytloop

		ld	de,(crc)
		pop	af
		pop	bc
		pop	hl
		ret

pos:		.db	0
crc:		.dw	0

; align this table such that adding $1e to either 16-word half will never overflow the low byte
		if lo($) + $3e > $ff
		.align	$40
		endif
ym_crc_tab:	.dw	$0000, $1021, $2042, $3063, $4084, $50a5, $60c6, $70e7
		.dw	$8108, $9129, $a14a, $b16b, $c18c, $d1ad, $e1ce, $f1ef
		.dw	$0000, $1231, $2462, $3653, $48c4, $5af5, $6ca6, $7e97
		.dw	$9188, $83b9, $b5ea, $a7db, $d94c, $cb7d, $fd2e, $ef1f
#endlocal

