; Registers

; ASCI registers
CNTLA0		equ	$00			; ASCI Control Register A Ch 0
CNTLA1		equ	$01			; ASCI Control Register A Ch 1
CNTLB0		equ	$02			; ASCI Control Register B Ch 0
CNTLB1		equ	$03			; ASCI Control Register B Ch 1
STAT0		equ	$04			; ASCI Status Register Ch 0
STAT1		equ	$05			; ASCI Status Register Ch 1
TDR0		equ	$06			; ASCI Transmit Data Register Ch 0
TDR1		equ	$07			; ASCI Transmit Data Register Ch 1
RDR0		equ	$08			; ASCI Receive Data Register Ch 0
RDR1		equ	$09			; ASCI Receive Data Register Ch 1
ASEXT0		equ	$12			; ASCI Extension Control Register Ch 0
ASEXT1		equ	$13			; ASCI Extension Control Register Ch 1
ASTC0L		equ	$1A			; ASCI Time Constant Low Ch 0
ASTC0H		equ	$1B			; ASCI Time Constant High Ch 0
ASTC1L		equ	$1C			; ASCI Time Constant Low Ch 1
ASTC1H		equ	$1D			; ASCI Time Constant High Ch 1

; CSI/O registers
CNTR		equ	$0A			; CSIO Control Register
TRD		equ	$0B			; CSIO Transmit/Receive Data Register

; Timer registers
TMDR0L		equ	$0C			; Timer Data Register Ch 0 low
TMDR0H		equ	$0D			; Timer Data Register Ch 0 high
RLDR0L		equ	$0E			; Timer Reload Register Ch 0 low
RLDR0H		equ	$0F			; Timer Reload Register Ch 0 high
TCR		equ	$10			; Timer Control Register
TMDR1L		equ	$14			; Timer Data Register Ch 1 low
TMDR1H		equ	$15			; Timer Data Register Ch 1 high
RLDR1L		equ	$16			; Timer Reload Register Ch 1 low
RLDR1H		equ	$17			; Timer Reload Register Ch 1 high

; DMA registers
SAR0L		equ	$20			; DMA Source Address register Ch 0L
SAR0H		equ	$21			; DMA Source Address register Ch 0H
SAR0B		equ	$22			; DMA Source Address register Ch 0B
DAR0L		equ	$23			; DMA Destination Address Register Ch 0L
DAR0H		equ	$24			; DMA Destination Address Register Ch 0H
DAR0B		equ	$25			; DMA Destination Address Register Ch 0B
BCR0L		equ	$26			; DMA Byte Count Register Ch 0L
BCR0H		equ	$27			; DMA Byte Count Register Ch 0H
MAR1L		equ	$28			; DMA Memory Address Register Ch 1L
MAR1H		equ	$29			; DMA Memory Address Register Ch 1H
MAR1B		equ	$2A			; DMA Memory Address Register Ch 1B
IAR1L		equ	$2B			; DMA I/O Address Register Ch 1L
IAR1H		equ	$2C			; DMA I/O Address Register Ch 1H
IAR1B		equ	$2D			; DMA I/O Address Register Ch 1B
BCR1L		equ	$2E			; DMA Byte Count Register Ch 1L
BCR1H		equ	$2F			; DMA Byte Count Register Ch 1H
DSTAT		equ	$30			; DMA Status Register
DMODE		equ	$31			; DMA Mode Register
DCNTL		equ	$32			; DMA/WAIT Control Register

; Interrupt registers
IL		equ	$33			; Interrupt Vector Low Register
ITC		equ	$34			; INT/Trap Control Register

; Refresh
RCR		equ	$36			; Refresh Control Register

; MMU registers
CBR		equ	$38			; MMU Common Base Register
BBR		equ	$39			; MMU Bank Base Register
CBAR		equ	$3a			; MMU Common/Bank Area Register

; I/O registers
OMCR		equ	$3E			; Operation Mode Control Register
ICR		equ	$3F			; I/O Control Register

; Other registers
CMR		equ	$1E			; Clock Multiplier Register
CCR		equ	$1F			; CPU Control Register
