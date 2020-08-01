# TRS-20 boot ROM

The boot ROM for the TRS-20 is aimed at allowing self-hosted reprogramming, to minimise wear on the ROM socket. The code sets up the CPU, UART, and MMU, then enters a simple boot monitor that supports uploading more code to execute.

```mermaid
graph TD
	A(Disable DRAM, INTs)
  A --> B(Set CPU speed to 18.432MHz)
  B --> C(Configure ASCI0 UART)
  C --> D(Configure MMU for ROM)
  D --> E(Jump to ROM)
  E --> F(DMA copy ROM to RAM)
  F --> G(Start PRT0 timer)
  G --> H(Start boot monitor)
```

The boot monitor allows uploading code using Y-modem, and then jumping to the most recently uploaded code.
