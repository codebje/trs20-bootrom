# TRS-20 boot ROM

The boot ROM for the TRS-20 is aimed at allowing self-hosted reprogramming, to minimise wear on the ROM socket. The code sets up the CPU, UART, and MMU, then boots up ZSDOS.

`zsystem.bin` is a binary image of a CCP and BDOS. It should be 5,362 in size with the CCP from `0000` to `07FF` and the BDOS from `0800` through to `15FF`. The components should be relocated to be at `E000` and `E800` respectively. Classic CP/M 2.2 will work, as will ZCPR1/ZSDOS. Other alternatives may also work, but haven't been tried.

The boot ROM can also be run as a CP/M command file. It will copy the OS to `10000`-`12000` and install a BIOS whose warm boot procedure will reload the OS components from there.
