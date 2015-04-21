/*
 * (C) Copyright 2010
 * EMAC Inc. (www.emacinc.com)
 *
 * Configuation settings for the SOM-9260 board.
 *
 * See file CREDITS for list of people who contributed to this
 * project.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

#ifndef __CONFIG_H
#define __CONFIG_H

#define	CLK_MODE_DIV1
#undef	CLK_MODE_DIV2
#undef	CLK_MODE_DIV4

/* ARM asynchronous clock */
#define AT91_MAIN_CLOCK		18432000	/* from 18.432 MHz crystal */

#ifdef CLK_MODE_DIV1
#define AT91_MASTER_CLOCK	132096000	/* peripheral = main / 3 */
#define CONFIG_SYS_HZ		1000		/* 1us resolution */
#endif

#ifdef CLK_MODE_DIV2
#define AT91_MASTER_CLOCK	132096000/2	/* peripheral = main / 6 */
#define CONFIG_SYS_HZ		1000/2		/* 1us resolution */
#endif

#ifdef CLK_MODE_DIV4
#define AT91_MASTER_CLOCK	132096000/4	/* peripheral = main / 12 */
#define CONFIG_SYS_HZ		1000/4		/* 1us resolution */
#endif

#define CONFIG_ARCH_CPU_INIT
#define AT91_CPU_NAME		"AT91SAM9260"
#define CONFIG_AT91SAM9260	1	/* It's an Atmel AT91SAM9260 SoC*/

#define AT91_SLOW_CLOCK		32768	/* slow clock */

#define CONFIG_ARM926EJS	1	/* This is an ARM926EJS Core	*/
#undef CONFIG_USE_IRQ			/* we don't need IRQ/FIQ stuff	*/

#define CONFIG_CMDLINE_TAG	1	/* enable passing of ATAGs	*/
#define CONFIG_SETUP_MEMORY_TAGS 1
#define CONFIG_INITRD_TAG	1

#define CONFIG_SKIP_LOWLEVEL_INIT
#define CONFIG_SKIP_RELOCATE_UBOOT

/*
 * Hardware drivers
 */

#define CONFIG_ATMEL_USART_MULTI	1
#define CONFIG_SERIAL_MULTI		1
#define CONFIG_SYS_CONSOLE_IS_IN_ENV	1

//#define CONFIG_ATMEL_USART		1
//#define CONFIG_USARTD			1
//#define CONFIG_USART0			1
//#define CONFIG_USART1			1
//#define CONFIG_USART2			1
#define CONFIG_USART3			1
//#define CONFIG_USART4			1
//#define CONFIG_USART5			1

#define CONFIG_IDENT_STRING		"\n\nEMAC Inc. SOM-9260M"

#define CONFIG_RTC_AT91
#define CONFIG_CMD_DATE


#define CONFIG_BOOTDELAY	3

/*
 * BOOTP options
 */
#define CONFIG_BOOTP_BOOTFILESIZE	1
#define CONFIG_BOOTP_BOOTPATH		1
#define CONFIG_BOOTP_GATEWAY		1
#define CONFIG_BOOTP_HOSTNAME		1

/*
 * Command line configuration.
 */
#include <config_cmd_default.h>
#undef CONFIG_CMD_BDI
#undef CONFIG_CMD_IMI
#undef CONFIG_CMD_FPGA
#undef CONFIG_CMD_LOADS
#undef CONFIG_CMD_IMLS
//#undef CONFIG_CMD_SOURCE

#define CONFIG_CMD_SF		1

#define CONFIG_CMD_PING		1
#define CONFIG_CMD_DHCP		1
#define CONFIG_CMD_USB		1

#define CONFIG_CMD_MMC		1
#define CONFIG_CMD_WINCE	1
#define CONFIG_CMD_FAT		1
#define CONFIG_CMD_EXT2		1
#define CONFIG_SILENT_CONSOLE	1
#define CONFIG_SYS_DEVICE_NULLDEV	1

#define CONFIG_SYS_HUSH_PARSER		1
#define CONFIG_SYS_PROMPT_HUSH_PS2	"> "
#define CONFIG_CMD_UNZIP	1

/* MMC */
#define CONFIG_MMC		1
#define CONFIG_AT91_MCI		1

/* SDRAM */
#define CONFIG_NR_DRAM_BANKS		1
#define PHYS_SDRAM			0x20000000
#define PHYS_SDRAM_SIZE			0x02000000	/* 32 megs */
#define PARAM_ADDR			0x20000000	/* Memory location to pass SRAM size from bootstrap to U-boot */
							/* If not defined PHYS_SDRAM_SIZE will be used to size SDRAM instead */
/* NOR flash */
#define PHYS_FLASH_1			0x10000000
#ifdef	CONFIG_SOM9260_64MB 
#define PHYS_FLASH_2			0x12000000
#endif  /* CONFIG_SOM9260_64MB */
#define PHYS_FLASH_SIZE			0x02000000  	/* 32 megs main flash */
#define CONFIG_SYS_FLASH_BASE		PHYS_FLASH_1
#define PHYS_FLASH_BANK_SIZE		0x02000000 	/* 32 MB Banks */

#ifdef	CONFIG_SOM9260_64MB 
#define CONFIG_SYS_MAX_FLASH_BANKS		2
#define CONFIG_SYS_FLASH_BANKS_LIST		{PHYS_FLASH_1, PHYS_FLASH_2}
#else
#define CONFIG_SYS_MAX_FLASH_BANKS		1
#endif	/* CONFIG_SOM9260_64MB */
#define CONFIG_SYS_MAX_FLASH_SECT		259
#define CONFIG_SYS_FLASH_ERASE_TOUT		(20*CFG_HZ) /* Timeout for Flash Erase */
#define CONFIG_SYS_FLASH_WRITE_TOUT		(20*CFG_HZ) /* Timeout for Flash Write */

/* Use Common Flash Interface */
#define CONFIG_SYS_FLASH_CFI			1
#define CONFIG_FLASH_CFI_DRIVER			1
#define CONFIG_FLASH_CFI_MTD			1
#define CONFIG_SYS_FLASH_PROTECTION		1
#define CONFIG_SYS_FLASH_USE_BUFFER_WRITE	1
#define CONFIG_MTD_PARTITIONS			1


/* SPI Flash */
#define CONFIG_ATMEL_SPI	1
#define CONFIG_SPI_FLASH	1
#define CONFIG_SPI_FLASH_ATMEL	1
#define CONFIG_HAS_SERIAL_FLASH 1
#define CONFIG_SF_DEFAULT_HZ	10000000

#ifdef CLK_MODE_DIV4
#define AT91_SPI_CLK			15000000*2
#else
#define AT91_SPI_CLK			15000000
#endif

/* Ethernet */
#define CONFIG_MACB			1
#define CONFIG_NET_MULTI		1
#define CONFIG_NET_RETRY_COUNT		20
#define CONFIG_RESET_PHY_R		1

/* USB */
#define CONFIG_USB_ATMEL
#define CONFIG_USB_OHCI_NEW		1
#define CONFIG_DOS_PARTITION		1
#define CONFIG_SYS_USB_OHCI_CPU_INIT		1
#define CONFIG_SYS_USB_OHCI_REGS_BASE		0x00500000	/* AT91SAM9260_UHP_BASE */
#define CONFIG_SYS_USB_OHCI_SLOT_NAME		"at91sam9260"
#define CONFIG_SYS_USB_OHCI_MAX_ROOT_PORTS	2
#define CONFIG_USB_STORAGE		1
#define CONFIG_CMD_FAT			1

#define CONFIG_SYS_LOAD_ADDR			0x21000000	/* load address */

#define CONFIG_SYS_MEMTEST_START		PHYS_SDRAM
#define CONFIG_SYS_MEMTEST_END			0x21e00000

#ifdef CONFIG_SYS_USE_SEEPROM_CS1

/* u-boot + env in Serial EEPROM on CS1 */
#define CONFIG_ENV_IS_IN_SPI_FLASH	1
#define CONFIG_ENV_OFFSET	0x0
#define CONFIG_ENV_SIZE		0x1000
#define CONFIG_ENV_SECT_SIZE	0x1000
#define CONFIG_BOOTCOMMAND	"protect off all;cp.b 0x10100000 0x20000000 ${kernelsize};bootm 0x20000000"
#define CONFIG_BOOTARGS		"console=ttyS3,115200 "			\
				"root=/dev/mtdblock3 "			\
				"rootfstype=jffs2"

#define CONFIG_ENV_SPI_BUS	0
#define CONFIG_ENV_SPI_CS	1
#define CONFIG_ENV_SPI_MODE	0
#define CONFIG_ENV_SPI_MAX_HZ	1000000

#else /* CONFIG_USE_NORFLASH */

/* bootstrap + u-boot + env in norflash */
#define CONFIG_ENV_IS_IN_FLASH	1
#define CONFIG_ENV_OFFSET	0x20000
#define CONFIG_ENV_SIZE		0x20000
#define CONFIG_BOOTCOMMAND	"protect off all;cp.b 0x10100000 0x20000000 ${kernelsize};bootm 0x20000000"
#define CONFIG_BOOTARGS		"console=ttyS3,115200 "			\
				"root=/dev/mtdblock3 "			\
				"rootfstype=jffs2"

#endif

#define CONFIG_BAUDRATE		115200
#define CONFIG_SYS_BAUDRATE_TABLE	{115200 , 19200, 38400, 57600, 9600 }


#define CONFIG_SYS_PROMPT		"U-Boot> "
#define CONFIG_SYS_CBSIZE		256
#define CONFIG_SYS_MAXARGS		16
#define CONFIG_SYS_PBSIZE		(CONFIG_SYS_CBSIZE + sizeof(CONFIG_SYS_PROMPT) + 16)
#define CONFIG_SYS_LONGHELP	1
#define CONFIG_CMDLINE_EDITING	1

#define ROUND(A, B)		(((A) + (B)) & ~((B) - 1))
/*
 * Size of malloc() pool
 */
#define CONFIG_SYS_MALLOC_LEN		ROUND(3 * CONFIG_ENV_SIZE + 128*1024, 0x1000)
#define CONFIG_SYS_GBL_DATA_SIZE	128	/* 128 bytes for initial data */

#define CONFIG_STACKSIZE	(32*1024)	/* regular stack */

#ifdef CONFIG_USE_IRQ
#error CONFIG_USE_IRQ not supported
#endif

#endif
