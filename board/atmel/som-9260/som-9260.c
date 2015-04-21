/*
 * (C) Copyright 2007-2008
 * Stelian Pop <stelian.pop@leadtechdesign.com>
 * Lead Tech Design <www.leadtechdesign.com>
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

#include <common.h>
#include <asm/arch/at91sam9260.h>
#include <asm/arch/at91sam9260_matrix.h>
#include <asm/arch/at91sam9_smc.h>
#include <asm/arch/at91_pmc.h>
#include <asm/arch/at91_rstc.h>
#include <asm/arch/gpio.h>
#include <asm/arch/io.h>
#if defined(CONFIG_RESET_PHY_R) && defined(CONFIG_MACB)
#include <net.h>
#endif

DECLARE_GLOBAL_DATA_PTR;

/* ------------------------------------------------------------------------- */
/*
 * Miscelaneous platform dependent initialisations
 */

#ifdef CONFIG_CMD_NAND
static void at91sam9g20ek_nand_hw_init(void)
{
	unsigned long csa;

	/* Enable CS3 */
	csa = at91_sys_read(AT91_MATRIX_EBICSA);
	at91_sys_write(AT91_MATRIX_EBICSA,
		       csa | AT91_MATRIX_CS3A_SMC_SMARTMEDIA);

	/* Configure SMC CS3 for NAND/SmartMedia */
	at91_sys_write(AT91_SMC_SETUP(3),
		       AT91_SMC_NWESETUP_(2) | AT91_SMC_NCS_WRSETUP_(0) |
		       AT91_SMC_NRDSETUP_(2) | AT91_SMC_NCS_RDSETUP_(0));
	at91_sys_write(AT91_SMC_PULSE(3),
		       AT91_SMC_NWEPULSE_(4) | AT91_SMC_NCS_WRPULSE_(3) |
		       AT91_SMC_NRDPULSE_(4) | AT91_SMC_NCS_RDPULSE_(3));
	at91_sys_write(AT91_SMC_CYCLE(3),
		       AT91_SMC_NWECYCLE_(7) | AT91_SMC_NRDCYCLE_(7));
	at91_sys_write(AT91_SMC_MODE(3),
		       AT91_SMC_READMODE | AT91_SMC_WRITEMODE |
		       AT91_SMC_EXNWMODE_DISABLE |
#ifdef CFG_NAND_DBW_16
		       AT91_SMC_DBW_16 |
#else /* CFG_NAND_DBW_8 */
		       AT91_SMC_DBW_8 |
#endif
		       AT91_SMC_TDF_(3));

	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_PIOC);

	/* Configure RDY/BSY */
	at91_set_gpio_input(AT91_PIN_PC13, 1);

	/* Enable NandFlash */
	at91_set_gpio_output(AT91_PIN_PC14, 1);
}
#endif

#ifdef CONFIG_HAS_SERIAL_FLASH
static void at91sam9g20ek_spi_hw_init(void)
{
//	at91_set_A_periph(AT91_PIN_PA3, 0);	/* SPI0_NPCS0 */
//	at91_set_B_periph(AT91_PIN_PC11, 0);	/* SPI0_NPCS1 */

	at91_set_A_periph(AT91_PIN_PA0, 0);	/* SPI0_MISO */
	at91_set_A_periph(AT91_PIN_PA1, 0);	/* SPI0_MOSI */
	at91_set_A_periph(AT91_PIN_PA2, 0);	/* SPI0_SPCK */

	/* Enable clock */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_SPI0);
}
#endif

#ifdef CONFIG_MACB
static void at91sam9g20ek_macb_hw_init(void)
{
	/* Enable clock */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_EMAC);

	/*
	 * Disable pull-up on:
	 *	RXDV (PA17) => PHY normal mode (not Test mode)
	 *	ERX0 (PA14) => PHY ADDR0
	 *	ERX1 (PA15) => PHY ADDR1
	 *	ERX2 (PA25) => PHY ADDR2
	 *	ERX3 (PA26) => PHY ADDR3
	 *	ECRS (PA28) => PHY ADDR4  => PHYADDR = 0x0
	 *
	 * PHY has internal pull-down
	 */
	writel(pin_to_mask(AT91_PIN_PA14) |
	       pin_to_mask(AT91_PIN_PA15) |
	       pin_to_mask(AT91_PIN_PA17) |
	       pin_to_mask(AT91_PIN_PA25) |
	       pin_to_mask(AT91_PIN_PA26) |
	       pin_to_mask(AT91_PIN_PA28),
	       pin_to_controller(AT91_PIN_PA0) + PIO_PUDR);

//	/* Need to reset PHY -> 500ms reset */
//	at91_sys_write(AT91_RSTC_MR, AT91_RSTC_KEY |
//				     (AT91_RSTC_ERSTL & (0x0D << 8)) |
//				     AT91_RSTC_URSTEN);
//
//	at91_sys_write(AT91_RSTC_CR, AT91_RSTC_KEY | AT91_RSTC_EXTRST);
//
//	/* Wait for end hardware reset */
//	while (!(at91_sys_read(AT91_RSTC_SR) & AT91_RSTC_NRSTL));
//
	/* Restore NRST value */
	at91_sys_write(AT91_RSTC_MR, AT91_RSTC_KEY |
				     (AT91_RSTC_ERSTL & (0x0 << 8)) |
				     AT91_RSTC_URSTEN);

	/* Re-enable pull-up */
	writel(pin_to_mask(AT91_PIN_PA14) |
	       pin_to_mask(AT91_PIN_PA15) |
	       pin_to_mask(AT91_PIN_PA17) |
	       pin_to_mask(AT91_PIN_PA25) |
	       pin_to_mask(AT91_PIN_PA26) |
	       pin_to_mask(AT91_PIN_PA28),
	       pin_to_controller(AT91_PIN_PA0) + PIO_PUER);

	at91_set_A_periph(AT91_PIN_PA19, 0);	/* ETXCK_EREFCK */
	at91_set_A_periph(AT91_PIN_PA17, 0);	/* ERXDV */
	at91_set_A_periph(AT91_PIN_PA14, 0);	/* ERX0 */
	at91_set_A_periph(AT91_PIN_PA15, 0);	/* ERX1 */
	at91_set_A_periph(AT91_PIN_PA18, 0);	/* ERXER */
	at91_set_A_periph(AT91_PIN_PA16, 0);	/* ETXEN */
	at91_set_A_periph(AT91_PIN_PA12, 0);	/* ETX0 */
	at91_set_A_periph(AT91_PIN_PA13, 0);	/* ETX1 */
	at91_set_A_periph(AT91_PIN_PA21, 0);	/* EMDIO */
	at91_set_A_periph(AT91_PIN_PA20, 0);	/* EMDC */

#ifndef CONFIG_RMII
	at91_set_B_periph(AT91_PIN_PA28, 0);	/* ECRS */
	at91_set_B_periph(AT91_PIN_PA29, 0);	/* ECOL */
	at91_set_B_periph(AT91_PIN_PA25, 0);	/* ERX2 */
	at91_set_B_periph(AT91_PIN_PA26, 0);	/* ERX3 */
	at91_set_B_periph(AT91_PIN_PA27, 0);	/* ERXCK */
	at91_set_B_periph(AT91_PIN_PA23, 0);    /* ETX2 */
	at91_set_B_periph(AT91_PIN_PA24, 0);    /* ETX3 */
	at91_set_B_periph(AT91_PIN_PA22, 0);	/* ETXER */
#endif

}
#endif

int board_init(void)
{
	/* arch number of AT91SAM9G20EK-Board */
	gd->bd->bi_arch_number = MACH_TYPE_AT91SAM9260EK;
	/* adress of boot parameters */
	gd->bd->bi_boot_params = PHYS_SDRAM + 0x100;

#ifdef CONFIG_CMD_NAND
	at91sam9g20ek_nand_hw_init();
#endif
#ifdef CONFIG_HAS_SERIAL_FLASH
	at91sam9g20ek_spi_hw_init();
#endif
#ifdef CONFIG_MACB
	at91sam9g20ek_macb_hw_init();
#endif

	/** Setup MMC/SD Peripheral **/
	at91_set_A_periph(AT91_PIN_PA6,  1);
	at91_set_A_periph(AT91_PIN_PA7,  1);
	at91_set_A_periph(AT91_PIN_PA8,  0);
	at91_set_A_periph(AT91_PIN_PA9,  1);
	at91_set_A_periph(AT91_PIN_PA10, 1);
	at91_set_A_periph(AT91_PIN_PA11, 1);

	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_MCI);

	/** Setup serial ports **/
	at91_serial_hw_init();

	return 0;
}

int dram_init(void)
{
#ifdef PARAM_ADDR
	unsigned long *param = (unsigned long *)PARAM_ADDR;
	unsigned long value;

	value = *param;

	if((value == 16) || (value == 32) || (value ==64))
		gd->bd->bi_dram[0].size = value * 1024 * 1024;
	else
		gd->bd->bi_dram[0].size = PHYS_SDRAM_SIZE;
#else
	gd->bd->bi_dram[0].size = PHYS_SDRAM_SIZE;
#endif
	gd->bd->bi_dram[0].start = PHYS_SDRAM;

	return 0;
}

#ifdef CONFIG_RESET_PHY_R
void reset_phy(void)
{
#ifdef CONFIG_MACB
	/*
	 * Initialize ethernet HW addr prior to starting Linux,
	 * needed for nfsroot
	 */
	eth_init(gd->bd);
#endif
}
#endif

int board_eth_init(bd_t *bis)
{
        int rc = 0;
#ifdef CONFIG_MACB
        rc = macb_eth_initialize(0, (void *)AT91SAM9260_BASE_EMAC, 0x00);
#endif
        return rc;
}

#ifdef CONFIG_ATMEL_SPI
#include <spi.h>

#define FLASH_CS_PIN	AT91_PIN_PC11

int spi_cs_is_valid(unsigned int bus, unsigned int cs)
{
	return bus == 0 && cs == 1;
}

void spi_cs_activate(struct spi_slave *slave)
{
	at91_set_gpio_output(FLASH_CS_PIN, 0);
}

void spi_cs_deactivate(struct spi_slave *slave)
{
	at91_set_gpio_output(FLASH_CS_PIN, 1);
}
#endif /* CONFIG_ATMEL_SPI */

