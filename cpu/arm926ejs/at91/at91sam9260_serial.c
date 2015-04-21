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
#include <asm/arch/at91_common.h>
#include <asm/arch/at91_pmc.h>
#include <asm/arch/gpio.h>
#include <asm/arch/io.h>

void at91_serial0_hw_init(void)
{
	at91_set_A_periph(AT91_PIN_PB4, 1);		/* TXD0 */
	at91_set_A_periph(AT91_PIN_PB5, 0);		/* RXD0 */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_US0);
}

void at91_serial1_hw_init(void)
{
	at91_set_A_periph(AT91_PIN_PB6, 1);		/* TXD1 */
	at91_set_A_periph(AT91_PIN_PB7, 0);		/* RXD1 */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_US1);
}

void at91_serial2_hw_init(void)
{
	at91_set_A_periph(AT91_PIN_PB8, 1);		/* TXD2 */
	at91_set_A_periph(AT91_PIN_PB9, 0);		/* RXD2 */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_US2);
}

void at91_serial3_hw_init(void)
{
	at91_set_A_periph(AT91_PIN_PB10, 1);		/* TXD3 */
	at91_set_A_periph(AT91_PIN_PB11, 0);		/* RXD3 */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_US3);
}

void at91_serial4_hw_init(void)
{
	at91_set_A_periph(AT91_PIN_PB12, 1);		/* TXD4 */
	at91_set_A_periph(AT91_PIN_PB13, 0);		/* RXD4 */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_US4);
}

void at91_serial5_hw_init(void)
{
	at91_set_B_periph(AT91_PIN_PA31, 1);		/* TXD5 */
	at91_set_B_periph(AT91_PIN_PA30, 0);		/* RXD5 */
	at91_sys_write(AT91_PMC_PCER, 1 << AT91SAM9260_ID_US5);
}

void at91_serialD_hw_init(void)
{
        at91_set_A_periph(AT91_PIN_PB14, 0);            /* DRXD */
        at91_set_A_periph(AT91_PIN_PB15, 1);            /* DTXD */
        at91_sys_write(AT91_PMC_PCER, 1 << AT91_ID_SYS);
}

void at91_serial_hw_init(void)
{
	at91_serial0_hw_init();
	at91_serial1_hw_init();
	at91_serial2_hw_init();
	at91_serial3_hw_init();
	at91_serial4_hw_init();
	at91_serial5_hw_init();
	at91_serialD_hw_init();
}
