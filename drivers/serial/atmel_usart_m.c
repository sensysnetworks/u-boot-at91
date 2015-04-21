/*
 * Multi-Serial support for the AT91 Series
 *
 * Copyright (C) 2009 EMAC Inc.
 *
 * Based on: atmel_usart.c 
 *
 * Copyright (C) 2004-2006 Atmel Corporation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <common.h>
#include <watchdog.h>

#include <asm/io.h>
#include <asm/arch/clk.h>
#include <asm/arch/memory-map.h>

#if !defined(CONFIG_SERIAL_MULTI)
#error "CONFIG_SERIAL_MULTI not defined!"
#endif

#include <serial.h>

#define DECLARE_AT91_SERIAL_FUNCTIONS(port) \
    int  at91serial##port##_init (void) {\
	return serial_init_dev(port);}\
    void at91serial##port##_setbrg (void) {\
	serial_setbrg_dev(port);}\
    int  at91serial##port##_getc (void) {\
	return serial_getc_dev(port);}\
    int  at91serial##port##_tstc (void) {\
	return serial_tstc_dev(port);}\
    void at91serial##port##_putc (const char c) {\
	serial_putc_dev(port, c);}\
    void at91serial##port##_puts (const char *s) {\
	serial_puts_dev(port, s);}

#define INIT_AT91_SERIAL_STRUCTURE(port,name,bus) {\
	name,\
	bus,\
	at91serial##port##_init,\
	at91serial##port##_setbrg,\
	at91serial##port##_getc,\
	at91serial##port##_tstc,\
	at91serial##port##_putc,\
	at91serial##port##_puts, }

#include "atmel_usart_m.h"


DECLARE_GLOBAL_DATA_PTR;

static unsigned long USART_BASE[7] = { USART0_BASE, USART1_BASE, USART2_BASE, USART3_BASE, USART4_BASE, USART5_BASE, USARTD_BASE };

/* Register access functions */

unsigned long usart_readl(unsigned int dev_index, unsigned long reg)
{
	return readl((void *)(USART_BASE[dev_index] + reg));
}

void usart_writel(unsigned int dev_index, unsigned long reg, unsigned long value)
{
	writel(value, (void *)(USART_BASE[dev_index] + reg));
}

void serial_setbrg_dev(unsigned int dev_index)
{
	unsigned long divisor;
	unsigned long usart_hz;

	usart_hz = get_usart_clk_rate(dev_index);
	divisor = (usart_hz / 16 + gd->baudrate / 2) / gd->baudrate;
	usart_writel(dev_index, USART3_BRGR, USART3_BF(CD, divisor));
}

int serial_init_dev(unsigned int dev_index)
{

	usart_writel(dev_index, USART3_CR, USART3_BIT(RSTRX) | USART3_BIT(RSTTX));

	serial_setbrg_dev(dev_index);

	usart_writel(dev_index, USART3_CR, USART3_BIT(RXEN) | USART3_BIT(TXEN));
	usart_writel(dev_index, USART3_MR, (USART3_BF(USART_MODE, USART3_USART_MODE_NORMAL)
			   | USART3_BF(USCLKS, USART3_USCLKS_MCK)
			   | USART3_BF(CHRL, USART3_CHRL_8)
			   | USART3_BF(PAR, USART3_PAR_NONE)
			   | USART3_BF(NBSTOP, USART3_NBSTOP_1)));
	return 0;
}

void serial_putc_dev(unsigned int dev_index, const char c)
{
	if (c == '\n') serial_putc_dev(dev_index, '\r');

	while (!(usart_readl(dev_index, USART3_CSR) & USART3_BIT(TXRDY)));

	usart_writel(dev_index, USART3_THR, c);
}

void serial_puts_dev(unsigned int dev_index, const char *s)
{
	while (*s) serial_putc_dev(dev_index, *s++);
}

int serial_getc_dev(unsigned int dev_index)
{
	while (!(usart_readl(dev_index, USART3_CSR) & USART3_BIT(RXRDY)))
		 WATCHDOG_RESET();
	return usart_readl(dev_index, USART3_RHR);
}

int serial_tstc_dev(unsigned int dev_index)
{
	return (usart_readl(dev_index, USART3_CSR) & USART3_BIT(RXRDY)) != 0;
}

DECLARE_AT91_SERIAL_FUNCTIONS(0);
DECLARE_AT91_SERIAL_FUNCTIONS(1);
DECLARE_AT91_SERIAL_FUNCTIONS(2);
DECLARE_AT91_SERIAL_FUNCTIONS(3);
DECLARE_AT91_SERIAL_FUNCTIONS(4);
DECLARE_AT91_SERIAL_FUNCTIONS(5);
DECLARE_AT91_SERIAL_FUNCTIONS(6);

struct serial_device at91_serial0_device = INIT_AT91_SERIAL_STRUCTURE(0, "at91ser0", "UART1");
struct serial_device at91_serial1_device = INIT_AT91_SERIAL_STRUCTURE(1, "at91ser1", "UART2");
struct serial_device at91_serial2_device = INIT_AT91_SERIAL_STRUCTURE(2, "at91ser2", "UART3");
struct serial_device at91_serial3_device = INIT_AT91_SERIAL_STRUCTURE(3, "at91ser3", "UART4");
struct serial_device at91_serial4_device = INIT_AT91_SERIAL_STRUCTURE(4, "at91ser4", "UART5");
struct serial_device at91_serial5_device = INIT_AT91_SERIAL_STRUCTURE(5, "at91ser5", "UART6");
struct serial_device at91_serial6_device = INIT_AT91_SERIAL_STRUCTURE(6, "at91ser6", "UART7");


