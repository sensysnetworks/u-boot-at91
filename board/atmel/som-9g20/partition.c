/*
 * (C) Copyright 2008
 * Ulf Samuelsson <ulf@atmel.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 *
 */
#include <common.h>
#include <config.h>
#include <asm/hardware.h>
#include <dataflash.h>

AT91S_DATAFLASH_INFO dataflash_info[CONFIG_SYS_MAX_DATAFLASH_BANKS];

struct dataflash_addr cs[CONFIG_SYS_MAX_DATAFLASH_BANKS] = {
	{CONFIG_SYS_DATAFLASH_LOGIC_ADDR_CS0, 0},	/* Logical adress, CS */
	{CONFIG_SYS_DATAFLASH_LOGIC_ADDR_CS1, 1}
};

/*define the area offsets*/
dataflash_protect_t area_list[NB_DATAFLASH_AREA] = {
	{0x00000000, 0x000041FF, FLAG_PROTECT_SET,   0, "Bootstrap"},
	{0x00004200, 0x000062FF, FLAG_PROTECT_SET,   0, "Environment0"},
	{0x00006300, 0x000083FF, FLAG_PROTECT_CLEAR, 0, "Environment1"},
	{0x00008400, 0x00041FFF, FLAG_PROTECT_SET,   0, "U-Boot"},
	{0x00042000, 0x002018FF, FLAG_PROTECT_SET,   0,	"Kernel0"},
	{0x00201900, 0x003C11FF, FLAG_PROTECT_CLEAR, 0,	"Kernel1"},

	{0x003C1200, 0x003C95FF, FLAG_PROTECT_SET,   0,	"Fpga0"},
	{0x003C9600, 0x003D19FF, FLAG_PROTECT_CLEAR, 0,	"Fpga1"},
	{0x003D1A00, 0xFFFFFFFF, FLAG_PROTECT_CLEAR, 0,	"FS"},
};
