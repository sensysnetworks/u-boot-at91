/*
 * Copyright (C) 2009 EMAC Inc.
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
 

#include <command.h>
#include <common.h>

typedef struct iniparam_s
{
	const char *label;
	const char *variable;
}iniparam_t;

enum INIPARAM
{
	BAUDRATE,
	COMPORT,
	TIMEOUT,
	RETRY,
	NET_IP,
	SUBNET,
	GATEWAY,
	SER_IP,
	VARNUM
};

iniparam_t param_desc[] = 
{
	{"BAUDRATE",	"BAUDRATE"	},
	{"COMPORT",	"COMPORT"	},
	{"TIMEOUT",	"TIMEOUT"	},
	{"RETRY",	"RETRY"		},
	{"NET_IP",	"ipaddr"	},
	{"SUBNET",	"SUBNET"	},
	{"GATEWAY",	"gatewayip"	},
	{"SER_IP",	"serverip"	},
};

void parse_ini(char * mem, unsigned long len)
{
	char * strp;
	char * strp1;
	char value[255];
	int found_eq;
	int length;
	int i;

	mem[len] = 0;
	
	for(i = 0; i < VARNUM; i++)
	{
		strp = strstr(mem, param_desc[i].label);
retry:		
		if(strp) 
		{
			found_eq = 0;
			strp += strlen(param_desc[i].label);

			/* Strip off leading spaces and equal sign */
			while(*strp == ' ' || *strp == '=') 
			{ 
				if(*strp == '=') found_eq = 1;
				strp++;
			}

			/* Check if equal sign occurred */
			if(!found_eq)
			{
				/* Repeat the last search from current location */
				strp = strstr(strp, param_desc[i].label);
				goto retry;
			}

			/* Mark the start of the variable's value */
			strp1 = strp;

			while(*strp != '\n') strp++;

			length = strp - strp1;
			
			if(length < 255)	
			{
				strncpy(value, strp1, length);
				value[length] = 0;
				setenv((char *)param_desc[i].variable, value);
			}
		}
	}
}

static int do_parse_ini(cmd_tbl_t *cmdtp, int flag, int argc, char *argv[])
{
	unsigned long addr;
	unsigned long len;

	if (argc < 3)
		goto usage;

	addr = simple_strtoul(argv[1], NULL, 16);
	len = simple_strtoul(argv[2], NULL, 16);

	printf("Parsing .ini @ %.8X...\n", addr);

	parse_ini((char *)addr, len);

	return 0;

usage:
	cmd_usage(cmdtp);
	return 1;
}

U_BOOT_CMD(parseini, 3, 1, do_parse_ini, "parse .ini from memory offset.", "offset length\n");
