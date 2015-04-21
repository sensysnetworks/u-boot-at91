/*
 * 2008 (c) STMicroelectronics, Inc.
 * Author: Ryan Chen <Ryan.Chen at st.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Opsycon AB, Sweden.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <common.h>
#include <command.h>
#include <linux/ctype.h>
#include <net.h>

/*
 * Windows CE Binary Image Data Format
 * The binary image (.bin) file format organizes data by sections. Each section contains 
 * a section header that specifies the starting address, length, and checksum for that 
 * section. 
 *
 * Refer to <http://msdn.microsoft.com/en-us/library/ms924510.aspx>
 *
 * The following table shows the .bin file format.
 * Field                       Length (bytes)  Description 
 * Sync bytes (optional)       7               Byte 0 is B, indicating a .bin file format. 
 *                                             Bytes 1-6 are reserved and set to 0, 0, 0, F, F, \n.
 * Run-time image address      4               Physical starting address of the run-time image. 
 * Run-time image length       4               Physical length, in bytes, of the run-time image. 
 * Record Address              4               Physical starting address of data record. 
 *                                             If this value is zero, the Record Address is the end of 
 *                                             the file, and record length contains the starting address 
 *                                             of the run-time image.
 * Record length               4               Length of record data, in bytes. 
 * Record checksum             4               Signed 32-bit sum of record data bytes. 
 * Record data                 Record length   Record data. 
 */
#define WINCE_IMAGE_SYNC_SIZE 7
#define WINCE_IMAGE_SYNC      "B000FF\n"

typedef struct {
	unsigned char sync_bytes[WINCE_IMAGE_SYNC_SIZE];
	unsigned int img_addr;
	unsigned int img_length;
} type_wince_image_header;

#define DRIVER_GLOBALS_PHYSICAL_MEMORY_START    0x20058000
#define VIRTUAL_TO_PHYSICAL_OFFSET		0x60000000

#define OAL_ARGS_SIGNATURE      'SGRA'
#define OAL_ARGS_VERSION        1
#define BSP_ARGS_VERSION        1
#define DEVICE_TYPE_ERROR 0XFFFF;

typedef __u8   BOOL;
typedef __u32  DWORD;
typedef __u8   UINT8;
typedef __u16  UINT16;
typedef __u32  UINT32;
typedef void * PVOID;

typedef struct {
  UINT32 signature;
  UINT16 oalVersion;
  UINT16 bspVersion;
} OAL_ARGS_HEADER;

typedef struct _DEVICE_LOCATION {
  DWORD IfcType;
  DWORD BusNumber;
  DWORD LogicalLoc;
  PVOID PhysicalLoc;
  DWORD Pin;
} DEVICE_LOCATION;

typedef struct {
    UINT32 flags;
    DEVICE_LOCATION devLoc;             /* KITL device location */
    union {
        struct {                        /* Serial class parameters */
            UINT32 baudRate;
            UINT32 dataBits;
            UINT32 stopBits;
            UINT32 parity;
        };         
        struct {                        /* Ether class parameters */
            UINT16 mac[3];
            UINT32 ipAddress;
            UINT32 ipMask;
            UINT32 ipRoute;
        };
    };
} OAL_KITL_ARGS;

typedef struct {
	OAL_ARGS_HEADER header;
	DWORD deviceType;
	UINT8 deviceId[16];
	OAL_KITL_ARGS kitl;
	DWORD dwPLLFrequency;		/* Frequency for the PLL A*/ 
	DWORD dwPrescaler;		/* Frequency prescaler for the Bus Clock and the Processor Clock*/
	DWORD dwBusFreqDivider;		/* Frequency Divider for the Bus Clock*/
	DWORD dwProcFreqDivider;	/* Frequency Divider for the Processor Clock*/
} BSP_ARGS;

typedef struct _DRIVER_GLOBALS
{
	// Offset 0x0800
        DWORD FirstNonZeroVariable;
        // The following structs will not be zero initialized (see defs above)
        BOOL bEboot;// reset through eboot  
        BSP_ARGS BSPArgs; //BSP parameteres passed by the bootloader  
    
} DRIVER_GLOBALS;

int check_sum(unsigned char * buf, int len, unsigned int checksum)
{
	unsigned int count,i;

	for (i = 0,count = 0 ; i < len ; i++)
		count += buf[i];

	if (count == checksum)
		return 0;

	return 1;
}

/* ======================================================================
 * Determine if a valid WinCE image exists at the given memory location.
 * First looks at the image header field, the makes sure that it is
 * WinCE image.
 * ====================================================================== */
int valid_wince_image (unsigned long addr)
{
	type_wince_image_header *p = (type_wince_image_header *)addr;

	if(strcmp((char *)p->sync_bytes, (char *)WINCE_IMAGE_SYNC) != 0)
		return 0;

	return 1;
}

/* ======================================================================
 * A very simple WinCE image loader, assumes the image is valid, returns the
 * entry point address.
 * ====================================================================== */
unsigned long load_wince_image (unsigned long addr)
{
	unsigned char *p = (unsigned char *)addr;
	u32 start_addr, total_length;
	u32 record_addr, record_length, record_checksum;
	u32 i = 0;

	if(valid_wince_image(addr) == 0)
		return ~0;

	printf("WinCE image is found: ");
	p += WINCE_IMAGE_SYNC_SIZE;
	start_addr = (u32)(p[3]<<24) + (u32)(p[2]<<16) + (u32)(p[1]<<8) + (u32)p[0];
	p += 4;
	total_length = (u32)(p[3]<<24) + (u32)(p[2]<<16) + (u32)(p[1]<<8) + (u32)p[0];
	printf(" Start Address = 0x%x @ Total Length = 0x%x\n", start_addr, total_length);
	p += 4;

	/* read each records */
	while(1) {
		record_length = (u32)(p[7]<<24) + (u32)(p[6]<<16) + (u32)(p[5]<<8) + (u32)p[4];
		record_checksum = (u32)(p[11]<<24) + (u32)(p[10]<<16) + (u32)(p[9]<<8) + (u32)p[8];
		record_addr = (u32)(p[3]<<24) + (u32)(p[2]<<16) + (u32)(p[1]<<8) + (u32)p[0];
		
		if(record_addr == 0)
			break;
	
		record_addr -= VIRTUAL_TO_PHYSICAL_OFFSET;
		if(check_sum((unsigned char *)&p[12], record_length, record_checksum) != 0) {
			printf("Checksum Error!\n");
			return (unsigned long)~0;
		}
		memcpy ((void *)record_addr, (const void *)&p[12], (unsigned long)record_length);
		printf("Region %d:\t Loading from 0x%.8x to 0x%.8x @ Length 0x%.8x\r", i, (unsigned int)&p[12], \
				(unsigned int)record_addr, record_length);
		p = p + 12 + record_length;
		i++;
	}

	printf("\nWinCE image extracted.\n");

	/* the lastest checksun should be zero */
	if(record_checksum != 0) {
		printf("Checksum Error!\n");
		return (unsigned long)~0;
	}

	/* the lastest length is entry address */
	return (unsigned long)record_length;
}


unsigned long ddectoul(char * str)
{
	char *strp;
	unsigned long value;
	unsigned long nval;
	int i;

	strp = str;

	value = 0;

	for(i = 0; i < 3; i++)
	{
		nval = simple_strtoul(strp, NULL, 10);
		while(*strp != '.' && *strp != '\0') strp++;
		if(*strp == '\0') return -1;
		strp++;
		value += nval << (8 * i);
	}

	value += simple_strtoul(strp, NULL, 10) << 24;

	return value;
}

void parsemac(UINT16 * macaddr)
{
	char * strp;
	unsigned long nval;
	int i;

	strp = getenv("ethaddr");

	for(i = 0; i < 5; i++)
	{
		nval = simple_strtoul(strp, NULL, 16);
		while(*strp != ':' && *strp != '\0') strp++;
		strp++;
		macaddr[i/2] = ((i % 2) == 0) ? nval : ((nval << 8) + macaddr[i/2]);
	}

	nval = simple_strtoul(strp, NULL, 16);

	macaddr[2] = (nval << 8) + macaddr[2];
}

int load_wince_driver_global(void)
{

	DRIVER_GLOBALS * driver_global;

	driver_global = (DRIVER_GLOBALS *) (DRIVER_GLOBALS_PHYSICAL_MEMORY_START + 0x800);

	memset(driver_global, 0x00, sizeof(DRIVER_GLOBALS));

	driver_global->BSPArgs.header.signature  = OAL_ARGS_SIGNATURE;
	driver_global->BSPArgs.header.oalVersion = OAL_ARGS_VERSION;
	driver_global->BSPArgs.header.bspVersion = BSP_ARGS_VERSION;
	driver_global->BSPArgs.deviceType = DEVICE_TYPE_ERROR;

	driver_global->BSPArgs.dwPLLFrequency = 0x320;
	driver_global->BSPArgs.dwPrescaler = 1;
	driver_global->BSPArgs.dwBusFreqDivider = 6;
	driver_global->BSPArgs.dwProcFreqDivider = 2;

	parsemac(&driver_global->BSPArgs.kitl.mac[0]);
	driver_global->BSPArgs.kitl.ipAddress = ddectoul(getenv("ipaddr"));
	driver_global->BSPArgs.kitl.ipMask    = ddectoul(getenv("SUBNET"));
	driver_global->BSPArgs.kitl.ipRoute   = ddectoul(getenv("gatewayip"));

	driver_global->bEboot = 1; 


	return 0;
}

/* ======================================================================
 * Interpreter command to boot WinCE from a memory image.  The image can
 * be an WinCE image.  WinCE image do not need the
 * bootline and other parameters.
 * ====================================================================== */
int do_bootwince (cmd_tbl_t *cmdtp, int flag, int argc, char *argv[])
{
	unsigned long addr;             /* Address of image            */

	/*
	 * Check the loadaddr variable.
	 * If we don't know where the image is then we're done.
	 */
	if (argc < 2)
		addr = load_addr;
	else
		addr = simple_strtoul (argv[1], NULL, 16);

#if defined(CONFIG_CMD_NET)
	/* Check to see if we need to tftp the image ourselves before starting */
	if ((argc == 2) && (strcmp (argv[1], "tftp") == 0)) {
		if (NetLoop (TFTP) <= 0)
			return 1;
		printf ("Automatic boot of WinCE image at address 0x%08lx ... \n", addr);
	}
#endif

	/*
	 * If the data at the load address is an WinCE image, then
	 * treat it like an WinCE image. Otherwise, return 1
	 */
	if (valid_wince_image (addr)) {
		addr = load_wince_image(addr);
		load_wince_driver_global();
	} else {
		puts ("## Not an WinCE image, exit!\n");
		return 1;
		/* leave addr as load_addr */
	}

	addr -= VIRTUAL_TO_PHYSICAL_OFFSET;
	printf ("## Starting Wince at 0x%08lx ...\n", addr);

	((void (*)(void)) addr) ();

	puts ("## WinCE terminated\n");
	return 1;
}

U_BOOT_CMD(bootwin, 2, 1, do_bootwince, "Boot WinCE image at memory offset.", "offset\n");
