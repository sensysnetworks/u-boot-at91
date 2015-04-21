#include <common.h>
#include <command.h>
#include <asm/arch/hardware.h>
#include <asm/arch/gpio.h>
#include <watchdog.h>

#include <dataflash.h>

/* gpio pins to interface with programming of fpga.
 */
#define	FPGA_PIN_CLOCK		AT91_PIN_PC12
#define	FPGA_PIN_DONE		AT91_PIN_PA4
#define	FPGA_PIN_D_IN		AT91_PIN_PC15
#define	FPGA_PIN_INIT_B		AT91_PIN_PC8
#define	FPGA_PIN_PROG_B		AT91_PIN_PB19

static int fpga_tick_counter = 0;

/* output pins */
#define		PRINTF		debug
#define		NAP()		{		\
	fpga_tick_counter++;			\
}

static void
PROG_B(int x)
{
	at91_set_gpio_value(FPGA_PIN_PROG_B, x);
}

static void
DATA(int x)
{
	at91_set_gpio_value(FPGA_PIN_D_IN, x);
}

/* input pins */
static int
INIT_B(void)
{
	int ret = at91_get_gpio_value(FPGA_PIN_INIT_B) != 0;
	return ret;
}

static int
DONE(void)
{
	int ret = at91_get_gpio_value(FPGA_PIN_DONE) != 0;
	return ret;
}

static void
TICK(void)
{
	NAP();
	at91_set_gpio_value(FPGA_PIN_CLOCK, 1);

	NAP();
	at91_set_gpio_value(FPGA_PIN_CLOCK, 0);
}

static unsigned int OUT_PINS[] = {
	FPGA_PIN_CLOCK,
	FPGA_PIN_D_IN,
	FPGA_PIN_PROG_B
};

static unsigned int IN_PINS[] = {
	FPGA_PIN_DONE,
	FPGA_PIN_INIT_B
};

static int
load_fpga(unsigned char *image, ulong length)
{
	int i;
	int j;

	WATCHDOG_RESET();

	/* configure pins to be output */
	for (i = 0; i < (sizeof(OUT_PINS) / sizeof(OUT_PINS[0])); i++) {
		unsigned int pin = OUT_PINS[i];
		at91_set_gpio_output(pin, 0);
	}
	/* configure pins to be input */
	for (i = 0; i < (sizeof(IN_PINS) / sizeof(IN_PINS[0])); i++) {
		unsigned int pin = IN_PINS[i];
		at91_set_gpio_input(pin, 1);
	}

	/* initial the output lines. set data to 0 and prog b to 0 */
	DATA(0);
	PROG_B(0);
	udelay(1);

	PRINTF("loading at %lx length %lx\n", (unsigned long) image,
	       length);

	PROG_B(1);
	for (i = 0; i < 100; i++) {
		TICK();
		if (INIT_B() != 0) {
			PRINTF("init_b on; ok to program after %d ticks\n",
			       i);
			break;
		}
	}
	if (DONE() != 0) {
		printf("error: done didn't drop after PROG_B\n");
		return 1;
	}

	i = at91_get_gpio_value(FPGA_PIN_PROG_B);
	PRINTF("init b = %d; done = %d; prog b = %d\n",
	       INIT_B(), DONE(), (i != 0));

	if (INIT_B() == 0) {
		printf("error: fpga is not enabling initb.\n");
		return 1;
	}

	/* start shifting bits */
	for (i = 0; i < length; i++) {
		unsigned char byte = image[i];
		int j;

		for (j = 0; j < 8; j++) {
			if ((byte & (1 << (7 - j)))) {
				DATA(1);
			} else {
				DATA(0);
			}
			TICK();
			if (INIT_B() == 0) {
				printf("error: init_b dropped pgm fpga\n");
				return 1;
			}
		}
		if ((i & 0xff) == 0) {
			PRINTF(".");
			WATCHDOG_RESET();
		}
	}
	PRINTF("\n");

	for (i = 0; i < 1000; i++) {
		if (DONE() != 0) {
			break;
		} else if (INIT_B() == 0) {
			printf("error: fpga dropped init_b pgm fpga\n");
			return 1;
		}
		NAP();
	}

	if (DONE() == 0) {
		printf("sent image to fpga but done didn't go high\n");
		return 1;
	}
	j = at91_get_gpio_value(FPGA_PIN_PROG_B);
	PRINTF("finished init b = %d; done = %d; prog b = %d waited %d\n",
	       INIT_B(), DONE(), (j != 0), i);

	printf("successfully programmed fpga\n");
	{
		volatile unsigned char *memptr =(void*) 0x10000000;
		unsigned char b;

		/* disable usb */
		b = memptr[7];
		b |= 0x3;
		memptr[7] = b;

		/* disable 5v */
		b = memptr[06];
		b |= 0x10;
		memptr[06] = b;
	}

	/* to conserve power, set the done pin to open drain,
	 * which require the pin to be configured as output.
	 */
	at91_set_gpio_output(FPGA_PIN_DONE, 1);
	at91_set_multi_drive(FPGA_PIN_DONE, 1);

	WATCHDOG_RESET();
	return 0;
}

#define	FPGA_MAXIMUM_SIZE	0x8400
#define	FPGA_FLASH_ADDRESS	0xd03c1200
#define	FPGA_BUF_ADDRESS	0x21000000

extern int gunzip(void *, int, unsigned char *, unsigned long *);

/*
 * load fpga
 */
static int
do_xlx(cmd_tbl_t * cmdtp, int flag, int argc, char *argv[])
{
	unsigned long src;
	unsigned long dst;
	unsigned long len;

	if (argc == 3) {
		dst = simple_strtoul(argv[1], 0L, 16);
		len = simple_strtoul(argv[2], 0L, 16);
	} else if (argc == 1) {
		/*
		 * first copy from flash to ram
		 */
		src = FPGA_FLASH_ADDRESS + FPGA_MAXIMUM_SIZE;
		dst = FPGA_BUF_ADDRESS;
		len = FPGA_MAXIMUM_SIZE;
		PRINTF("cp %lx to %lx\n", src, dst);
		if (read_dataflash(src, len, (char *) dst) != 1) {
			printf
			    ("xlx error: failed to read from flash %lx\n",
			     src);
			return 1;
		}

		/*
		 * gunzip - set len to ~0 so that unzip compute the len.
		 */
		src = FPGA_BUF_ADDRESS;
		dst = FPGA_BUF_ADDRESS + FPGA_MAXIMUM_SIZE;
		len = FPGA_MAXIMUM_SIZE;
		PRINTF("gunzip %lx to %lx\n", src, dst);
		if (gunzip((void *) dst, ~0UL, (void *) src, &len) != 0) {
			printf("xlx error: failed to gunzip data\n");
			return 1;
		}
		PRINTF("gunzip size is %lx\n", len);
	} else {
		printf("Usage:\n%s\n", cmdtp->usage);
		return 1;
	}

	return load_fpga((unsigned char *) dst, len);
}

static const unsigned long	C001C0DE = 0xC001C0DE;
static const int		PRIMARY = 1;
static const int		BACKUP = 0;
static const int		SUCCESS = (1<<16);

static int
sboot(cmd_tbl_t * cmdtp, long partition)
{
	/* if not valid, default to 1st */
	if (partition != BACKUP && partition != PRIMARY) {
	  	partition = PRIMARY;
	}
  	printf("booting off %s partition\n", (partition == PRIMARY) ? "primary" : "backup" );

  	// write signature and partition.

  	// load fpga - fail of loading fpga doesn't
	// stop loading the kernel. at least, we can
	// go to linux and fix things.
	{
		unsigned long src = FPGA_FLASH_ADDRESS + FPGA_MAXIMUM_SIZE;
		unsigned long dst = FPGA_BUF_ADDRESS;
		unsigned long len = FPGA_MAXIMUM_SIZE;

		unsigned long gzip_src = FPGA_BUF_ADDRESS;
		unsigned long gzip_dst = FPGA_BUF_ADDRESS + FPGA_MAXIMUM_SIZE;
		unsigned long gzip_len = FPGA_MAXIMUM_SIZE;

		if (partition == BACKUP) {
		  	src = FPGA_FLASH_ADDRESS;
		}

		if (read_dataflash(src, len, (char *) dst) != 1) {
			printf("sboot error: failed to read from flash %lx\n", src);
		} else if (gunzip((void *) gzip_dst, ~0UL, (void *) gzip_src, &gzip_len) != 0) {
			printf("sboot error: failed to gunzip data\n");
		} else if (load_fpga((unsigned char *)gzip_dst, gzip_len) != 0) {
			printf("sboot error: failed to load to fpga\n");
		}
	}

  	// load kernel
	{
	  	extern int do_bootm(void *, int, int, char  *argv[]);

		char cmdline_buf[256];
	  	char *argv[] = { "cmd", "d0201900", 0L };
		char *rootfs = "/dev/mtdblock1";

		if (partition == BACKUP) {
		  	argv[1] = "d0042000";
			rootfs = "/dev/mtdblock0";
		}
		sprintf(cmdline_buf,
			"console=ttyS5,115200 root=%s mtdparts=atmel_nand:64M(root0),64M(root1),64M(data) ro rootfstype=jffs2", 
			rootfs);

		setenv("bootargs", cmdline_buf);
	  	if (do_bootm(cmdtp, 0, 2, argv) != 0) {
		  	printf("sboot error: failed to boot\n");
		  	return 1;
		}
	}

  	// boot

  	return 0;
}

static int
do_sboot(cmd_tbl_t * cmdtp, int flag, int argc, char *argv[])
{
  	unsigned long *bp = (void*)(0x20000200);
  	long partition = PRIMARY;

  	// if asked, boot off the specific partition. if not, see if it's stored and
  	// boot off the partition.
  	if (argc == 2) {
	  	partition = simple_strtoul(argv[1], 0L, 16);
	} else {
	  	// find the partition
	  	if (*bp == C001C0DE) {
		  	unsigned long word = bp[1];
		  	partition = word & 0x1;

			if (partition == PRIMARY && (word & SUCCESS) == 0) {
			  	partition = BACKUP;
			} else {
			  	partition = PRIMARY;
			}
		}

		/*
 		 * sign the partition - if kernel successfully booted, 
		 * it should write bp[1] to PRIMARY again.
		 */
		bp[0] = C001C0DE;
		bp[1] = partition;
	}

	return sboot(cmdtp, partition);
}

/**************************************************/
U_BOOT_CMD(xlx, 3, 0, do_xlx, "load xilinx fpga", "address size");
U_BOOT_CMD(sboot, 3, 0, do_sboot, "sensys custom boot", "\n [ partition ]   - boot checking for errors");
