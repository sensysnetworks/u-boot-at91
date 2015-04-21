/*
 * fat.c
 *
 * R/O (V)FAT 12/16/32 filesystem implementation by Marcus Sundberg
 *
 * 2002-07-28 - rjones@nexus-tech.net - ported to ppcboot v1.1.6
 * 2003-03-10 - kharris@nexus-tech.net - ported to uboot
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

#include <common.h>
#include <config.h>
#include <rtc.h>

#define DOS_FAT_EPOCH         80

#define DOS_PART_TBL_OFFSET	    0x1be
#define DOS_PART_MAGIC_OFFSET	  0x1fe
#define DOS_FS_TYPE_OFFSET	    0x36
#define DOS_FS_TYPE_OFFSET_ALT  0x52
#include <fat.h>

typedef struct {
	unsigned int bsec:5, min:6, hour:5;
} DOS_FAT_HMS;

typedef struct {
	unsigned int day:5, mth:4, year:7;
} DOS_FAT_YMD;

#define LASTCLUSTER32           0x0FFFFFFF
#define LASTCLUSTER16           0xFFFF
#define LASTCLUSTER12           0x0FFF
#define INVALIDCLUSTERENTRY   (-1)

#define DOS_PART_TBL_OFFSET	  0x1be
#define DOS_PART_MAGIC_OFFSET	0x1fe
#define DOS_FS_TYPE_OFFSET	  0x36

#define LS_08 8
#define LS_32 32
#define LS_64 64

#if defined(CONFIG_SUPPORT_VFAT)
#undef CONFIG_SUPPORT_VFAT
#endif

#define SECTOR_SIZE_512  512	/*512 mandatory */
#if defined(__LITTLE_ENDIAN)

//processor may have an instructions for these  (does the compiler optimization check this ?)
#define CPU322FAT16(x) ((x&0xFFFF0000)>>16)
#define CPU162FAT32(x) ((x&0x0000FFFF)<<16)
#define CPU162FAT16(x) ((x&0x0000FFFF))
#define CPU322FAT32(x) ((x&0xFFFFFFFF))

#else

//high 16 of 32 to lo 16 of 32 with a twist
#define CPU322FAT16(x) (((x&0xFF000000)>>24)|((x&0x00FF0000)>>8))

//high 8 of lo 16 to lo 8 of lo 16 with a twist
#define CPU162FAT16(x) (((x&0x000000FF)<< 8)|((x&0x0000FF00)>>8))

//high 8 of lo 16 to lo   8 of high 16 with a twist
//lo   8 of lo 16 to high 8 of high 16 with a twist
#define CPU162FAT32(x) (((x&0x000000FF)<<24)|((x&0x0000FF00)<<8))

#define CPU322FAT32(x) (CPU162FAT32(x)|CPU322FAT16(x))

#endif

#define FS_CLUSTER_TO_SECTOR(mydata,dir_cluster) (mydata->data_begin + (dir_cluster * mydata->clust_size))

static struct rtc_time rtc;

__attribute__ ((__aligned__(__alignof__(dir_entry))))
__u8 do_fat_read_block[MAX_CLUSTSIZE];
__attribute__ ((__aligned__(__alignof__(dir_entry))))
__u8 get_parent_block[MAX_CLUSTSIZE];
__attribute__ ((__aligned__(__alignof__(dir_entry))))
__u8 get_dentfromdir_block[MAX_CLUSTSIZE];
#if defined(CONFIG_SUPPORT_VFAT)
__attribute__ ((__aligned__(__alignof__(dir_entry))))
__u8 get_vfatname_block[MAX_CLUSTSIZE];
#endif

static block_dev_desc_t *cur_dev = NULL;
unsigned long part_offset = 0;
static int cur_part = 1;

static __u32 last_dir_cluster = 0;
static __u8 *last_dir_buffer;

int get_mydata(fsdata * mydata, int *rootdir_size);

int disk_read_fat(fsdata * mydata, __u32 startblock, __u32 getsize,
		  __u8 * bufptr);
int disk_write_fat(fsdata * mydata, __u32 startblock, __u32 putsize,
		   __u8 * bufptr);

clock_t clock()
{
	return 0;
}

static __u8 GetFileTimeMS()
{
	union {
		__u32 ms;
		clock_t clk;
	} clk;
	clk.clk = clock();
	//TODO check
	return (clk.ms % 1000) / 4;
}

static __u16 GetFileDate(struct rtc_time *rtc)
{
	union {
		__u16 dfu;
		DOS_FAT_YMD dft;
	} dtfu;
	dtfu.dft.year = rtc->tm_year - DOS_FAT_EPOCH;
	dtfu.dft.mth = rtc->tm_mon;
	dtfu.dft.day = rtc->tm_mday;

	return (dtfu.dfu);
}

static __u16 GetFileTime(struct rtc_time *rtc)
{
	union {
		__u16 dfu;
		DOS_FAT_HMS dft;
	} dtfu;
	dtfu.dft.hour = rtc->tm_hour;
	dtfu.dft.min = rtc->tm_min;
	dtfu.dft.bsec = rtc->tm_sec / 2;
	return (dtfu.dfu);
}

/*
 * Convert a string to lowercase.
 */
static void downcase(char *str)
{
	while (*str != '\0') {
		TOLOWER(*str);
		str++;
	}
}

static void upcase(char *str)
{
	while (*str != '\0') {
		TOUPPER(*str);
		str++;
	}
}

/*
 * Get the first occurence of a directory delimiter ('/' or '\') in a string.
 * Return index into string if found, -1 otherwise.
 */
static int dirdelim(char *str)
{
	char *start = str;

	while (*str != '\0') {
		if (ISDIRDELIM(*str))
			return str - start;
		str++;
	}
	return -1;
}

/*
 * Match volume_info fs_type strings.
 * Return 0 on match, -1 otherwise.
 */
static int compare_sign(char *str1, char *str2)
{
	char *end = str1 + SIGNLEN;

	while (str1 != end) {
		if (*str1 != *str2) {
			return -1;
		}
		str1++;
		str2++;
	}
	return 0;
}

/*
 * Extract zero terminated short name from a directory entry.
 */
static void get_name(dir_entry * dirent, char *s_name)
{
	char *ptr;

	memcpy(s_name, dirent->name, 8);
	s_name[8] = '\0';
	ptr = s_name;
	while (*ptr && *ptr != ' ')
		ptr++;
	if (dirent->ext[0] && dirent->ext[0] != ' ') {
		*ptr = '.';
		ptr++;
		memcpy(ptr, dirent->ext, 3);
		ptr[3] = '\0';
		while (*ptr && *ptr != ' ')
			ptr++;
	}
	*ptr = '\0';
	if (*s_name == DELETED_FLAG) {
		*s_name = '\0';
	} else if (*s_name == aRING) {
		*s_name = DELETED_FLAG;
	}
	downcase(s_name);
}

int disk_read(__u32 startblock, __u32 getsize, __u8 * bufptr)
{
	startblock += part_offset;
	if (cur_dev == NULL) {
		FAT_ERROR("Error reading disk blocks\n");
		return -1;
	}
	if (cur_dev->block_read) {
		return cur_dev->block_read(cur_dev->dev, startblock, getsize,
					   (unsigned long *)bufptr);
	}
	FAT_ERROR("Error: no block_read routine available\n");
	return -1;
}

int disk_write(__u32 startblock, __u32 putsize, __u8 * bufptr)
{
	startblock += part_offset;
	if (cur_dev == NULL) {
		FAT_ERROR("Error writing disk blocks\n");
		return -1;
	}
	if (cur_dev->block_write) {
		return cur_dev->block_write(cur_dev->dev, startblock, putsize,
					    (unsigned long *)bufptr);
	}
	FAT_ERROR("Error: no block_write routine available\n");
	return -1;
}

static int check_write = 0;	//defined(FAT_CHECK_IF_WRITE_UNAVAILABLE);
static int fail_if_no_write = 1;	//defined(FAT_FAIL_IF_WRITE_UNAVAILABLE);

static int set_check_write_flag(int flag)
{
	int res = check_write;
	check_write = flag;
	return res;
}

static int set_fail_if_no_write(int flag)
{
	int res = fail_if_no_write;
	fail_if_no_write = flag;
	return res;
}

int fat_register_device(block_dev_desc_t * dev_desc, int part_no)
{
	unsigned char buffer[SECTOR_SIZE_512];
	disk_partition_t info;

	if (!dev_desc->block_read) {
		printf
		    ("** Can't read from device %d. block_read unavailable **\n",
		     dev_desc->dev);
		FAT_ERROR("Error: no block read routine available\n");
		return -1;
	}
	//fat file write requires block_write
	if (check_write && !dev_desc->block_write) {
		printf
		    ("** Can't write to device %d. block_write unavailable **\n",
		     dev_desc->dev);
		if (fail_if_no_write) {
			FAT_ERROR("Error: no block write routine available\n");
			return -1;
		}
	}
	cur_dev = dev_desc;

	/* check if we have a MBR (on floppies we have only a PBR) */
	if (dev_desc->block_read(dev_desc->dev, 0, 1, (ulong *) buffer) != 1) {
		printf("** Can't read from device %d  block_read failed**\n",
		       dev_desc->dev);
		FAT_ERROR("Error: can't read from device\n");
		return -1;
	}
	if (buffer[DOS_PART_MAGIC_OFFSET] != 0x55
	    || buffer[DOS_PART_MAGIC_OFFSET + 1] != 0xaa) {
		/* no signature found */
		printf("** Can't use from device %d  no signature found**\n",
		       dev_desc->dev);
		FAT_ERROR("Error: no signature found on device\n");
		return -1;
	}
#if (defined(CONFIG_CMD_IDE) || \
     defined(CONFIG_CMD_MG_DISK) || \
     defined(CONFIG_CMD_SATA) || \
     defined(CONFIG_CMD_SCSI) || \
     defined(CONFIG_CMD_USB) || \
     defined(CONFIG_MMC) || \
     defined(CONFIG_SYSTEMACE) )
	/* First we assume, there is a MBR */
	if (!get_partition_info(dev_desc, part_no, &info)) {
		part_offset = info.start;
		cur_part = part_no;
	} else if (!strncmp((char *)&buffer[DOS_FS_TYPE_OFFSET], "FAT", 3)) {
		/* ok, we assume we are on a PBR only */
		cur_part = 1;
		part_offset = 0;
	} else if (!strncmp((char *)&buffer[DOS_FS_TYPE_OFFSET_ALT], "FAT", 3)) {
		/* ok, we assume we are on a PBR only */
		cur_part = 1;
		part_offset = 0;
	} else {
		printf("** Partition %d not valid on device %d **\n", part_no,
		       dev_desc->dev);
		FAT_ERROR("Error: not a valid device\n");
		return -1;
	}

#else
	if (!strncmp((char *)&buffer[DOS_FS_TYPE_OFFSET], "FAT", 3)) {
		/* ok, we assume we are on a PBR only */
		cur_part = 1;
		part_offset = 0;
		info.start = part_offset;
	} else {
		/* FIXME we need to determine the start block of the
		 * partition where the DOS FS resides. This can be done
		 * by using the get_partition_info routine. For this
		 * purpose the libpart must be included.
		 */
		part_offset = 32;
		cur_part = 1;
	}
#endif
	return 0;
}

/*
 * Get the entry at index 'entry' in a FAT (12/16/32) table.
 * On failure 0x00 is returned.
 */
static __u32 get_fatent(fsdata * mydata, __u32 entry)
{
	__u32 bufnum = 0;
	__u32 offset = 0;
	__u32 ret = 0x00;

	switch (mydata->fatsize) {
	case 32:
		bufnum = entry / FAT32BUFSIZE;
		offset = entry - bufnum * FAT32BUFSIZE;
		break;
	case 16:
		bufnum = entry / FAT16BUFSIZE;
		offset = entry - bufnum * FAT16BUFSIZE;
		break;
	case 12:
		bufnum = entry / FAT12BUFSIZE;
		offset = entry - bufnum * FAT12BUFSIZE;
		break;
	}

	/* Read a new block of FAT entries into the cache. */
	if (bufnum != mydata->fatbufnum) {
		int getsize = FATBUFSIZE / FS_BLOCK_SIZE;
		__u8 *bufptr = mydata->fatbuf;
		__u32 fatlength = mydata->fatlength;
		__u32 startblock = bufnum * FATBUFBLOCKS;

		startblock += mydata->fat_sect;	/* Offset from start of disk */

		if (getsize > fatlength)
			getsize = fatlength;
		//fatlength  *= SECTOR_SIZE_512;            /* We want it in bytes now */

		if (disk_read_fat(mydata, startblock, getsize, bufptr) < 0) {
			FAT_ERROR("Error reading FAT blocks\n");
			return 0;
		}
		mydata->fatbufnum = bufnum;
	}

	/* Get the actual entry from the table */
	switch (mydata->fatsize) {
	case 32:
		ret = FAT2CPU32(((__u32 *) mydata->fatbuf)[offset]);
		break;
	case 16:
		ret = FAT2CPU16(((__u16 *) mydata->fatbuf)[offset]);
		break;
	case 12:
		{
			__u32 off16 = (offset * 3) / 4;
			__u16 val1, val2;

			switch (offset & 0x3) {
			case 0:
				ret =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				ret &= 0xfff;
				break;
			case 1:
				val1 =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				val1 &= 0xf000;
				val2 =
				    FAT2CPU16(((__u16 *) mydata->fatbuf)[off16 +
									 1]);
				val2 &= 0x00ff;
				ret = (val2 << 4) | (val1 >> 12);
				break;
			case 2:
				val1 =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				val1 &= 0xff00;
				val2 =
				    FAT2CPU16(((__u16 *) mydata->fatbuf)[off16 +
									 1]);
				val2 &= 0x000f;
				ret = (val2 << 8) | (val1 >> 8);
				break;
			case 3:
				ret =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				;
				ret = (ret & 0xfff0) >> 4;
				break;
			default:
				break;
			}
		}
		break;
	}
	FAT_DPRINT("ret: %d, offset: %d\n", ret, offset);

	return ret;
}

static __u32 fetch_set_fatent(fsdata * mydata, __u32 entry, __u32 nclno,
			      __u32 * last_startblock, int *last_getsize,
			      __u32 * last_bufnum)
{
	__u32 bufnum = 0;
	__u32 offset = 0;
	__u8 *bufptr = mydata->fatbuf;
	int getsize = FATBUFSIZE / FS_BLOCK_SIZE;
	__u32 fatlength = mydata->fatlength;
	__u32 startblock;
	__u32 fbs = FAT32BUFSIZE;
	//ranch bound entry
	if (entry >= mydata->fatlast) {
		FAT_ERROR("Error reading FAT blocks\n");
		return INVALIDCLUSTERENTRY;
	}
	switch (mydata->fatsize) {
	case 32:
		bufnum = entry / fbs;
		offset = entry - bufnum * fbs;
		break;
	case 16:
		bufnum = entry / FAT16BUFSIZE;
		offset = entry - bufnum * FAT16BUFSIZE;
		break;
	case 12:
		bufnum = entry / FAT12BUFSIZE;
		offset = entry - bufnum * FAT12BUFSIZE;
		break;
	}
	/* Write last then read a new block of FAT entries from the cache. */
	if (bufnum != mydata->fatbufnum) {
		long fl;
		startblock = bufnum * FATBUFBLOCKS;
		fl = fatlength - startblock;
		if (fl <= 0) {
			FAT_ERROR("Error writing FAT blocks\n");
			return INVALIDCLUSTERENTRY;
		}
		if (fl < getsize) {
			getsize = fl;
		}
		startblock += mydata->fat_sect;	/* Offset from start of disk */
		if (*last_bufnum != -1) {
			if (mydata->fat_dirty
			    && disk_write_fat(mydata, *last_startblock,
					      *last_getsize,
					      mydata->fatbuf) < 0) {
				FAT_ERROR("Error writing FAT blocks\n");
				return INVALIDCLUSTERENTRY;
			}
		}
		if (disk_read_fat(mydata, startblock, getsize, bufptr) < 0) {
			FAT_ERROR("Error reading FAT blocks\n");
			return INVALIDCLUSTERENTRY;
		}
		*last_startblock = startblock;
		*last_getsize = getsize;
		*last_bufnum = bufnum;
		mydata->fatbufnum = bufnum;
		mydata->fat_dirty = 0;
	} else {
		long fl;
		startblock = bufnum * FATBUFBLOCKS;
		fl = fatlength - startblock;
		if (fl <= 0) {
			FAT_ERROR("Error writing FAT blocks\n");
			return INVALIDCLUSTERENTRY;
		}
		if (fl < getsize) {
			getsize = fl;
		}
		startblock += mydata->fat_sect;	/* Offset from start of disk */
		*last_startblock = startblock;
		*last_getsize = getsize;
		*last_bufnum = bufnum;
	}
	switch (mydata->fatsize) {
	case 32:
		{
			__u32 *fp = (__u32 *) mydata->fatbuf;
			//      fp[offset]  = cpu_to_le32(nclno);
			entry = fp[offset];
			fp[offset] = CPU322FAT32(nclno);
			mydata->fat_dirty = 1;
		}
		break;
	case 16:
		{
			__u16 *fp = (__u16 *) mydata->fatbuf;
			//      fp[offset]  = cpu_to_le16(nclno);
			entry = fp[offset];
			fp[offset] = CPU162FAT16(nclno);
			mydata->fat_dirty = 1;
		}
		break;
	case 12:
		{
			__u8 *fp = (__u8 *) mydata->fatbuf;
			__u32 off16 = (offset * 3) / 2;
			fp = &fp[off16];

			if ((offset & 1) == 1) {
				entry =
				    (((fp[0] & 0xf0) >> 4) |
				     ((fp[1] & 0xff) << 4));
				fp[0] = (nclno << 4) | (fp[0] & 0x0f);
				fp[1] = nclno >> 4;
			} else {
				entry = ((fp[0]) | ((fp[1] & 0x0f) << 8));
				fp[0] = nclno & 0xff;
				fp[1] = (fp[1] & 0xf0) | (nclno >> 8);
			}
			mydata->fat_dirty = 1;
		}
		break;
	}
	if (entry == INVALIDCLUSTERENTRY) {
		printf("Invalid Cluster\n");
	}
	return entry;
}

static __u32 allocate_fatent(fsdata * mydata, __u32 * last_startblock,
			     int *last_getsize, __u32 * last_bufnum,
			     __u32 first)
{
	__u32 bufnum = 0;
	__u32 offset = 0;
	__u32 ret = 0x00;
	__u32 entry = first;
	__u32 fbs = FAT32BUFSIZE;
      top:
	switch (mydata->fatsize) {
	case 32:
		bufnum = entry / fbs;
		offset = entry - bufnum * fbs;
		break;
	case 16:
		bufnum = entry / FAT16BUFSIZE;
		offset = entry - bufnum * FAT16BUFSIZE;
		break;
	case 12:
		bufnum = entry / FAT12BUFSIZE;
		offset = entry - bufnum * FAT12BUFSIZE;
		break;
	}
	/* Read a new block of FAT entries into the cache. */
	if (bufnum != mydata->fatbufnum) {
		int getsize = FATBUFSIZE / FS_BLOCK_SIZE;
		__u8 *bufptr = mydata->fatbuf;
		__u32 fatlength = mydata->fatlength;
		__u32 startblock = bufnum * FATBUFBLOCKS;
		long fl = fatlength - startblock;
		if (fl <= 0) {
			FAT_ERROR("Error writing FAT blocks\n");
			return INVALIDCLUSTERENTRY;
		}
		if (fl < getsize) {
			getsize = fl;
		}
		startblock += mydata->fat_sect;	/* Offset from start of disk */
		//fatlength  *= SECTOR_SIZE_512;            /* We want it in bytes now */
		if (*last_bufnum != -1) {
			if (mydata->fat_dirty
			    && disk_write_fat(mydata, *last_startblock,
					      *last_getsize,
					      mydata->fatbuf) < 0) {
				FAT_ERROR("Error writing FAT blocks\n");
				return 0;
			}
		}
		if (disk_read_fat(mydata, startblock, getsize, bufptr) < 0) {
			FAT_ERROR("Error reading FAT blocks\n");
			return 0;
		}
		*last_startblock = startblock;
		*last_getsize = getsize;
		*last_bufnum = bufnum;
		mydata->fatbufnum = bufnum;
		mydata->fat_dirty = 0;
	} else {
	}
	/* Get the actual entry from the table */
	switch (mydata->fatsize) {
	case 32:
		ret = FAT2CPU32(((__u32 *) mydata->fatbuf)[offset]);
		if (ret == 0) {
			((__u32 *) mydata->fatbuf)[offset] =
			    CPU322FAT32(LASTCLUSTER32);
			mydata->fat_dirty = 1;
		}
		break;
	case 16:
		ret = FAT2CPU16(((__u16 *) mydata->fatbuf)[offset]);
		if (ret == 0) {
			((__u16 *) mydata->fatbuf)[offset] =
			    CPU162FAT16(LASTCLUSTER16);
			mydata->fat_dirty = 1;
		}
		break;
	case 12:
		{
			__u32 off16 = (offset * 3) / 4;
			__u16 val1, val2;
			switch (offset & 0x3) {
			case 0:
				ret =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				ret &= 0xfff;
				break;
			case 1:
				val1 =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				val1 &= 0xf000;
				val2 =
				    FAT2CPU16(((__u16 *) mydata->fatbuf)[off16 +
									 1]);
				val2 &= 0x00ff;
				ret = (val2 << 4) | (val1 >> 12);
				break;
			case 2:
				val1 =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				val1 &= 0xff00;
				val2 =
				    FAT2CPU16(((__u16 *) mydata->fatbuf)[off16 +
									 1]);
				val2 &= 0x000f;
				ret = (val2 << 8) | (val1 >> 8);
				break;
			case 3:
				ret =
				    FAT2CPU16(((__u16 *) mydata->
					       fatbuf)[off16]);
				ret = (ret & 0xfff0) >> 4;
				break;
			}
			if (ret == 0) {
				__u32 off16 = (offset * 3) / 2;
				__u8 *fp = (__u8 *) mydata->fatbuf;
				__u16 nclno = LASTCLUSTER12;
				fp = &fp[off16];
				if ((offset & 1) == 1) {
					fp[0] = (nclno << 4) | (fp[0] & 0x0f);
					fp[1] = nclno >> 4;
				} else {
					fp[0] = nclno & 0xff;
					fp[1] = (fp[1] & 0xf0) | (nclno >> 8);
				}
				mydata->fat_dirty = 1;
			}
		}
		break;
	}
	if (ret) {
		ret = 0x00;
		entry++;
		if (entry < mydata->fatlast) {
			goto top;
		}
		return 0;
	}
	return entry;
}

/*
 * Write at most 'size' bytes to the specified cluster from 'buffer'.
 * Return 0 on success, -1 otherwise.
 */
static int put_cluster(fsdata * mydata, __u32 clustnum, __u8 * buffer,
		       unsigned long size)
{
	int idx = 0;
	int rdx = 0;
	__u32 startsect;

	if (clustnum > 0) {
		startsect = FS_CLUSTER_TO_SECTOR(mydata, clustnum);
	} else {
		startsect = mydata->rootdir_sect;
	}

	FAT_DPRINT("pc - clustnum: %d, startsect: %d\n", clustnum, startsect);
	idx = size / FS_BLOCK_SIZE;
	if (idx) {
		if (disk_write(startsect, idx, buffer) < 0) {
			FAT_ERROR("Error writing data\n");
			return -1;
		}
	}
	rdx = size % FS_BLOCK_SIZE;
	if (rdx) {
		__u8 tmpbuf[FS_BLOCK_SIZE];

		memset(&tmpbuf[rdx], 0x00, sizeof(tmpbuf) - rdx);

		buffer += size - rdx;	//idx*FS_BLOCK_SIZE;
		memcpy(tmpbuf, buffer, rdx);

		if (disk_write(startsect + idx, 1, tmpbuf) < 0) {
			FAT_ERROR("Error writing data\n");
			return -1;
		}
	}
	return 0;
}

/*
 * Read at most 'size' bytes from the specified cluster into 'buffer'.
 * Return 0 on success, -1 otherwise.
 */
static int get_cluster(fsdata * mydata, __u32 clustnum, __u8 * buffer,
		       unsigned long size)
{
	int idx = 0;
	int rdx = 0;
	__u32 startsect;

	if (clustnum > 0) {
		startsect = FS_CLUSTER_TO_SECTOR(mydata, clustnum);
	} else {
		startsect = mydata->rootdir_sect;
	}

	FAT_DPRINT("gc - clustnum: %d, startsect: %d\n", clustnum, startsect);
	idx = size / FS_BLOCK_SIZE;
	if (idx) {
		if (disk_read(startsect, idx, buffer) < 0) {
			FAT_ERROR("Error reading data\n");
			return -1;
		}
	}
	rdx = size % FS_BLOCK_SIZE;
	if (rdx) {
		__u8 tmpbuf[FS_BLOCK_SIZE];

		memset(&tmpbuf[rdx], 0x00, sizeof(tmpbuf) - rdx);

		if (disk_read(startsect + idx, 1, tmpbuf) < 0) {
			FAT_ERROR("Error reading data\n");
			return -1;
		}

		buffer += size - rdx;	//idx*FS_BLOCK_SIZE;
		memcpy(buffer, tmpbuf, rdx);
	}
	return 0;
}

/*
 * Read at most 'maxsize' bytes from the file associated with 'dentptr'
 * into 'buffer'.
 * Return the number of bytes read or -1 on fatal errors.
 */
static long get_contents(fsdata * mydata, dir_entry * dentptr, __u8 * buffer,
			 unsigned long maxsize)
{
	unsigned long filesize = FAT2CPU32(dentptr->size);
	unsigned long gotsize = 0;
	unsigned int bytesperclust = mydata->clust_size * SECTOR_SIZE_512;
	__u32 curclust = START(dentptr);
	__u32 endclust;
	__u32 newclust;
	unsigned long actsize;
	FAT_DPRINT("Filesize: %ld bytes\n", filesize);
	if (maxsize > 0 && filesize > maxsize) {
		filesize = maxsize;
	}
	FAT_DPRINT("Reading: %ld bytes\n", filesize);
	actsize = bytesperclust;
	if (filesize <= actsize) {
		if (filesize < actsize) {
			memset(&buffer[filesize], 0x00, (actsize - filesize));
		}
		if (get_cluster(mydata, curclust, buffer, (int)filesize) != 0) {
			FAT_ERROR("Error reading cluster\n");
			return -1;
		}
		return filesize;
	}
	endclust = curclust;
	do {
		/* search for consecutive clusters */
		while (actsize < filesize) {
			newclust = get_fatent(mydata, endclust);
			if (newclust == 0) {
				FAT_ERROR("1 FAT entry read error\n");
				return -1;
			}
			if ((newclust - 1) != endclust) {
				goto getit;
			}
			if (CHECK_CLUST(newclust, mydata->fatsize)) {
				FAT_DPRINT("1 curclust: 0x%x\n", newclust);
				FAT_DPRINT("Last FAT entry\n");
				return gotsize;
			}
			endclust = newclust;
			actsize += bytesperclust;
		}
		/* actsize >= file size */
		actsize -= bytesperclust;
		if (actsize) {
			/* get remaining clusters */
			if (get_cluster(mydata, curclust, buffer, (int)actsize)
			    != 0) {
				FAT_ERROR("Error reading cluster\n");
				return -1;
			}
		}
		/* get remaining bytes */
		gotsize += (int)actsize;
		filesize -= actsize;
		buffer += actsize;
		actsize = filesize;
		if (actsize) {
			if (get_cluster(mydata, endclust, buffer, (int)actsize)
			    != 0) {
				FAT_ERROR("Error reading cluster\n");
				return -1;
			}
		}
		gotsize += actsize;
		return gotsize;
	      getit:
		if (get_cluster(mydata, curclust, buffer, (int)actsize) != 0) {
			FAT_ERROR("Error reading cluster\n");
			return -1;
		}
		gotsize += (int)actsize;
		filesize -= actsize;
		buffer += actsize;
		curclust = get_fatent(mydata, endclust);
		if (curclust == 0) {
			FAT_ERROR("2 FAT entry read error\n");
			return -1;
		}
		if (CHECK_CLUST(curclust, mydata->fatsize)) {
			FAT_DPRINT("2 curclust: 0x%x\n", curclust);
			FAT_DPRINT("Last FAT entry\n");
			return gotsize;
		}
		actsize = bytesperclust;
		endclust = curclust;
	}
	while (1);
}

/*
 * Write at most 'maxsize' bytes to the file associated with 'dentptr'
 * form 'buffer'.
 * Return the number of bytes read or -1 on fatal errors.
 */
static long put_contents(fsdata * mydata, dir_entry * dentptr, __u8 * buffer,
			 unsigned long maxsize)
{
	unsigned long filesize = FAT2CPU32(dentptr->size);
	unsigned long gotsize = 0;
	unsigned int bytesperclust = mydata->clust_size * SECTOR_SIZE_512;
	__u32 curclust = START(dentptr);
	__u32 endclust;
	__u32 newclust;
	__u32 saveclust;
	unsigned long actsize;
	saveclust = curclust;
	FAT_DPRINT("Filesize: %ld bytes\n", filesize);
	if (maxsize > 0 && filesize > maxsize) {
		filesize = maxsize;
	}
	FAT_DPRINT("Writing: %ld bytes\n", filesize);
	actsize = bytesperclust;
	if (filesize <= actsize) {
		if (filesize < actsize) {
			memset(&buffer[filesize], 0x00, (actsize - filesize));
		}
		if (put_cluster(mydata, curclust, buffer, (int)filesize) != 0) {
			FAT_ERROR("Error writing cluster\n");
			return -1;
		}
		return filesize;
	}
	endclust = curclust;
	do {
		/* search for consecutive clusters */
		while (actsize < filesize) {
			newclust = get_fatent(mydata, endclust);
			if (newclust == 0) {
				FAT_ERROR("3 FAT entry read error\n");
				return -1;
			}
			if ((newclust - 1) != endclust) {
				goto getit;
			}
			if (CHECK_CLUST(newclust, mydata->fatsize)) {
				FAT_DPRINT("3 curclust: 0x%x\n", newclust);
				FAT_DPRINT("Last FAT entry\n");
				return gotsize;
			}
			endclust = newclust;
			actsize += bytesperclust;
		}
		/* actsize >= file size */
		actsize -= bytesperclust;
		if (actsize == 0) {
			/* filesize <= bytespercluster  */
		} else {
			/* put span of clusters  */
			if (put_cluster(mydata, curclust, buffer, (int)actsize)
			    != 0) {
				FAT_ERROR("Error writing cluster\n");
				return -1;
			}
		}
		/* put remaining bytes */
		gotsize += (int)actsize;
		filesize -= actsize;
		buffer += actsize;
		actsize = filesize;
		if (actsize) {
			if (put_cluster(mydata, endclust, buffer, (int)actsize)
			    != 0) {
				FAT_ERROR("Error writing cluster\n");
				return -1;
			}
		}
		gotsize += actsize;
		return gotsize;
	      getit:
		if (put_cluster(mydata, curclust, buffer, (int)actsize) != 0) {
			FAT_ERROR("Error writing cluster\n");
			return -1;
		}
		gotsize += (int)actsize;
		filesize -= actsize;
		buffer += actsize;
		curclust = get_fatent(mydata, endclust);
		if (curclust == 0) {
			FAT_ERROR("4 FAT entry read error\n");
			return -1;
		}
		if (CHECK_CLUST(curclust, mydata->fatsize)) {
			FAT_DPRINT("4 curclust: 0x%x\n", curclust);
			FAT_DPRINT("Last FAT entry\n");
			return gotsize;
		}
		actsize = bytesperclust;
		endclust = curclust;
	}
	while (1);
}

#if defined(CONFIG_SUPPORT_VFAT)

static int slot2str(dir_slot * slotptr, char *l_name, int *idx)
{
	int j;
	for (j = 0; j <= 8; j += 2) {
		l_name[*idx] = slotptr->name0_4[j];
		if (l_name[*idx] == 0x00)
			return 1;
		(*idx)++;
	}
	for (j = 0; j <= 10; j += 2) {
		l_name[*idx] = slotptr->name5_10[j];
		if (l_name[*idx] == 0x00)
			return 1;
		(*idx)++;
	}
	for (j = 0; j <= 2; j += 2) {
		l_name[*idx] = slotptr->name11_12[j];
		if (l_name[*idx] == 0x00)
			return 1;
		(*idx)++;
	}
	return 0;
}

static int str2slot(dir_slot * slotptr, char *l_name, int *idx)
{
	int j;
	for (j = 0; j <= 8; j += 2) {
		slotptr->name0_4[j] = l_name[*idx];
		if (l_name[*idx] == 0x00)
			return 1;
		(*idx)++;
	}
	for (j = 0; j <= 10; j += 2) {
		slotptr->name5_10[j] = l_name[*idx];
		if (l_name[*idx] == 0x00)
			return 1;
		(*idx)++;
	}
	for (j = 0; j <= 2; j += 2) {
		slotptr->name11_12[j] = l_name[*idx];
		if (l_name[*idx] == 0x00)
			return 1;
		(*idx)++;
	}
	return 0;
}

static int get_vfatname(fsdata * mydata, int curclust, __u8 * cluster,
			dir_entry ** retdent, char *l_name)
{
	dir_entry *realdent;
	dir_slot *slotptr = (dir_slot *) * retdent;
	__u8 *nextclust = &cluster[mydata->clust_size * SECTOR_SIZE_512];
	__u8 counter = (slotptr->id & ~LAST_LONG_ENTRY_MASK) & 0xff;
	int idx = 0;

	while ((__u8 *) slotptr < nextclust) {
		if (counter == 0)
			break;
		if (((slotptr->id & ~LAST_LONG_ENTRY_MASK) & 0xff) != counter) {
			FAT_ERROR("Error: invalid slot counter\n");
			return -1;
		}
		slotptr++;
		counter--;
	}

	if ((__u8 *) slotptr >= nextclust) {
		dir_slot *slotptr2;

		slotptr--;
		curclust = get_fatent(mydata, curclust);
		if (curclust == 0) {
			FAT_ERROR("5 FAT entry read error\n");
			return -1;
		}

		if (CHECK_CLUST(curclust, mydata->fatsize)) {
			FAT_DPRINT("5 curclust: 0x%x\n", curclust);
			FAT_DPRINT("Last FAT entry\n");
			return -1;
		}
		if (get_cluster
		    (mydata, curclust, get_vfatname_block,
		     mydata->clust_size * SECTOR_SIZE_512) != 0) {
			FAT_ERROR("Error reading directory block\n");
			return -1;
		}
		slotptr2 = (dir_slot *) get_vfatname_block;
		while (slotptr2->id > 0x01) {
			slotptr2++;
		}
		/* Save the real directory entry */
		realdent = (dir_entry *) slotptr2 + 1;
		while ((__u8 *) slotptr2 >= get_vfatname_block) {
			slot2str(slotptr2, l_name, &idx);
			slotptr2--;
		}
	} else {
		/* Save the real directory entry */
		realdent = (dir_entry *) slotptr;
	}

	do {
		slotptr--;
		if (slot2str(slotptr, l_name, &idx))
			break;
	}
	while (!(slotptr->id & LAST_LONG_ENTRY_MASK));

	l_name[idx] = '\0';
	if (*l_name == DELETED_FLAG) {
		*l_name = '\0';
	} else if (*l_name == aRING) {
		*l_name = DELETED_FLAG;
	}
	downcase(l_name);

	*retdent = realdent;

	/* Return the real directory entry */
	//memcpy(retdent, realdent, sizeof(dir_entry));

	return 0;
}

/* Calculate short name checksum */
static __u8 mkcksum(const char *str)
{
	int i;
	__u8 ret = 0;
	for (i = 0; i < 11; i++) {
		ret = (((ret & 1) << 7) | ((ret & 0xfe) >> 1)) + str[i];
	}
	return ret;
}

#endif //#if defined(CONFIG_SUPPORT_VFAT)

/*
 * Read boot sector and volume info from a FAT filesystem
 */
static int read_bootsectandvi(boot_sector * bs, volume_info * volinfo,
			      int *fatsize)
{
	__u8 block[FS_BLOCK_SIZE];
	volume_info *vistart;
	char *fstype;
	if (disk_read(0, 1, block) < 0) {
		FAT_ERROR("Error reading block\n");
		return -1;
	}
	memcpy(bs, block, sizeof(boot_sector));
	bs->reserved = FAT2CPU16(bs->reserved);
	bs->fat_length = FAT2CPU16(bs->fat_length);
	bs->secs_track = FAT2CPU16(bs->secs_track);
	bs->heads = FAT2CPU16(bs->heads);
	bs->hidden = FAT2CPU32(bs->hidden);
	bs->total_sect = FAT2CPU32(bs->total_sect);
	/* FAT32 entries */
	if (bs->fat_length == 0) {
		/* Assume FAT32 */
		bs->fat32_length = FAT2CPU32(bs->fat32_length);
		bs->flags = FAT2CPU16(bs->flags);
		bs->root_cluster = FAT2CPU32(bs->root_cluster);
		bs->info_sector = FAT2CPU16(bs->info_sector);
		bs->backup_boot = FAT2CPU16(bs->backup_boot);
		vistart = (volume_info *) (block + sizeof(boot_sector));
		*fatsize = 32;
	} else {
		vistart = (volume_info *) & (bs->fat32_length);
		*fatsize = 0;
	}
	memcpy(volinfo, vistart, sizeof(volume_info));
	/*
	 * Terminate fs_type string. Writing past the end of vistart
	 * is ok - it's just the buffer.
	 */
	fstype = vistart->fs_type;
	fstype[8] = '\0';
	if (*fatsize == 32) {
		if (compare_sign(FAT32_SIGN, vistart->fs_type) == 0) {
			return 0;
		}
	} else {
		if (compare_sign(FAT12_SIGN, vistart->fs_type) == 0) {
			*fatsize = 12;
			return 0;
		}
		if (compare_sign(FAT16_SIGN, vistart->fs_type) == 0) {
			*fatsize = 16;
			return 0;
		}
	}
	if (compare_sign(FAT32_SIGN, &block[0x36]) == 0) {
		*fatsize = 32;
		return 0;
	}
	if (compare_sign(FAT16_SIGN, &block[0x36]) == 0) {
		*fatsize = 16;
		return 0;
	}
	if (compare_sign(FAT12_SIGN, &block[0x36]) == 0) {
		*fatsize = 12;
		return 0;
	}
	FAT_ERROR("Error: broken fs_type sign\n");
	return -1;
}

static int delink(__u32 clno, fsdata * mydata)
{
	__u32 entry = clno;
	__u32 nclno = 0;

	__u32 last_startblock = -1;
	int last_getsize = 0;
	__u32 last_bufnum = -1;

	//remove chain from fat starting at clno
	//if this fails fragments created or worse
	if (CHECK_CLUST(entry, mydata->fatsize)) {
		FAT_ERROR("Fat delink error. Attempt to link invalid entry!\n");
		return -1;
	}
	do {
		nclno =
		    fetch_set_fatent(mydata, entry, 0, &last_startblock,
				     &last_getsize, &last_bufnum);
		if (nclno == INVALIDCLUSTERENTRY) {
			FAT_ERROR("Invalid cluster setting FAT entry\n");
			FAT_ERROR
			    ("Serious fat delink error. Disk fragmented!\n");
			return -1;
		}
		entry = nclno;

	}
	while (CHECK_CLUST(entry, mydata->fatsize) == 0);

	if (disk_write_fat
	    (mydata, last_startblock, last_getsize, mydata->fatbuf) < 0) {
		FAT_ERROR("Error writing FAT blocks\n");
		FAT_ERROR("Serious fat delink error. Disk fragmented!\n");
		return 0;
	}
	return 0;
}

static __u32 cluster_allocate(fsdata * mydata, unsigned int clusters)
{
	unsigned int cls = 0;
	__u32 clno;
	__u32 nclno;
	__u32 lclno;
	__u32 last_startblock = 0;
	int last_getsize = 0;
	__u32 last_bufnum = -1;
	mydata->fatbufnum = -1;
	lclno = clno =
	    allocate_fatent(mydata, &last_startblock, &last_getsize,
			    &last_bufnum, 0);
	if (clno == 0) {
		FAT_ERROR("Error allocating FAT blocks\n");
		return 0;
	}
	cls++;
	while (cls < clusters) {
		nclno =
		    allocate_fatent(mydata, &last_startblock, &last_getsize,
				    &last_bufnum, lclno + 1);
		if (nclno == 0) {
			delink(clno, mydata);
			FAT_ERROR("Error allocating FAT blocks\n");
			return 0;
		}
		if (fetch_set_fatent
		    (mydata, lclno, nclno, &last_startblock, &last_getsize,
		     &last_bufnum) == INVALIDCLUSTERENTRY) {
			delink(clno, mydata);
			FAT_ERROR("Error writing FAT blocks\n");
			return 0;
		}
		lclno = nclno;
		cls++;
	}
	if (disk_write_fat
	    (mydata, last_startblock, last_getsize, mydata->fatbuf) < 0) {
		delink(clno, mydata);
		FAT_ERROR("Error writing FAT blocks\n");
		return 0;
	}
	return clno;
}

static __u32 cluster_allocate_link(fsdata * mydata, __u32 clustno)
{
	__u32 last_startblock = 0;
	int last_getsize = 0;
	__u32 last_bufnum = -1;
	__u32 clno;
	clno =
	    allocate_fatent(mydata, &last_startblock, &last_getsize,
			    &last_bufnum, 0);
	if (clno) {
		if (fetch_set_fatent
		    (mydata, clustno, clno, &last_startblock, &last_getsize,
		     &last_bufnum) == INVALIDCLUSTERENTRY) {
			delink(clno, mydata);
			FAT_ERROR("Error writing FAT blocks\n");
			return 0;
		}
		if (disk_write_fat
		    (mydata, last_startblock, last_getsize,
		     mydata->fatbuf) < 0) {
			delink(clno, mydata);
			FAT_ERROR("Error writing FAT blocks\n");
			return 0;
		}
		return clno;
	}
	return 0;
}

/*
 * Get the directory entry associated with 'filename' from the directory
 * starting at START (retdent)
 */
static dir_entry *get_dentfromdir(fsdata * mydata, char *filename,
				  dir_entry * retdent, int dols, int *error)
{
#if defined(CONFIG_SUPPORT_VFAT)
	__u16 prevcksum = 0xffff;
#endif
	__u32 curclust = START(retdent);
	int files = 0;
	int dirs = 0;
	int flag = 0;

	if (dols == LS_08) {
		dols = LS_NO;
		flag = 1;
	}
	FAT_DPRINT("get_dentfromdir: %s\n", filename);
	*error = 0;
	while (1) {
		dir_entry *dentptr;
		int i;
		last_dir_buffer = get_dentfromdir_block;
		last_dir_cluster = curclust;
		if (get_cluster
		    (mydata, curclust, get_dentfromdir_block,
		     mydata->clust_size * SECTOR_SIZE_512) != 0) {
			FAT_ERROR("Error reading directory block\n");
			*error = 1;
			return NULL;
		}
		dentptr = (dir_entry *) get_dentfromdir_block;

		for (i = 0; i < DIRENTSPERCLUST; i++) {
			char s_name[14], l_name[256];

			l_name[0] = '\0';
			if (dentptr->name[0] == DELETED_FLAG) {
				dentptr++;
				continue;
			}
			if (dentptr->name[0] == 0) {
				if (dols != LS_NO && dols != LS_64) {
					printf("\n%d file(s), %d dir(s)\n\n",
					       files, dirs);
				}
				FAT_DPRINT("Dentname == NULL - %d\n", i);

				//at last entry of the directory
				//directory name or file name not found in non-root directory
				*error = LS_08;
				if (flag == 0) {
					return NULL;
				} else {
					retdent->starthi =
					    CPU322FAT16(curclust);
					retdent->start = CPU162FAT16(curclust);
					return dentptr;
				}
			}
#ifndef CONFIG_SUPPORT_VFAT
			if ((dentptr->attr & ATTR_VOLUME)) {
				dentptr++;
				continue;
			}
#else
			if ((dentptr->attr & ATTR_VOLUME)) {
				if (((dentptr->attr & ATTR_VFAT) == ATTR_VFAT)
				    && (dentptr->
					name[0] & LAST_LONG_ENTRY_MASK)) {
					dir_entry *retdent = dentptr;

					prevcksum =
					    ((dir_slot *) retdent)->
					    alias_checksum;
					get_vfatname(mydata, curclust,
						     get_dentfromdir_block,
						     &retdent, l_name);

					if (dols != LS_NO) {
						int isdir =
						    (retdent->attr & ATTR_DIR);
						char dirc;
						int doit = 0;
						if (isdir) {
							dirs++;
							dirc = '/';
							doit = 1;
						} else {
							dirc = ' ';
							if (l_name[0] != 0) {
								files++;
								doit = 1;
							}
						}
						if (doit) {
							if (dols != LS_64) {
								if (dirc == ' ') {
									printf
									    (" %8ld   %s%c\n",
									     (long)
									     FAT2CPU32
									     (retdent->
									      size),
									     l_name,
									     dirc);
								} else {
									printf
									    ("            %s%c\n",
									     l_name,
									     dirc);
								}
							}
						}
						dentptr++;
						continue;
					}
					FAT_DPRINT("vfatname: |%s|\n", l_name);
				} else {
					/* Volume label or VFAT entry */
					dentptr++;
					continue;
				}
			}
			if ((dols != LS_NO && dols != LS_32)
			    && mkcksum(dentptr->name) == prevcksum) {
				dentptr++;
				continue;
			}
#endif //#ifndef CONFIG_SUPPORT_VFAT
			//
			get_name(dentptr, s_name);
			//
			if (dols != LS_NO && dols != LS_32) {
				int isdir = (dentptr->attr & ATTR_DIR);
				char dirc;
				int doit = 0;
				if (isdir) {
					dirs++;
					dirc = '/';
					doit = 1;
				} else {
					dirc = ' ';
					if (s_name[0] != 0) {
						files++;
						doit = 1;
					}
				}
				if (doit) {
					if (dols != LS_64) {
						if (dirc == ' ') {
							printf(" %8ld   %s%c\n",
							       (long)
							       FAT2CPU32
							       (dentptr->size),
							       s_name, dirc);
						} else {
							printf
							    ("            %s%c\n",
							     s_name, dirc);
						}
					}
				}
				dentptr++;
				continue;
			}
			//
#ifndef CONFIG_SUPPORT_VFAT
			if (strcmp(filename, s_name)) {
				FAT_DPRINT("Mismatch: |%s|\n", s_name);
				dentptr++;
				continue;
			}
#else
			if (strcmp(filename, s_name)
			    && strcmp(filename, l_name)) {
				FAT_DPRINT("Mismatch: |%s|%s|\n", s_name,
					   l_name);
				dentptr++;
				continue;
			}
#endif
			//
			memcpy(retdent, dentptr, sizeof(dir_entry));
			FAT_DPRINT("DentName: %s", s_name);
			FAT_DPRINT(", start: 0x%x", START(dentptr));
			FAT_DPRINT(", size:  0x%x %s\n",
				   FAT2CPU32(dentptr->size),
				   (dentptr->attr & ATTR_DIR) ? "(DIR)" : "");
			return dentptr;
		}
		{
			__u32 prevclust = curclust;
			curclust = get_fatent(mydata, prevclust);
			if (CHECK_CLUST(curclust, mydata->fatsize)) {
				FAT_DPRINT("6 curclust: 0x%x\n", curclust);
				FAT_DPRINT("Last FAT entry\n");
				*error = 2;
				retdent->starthi = CPU322FAT16(prevclust);
				retdent->start = CPU162FAT16(prevclust);
				return NULL;
			}
		}
	}
	return NULL;
}

//doesnt work for root FAT12/16
//check all clusters of a directory  or FAT32 root for a deleted entry or last one
static __u32 check_dent(fsdata * mydata, int *error, __u32 curclust,
			dir_entry ** enddent)
{
	__u32 prevc = curclust;
	int limit;
	*error = 0;
	while (1) {
		dir_entry *dentptr;
		int i;

		if (get_cluster
		    (mydata, curclust, get_dentfromdir_block,
		     mydata->clust_size * SECTOR_SIZE_512) != 0) {
			FAT_ERROR("Error: reading directory block\n");
			*error = 3;
			return 0;
		}
		/*
		   dentptr = (dir_entry *) get_dentfromdir_block;
		   limit = DIRENTSPERCLUST;
		   for (i = 0; i < limit; i++)
		   {
		   if (dentptr->name[0] != DELETED_FLAG)
		   {
		   break;
		   }
		   if ( i+1 == limit )
		   {
		   __u32 pc = get_fatent(mydata, curclust);
		   if ( CHECK_CLUST(pc, mydata->fatsize) )
		   {
		   //dont delete none left
		   break;
		   }
		   {
		   __u32 last_startblock = -1;
		   int   last_getsize    = 0;
		   __u32 last_bufnum     = -1;
		   __u32 tc = fetch_set_fatent(mydata, curclust, 0 ,&last_startblock,&last_getsize,&last_bufnum);
		   __u32 lc = fetch_set_fatent(mydata, prevc   , tc,&last_startblock,&last_getsize,&last_bufnum);
		   if ( disk_write_fat(mydata,last_startblock, last_getsize, mydata->fatbuf) < 0)
		   {

		   FAT_ERROR("6 FAT entry write error\n");
		   }
		   curclust = tc;
		   continue;
		   }
		   }
		   }
		 */
		dentptr = (dir_entry *) get_dentfromdir_block;
		limit = DIRENTSPERCLUST;
		for (i = 0; i < limit; i++) {
			if (dentptr->name[0] == DELETED_FLAG) {
				*enddent = dentptr;
				return curclust;
			}
			if (dentptr->name[0] == 0) {
				return 0;
			}
			dentptr++;
		}
		prevc = curclust;
		curclust = get_fatent(mydata, prevc);
		if (curclust == 0) {
			FAT_ERROR("6 FAT entry read error\n");
			return -1;
		}

		if (CHECK_CLUST(curclust, mydata->fatsize)) {
			FAT_DPRINT("7 curclust: 0x%x\n", curclust);
			FAT_DPRINT("Last FAT entry\n");
			*error = 0;
			return 0;
		}
	}
	return 0;
}

static int put_direntry_name(const char *subname, dir_entry * dentptr)
{

	int pos;
	int ndx = 0;
	memset(dentptr->name, ' ', 8);
	memset(dentptr->ext, ' ', 3);

	for (pos = 0; pos < 8; pos++) {
		if (subname[pos] == 0) {
			ndx = pos;
			break;
		}
		if (subname[pos] == '.') {

			ndx = pos + 1;
			break;
		}
		dentptr->name[pos] = subname[pos];
		TOUPPER(dentptr->name[pos]);
	}
	if (pos == 8) {
		while (subname[pos] && subname[pos] != '.')
			pos++;
		ndx = pos + 1;
	}

	for (pos = 0; pos < 3; pos++, ndx++) {
		if (subname[ndx] == 0) {
			break;
		}
		dentptr->ext[pos] = subname[ndx];
		TOUPPER(dentptr->ext[pos]);
	}
	return 0;
}

static int make_direntry(int isdir, __u32 curclust, const char *subname,
			 fsdata * mydata, dir_entry * dentptr,
			 unsigned long maxsize)
{
	dentptr->attr = 0;
	if (isdir) {
		dentptr->attr |= ATTR_DIR;
	}
	if (isdir == 2) {
		memcpy(dentptr->name, ".       ", 8);
		memcpy(dentptr->ext, "   ", 3);
	} else if (isdir == 3) {
		memcpy(dentptr->name, "..      ", 8);
		memcpy(dentptr->ext, "   ", 3);
	} else {
		put_direntry_name(subname, dentptr);
	}
	rtc_get(&rtc);
	dentptr->lcase = 0;
	dentptr->ctime_ms = GetFileTimeMS();
	dentptr->time = dentptr->ctime = CPU162FAT16(GetFileTime(&rtc));
	dentptr->cdate = dentptr->date = dentptr->adate =
	    CPU162FAT16(GetFileDate(&rtc));
	dentptr->starthi = CPU322FAT16(curclust);
	dentptr->start = CPU162FAT16(curclust);
	dentptr->size = maxsize;
	return 1;
}

static int make_first_direntry(__u32 parent_clustno, __u32 curclust,
			       fsdata * mydata)
{
	dir_entry *dentptr = (dir_entry *) get_parent_block;
	memset(get_parent_block, 0, sizeof(get_parent_block));
	if (make_direntry(2, curclust, ".", mydata, dentptr, MAX_CLUSTSIZE) ==
	    0) {
		return 0;
	}
	dentptr++;
	if (make_direntry
	    (3, parent_clustno, "..", mydata, dentptr, MAX_CLUSTSIZE) == 0) {
		return 0;
	}
	if (put_cluster
	    (mydata, curclust, (__u8 *) get_parent_block, MAX_CLUSTSIZE) != 0) {
		return 0;
	}
	return 1;
}

static int write_dirent(__u32 parent_clustno, int isdir, const char *subname,
			__u32 cursect, __u8 * read_block, fsdata * mydata,
			dir_entry * dentptr, unsigned long maxsize)
{
	__u32 curclust = 0;
	unsigned int clusters = 0;
	unsigned int bytesperclust = mydata->clust_size * SECTOR_SIZE_512;
	if (isdir == 0) {
		clusters = (maxsize + (bytesperclust - 1)) / bytesperclust;
	} else if (isdir == 1) {
		clusters = 1;
		maxsize = bytesperclust;
	} else {
		return 0;
	}
	//
	// allocate a cluster for new directory or clusters for file
	//
	if ((curclust = cluster_allocate(mydata, clusters)) == 0) {
		return 0;
	}
	if (isdir == 1) {
		if (make_first_direntry(parent_clustno, curclust, mydata) == 0) {
			return 0;
		}
	}
	if (make_direntry(isdir, curclust, subname, mydata, dentptr, maxsize) ==
	    0) {
		return 0;
	}
	//
	if (mydata->fatsize != 32) {
		if (parent_clustno == 0) {
			if (cursect >=
			    mydata->rootdir_sect + mydata->rootdir_size) {
				//outside root
				clusters = mydata->clust_size;
				//clusters = ( mydata->rootdir_size - ( cursect - mydata->rootdir_sect ) );
				//clusters = ( mydata->clust_size >= clusters   ) ? clusters : mydata->clust_size;
			} else {
				clusters =
				    (cursect - mydata->rootdir_sect) +
				    mydata->clust_size;
				if (clusters <= mydata->rootdir_size) {
					clusters = mydata->clust_size;
				} else {
					clusters =
					    mydata->clust_size - (clusters -
								  mydata->
								  rootdir_size);
					if (clusters < 0
					    || clusters >
					    mydata->clust_size - 1) {
						clusters = 0;
					}
				}
			}
		} else {
			clusters = mydata->clust_size;
		}
	} else {
		clusters = mydata->clust_size;
	}
	if (disk_write(cursect, clusters, read_block) < 0) {
		return 0;
	}
	return 1;
}

//dodir == 2 read
//dodir == 0 write/unlink
long prepare_filename(const char *filename, char *fnamecopy, char **subname,
		      int *ndx, int *dols, int *isdir, int dodir)
{
	/* "cwd" is always the root... */
	while (ISDIRDELIM(*filename))
		filename++;

	/* Make a copy of the filename and convert it to lowercase */
	strcpy(fnamecopy, filename);
	downcase(fnamecopy);

	if (*fnamecopy == '\0') {
		if (dodir == 2 && *dols != LS_NO) {
			*dols = LS_ROOT;
			return 0;
		}
		FAT_ERROR("Error: blank file name\n");
		return -1;
	} else if ((*ndx = dirdelim(fnamecopy)) >= 0) {
		*isdir = 1;
		fnamecopy[*ndx] = '\0';
		*subname = fnamecopy + *ndx + 1;

		/* Handle multiple delimiters */
		while (ISDIRDELIM(**subname))
			*subname++;
	} else {
		if (dodir == 0) {
			*subname = fnamecopy;
		} else {
			if (*dols != LS_NO) {
				*isdir = dodir;
			}
		}
	}
	return 0;
}

long do_fat_read(const char *filename, void *buffer, unsigned long maxsize,
		 int dols)
{

#if CONFIG_NIOS			/* NIOS CPU cannot access big automatic arrays */
	static
#endif
	static char fnamecopy[2048];
	char *subname = "";
	fsdata datablock;
	fsdata *mydata = &datablock;
	int files = 0;
	int dirs = 0;
#if defined(CONFIG_SUPPORT_VFAT)
	__u16 prevcksum = 0xffff;
#endif
	int isdir = 0;
	int ren_cursect = -1;
	__u32 ren_curclust = -1;
	__u32 curclust = 0;
	dir_entry *dentptr;
	dir_entry *tdentptr = 0;
	int firsttime;
	int rootdir_size;
	int cursect;
	int idx;
	int limit;
	int step;
	int maxstep;
	int sr;
	int lstflag;
	lstflag = (dols == LS_32 || dols == LS_64) ? 0 : 1;
	get_mydata(mydata, &rootdir_size);
	cursect = mydata->rootdir_sect;
	if (prepare_filename
	    (filename, fnamecopy, &subname, &idx, &dols, &isdir, 2) < 0) {
		return -1;
	}
	/* if isdir == 0 subname is a file      */
	/* if isdir == 1 subname is a directory */
	/* if isdir == 2 subname is a file to list */
	limit = (mydata->fatsize == 32) ? DIRENTSPERCLUST : DIRENTSPERBLOCK;
	maxstep = (mydata->fatsize == 32) ? 1 : rootdir_size;
	step = (mydata->fatsize == 32) ? mydata->clust_size : 1;
	for (sr = 0; sr < maxstep; sr++) {
		int i;
		if (mydata->fatsize != 32) {
			ren_cursect = cursect;
			if (disk_read(cursect, step, do_fat_read_block) < 0) {
				FAT_ERROR("Error reading rootdir block\n");
				return -1;
			}
		} else {
			if (curclust == 0) {
				ren_cursect = cursect;
				if (disk_read(cursect, step, do_fat_read_block)
				    < 0) {
					FAT_ERROR
					    ("Error reading rootdir block\n");
					return -1;
				}
				curclust = 2;
			} else {
				ren_curclust = curclust;
				if (get_cluster
				    (mydata, curclust, do_fat_read_block,
				     mydata->clust_size * SECTOR_SIZE_512) !=
				    0) {
					FAT_ERROR("Error reading cluster\n");
					return -1;
				}
			}
		}
		dentptr = (dir_entry *) do_fat_read_block;
		for (i = 0; i < limit; i++) {
			char s_name[14], l_name[256];
			l_name[0] = '\0';
			if (dentptr->name[0] == DELETED_FLAG) {
				dentptr++;
				continue;
			}
			if (dentptr->name[0] == 0) {
				FAT_DPRINT("RootDentname == NULL - %d\n", i);
				if (dols == LS_ROOT) {
					if (lstflag) {
						printf
						    ("\n%d file(s), %d dir(s)\n\n",
						     files, dirs);
						return 0;
					}
				}
				//at the last entry of the directory
				//if isdir == 0 file name not found in root directory
				//if isdir == 1 directory name not found in root directory
				FAT_ERROR("Error: file not found\n");
				return -1;
			}
#ifndef CONFIG_SUPPORT_VFAT
			if ((dentptr->attr & ATTR_VOLUME)) {
				dentptr++;
				continue;
			}
#else
			if ((dentptr->attr & ATTR_VOLUME)) {
				if (((dentptr->attr & ATTR_VFAT) == ATTR_VFAT)
				    && (dentptr->
					name[0] & LAST_LONG_ENTRY_MASK)) {
					dir_entry *retdent = dentptr;
					prevcksum =
					    ((dir_slot *) retdent)->
					    alias_checksum;
					get_vfatname(mydata, 0,
						     do_fat_read_block,
						     &retdent, l_name);
					if (dols == LS_ROOT) {
						int isdir =
						    (retdent->attr & ATTR_DIR);
						char dirc;
						int doit = 0;
						if (isdir) {
							dirs++;
							dirc = '/';
							doit = 1;
						} else {
							dirc = ' ';
							if (l_name[0] != 0) {
								files++;
								doit = 1;
							}
						}
						if (doit) {
							if (lstflag) {
								if (dirc == ' ') {
									printf
									    (" %8ld   %s%c\n",
									     (long)
									     FAT2CPU32
									     (retdent->
									      size),
									     l_name,
									     dirc);
								} else {
									printf
									    ("            %s%c\n",
									     l_name,
									     dirc);
								}
							}
						}
						dentptr++;
						continue;
					}
					FAT_DPRINT("Rootvfatname: |%s|\n",
						   l_name);
				} else {
					/* Volume label or VFAT entry */
					dentptr++;
					continue;
				}
			}
			if (dols == LS_ROOT
			    && mkcksum(dentptr->name) == prevcksum) {
				dentptr++;
				continue;
			}
#endif //#ifndef CONFIG_SUPPORT_VFAT
			get_name(dentptr, s_name);
			if (dols == LS_ROOT) {
				int isdir = (dentptr->attr & ATTR_DIR);
				char dirc;
				int doit = 0;
				if (isdir) {
					dirc = '/';
					if (s_name[0] != 0) {
						dirs++;
						doit = 1;
					}
				} else {
					dirc = ' ';
					if (s_name[0] != 0) {
						files++;
						doit = 1;
					}
				}
				if (doit) {
					if (lstflag) {
						if (dirc == ' ') {
							printf(" %8ld   %s%c\n",
							       (long)
							       FAT2CPU32
							       (dentptr->size),
							       s_name, dirc);
						} else {
							printf
							    ("            %s%c\n",
							     s_name, dirc);
						}
					}
				}
				dentptr++;
				continue;
			}
#ifndef CONFIG_SUPPORT_VFAT
			if (strcmp(fnamecopy, s_name)) {
				FAT_DPRINT("RootMismatch: |%s|\n", s_name);
				dentptr++;
				continue;
			}
#else
			if (strcmp(fnamecopy, s_name)
			    && strcmp(fnamecopy, l_name)) {
				FAT_DPRINT("RootMismatch: |%s|%s|\n", s_name,
					   l_name);
				dentptr++;
				continue;
			}
#endif
			if (!isdir && (dentptr->attr & ATTR_DIR)) {
				FAT_DPRINT
				    ("Error looking for file name but found a directory name\n");
				return -1;
			}
			if (dols == LS_NO || dols == LS_08) {
				if (isdir && !(dentptr->attr & ATTR_DIR)) {
					FAT_DPRINT
					    ("Error looking for directory name but found a file name\n");
					return -1;
				}
			}
			FAT_DPRINT("RootName: %s", s_name);
			FAT_DPRINT(", start: 0x%x", START(dentptr));
			FAT_DPRINT(", size:  0x%x %s\n",
				   FAT2CPU32(dentptr->size),
				   isdir ? "(DIR)" : "");
			// if isdir == 0 found file name in root directory
			// if isdir == 1 found directory name in root directory
			if (isdir == 2 && dols != LS_NO && !(idx >= 0)) {
				if (dentptr->attr & ATTR_DIR) {
					isdir = 3;
				} else {
					if (dols == LS_32) {
						put_direntry_name(buffer,
								  dentptr);

						if (ren_curclust == -1
						    && ren_cursect) {
							if (disk_write
							    (ren_cursect, step,
							     do_fat_read_block)
							    < 0) {
								FAT_ERROR
								    ("Error writing rootdir block\n");
								return -1;
							}
						} else {
							if (put_cluster
							    (mydata,
							     ren_curclust,
							     do_fat_read_block,
							     mydata->
							     clust_size *
							     SECTOR_SIZE_512) !=
							    0) {
								FAT_ERROR
								    ("Error writing cluster\n");
								return -1;
							}
						}
						return 0;
					} else {
						if (lstflag) {
							printf(" %8ld   %s%c\n",
							       (long)
							       FAT2CPU32
							       (dentptr->size),
							       s_name, ' ');
						}
						return 0;
					}
				}
			} else {
				if (isdir == 1 && (dols != LS_NO)
				    && *subname == 0) {
					isdir = 3;
				}
			}
			goto rootdir_done;
			/* We got a match */
		}
		if (mydata->fatsize == 32) {
			__u32 newclust = get_fatent(mydata, curclust);
			if (newclust == 0) {
				FAT_ERROR("7 FAT entry read error\n");
				return -1;
			}
			if (CHECK_CLUST(newclust, mydata->fatsize)) {
				FAT_DPRINT("8 curclust: 0x%x\n", newclust);
				FAT_DPRINT("Last FAT entry\n");
				if (dols == LS_ROOT) {
					return 0;
				}
				return -1;
			} else {
				curclust = newclust;
				maxstep++;
			}
		} else {
			cursect += step;
		}
	}
	//sr==maxstep
	//in FAT 12/16 searched all sectors  in root directory sector  by sector    nothing found
	//in FAT 32    searched all clusters in root chain     cluster by cluster   nothing found
	if (dols == LS_ROOT) {
		return 0;
	}
	FAT_ERROR("Name not found\n");
	return -1;
      rootdir_done:
	firsttime = 1;
	while (isdir) {
		__u32 parent_clustno = START(dentptr);
		dir_entry dent;
		int error;
		int startsect;
		char *nextname = NULL;
		startsect = FS_CLUSTER_TO_SECTOR(mydata, parent_clustno);
		dent = *dentptr;
		dentptr = &dent;
		if (isdir == 3) {
			isdir = 0;
		} else {
			idx = dirdelim(subname);
			if (idx >= 0) {
				subname[idx] = '\0';
				nextname = subname + idx + 1;
				/* Handle multiple delimiters */
				while (ISDIRDELIM(*nextname))
					nextname++;
				if ((dols != LS_NO) && *nextname == '\0') {
					firsttime = 0;
				}
			} else {
				if ((dols != LS_NO && dols != LS_32)
				    && firsttime) {
					firsttime = 0;
				} else {
					isdir = 0;
				}
			}
		}
		error = 0;
		last_dir_cluster = -1;
		if ((tdentptr =
		     get_dentfromdir(mydata, subname, dentptr,
				     isdir ? LS_NO : dols, &error)) == NULL) {
			//at the end of the directory or ERROR
			if (!isdir && (dols != LS_NO && dols != LS_32)) {
				return 0;
			}
			FAT_DPRINT("Name not found in directory\n");
			//if (error==0)
			//if isdir == 0 file name not found in non-root directory
			//if isdir == 1 directory name not found in non-root directory
			return -1;
		}
		if (idx >= 0) {
			if (!(dentptr->attr & ATTR_DIR)) {
				FAT_DPRINT
				    ("Error looking for directory name but found a file\n");
				return -1;
			}
			subname = nextname;
		}
	}
	if (firsttime == 1) {
		if (dols == LS_32) {
			put_direntry_name(buffer, tdentptr);
			if (last_dir_cluster != -1) {
				if (put_cluster
				    (mydata, last_dir_cluster, last_dir_buffer,
				     mydata->clust_size * SECTOR_SIZE_512) !=
				    0) {
					FAT_ERROR("Error writing cluster\n");
					return -1;
				}
			}
			return 0;
		} else if (dols == LS_NO) {
			long ret =
			    get_contents(mydata, dentptr, buffer, maxsize);
			FAT_DPRINT("Size: %d, got: %ld\n",
				   FAT2CPU32(dentptr->size), ret);
			return ret;
		}
	}
	return 0;
}

long do_fat_write(const char *filename, void *buffer, unsigned long maxsize)
{

#if CONFIG_NIOS			/* NIOS CPU cannot access big automatic arrays */
	static
#endif
	static char fnamecopy[2048];
	fsdata datablock;
	fsdata *mydata = &datablock;
	char *subname = "";
	dir_entry *dentptr = 0;
	dir_entry *tdentptr = 0;
#if defined(CONFIG_SUPPORT_VFAT)
	__u16 prevcksum = 0xffff;
#endif
	int isdir = 0;
	long ret = 0;
	int rootdir_size;
	int idx;
	int sr;
	__u32 clno = 0;
	int cursect;
	int limit;
	int step;
	int maxstep;
	__u32 curclust = 0;
	__u32 prvclust = 0;
	int dols = LS_NO;
	int ftt = 1;
	get_mydata(mydata, &rootdir_size);
	if (prepare_filename
	    (filename, fnamecopy, &subname, &idx, &dols, &isdir, 0) < 0) {
		return -1;
	}
	/* if isdir == 1 subname is a directory */
      find_dele:
	cursect = mydata->rootdir_sect;
	sr = rootdir_size % mydata->clust_size;
	limit =
	    (mydata->fatsize == 32) ? DIRENTSPERCLUST : (sr ==
							 0) ? DIRENTSPERCLUST :
	    DIRENTSPERBLOCK;
	maxstep =
	    (mydata->fatsize == 32) ? 1 : (sr ==
					   0) ? rootdir_size /
	    mydata->clust_size : rootdir_size;
	step =
	    (mydata->fatsize == 32) ? mydata->clust_size : (sr ==
							    0) ? mydata->
	    clust_size : 1;
	for (sr = 0; sr < maxstep; sr++) {
		int i;
		int deletes = 0;
		if (mydata->fatsize != 32) {
			if (disk_read(cursect, step, do_fat_read_block) < 0) {
				FAT_ERROR("Error reading rootdir block\n");
				return -1;
			}
			last_dir_cluster = 0;
			last_dir_buffer = do_fat_read_block;
		} else {
			if (curclust == 0) {
				if (disk_read(cursect, step, do_fat_read_block)
				    < 0) {
					FAT_ERROR
					    ("Error reading rootdir block\n");
					return -1;
				}
				curclust = 2;
				last_dir_cluster = curclust;
				last_dir_buffer = do_fat_read_block;
			} else {
				last_dir_cluster = curclust;
				last_dir_buffer = do_fat_read_block;
				if (get_cluster
				    (mydata, curclust, do_fat_read_block,
				     mydata->clust_size * SECTOR_SIZE_512) !=
				    0) {
					FAT_ERROR("Error reading cluster\n");
					return -1;
				}
			}
		}
		dentptr = (dir_entry *) do_fat_read_block;
		for (i = 0; i < limit; i++) {
			char s_name[14], l_name[256];
			l_name[0] = '\0';
			/*
			   if (dentptr->name[0] == DELETED_FLAG)
			   {
			   if ( ftt )
			   {
			   deletes++;
			   dentptr++;
			   continue;
			   }
			   else
			   {
			   if (mydata->fatsize != 32)
			   {
			   last_dir_cluster = curclust;
			   last_dir_buffer  = do_fat_read_block;
			   if ( write_dirent(curclust,isdir,(isdir)? fnamecopy: subname,cursect,do_fat_read_block,mydata,dentptr,maxsize) == 0 )
			   {
			   FAT_ERROR("Error writing directory entry\n");
			   return -1;
			   }
			   goto rootdir_done;
			   }
			   goto shouldnthappen;
			   }
			   }
			 */
			if ((dentptr->name[0] == 0)
			    || (dentptr->name[0] == DELETED_FLAG)) {
				FAT_DPRINT("RootDentname == NULL - %d\n", i);
				//at the end of the directory
				//if isdir == 0 file name not found in root directory
				//if isdir == 1 directory name not found in root directory
				//TODO :: need to reuse deleted entry if one was found in root directory
				//however when directory fills up ..... maybe TODONE
				//fill in direntry allocate cluster and write back dir
				if (mydata->fatsize != 32) {
					//        cursect = FS_CLUSTER_TO_SECTOR(mydata,curclust);
					last_dir_cluster = curclust;
					last_dir_buffer = do_fat_read_block;
					if (write_dirent(curclust,
							 isdir,
							 (isdir) ? fnamecopy :
							 subname, cursect,
							 do_fat_read_block,
							 mydata, dentptr,
							 maxsize) == 0) {
						FAT_ERROR
						    ("Error writing directory entry\n");
						return -1;
					}
				} else {
					prvclust = curclust;
					cursect =
					    FS_CLUSTER_TO_SECTOR(mydata,
								 curclust);
					last_dir_cluster = curclust;
					last_dir_buffer = do_fat_read_block;
					if (write_dirent(curclust,
							 isdir,
							 (isdir) ? fnamecopy :
							 subname, cursect,
							 do_fat_read_block,
							 mydata, dentptr,
							 maxsize) == 0) {
						FAT_ERROR
						    ("Error writing directory entry\n");
						return -1;
					}
				}
				goto rootdir_done;
			}
#ifndef CONFIG_SUPPORT_VFAT
			if ((dentptr->attr & ATTR_VOLUME)) {
				dentptr++;
				continue;
			}
#else
			if ((dentptr->attr & ATTR_VOLUME)) {
				if (((dentptr->attr & ATTR_VFAT) == ATTR_VFAT)
				    && (dentptr->
					name[0] & LAST_LONG_ENTRY_MASK)) {
					dir_entry *retdent = dentptr;
					prevcksum =
					    ((dir_slot *) retdent)->
					    alias_checksum;
					get_vfatname(mydata, 0,
						     do_fat_read_block,
						     &retdent, l_name);
					FAT_DPRINT("Rootvfatname: |%s|\n",
						   l_name);
				} else {
					/* Volume label or VFAT entry */
					dentptr++;
					continue;
				}
			}
#endif //#ifndef CONFIG_SUPPORT_VFAT
			get_name(dentptr, s_name);
#ifndef CONFIG_SUPPORT_VFAT
			if (strcmp(fnamecopy, s_name)) {
				FAT_DPRINT("RootMismatch: |%s|\n", s_name);
				dentptr++;
				continue;
			}
#else
			if (strcmp(fnamecopy, s_name)
			    && strcmp(fnamecopy, l_name)) {
				FAT_DPRINT("RootMismatch: |%s|%s|\n", s_name,
					   l_name);
				dentptr++;
				continue;
			}
#endif
			if (isdir && !(dentptr->attr & ATTR_DIR)) {
				FAT_ERROR
				    ("Looking for directoy name but found a file name\n");
				return -1;
			}
			if (!isdir && (dentptr->attr & ATTR_DIR)) {
				FAT_ERROR
				    ("Looking for file name but found a directory name\n");
				return -1;
			}
			FAT_DPRINT("RootName: %s", s_name);
			FAT_DPRINT(", start: 0x%x", START(dentptr));
			FAT_DPRINT(", size:  0x%x %s\n",
				   FAT2CPU32(dentptr->size),
				   isdir ? "(DIR)" : "");
			// if isdir == 0 found file name in root directory
			// if isdir == 1 found directory name in root directory
			if (isdir == 0) {
				clno = START(dentptr);
				tdentptr = dentptr;
			}
			goto rootdir_done;
			/* We got a match */
		}
		//end for
		if (deletes == limit) {
			deletes = limit;
		}
		/////////////////////////////////
		if (mydata->fatsize == 32) {
			int startsect;
			__u32 clustno;
			__u32 parent_clustno = 2;
			__u32 newclust = get_fatent(mydata, curclust);
			if (newclust == 0) {
				FAT_ERROR("8 FAT entry read error\n");
				FAT_ERROR("No more disk space or error\n");
				return -1;
			}
			if (CHECK_CLUST(newclust, mydata->fatsize)) {
				int error = 0;
				int append = 0;
				dir_entry *enddent = 0;
				startsect =
				    FS_CLUSTER_TO_SECTOR(mydata, curclust);
				//at the end of the directory
				//expand directory , link and redo  unless a deleted entry exist
				memmove(get_dentfromdir_block,
					do_fat_read_block,
					sizeof(get_dentfromdir_block));
				dentptr = (dir_entry *) get_dentfromdir_block;
				//since FAT32 uses a cluster oriented root check_dent is okay
				clustno =
				    check_dent(mydata, &error, curclust,
					       &enddent);
				// printf("check_dent %08X\n",clustno);
				if (error != 0) {
					FAT_ERROR
					    ("Error finding directory entry\n");
					return -1;
				}
				if (clustno == 0) {
					//expand directory
					append = 1;
					if ((clustno =
					     cluster_allocate_link(mydata,
								   curclust)) ==
					    0) {
						FAT_ERROR
						    ("Error during cluster_allocate_link\n");
						return -1;
					}
					memset(do_fat_read_block, 0,
					       sizeof(do_fat_read_block));
					startsect =
					    FS_CLUSTER_TO_SECTOR(mydata,
								 clustno);
					dentptr =
					    (dir_entry *) do_fat_read_block;
					parent_clustno = 2;
					last_dir_cluster = clustno;
					last_dir_buffer = do_fat_read_block;
					if (write_dirent(parent_clustno,
							 isdir,
							 (isdir) ? fnamecopy :
							 subname, startsect,
							 do_fat_read_block,
							 mydata, dentptr,
							 maxsize) == 0) {
						FAT_ERROR
						    ("Error (1) writing directory\n");
						return -1;
					}
				} else {
					dentptr = enddent;
					last_dir_cluster = curclust;
					last_dir_buffer = get_dentfromdir_block;
					if (write_dirent(parent_clustno,
							 isdir,
							 (isdir) ? fnamecopy :
							 subname, startsect,
							 get_dentfromdir_block,
							 mydata, dentptr,
							 maxsize) == 0) {
						FAT_ERROR
						    ("Error (1) writing directory\n");
						return -1;
					}
				}
				if (append) {
					//TODO :: update size in parent of this dir entry by mydata->clust_size
				}
				//
			} else {
				//ready fetch next cluster of root FAT32
				curclust = newclust;
				maxstep++;
			}
		} else		//(mydata->fatsize != 32)
		{
			//ready fetch next sector of root !FAT32
			cursect += step;
		}
	}
	if (ftt && (mydata->fatsize != 32)) {
		ftt = 0;
		goto find_dele;
	}
	if (ftt == 0 && (mydata->fatsize != 32)) {
		FAT_ERROR("Root directory full\n");
		return -1;
	}
	goto rootdir_done;
	//shouldnthappen:
	FAT_ERROR("Root directory full\n");
	return -1;
      rootdir_done:
	while (isdir) {
		__u32 parent_clustno = START(dentptr);
		dir_entry dent;
		int error;
		int startsect;
		char *nextname = NULL;
		startsect = FS_CLUSTER_TO_SECTOR(mydata, parent_clustno);
		dent = *dentptr;
		dentptr = &dent;
		idx = dirdelim(subname);
		if (idx >= 0) {
			subname[idx] = '\0';
			nextname = subname + idx + 1;
			/* Handle multiple delimiters */
			while (ISDIRDELIM(*nextname))
				nextname++;
		} else {
			isdir = 0;
		}
		error = 0;
		tdentptr =
		    get_dentfromdir(mydata, subname, dentptr, LS_08, &error);
		if (tdentptr == NULL || error == LS_08) {
			int append = 0;
			if (error == 2) {
				//at the end of the directory
				//expand directory , link and redo  unless a deleted entry exist
				dir_entry *enddent = 0;
				__u32 clustno =
				    check_dent(mydata, &error, START(dentptr),
					       &enddent);
				if (error != 0) {
					FAT_ERROR
					    ("Error finding directory entry\n");
					return -1;
				}
				if (clustno == 0) {
					//expand directory
					append = 1;
					clustno = START(dentptr);
					if ((clustno =
					     cluster_allocate_link(mydata,
								   clustno)) ==
					    0) {
						FAT_ERROR
						    ("Error during cluster_allocate_link\n");
						return -1;
					}
					memset(get_dentfromdir_block, 0,
					       sizeof(get_dentfromdir_block));
					startsect =
					    FS_CLUSTER_TO_SECTOR(mydata,
								 clustno);
					dentptr =
					    (dir_entry *) get_dentfromdir_block;
				}
			} else if (error == LS_08) {
				__u32 clustno;
				clustno = START(dentptr);
				dentptr = tdentptr;
				tdentptr = NULL;
				startsect =
				    FS_CLUSTER_TO_SECTOR(mydata, clustno);
			} else if (error == 1) {
				FAT_ERROR("Error (1) reading directory\n");
				return -1;
			}
			if (write_dirent
			    (parent_clustno, isdir, subname, startsect,
			     get_dentfromdir_block, mydata, dentptr,
			     maxsize) == 0) {
				FAT_ERROR("Error (2) writing directory\n");
				return -1;
			}
			if (append) {
				//TODO :: update size in parent of this dir entry by mydata->clust_size
			}
		}
		if (idx >= 0) {
			if (!(dentptr->attr & ATTR_DIR)) {
				FAT_ERROR
				    ("Error looking for directory name but found a file\n");
				return -1;
			}
			subname = nextname;
		}
		if ((isdir == 0) && tdentptr && (error == 0)) {
			clno = START(tdentptr);
		}
		if ((isdir == 1) && tdentptr) {
			dentptr = tdentptr;
		}
	}
	if (clno) {
		//no overwrite of current contents
		unsigned int bytesperclust =
		    (mydata->clust_size * SECTOR_SIZE_512);
		unsigned int clusttoalloc =
		    ((maxsize + (bytesperclust - 1)) / (bytesperclust));
		__u32 curclust = cluster_allocate(mydata, clusttoalloc);
		if (curclust == 0) {
			FAT_ERROR("Error during cluster_allocate\n");
			return -1;
		}
		{
			int wcursect = 0;
			if (mydata->fatsize != 32) {
/*
	rtc_get( &rtc );
	tdentptr->lcase = 0;
	tdentptr->ctime_ms = GetFileTimeMS();
	tdentptr->time = CPU162FAT16(GetFileTime(&rtc));
	tdentptr->date = CPU162FAT16(GetFileDate(&rtc));
*/	
	tdentptr->size = maxsize;

				tdentptr->starthi = 0;
				tdentptr->start = CPU162FAT16(curclust);
				if (last_dir_cluster == 0) {
					wcursect = cursect;
					if (last_dir_buffer == 0)
						last_dir_buffer =
						    do_fat_read_block;
					if (disk_write
					    (wcursect, 1,
					     last_dir_buffer) < 0) {
						FAT_ERROR
						    ("Error writing sector\n");
						return -1;
					}
				} else {
					wcursect =
					    FS_CLUSTER_TO_SECTOR(mydata,
								 last_dir_cluster);
					if (disk_write
					    (wcursect, mydata->clust_size,
					     last_dir_buffer) < 0) {
						FAT_ERROR
						    ("Error writing cluster\n");
						return -1;
					}
				}
			} else {

/*	rtc_get( &rtc );
	tdentptr->lcase = 0;
	tdentptr->ctime_ms = GetFileTimeMS();
	tdentptr->time = CPU162FAT16(GetFileTime(&rtc));
	tdentptr->date = CPU162FAT16(GetFileDate(&rtc));
*/
        
				tdentptr->size = maxsize;
        			tdentptr->starthi = CPU322FAT16(curclust);
				tdentptr->start = CPU162FAT16(curclust);
				wcursect =
				    FS_CLUSTER_TO_SECTOR(mydata,
							 last_dir_cluster);
				if (disk_write
				    (wcursect, mydata->clust_size,
				     last_dir_buffer) < 0) {
					FAT_ERROR("Error writing cluster\n");
					return -1;
				}
			}
		}
		ret = put_contents(mydata, tdentptr, buffer, maxsize);
		FAT_DPRINT("Size: %d, got: %ld\n", FAT2CPU32(dentptr->size),
			   ret);
		delink(clno, mydata);
		return ret;
	}
	ret = put_contents(mydata, dentptr, buffer, maxsize);
	FAT_DPRINT("Size: %d, got: %ld\n", FAT2CPU32(dentptr->size), ret);
	return ret;
}

int deltree(fsdata * mydata, __u32 sclust)
{
	//last_dir_cluster = curclust;
	//last_dir_buffer  = do_fat_read_block;
	__u32 cclust = sclust;
	__u32 nc;
	char *rbuffer = malloc(MAX_CLUSTSIZE);	//8k at a crack
	if (rbuffer == 0) {
		FAT_ERROR("Error allocating fat buffer\n");
		return -1;
	}
	do {
		nc = get_fatent(mydata, cclust);
		if (nc == INVALIDCLUSTERENTRY) {
			free(rbuffer);
			FAT_ERROR("Error reading fat\n");
			return -1;
		} else {
			dir_entry *dep = (dir_entry *) rbuffer;

			if (get_cluster
			    (mydata, cclust, rbuffer,
			     mydata->clust_size * SECTOR_SIZE_512) == 0) {
				int ndx;

				for (ndx = 0;
				     ndx < (MAX_CLUSTSIZE / sizeof(dir_entry));
				     ndx++) {
					if (dep->name[0] == 0)
						break;

					if (dep->attr & ATTR_VOLUME) {
						dep++;
						continue;
					}
					if (dep->name[0] == DELETED_FLAG) {
						dep++;
						continue;
					}
					if (dep->attr & ATTR_DIR) {

						if (!((dep->name[0] == '.')
						      && ((dep->name[1] == ' ')
							  || (dep->name[1] ==
							      '.'
							      && dep->name[2] ==
							      ' ')))) {
							__u32 xclust =
							    START(dep);
							if (deltree
							    (mydata,
							     xclust) < 0) {
								free(rbuffer);
								FAT_ERROR
								    ("Error geting cluster\n");
								return -1;
							}
							delink(xclust, mydata);
						}
						dep++;
						continue;
					} else {
						//regular file
						delink(START(dep), mydata);
						dep++;
						continue;
					}
				}
			} else {
				free(rbuffer);
				FAT_ERROR("Error geting cluster\n");
				return -1;
			}
		}
		cclust = nc;
	}
	while (!(CHECK_CLUST(nc, mydata->fatsize) ? 1 : 0));
	free(rbuffer);

	return 0;
}

int do_fat_unlink(const char *filename)
{
#if CONFIG_NIOS			/* NIOS CPU cannot access big automatic arrays */
	static
#endif
	static char fnamecopy[2048];
	fsdata datablock;
	fsdata *mydata = &datablock;
	char *subname = "";
	dir_entry *dentptr = 0;
	dir_entry *tdentptr = 0;
#if defined(CONFIG_SUPPORT_VFAT)
	__u16 prevcksum = 0xffff;
#endif
	int isdir = 0;
	long ret = 0;
	int rootdir_size;
	int cursect;
	int idx;
	__u32 clno = 0;
	int limit;
	int step;
	int maxstep;
	int sr;
	__u32 curclust = 0;
	int dols = LS_NO;

	get_mydata(mydata, &rootdir_size);
	cursect = mydata->rootdir_sect;

	if (prepare_filename
	    (filename, fnamecopy, &subname, &idx, &dols, &isdir, 0) < 0) {
		return -1;
	}

	/* if isdir == 0 subname is a file */
	/* if isdir == 1 subname is a directory */

	limit = (mydata->fatsize == 32) ? DIRENTSPERCLUST : DIRENTSPERBLOCK;
	maxstep = (mydata->fatsize == 32) ? 1 : rootdir_size;
	step = (mydata->fatsize == 32) ? mydata->clust_size : 1;

	for (sr = 0; sr < maxstep; sr++) {
		int i;
		if (mydata->fatsize != 32) {
			if (disk_read(cursect, step, do_fat_read_block) < 0) {
				FAT_ERROR("Error reading rootdir block\n");
				return -1;
			}
		} else {
			if (curclust == 0) {
				if (disk_read(cursect, step, do_fat_read_block)
				    < 0) {
					FAT_ERROR
					    ("Error reading rootdir block\n");
					return -1;
				}
				curclust = 2;
			} else {
				last_dir_cluster = curclust;
				last_dir_buffer = do_fat_read_block;
				if (get_cluster
				    (mydata, curclust, do_fat_read_block,
				     mydata->clust_size * SECTOR_SIZE_512) !=
				    0) {
					FAT_ERROR("Error reading cluster\n");
					return -1;
				}

			}
		}

		dentptr = (dir_entry *) do_fat_read_block;
		for (i = 0; i < limit; i++) {
			char s_name[14], l_name[256];

			l_name[0] = '\0';

			if (dentptr->name[0] == DELETED_FLAG) {
				dentptr++;
				continue;
			}
			if (dentptr->name[0] == 0) {
				FAT_ERROR("Name not found\n");
				return -1;
			}
#ifndef CONFIG_SUPPORT_VFAT
			if ((dentptr->attr & ATTR_VOLUME)) {
				dentptr++;
				continue;
			}
#else
			if ((dentptr->attr & ATTR_VOLUME)) {
				if (((dentptr->attr & ATTR_VFAT) == ATTR_VFAT)
				    && (dentptr->
					name[0] & LAST_LONG_ENTRY_MASK)) {
					dir_entry *retdent = dentptr;

					prevcksum =
					    ((dir_slot *) retdent)->
					    alias_checksum;
					get_vfatname(mydata, 0,
						     do_fat_read_block,
						     &retdent, l_name);

					FAT_DPRINT("Rootvfatname: |%s|\n",
						   l_name);
				} else {
					/* Volume label or VFAT entry */
					dentptr++;
					continue;
				}
			}
#endif //#ifndef CONFIG_SUPPORT_VFAT
			get_name(dentptr, s_name);

#ifndef CONFIG_SUPPORT_VFAT
			if (strcmp(fnamecopy, s_name)) {
				FAT_DPRINT("RootMismatch: |%s|\n", s_name);
				dentptr++;
				continue;
			}
#else
			if (strcmp(fnamecopy, s_name)
			    && strcmp(fnamecopy, l_name)) {
				FAT_DPRINT("RootMismatch: |%s|%s|\n", s_name,
					   l_name);
				dentptr++;
				continue;
			}
#endif
			if (isdir && !(dentptr->attr & ATTR_DIR)) {
				FAT_ERROR
				    ("Looking for directoy name but found a file name\n");
				return -1;
			}
			if (!isdir && (dentptr->attr & ATTR_DIR)) {
				FAT_ERROR
				    ("Looking for file name but found a directory name\n");
				return -1;
			}

			FAT_DPRINT("RootName: %s", s_name);
			FAT_DPRINT(", start: 0x%x", START(dentptr));
			FAT_DPRINT(", size:  0x%x %s\n",
				   FAT2CPU32(dentptr->size),
				   isdir ? "(DIR)" : "");

			// if isdir == 0 found file name in root directory
			// if isdir == 1 found directory name in root directory
			if ((isdir == 0) || ((isdir == 1) && (*subname == 0))) {
				clno = START(dentptr);	//
				if (isdir == 1) {
					deltree(mydata, clno);
				}
				//      TODO should we eradicate the entry or save for undelete? hmmmm... eradicate!
				if (mydata->fatsize == 32) {
					dentptr->start =
					    CPU322FAT16(INVALIDCLUSTERENTRY);
					dentptr->starthi =
					    CPU162FAT16(INVALIDCLUSTERENTRY);
					cursect =
					    FS_CLUSTER_TO_SECTOR(mydata,
								 curclust);
					dentptr->name[0] = DELETED_FLAG;
					if (disk_write
					    (cursect, step,
					     do_fat_read_block) < 0) {
						FAT_ERROR
						    ("Error: disk write error\n");
						return -1;
					}
					delink(clno, mydata);
					return 0;
				} else {
					dentptr->start =
					    CPU322FAT16(INVALIDCLUSTERENTRY);
					dentptr->starthi = 0;
					cursect = cursect;
					dentptr->name[0] = DELETED_FLAG;
					if (disk_write
					    (cursect, step,
					     do_fat_read_block) < 0) {
						FAT_ERROR
						    ("Error: disk write error\n");
						return -1;
					}
					delink(clno, mydata);
					return 0;
				}

			}
			goto rootdir_done;
			/* We got a match */
		}

		if (mydata->fatsize == 32) {
			__u32 newclust = get_fatent(mydata, curclust);
			if (newclust == 0) {
				FAT_ERROR("8 FAT entry read error\n");
				return -1;
			}

			if (CHECK_CLUST(newclust, mydata->fatsize)) {
				FAT_ERROR("Name not found\n");
				return -1;
			} else {
				curclust = newclust;
				maxstep++;
			}
		} else {
			cursect += step;
		}
	}
	FAT_ERROR("Name not found\n");
	return -1;

      rootdir_done:

	while (isdir) {
		__u32 parent_clustno = START(dentptr);
		dir_entry dent;
		int error;
		int startsect;
		char *nextname = NULL;

		startsect = FS_CLUSTER_TO_SECTOR(mydata, parent_clustno);

		dent = *dentptr;
		dentptr = &dent;

		idx = dirdelim(subname);
		if (idx >= 0) {
			subname[idx] = '\0';
			nextname = subname + idx + 1;

			/* Handle multiple delimiters */
			while (ISDIRDELIM(*nextname))
				nextname++;
		} else {
			isdir = 0;
		}

		error = 0;
		tdentptr =
		    get_dentfromdir(mydata, subname, dentptr, LS_NO, &error);
		if (tdentptr == NULL) {
			if (error == 1) {
				FAT_ERROR("Error (1) reading directory\n");
			} else {
				//name not found in non-root directory
				FAT_ERROR("Name not found\n");
			}
			return -1;
		}

		if (idx >= 0) {
			if (!(dentptr->attr & ATTR_DIR)) {
				FAT_ERROR
				    ("Error looking for directory name but found a file\n");
				return -1;
			}
			subname = nextname;
		}
		if (((isdir == 0) && tdentptr && (error == 0))
		    || ((isdir == 1) && tdentptr && (error == 0)
			&& (*nextname == 0))) {
			clno = START(tdentptr);
			if (isdir == 1) {
				deltree(mydata, clno);
			}
			//    TODO should we eradicate the entry or save for undelete? hmmmm... eradicate!
			if (mydata->fatsize == 32) {
				tdentptr->name[0] = DELETED_FLAG;
				tdentptr->start =
				    CPU322FAT16(INVALIDCLUSTERENTRY);
				tdentptr->starthi =
				    CPU162FAT16(INVALIDCLUSTERENTRY);
				cursect =
				    FS_CLUSTER_TO_SECTOR(mydata,
							 parent_clustno);
			} else {
				tdentptr->name[0] = DELETED_FLAG;
				tdentptr->start =
				    CPU322FAT16(INVALIDCLUSTERENTRY);
				tdentptr->starthi = 0;
				cursect =
				    FS_CLUSTER_TO_SECTOR(mydata,
							 parent_clustno);
			}

			if (disk_write(cursect, step, get_dentfromdir_block) <
			    0) {
				FAT_ERROR("Error: disk write error\n");
				return -1;
			}
			delink(clno, mydata);
			if (isdir == 1) {

			}
			return 0;
		}
		if ((isdir == 1) && tdentptr) {
			dentptr = tdentptr;
		}
	}
	if (clno) {
		//
	}
	return ret;
}

int do_fat_rename(const char *oldfilename, const char *newfilename)
{

	long res = do_fat_read(oldfilename, (char *)newfilename, 0, LS_32);

	return res;
}

int get_mydata(fsdata * mydata, int *rootdir_size)
{
	boot_sector bs;
	volume_info volinfo;
	if (read_bootsectandvi(&bs, &volinfo, &mydata->fatsize)) {
		FAT_ERROR("Error reading boot sector\n");
		return -1;
	}
	if (mydata->fatsize == 32) {
		mydata->fatlength = bs.fat32_length;
	} else {
		mydata->fatlength = bs.fat_length;
	}
	mydata->fat_dirty = 0;
	mydata->fat_sect = bs.reserved;
	mydata->rootdir_sect = mydata->fat_sect + mydata->fatlength * bs.fats;
	mydata->clust_size = bs.cluster_size;
	if (mydata->fatsize == 32) {
		mydata->rootdir_size = mydata->clust_size;
		*rootdir_size = mydata->rootdir_size;
		mydata->data_begin =
		    mydata->rootdir_sect /* + rootdir_size */  -
		    (mydata->clust_size * 2);
	} else {
		mydata->rootdir_size =
		    (int)((bs.dir_entries[1] * (int)256 +
			   bs.dir_entries[0]) * sizeof(dir_entry)) /
		    SECTOR_SIZE_512;
		*rootdir_size = mydata->rootdir_size;
		mydata->data_begin =
		    mydata->rootdir_sect + *rootdir_size -
		    (mydata->clust_size * 2);
	}
	mydata->fats = bs.fats;
	mydata->fatbufnum = -1;
	switch (mydata->fatsize) {
	case 32:
		mydata->fatlast = ((mydata->fatlength) * (SECTOR_SIZE_512 / 4));
		break;
	case 16:
		mydata->fatlast = (mydata->fatlength) * (SECTOR_SIZE_512 / 2);
		break;
	case 12:
		mydata->fatlast =
		    ((mydata->fatlength * SECTOR_SIZE_512) * 2) / 3;
		break;
	default:
		FAT_ERROR("Unsupported FAT size\n");
		return -1;
	}
	mydata->fat_in_use = 0;
	memset(mydata->fat_broken, 0, sizeof(mydata->fat_broken));

	FAT_DPRINT("FAT%d, fatlength: %d\n", mydata->fatsize,
		   mydata->fatlength);
	FAT_DPRINT("Rootdir begins at sector: %d, offset: %x, size: %d\n"
		   "Data begins at: %08x\n", mydata->rootdir_sect,
		   mydata->rootdir_sect * SECTOR_SIZE_512, *rootdir_size,
		   mydata->data_begin);
	FAT_DPRINT("Cluster size: %d\n", mydata->clust_size);

	return 0;
}

int get_file_fat_boot_sector(boot_sector * bs, volume_info * volinfo,
			     int *fatsize)
{
	if (disk_read(0, 1, (__u8 *) bs) < 0) {
		FAT_ERROR("Error reading block\n");
		return -1;
	}
	bs->reserved = FAT2CPU16(bs->reserved);
	bs->fat_length = FAT2CPU16(bs->fat_length);
	bs->secs_track = FAT2CPU16(bs->secs_track);
	bs->heads = FAT2CPU16(bs->heads);
	bs->hidden = FAT2CPU32(bs->hidden);
	bs->total_sect = FAT2CPU32(bs->total_sect);

	bs->fat32_length = FAT2CPU32(bs->fat32_length);
	bs->flags = FAT2CPU16(bs->flags);
	bs->root_cluster = FAT2CPU32(bs->root_cluster);
	bs->info_sector = FAT2CPU16(bs->info_sector);
	bs->backup_boot = FAT2CPU16(bs->backup_boot);

	return bs->fats;
}

int set_fat(int fatsize, void *fatbuf, __u32 entry, __u32 nclno)
{
	__u32 bufnum;
	__u32 offset;
	__u32 fbs = FAT32BUFSIZE;
	switch (fatsize) {
	case 32:
		bufnum = entry / fbs;
		offset = entry - bufnum * fbs;
		break;
	case 16:
		bufnum = entry / FAT16BUFSIZE;
		offset = entry - bufnum * FAT16BUFSIZE;
		break;
	case 12:
		bufnum = entry / FAT12BUFSIZE;
		offset = entry - bufnum * FAT12BUFSIZE;
		break;

	default:
		FAT_ERROR("Unsupported FAT size\n");
		return 0;
	}
	switch (fatsize) {
	case 32:
		((__u32 *) fatbuf)[offset] = CPU322FAT32(nclno);
		break;
	case 16:
		((__u16 *) fatbuf)[offset] = CPU162FAT16(nclno);
		break;
	case 12:
		{
			__u32 off16 = (offset * 3) / 2;
			__u8 *fp = (__u8 *) fatbuf;
			fp = &fp[off16];
			if ((offset & 1) == 1) {
				fp[0] = (nclno << 4) | (fp[0] & 0x0f);
				fp[1] = nclno >> 4;
			} else {
				fp[0] = nclno & 0xff;
				fp[1] = (fp[1] & 0xf0) | (nclno >> 8);
			}
		}
	}
	return 1;
}

//__u8 systemid[8],__u16 sector_size,__u8 cluster_size,__u8 nofats,__u16 dir_entries,__u16 sectors,__u8 media,__u16 fat_length

int create_fat(int megabytes, int cluster_size, int reserved_size,
	       int media_byte, int fat_size, int rootdirsize)
{
	__u16 megas = megabytes;
	__u8 media = media_byte;
	int cls = cluster_size;
	int rsv = reserved_size;
	int tl = 1024 * 1024 * megas;
	int fatsize;
	int fats;
	volume_info volinfo;
	union {
		boot_sector bs;
		__u8 sd[SECTOR_SIZE_512];
	} bs;
	struct boot_sector *bsp = (struct boot_sector *)&bs;
	struct volume_info *vistart;
	read_bootsectandvi(bsp, &volinfo, &fatsize);
	fats = get_file_fat_boot_sector(bsp, &volinfo, &fatsize);
	if (fats <= 0) {
		fats = 2;
	}

	read_bootsectandvi(bsp, &volinfo, &fatsize);

	if (disk_read(0, 1, (__u8 *) bsp) < 0) {
		FAT_ERROR("Error reading block\n");
		return -1;
	}
	if (1) {
		int ts = tl;
		__u8 mb = media;
		__u8 cs = cls;
		__u8 rs = rsv;
		__u16 ss = SECTOR_SIZE_512;
		__u8 fs = fats;
		//  _u8     ignored[3];     /* Bootstrap code */

		strncpy(bsp->system_id, "EMAC-NSD2", sizeof(bsp->system_id));

		*(__u16 *) & bsp->sector_size[0] = CPU162FAT16(ss);
		*(__u8 *) & bsp->cluster_size = (__u8) cs;
		*(__u8 *) & bsp->reserved = (__u8) rs;
		*(__u8 *) & bsp->fats = (__u8) fs;
		*(__u16 *) & bsp->sectors[0] = CPU162FAT16(0);
		*(__u8 *) & bsp->media = (__u8) mb;

		*(__u16 *) & bsp->secs_track = CPU162FAT16(0);
		*(__u16 *) & bsp->heads = CPU162FAT16(0);
		*(__u32 *) & bsp->hidden = CPU322FAT32(0);
		*(__u32 *) & bsp->total_sect =
		    CPU322FAT32((ts + (ss - 1)) / ss);

		if (fat_size == 32) {
			*(__u16 *) & bsp->dir_entries[0] =
			    CPU162FAT16(rootdirsize *
					((cs * ss) / (sizeof(dir_entry))));
			*(__u16 *) & bsp->fat_length = CPU162FAT16(0);
			*(__u16 *) & bsp->flags = CPU162FAT16(0);	//Bit 8: fat mirroring, low 4: active fat */
			*(__u16 *) & bsp->version = CPU162FAT16(0);
			*(__u32 *) & bsp->root_cluster = CPU322FAT32(2);
			*(__u16 *) & bsp->info_sector = CPU162FAT16(0);
			*(__u16 *) & bsp->backup_boot = CPU162FAT16(0);
			*(__u16 *) & bsp->reserved2[0] = CPU162FAT16(0);
			*(__u16 *) & bsp->reserved2[1] = CPU162FAT16(0);
			*(__u16 *) & bsp->reserved2[2] = CPU162FAT16(0);
			*(__u16 *) & bsp->reserved2[3] = CPU162FAT16(0);
			*(__u16 *) & bsp->reserved2[4] = CPU162FAT16(0);
			*(__u16 *) & bsp->reserved2[5] = CPU162FAT16(0);
			*(__u32 *) & bsp->fat32_length =
			    CPU322FAT32((((ts +
					   ((cs * ss) -
					    1)) / (cs * ss)) * sizeof(__u32)) /
					ss);
		} else {
			*(__u32 *) & bsp->fat32_length = CPU322FAT32(0);
			*(__u16 *) & bsp->dir_entries[0] =
			    CPU162FAT16(rootdirsize);
		}

		if (fat_size == 32) {
		} else if (fat_size == 16) {
			__u32 tst =
			    (((ts +
			       ((cs * ss) -
				1)) / (cs * ss)) * sizeof(__u16)) / (ss);
			if (tst > ((1 << fat_size) * sizeof(__u16)) / (ss)) {
				FAT_ERROR("Error in total size\n");
				return -1;
			}
			*(__u16 *) & bsp->fat_length = CPU162FAT16(tst);
		} else {
			__u32 tst =
			    (((ts +
			       ((cs * ss) -
				1)) / (cs * ss)) * (3 * sizeof(__u8))) / (2 *
									  ss);
			if (tst >
			    ((1 << fat_size) * 3 * sizeof(__u8)) / (ss * 2)) {
				FAT_ERROR("Error in total size\n");
				return -1;
			}
			*(__u16 *) & bsp->fat_length = CPU162FAT16(tst);
		}

	}
	if (fat_size == 12) {
		vistart = (volume_info *) & (bsp->fat32_length);
		strncpy(vistart->fs_type, "FAT12   ", sizeof(vistart->fs_type));
		strncpy(vistart->volume_label, "FAT12FS    ",
			sizeof(vistart->volume_label));
	} else if (fat_size == 16) {
		vistart = (volume_info *) & (bsp->fat32_length);
		strncpy(vistart->fs_type, "FAT16   ", sizeof(vistart->fs_type));
		strncpy(vistart->volume_label, "FAT16FS    ",
			sizeof(vistart->volume_label));
	} else if (fat_size == 32) {
		vistart = (volume_info *) (bs.sd + sizeof(boot_sector));
		strncpy(vistart->fs_type, "FAT32   ", sizeof(vistart->fs_type));
		strncpy(vistart->volume_label, "FAT32FS    ",
			sizeof(vistart->volume_label));
	}
	if (disk_write(0, 1, (__u8 *) bsp) < 0) {
		FAT_ERROR("Error writing block\n");
		return -1;
	}

	disk_write(0, 1, (__u8 *) & bs);
	read_bootsectandvi(bsp, &volinfo, &fatsize);

	fats = get_file_fat_boot_sector(bsp, &volinfo, &fatsize);
	if (fats > 0) {
		int res;
		int rootdir_size;
		fsdata datablock;
		fsdata *mydata = &datablock;
		int fatlength =
		    ((fatsize ==
		      32) ? bsp->fat32_length : bsp->fat_length) * fats;
		long fatbytes = fatlength * SECTOR_SIZE_512;
		void *clearbuffer = malloc(fatbytes);
		if (clearbuffer == NULL) {
			return -1;
		}
		res = get_mydata(mydata, &rootdir_size);
		if (res < 0) {
			return -1;
		}
		memset(clearbuffer, 0, fatbytes);
		set_fat(mydata->fatsize, clearbuffer, 0,
			(fatsize == 12) ? -16 : (fatsize ==
						 16) ? -8 : (fatsize ==
							     32) ? -8 : -1);
		set_fat(mydata->fatsize, clearbuffer, 1,
			(fatsize == 12) ? -1 : (fatsize ==
						16) ? -1 : (fatsize ==
							    32) ? -1 : -1);
		set_fat(mydata->fatsize, clearbuffer, 2,
			(fatsize == 12) ? 0 : (fatsize == 16) ? 0 : (fatsize ==
								     32) ?
			0x0fffffff : 0);
		if (disk_write(bsp->reserved, fatlength, clearbuffer) < 0) {
			free(clearbuffer);
			return -1;
		}
		free(clearbuffer);
		clearbuffer = malloc(rootdir_size * SECTOR_SIZE_512);
		if (clearbuffer == NULL) {
			return -1;
		}
		memset(clearbuffer, 0, rootdir_size * SECTOR_SIZE_512);
		{
			dir_entry *dep = (dir_entry *) clearbuffer;

			strncpy(dep->name, volinfo.volume_label,
				sizeof(volinfo.volume_label));
			dep->attr = ATTR_VOLUME;

			dep->lcase = 0;
			dep->ctime_ms = 0;
			dep->ctime = 0;
			dep->cdate = 0;
			dep->adate = 0;
			dep->starthi = 0;
			dep->time = 0;
			dep->date = 0;
			dep->start = 0;
			dep->size = 0;
			if (disk_write
			    (bsp->reserved + fatlength, rootdir_size,
			     clearbuffer) < 0) {
				free(clearbuffer);
				return -1;
			}
		}
		memset(clearbuffer, 0, rootdir_size * SECTOR_SIZE_512);
		{
			int tally = 0;
			int ndx;
			int bpen =
			    (fatsize == 12) ? 3 : (fatsize == 16) ? 2 : 4;
			int bped =
			    (fatsize == 12) ? 2 : (fatsize == 16) ? 1 : 1;
			int TC =
			    (bped * (fatlength / bsp->fats) * SECTOR_SIZE_512 /
			     bpen) * bsp->cluster_size;
			for (ndx = bsp->reserved + fatlength + rootdir_size;
			     ndx < TC; ndx += rootdir_size) {
				tally += rootdir_size * SECTOR_SIZE_512;
				if (disk_write(ndx, rootdir_size, clearbuffer) <
				    0) {
					free(clearbuffer);
					return -1;
				}
			}
			tally = (tl - tally);
			//tally should be shown to fake_disk
		}
		free(clearbuffer);
	}
	return 0;
}

/* file.c functions*/
int file_fat_detectfs(void)
{
	boot_sector bs;
	volume_info volinfo;
	int fatsize;
	char vol_label[12];

	if (cur_dev == NULL) {
		printf("No current device\n");
		return 1;
	}
#if defined(CONFIG_CMD_IDE) || \
    defined(CONFIG_CMD_MG_DISK) || \
    defined(CONFIG_CMD_SATA) || \
    defined(CONFIG_CMD_SCSI) || \
    defined(CONFIG_CMD_USB) || \
    defined(CONFIG_MMC)
	printf("Interface:  ");
	switch (cur_dev->if_type) {
	case IF_TYPE_IDE:
		printf("IDE");
		break;
	case IF_TYPE_SATA:
		printf("SATA");
		break;
	case IF_TYPE_SCSI:
		printf("SCSI");
		break;
	case IF_TYPE_ATAPI:
		printf("ATAPI");
		break;
	case IF_TYPE_USB:
		printf("USB");
		break;
	case IF_TYPE_DOC:
		printf("DOC");
		break;
	case IF_TYPE_MMC:
		printf("MMC");
		break;
	default:
		printf("Unknown");
	}
	printf("\n  Device %d: ", cur_dev->dev);
	dev_print(cur_dev);
#endif
	if (read_bootsectandvi(&bs, &volinfo, &fatsize)) {
		printf("\nNo valid FAT fs found\n");
		return 1;
	}
	memcpy(vol_label, volinfo.volume_label, 11);
	vol_label[11] = '\0';
	volinfo.fs_type[5] = '\0';
	printf("Partition %d: Filesystem: %s \"%s\"\n", cur_part,
	       volinfo.fs_type, vol_label);
	return 0;
}

int file_fat_ls(const char *dir)
{
	printf("listing %s\n", dir);
	return do_fat_read(dir, NULL, 0, LS_YES);
}

int file_fat_ls_none(const char *dir)
{
	printf("listing %s\n", dir);
	return do_fat_read(dir, NULL, 0, LS_64);
}

long file_fat_read(const char *filename, void *buffer, unsigned long maxsize)
{
	printf("reading %s\n", filename);
	return do_fat_read(filename, buffer, maxsize, LS_NO);
}

long file_fat_write(const char *filename, void *buffer, unsigned long maxsize)
{
	printf("writing %s\n", filename);
	return do_fat_write(filename, buffer, maxsize);
}

int file_fat_unlink(const char *filename)
{
	printf("unlinking %s\n", filename);
	return do_fat_unlink(filename);
}

int file_fat_rename(const char *oldfilename, const char *newfilename)
{
	printf("renaming %s to  %s \n", oldfilename, newfilename);
	return do_fat_rename(oldfilename, newfilename);
}

int file_fat_format(int megabytes, int cluster_size, int reserved_size,
		    int media_byte, int fat_size, int rootdirsize)
{
	printf("formatting \n");
	return create_fat(megabytes, cluster_size, reserved_size, media_byte,
			  fat_size, rootdirsize);
}

//broken move into mydata
#define FAT_INUSE_ERROR 1
//FAT read/write

//for swapping fat  and multiple writes
int disk_read_fat(fsdata * mydata, __u32 startblock, __u32 getsize,
		  __u8 * bufptr)
{
	int res = 0;
	__u32 startblock2 =
	    startblock + (mydata->fat_in_use * mydata->fatlength);
	goto dotry;
      retry:if (FAT_INUSE_ERROR) {
	      dotry:
		res = disk_read(startblock2, getsize, bufptr);
		if (FAT_INUSE_ERROR && res < 0) {
			//is fat_broken[fat_in_use]
			int maxfats = mydata->fats;
			mydata->fat_broken[mydata->fat_in_use] = 1;
			mydata->fat_in_use++;
			mydata->fat_in_use %= mydata->fats;
			while (maxfats
			       && mydata->fat_broken[mydata->fat_in_use]) {
				mydata->fat_in_use++;
				mydata->fat_in_use %= mydata->fats;
				maxfats--;
			}
			if (maxfats) {
				startblock2 =
				    startblock +
				    (mydata->fat_in_use * mydata->fatlength);
				goto retry;
			} else {
				//all fats broken
				return -1;
			}
		}
	}
	return res;
}

int disk_write_fat(fsdata * mydata, __u32 startblock, __u32 putsize,
		   __u8 * bufptr)
{
	int res = 0;
	int maxfats = mydata->fats;
	__u32 startblock2 =
	    startblock + (mydata->fat_in_use * mydata->fatlength);
	goto dotry;
      retry:if (FAT_INUSE_ERROR) {
	      dotry:
		res = disk_write(startblock2, putsize, bufptr);
		if (mydata->fatsize >= mydata->fats) {
			mydata->fat_broken[mydata->fat_in_use] =
			    (res < 0) ? 1 : 0;
			while (maxfats) {
				mydata->fat_in_use++;
				mydata->fat_in_use %= mydata->fats;
				maxfats--;
				if (mydata->fat_broken[mydata->fat_in_use] == 0) {
					break;
				}
			}
			if (maxfats) {
				startblock2 =
				    startblock +
				    (mydata->fat_in_use * mydata->fatlength);
				goto retry;
			} else {
				long fres = -1;
				int rs;
				for (rs = 0; rs < mydata->fats; rs++) {
					fres &=
					    (mydata->fat_broken[rs]) ? -1 : 0;
				}
				if (fres == 0) {
					return putsize;
				}
				//all fats broken
				return -1;
			}
		}
	}
	return res;
}
