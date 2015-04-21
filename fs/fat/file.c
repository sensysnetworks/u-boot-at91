/*
 * file.c
 *
 * Mini "VFS" by Marcus Sundberg
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

#include <malloc.h>
#include <part.h>
#include <fat.h>
#include <linux/stat.h>
#include <linux/time.h>

/* Supported filesystems */
static const struct filesystem filesystems[] = { { file_fat_detectfs,
		file_fat_ls, file_fat_read, file_fat_write, file_fat_unlink,
		file_fat_rename, file_fat_ls_none, "FAT" }, };

#define NUM_FILESYS	(sizeof(filesystems)/sizeof(struct filesystem))

/* The filesystem which was last detected */
static int current_filesystem = FSTYPE_NONE;

/* The current working directory */
#define CWD_LEN		511
char file_cwd[CWD_LEN+1] = "/";

const char * file_getfsname(int idx) {
	if (idx < 0 || idx >= NUM_FILESYS)
		return NULL;

	return filesystems[idx].name;
}

static void pathcpy(char *dest, const char *src) {
	char *origdest = dest;

	do {
		if (dest-file_cwd >= CWD_LEN) {
			*dest = '\0';
			return;
		}
		*(dest) = *(src);
		if (*src == '\0') {
			if (dest-- != origdest && ISDIRDELIM(*dest)) {
				*dest = '\0';
			}
			return;
		}
		++dest;
		if (ISDIRDELIM(*src)) {
			while (ISDIRDELIM(*src))
				src++;
		} else {
			src++;
		}
	} while (1);
}

int file_cd(const char *path) {
	if (ISDIRDELIM(*path)) {
		while (ISDIRDELIM(*path))
			path++;
		strncpy(file_cwd+1, path, CWD_LEN-1);
	} else {
		const char *origpath = path;
		char *tmpstr = file_cwd;
		int back = 0;

		while (*tmpstr != '\0')
			tmpstr++;
		do {
			tmpstr--;
		} while (ISDIRDELIM(*tmpstr));

		while (*path == '.') {
			path++;
			while (*path == '.') {
				path++;
				back++;
			}
			if (*path != '\0' && !ISDIRDELIM(*path)) {
				path = origpath;
				back = 0;
				break;
			}
			while (ISDIRDELIM(*path))
				path++;
			origpath = path;
		}

		while (back--) {
			/* Strip off path component */
			while (!ISDIRDELIM(*tmpstr)) {
				tmpstr--;
			}
			if (tmpstr == file_cwd) {
				/* Incremented again right after the loop. */
				tmpstr--;
				break;
			}
			/* Skip delimiters */
			while (ISDIRDELIM(*tmpstr))
				tmpstr--;
		}
		tmpstr++;
		if (*path == '\0') {
			if (tmpstr == file_cwd) {
				*tmpstr = '/';
				tmpstr++;
			}
			*tmpstr = '\0';
			return 0;
		}
		*tmpstr = '/';
		pathcpy(tmpstr+1, path);
	}

	return 0;
}

int file_detectfs(void) {
	int i;

	current_filesystem = FSTYPE_NONE;

	for (i = 0; i < NUM_FILESYS; i++) {
		if (filesystems[i].detect() == 0) {
			strcpy(file_cwd, "/");
			current_filesystem = i;
			break;
		}
	}
	return current_filesystem;
}

static int validnamechar(char fc) {
	if ( (fc >= 'A' && fc <= 'Z')|| (fc >= 'a' && fc <= 'z')|| (fc >= '0' && fc
			<= '9')|| (fc == '_' )|| (fc == '$' )|| (fc == '@' )|| (fc == '.' )) {
		return 1;
	}
	return 0;
}

static int validpathnamechar(char fc) {
	if (fc == '\\' || fc == '/') {
		return 1;
	}
	return validnamechar(fc);
}

static int validfilenamechar(char fc) {
	if (fc == '\\' || fc == '/') {
		return 0;
	}
	return validnamechar(fc);
}

//check for invalid chars in path(mode = 0)  or name(mode = 1)
static int name_validate(const char * fp, int mode) {
	int dc = 0;
	int ok = 1;
	if (mode) {
		while (*fp && (ok = validfilenamechar(*fp))) {
			if (*fp == '.')
				dc++;
			fp++;
		}
	} else {
		while (*fp && (ok = validpathnamechar(*fp))) {
			if (*fp == '.')
				dc++;
			fp++;
		}
	}
	return (ok && (dc<=1)) ? 1 : 0;
}

int file_ls(const char *dir) {
	char fullpath[1024];
	const char *arg;
	if (name_validate(dir, 0) == 0)
		return -1;

	if (current_filesystem == FSTYPE_NONE) {
		printf("Can't list files without a filesystem!\n");
		return -1;
	}

	if (ISDIRDELIM(*dir)) {
		arg = dir;
	} else {
		if ((strlen(file_cwd) + strlen(dir) + 1 ) >= sizeof(fullpath)) {
			printf("Can't ls file without causing buffer overrun!\nSupply full path.\n");
			return -1;
		}
		if (strcmp(file_cwd, "/")== 0)
			strcpy(file_cwd, "");
		sprintf(fullpath, "%s/%s", file_cwd, dir);
		arg = fullpath;
	}
	return filesystems[current_filesystem].ls(arg);
}

long file_read(const char *filename, void *buffer, unsigned long maxsize) {
	char fullpath[1024];
	const char *arg;
	if (name_validate(filename, 0) == 0)
		return -1;

	if (current_filesystem == FSTYPE_NONE) {
		printf("Can't load file without a filesystem!\n");
		return -1;
	}

	if (ISDIRDELIM(*filename)) {
		arg = filename;
	} else {
		if ((strlen(file_cwd) + strlen(filename) + 1 ) >= sizeof(fullpath)) {
			printf("Can't load file without causing buffer overrun!\nSupply full path.\n");
			return -1;
		}
		if (strcmp(file_cwd, "/")== 0)
			strcpy(file_cwd, "");
		sprintf(fullpath, "%s/%s", file_cwd, filename);
		arg = fullpath;
	}
	return filesystems[current_filesystem].read(arg, buffer, maxsize);
}

long file_write(const char *filename, void *buffer, unsigned long maxsize) {
	char fullpath[1024];
	const char *arg;
	if (name_validate(filename, 0) == 0)
		return -1;

	if (current_filesystem == FSTYPE_NONE) {
		printf("Can't write file without a filesystem!\n");
		return -1;
	}

	if (ISDIRDELIM(*filename)) {
		arg = filename;
	} else {
		if ((strlen(file_cwd) + strlen(filename) + 1 ) >= sizeof(fullpath)) {
			printf("Can't write file without causing buffer overrun!\nSupply full path.\n");
			return -1;
		}
		if (strcmp(file_cwd, "/")== 0)
			strcpy(file_cwd, "");
		sprintf(fullpath, "%s/%s", file_cwd, filename);
		arg = fullpath;
	}
	return filesystems[current_filesystem].write(arg, buffer, maxsize);
}

int file_unlink(const char *filename) {
	char fullpath[1024];
	const char *arg;
	if (name_validate(filename, 0) == 0)
		return -1;

	if (current_filesystem == FSTYPE_NONE) {
		printf("Can't unlink file without a filesystem!\n");
		return -1;
	}

	if (ISDIRDELIM(*filename)) {
		arg = filename;
	} else {
		if ((strlen(file_cwd) + strlen(filename) + 1 ) >= sizeof(fullpath)) {
			printf("Can't unlink file without causing buffer overrun!\nSupply full path.\n");
			return -1;
		}
		if (strcmp(file_cwd, "/")== 0)
			strcpy(file_cwd, "");
		sprintf(fullpath, "%s/%s", file_cwd, filename);
		arg = fullpath;
	}
	return filesystems[current_filesystem].unlink(arg);
}

int file_rename(const char *filename, const char *newfilename) {
	char fullpath1[1024];
	const char *arg1;
	const char *arg2;
	if (name_validate(filename, 0) == 0)
		return -1;
	if (name_validate(newfilename, 1) == 0)
		return -1;
	if (current_filesystem == FSTYPE_NONE) {
		printf("Can't rename file without a filesystem!\n");
		return -1;
	}

	if (ISDIRDELIM(*filename)) {
		char * fp;
		int cnt = strlen(filename);
		if (cnt > sizeof(fullpath1)) {
			printf("Can't rename file without causing buffer overrun!\n");
			return -1;
		}
		arg1 = filename;
		strcpy(fullpath1, arg1);
		fp = &fullpath1[cnt]-1;
		while (cnt && *fp!= '\\' && *fp!= '/') {
			*fp = 0;
			fp--;
			cnt--;
		}
		if (cnt)
			fp++;
		strcpy(fp, newfilename);
		if (filesystems[current_filesystem].ls_none(fullpath1) >= 0) {
			printf("New file exist!\n");
			return -1;
		}
	} else {
		int len = strlen(file_cwd);
		if ( ( (len + strlen(filename) + 1 ) >= sizeof(fullpath1) )|| ( (len
				+ strlen(newfilename) + 1 ) >= sizeof(fullpath1) )) {
			printf("Can't rename file without causing buffer overrun!\n");
			return -1;
		}
		if (strcmp(file_cwd, "/") == 0)
			strcpy(file_cwd, "");

		sprintf(fullpath1, "%s/%s", file_cwd, newfilename);

		if (filesystems[current_filesystem].ls_none(fullpath1) >= 0) {
			printf("New file exist!\n");
			return -1;
		}
		sprintf(fullpath1, "%s/%s", file_cwd, filename);

		arg1 = fullpath1;
	}

	arg2 = newfilename;

	return filesystems[current_filesystem].rename(arg1, arg2);
}
