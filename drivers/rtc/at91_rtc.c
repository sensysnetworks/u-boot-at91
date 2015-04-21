/*
 */

#include <common.h>
#include <command.h>
#include <rtc.h>

#if defined(CONFIG_CMD_DATE)

#include <asm/arch/at91sam9260.h>
#include <asm/arch/io.h>

#define MIN_TO_SECS(x)    (60 * (x))
#define HRS_TO_SECS(x)    (60 * MIN_TO_SECS(x))
#define DAYS_TO_SECS(x)   (24 * HRS_TO_SECS(x))

#define NUM_SECS_IN_MIN   MIN_TO_SECS(1)
#define NUM_SECS_IN_HR    HRS_TO_SECS(1)
#define NUM_SECS_IN_DAY   DAYS_TO_SECS(1)

//at91_sys_write
//at91_sys_read

#define AT91_RTT_MR	AT91_RTT + 0x00
#define AT91_RTT_AR	AT91_RTT + 0x04
#define AT91_RTT_VR	AT91_RTT + 0x08
#define AT91_RTT_SR 	AT91_RTT + 0x0C

#define AT91_GPBR_0	AT91_GPBR + 0x00


#define AT91_RTT_RTTRST 0x00040000

static void rtc_init(void)
{
}

void rtc_reset(void)
{
	rtc_init();
}

int rtc_set(struct rtc_time *tmp)
{
	unsigned long offset;
	unsigned long mr;

	offset = mktime(tmp->tm_year, tmp->tm_mon, tmp->tm_mday, tmp->tm_hour, tmp->tm_min, tmp->tm_sec);

	mr = at91_sys_read(AT91_RTT_MR);

	at91_sys_write(AT91_GPBR_0, offset);
	at91_sys_write(AT91_RTT_MR, mr | AT91_RTT_RTTRST);

	return 0;
}

int rtc_get(struct rtc_time *tmp)
{
	unsigned long offset;
	unsigned long t1, t2;

	offset = at91_sys_read(AT91_GPBR_0);

	t1 = at91_sys_read(AT91_RTT_VR);
	t2 = at91_sys_read(AT91_RTT_VR);
	if (t1 != t2) t1 = at91_sys_read(AT91_RTT_VR);

	to_tm(offset + t1, tmp);

	return 0;
}

#endif
