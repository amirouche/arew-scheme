/*
 * Print out the resource usage for an application
 *
 * void rusage_start(void)
 * Note the current resource usage (in a static storage)
 * 
 * void rusage_report(char * title)
 * print the resouce usage from the previously noted point
 *
 */

#include <stdio.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>                                                  

/* Static data to note the resource usage */
struct rusage noted_rusage;

/* perform dist -= src where dist and src are rusage struc */
void diff_rusage(struct rusage * dest, const struct rusage * src)
{
  dest->ru_utime.tv_sec -= src->ru_utime.tv_sec;
  dest->ru_utime.tv_usec -= src->ru_utime.tv_usec;
  if( dest->ru_utime.tv_usec < 0 )
    dest->ru_utime.tv_sec--,
      dest->ru_utime.tv_usec += 1000000;
  dest->ru_stime.tv_sec -= src->ru_stime.tv_sec;
  dest->ru_stime.tv_usec -= src->ru_stime.tv_sec;
  if( dest->ru_stime.tv_usec < 0 )
    dest->ru_stime.tv_sec--,
      dest->ru_stime.tv_usec += 1000000;
  dest->ru_maxrss -= src->ru_maxrss;
  dest->ru_ixrss -= src->ru_ixrss;
  dest->ru_idrss -= src->ru_idrss;
  dest->ru_isrss -= src->ru_isrss;
  dest->ru_minflt -= src->ru_minflt;
  dest->ru_majflt -= src->ru_majflt;
  dest->ru_nswap -= src->ru_nswap;
  dest->ru_inblock -= src->ru_inblock;
  dest->ru_oublock -= src->ru_oublock;
  dest->ru_msgsnd -= src->ru_msgsnd;
  dest->ru_msgrcv -= src->ru_msgrcv;
  dest->ru_nsignals -= src->ru_nsignals;
  dest->ru_nvcsw -= src->ru_nvcsw;
  dest->ru_nivcsw -= src->ru_nivcsw;
};

void print_rusage(const struct rusage * ru, const char title [])
{
  fprintf(stderr,"Rusage for '%s'\n",title);
  
  fprintf(stderr," user time used   %d.%06d\n",ru->ru_utime.tv_sec,
	  ru->ru_utime.tv_usec);
  fprintf(stderr," system time used   %d.%06d\n",ru->ru_stime.tv_sec,
	  ru->ru_stime.tv_usec);
  fprintf(stderr," max resident set size   %d\n",ru->ru_maxrss);
  fprintf(stderr," integral shared text memory size   %d\n",ru->ru_ixrss);
  fprintf(stderr," integral unshared data size   %d\n",ru->ru_idrss);
  fprintf(stderr," integral unshared stack size   %d\n",ru->ru_isrss);
  fprintf(stderr," page reclaims   %d\n",ru->ru_minflt);
  fprintf(stderr," page faults   %d\n",ru->ru_majflt);
  fprintf(stderr," swaps   %d\n",ru->ru_nswap);
  fprintf(stderr," block input operations   %d\n",ru->ru_inblock);
  fprintf(stderr," block output operations   %d\n",ru->ru_oublock);
  fprintf(stderr," messages sent   %d\n",ru->ru_msgsnd);
  fprintf(stderr," messages received   %d\n",ru->ru_msgrcv);
  fprintf(stderr," signals received   %d\n",ru->ru_nsignals);
  fprintf(stderr," voluntary context switches   %d\n",ru->ru_nvcsw);
  fprintf(stderr," involuntary context switches   %d\n",ru->ru_nivcsw);
}


/* Note the current resource usage (in a static storage) */
void rusage_start(void)
{  assert( getrusage(0,&noted_rusage) == 0 ); }
     

/* print the resouce usage from the previously noted point */
void rusage_report(char * title)
{
  struct rusage ru_now;
  assert( getrusage(0,&ru_now) == 0 );
  diff_rusage(&ru_now,&noted_rusage);
  print_rusage(&ru_now,title);
}


