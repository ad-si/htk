/* This is the important bits of sys/time.h if all you want to use
   is the select function.  Writing this file is a somewhat desperate 
   remedy after the following solutions were tried and failed:
   (1) you can't include sys/time.h or sys/select.h directly using 
       -#include in the Haskell file, because Stg.h, curse it, defines 
       _POSIX_SOURCE, which makes select unavailable.
   (2) you can't preprocess Time.h separately, because then some
       of the types (not ones defined in this file!) clash and
       gcc complains.
       */

typedef long    tv_sec_t;    /* Used to be time_t */      

typedef long    suseconds_t;     

struct timeval {
        tv_sec_t          tv_sec;          
        suseconds_t     tv_usec;         
};

typedef long    fds_mask;

#define _NBBY 8

#define FD_NFDBITS      (sizeof (fds_mask) * _NBBY)     

typedef struct fd_set {
        long    fds_bits[((( 1024  )+((  (sizeof (fds_mask) * 8 )  )-1))/(  (sizeof (fds_mask) * 8 )  )) ];
} fd_set;

#define FD_SET(__n, __p)        ((__p)->fds_bits[(__n)/FD_NFDBITS] |= (1ul << ((__n) % FD_NFDBITS)))

#define FD_CLR(__n, __p)        ((__p)->fds_bits[(__n)/FD_NFDBITS] &= ~(1ul << ((__n) % FD_NFDBITS)))


#define FD_ISSET(__n, __p)      (((__p)->fds_bits[(__n)/FD_NFDBITS] & (1ul << ((__n) % FD_NFDBITS))) != 0l)

#define FD_ZERO(__p)    memset((void *)(__p), 0, sizeof (*(__p)))

extern int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);



















