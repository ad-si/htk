THIS	   = concurrency
IMPS	   = *.hi

LIBS       = $(UTILDIR)/libutil.a

LIB        = libconcurrency.a

SRCS       = Thread.hs\
             ThreadWait.hs\
             Variable.hs\
	     Lock.hs\
             BSem.hs\
	     QSem.hs\
	     Mutex.hs\
	     RVar.hs\
	     Event.hs\
	     Selective.hs\
	     SAP.hs\
	     Future.hs\
	     Concurrency.hs\
             Toggle.hs\
             PrimEvent.hs\
             BaseEvent.hs\
             TwoWayChannel.hs\
             EV.hs\
             Channels.hs\
#	     Buffer.hs\

SRCSC      = 
SRCSH      =

PGMS       = 

PGM_OBJS   =

EXTRA_DEPS = 


EXTRA_HC_OPTIONS += -O -H30M

include top/mk/boilerplate.mk

ThreadWait : Time.h


# DO NOT DELETE: Beginning of Haskell dependencies
Thread.o : Thread.hs
Thread.o : top/util/Maybes.hi
Thread.o : top/util/Computation.hi
Thread.o : top/util/Debug.hi
ThreadWait.o : ThreadWait.hs
ThreadWait.o : top/util/Debug.hi
Variable.o : Variable.hs
Variable.o : ./Thread.hi
Variable.o : ./Lock.hi
Variable.o : top/util/Debug.hi
Lock.o : Lock.hs
Lock.o : top/util/Computation.hi
Lock.o : top/util/Debug.hi
BSem.o : BSem.hs
BSem.o : ./Thread.hi
BSem.o : ./Variable.hi
BSem.o : ./Lock.hi
BSem.o : top/util/Debug.hi
QSem.o : QSem.hs
QSem.o : ./Thread.hi
QSem.o : ./BSem.hi
QSem.o : ./Variable.hi
QSem.o : ./Lock.hi
QSem.o : top/util/Debug.hi
Mutex.o : Mutex.hs
Mutex.o : ./Thread.hi
Mutex.o : ./Variable.hi
Mutex.o : ./Lock.hi
Mutex.o : ./BSem.hi
Mutex.o : top/util/Debug.hi
RVar.o : RVar.hs
RVar.o : ./Thread.hi
RVar.o : ./Variable.hi
RVar.o : ./Mutex.hi
RVar.o : top/util/Debug.hi
Event.o : Event.hs
Event.o : top/util/Debug.hi
Event.o : top/util/Computation.hi
Selective.o : Selective.hs
Selective.o : top/util/Computation.hi
Selective.o : ./Thread.hi
Selective.o : ./Event.hi
Selective.o : ./EV.hi
Selective.o : ./Channels.hi
SAP.o : SAP.hs
SAP.o : ./Thread.hi
SAP.o : ./Variable.hi
SAP.o : ./Selective.hi
SAP.o : top/util/Debug.hi
Future.o : Future.hs
Future.o : ./Thread.hi
Future.o : ./Selective.hi
Future.o : top/util/Debug.hi
Concurrency.o : Concurrency.hs
Concurrency.o : ./Thread.hi
Concurrency.o : ./Variable.hi
Concurrency.o : ./Lock.hi
Concurrency.o : ./Event.hi
Concurrency.o : ./Selective.hi
Concurrency.o : ./Mutex.hi
Concurrency.o : ./RVar.hi
Concurrency.o : ./BSem.hi
Concurrency.o : ./SAP.hi
Concurrency.o : ./Future.hi
Concurrency.o : top/util/Debug.hi
Toggle.o : Toggle.hs
PrimEvent.o : PrimEvent.hs
PrimEvent.o : top/util/Computation.hi
PrimEvent.o : ./Toggle.hi
BaseEvent.o : BaseEvent.hs
BaseEvent.o : top/util/Computation.hi
BaseEvent.o : top/util/ExtendedPrelude.hi
BaseEvent.o : ./PrimEvent.hi
TwoWayChannel.o : TwoWayChannel.hs
TwoWayChannel.o : top/util/Queue.hi
TwoWayChannel.o : ./PrimEvent.hi
TwoWayChannel.o : ./Toggle.hi
TwoWayChannel.o : top/util/Debug.hi
EV.o : EV.hs
EV.o : top/util/Debug.hi
EV.o : ./Event.hi
EV.o : ./BaseEvent.hi
Channels.o : Channels.hs
Channels.o : ./Event.hi
Channels.o : ./BaseEvent.hi
Channels.o : ./PrimEvent.hi
Channels.o : ./TwoWayChannel.hi
Channels.o : ./EV.hi
# DO NOT DELETE: End of Haskell dependencies
