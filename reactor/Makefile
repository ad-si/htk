HCINC      = '-\#include "signaldispatcher.h"'  '-\#include "collector.h"'


THIS	   = reactor
IMPS	   = *.hi

LIBS       = $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a\

LIB        = libreactor.a

SRCS       = Listener.hs\
	     ExternalEvent.hs\
	     Interaction.hs\
	     SIMClasses.hs\
	     EventStream.hs\
	     InterActor.hs\
	     Controller.hs\
	     EventLoop.hs\
	     WatchDog.hs\
	     ChildProcess.hs\
	     EventBroker.hs\
	     InfoBus.hs\
	     Observer.hs\
	     Interpreter.hs\
	     Dispatcher.hs\
	     Signal.hs\
	     Collector.hs\
             RegularExpression.hs\
	     Expect.hs\
	     SIM.hs 
# the following modules don't seem to be used anywhere and
# are now obsolete.
#	     Sensor.hs\
#	     NameSpace.hs\
      
SRCSH      = signaldispatcher.h collector.h

SRCSC      = signaldispatcher.c collector.c

PGMS       = pgm
PGM_OBJS   = test/Main.hs
EXTRA_DEPS = 


EXTRA_HC_OPTIONS += -O -H30M

include top/mk/boilerplate.mk

pgm:    $(LIBS) $(LIB) test/Main.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) test/Main.o $(LIB) $(LIBS)

# DO NOT DELETE: Beginning of Haskell dependencies
Listener.o : Listener.hs
Listener.o : top/concurrency/Concurrency.hi
Listener.o : top/util/Object.hi
Listener.o : top/util/Dynamics.hi
Listener.o : top/concurrency/Event.hi
Listener.o : top/util/Debug.hi
ExternalEvent.o : ExternalEvent.hs
ExternalEvent.o : top/concurrency/Concurrency.hi
ExternalEvent.o : top/util/Dynamics.hi
ExternalEvent.o : ./Listener.hi
ExternalEvent.o : top/util/Debug.hi
Interaction.o : Interaction.hs
Interaction.o : top/util/ExtendedPrelude.hi
Interaction.o : top/concurrency/Concurrency.hi
Interaction.o : ./Listener.hi
Interaction.o : ./ExternalEvent.hi
Interaction.o : top/util/Object.hi
Interaction.o : top/util/Dynamics.hi
Interaction.o : top/concurrency/Event.hi
Interaction.o : top/util/Debug.hi
SIMClasses.o : SIMClasses.hs
SIMClasses.o : top/util/Dynamics.hi
SIMClasses.o : top/concurrency/Concurrency.hi
SIMClasses.o : ./Interaction.hi
SIMClasses.o : ./Listener.hi
SIMClasses.o : top/util/Object.hi
SIMClasses.o : ./ExternalEvent.hi
SIMClasses.o : top/util/Debug.hi
EventStream.o : EventStream.hs
EventStream.o : top/concurrency/Concurrency.hi
EventStream.o : ./Interaction.hi
EventStream.o : ./ExternalEvent.hi
EventStream.o : ./Listener.hi
EventStream.o : ./SIMClasses.hi
EventStream.o : top/util/Dynamics.hi
EventStream.o : top/util/Debug.hi
InterActor.o : InterActor.hs
InterActor.o : top/concurrency/Concurrency.hi
InterActor.o : ./EventStream.hi
InterActor.o : ./Interaction.hi
InterActor.o : top/util/Dynamics.hi
InterActor.o : top/util/Debug.hi
InterActor.o : ./SIMClasses.hi
Controller.o : Controller.hs
Controller.o : top/util/Computation.hi
Controller.o : top/concurrency/Concurrency.hi
Controller.o : ./Interaction.hi
Controller.o : ./InterActor.hi
Controller.o : ./SIMClasses.hi
Controller.o : top/util/Debug.hi
EventLoop.o : EventLoop.hs
EventLoop.o : top/concurrency/Concurrency.hi
EventLoop.o : ./Interaction.hi
EventLoop.o : ./InterActor.hi
EventLoop.o : ./SIMClasses.hi
EventLoop.o : top/util/Debug.hi
WatchDog.o : WatchDog.hs
WatchDog.o : top/concurrency/Concurrency.hi
WatchDog.o : ./Interaction.hi
WatchDog.o : top/concurrency/Future.hi
WatchDog.o : ./SIMClasses.hi
WatchDog.o : top/util/Debug.hi
ChildProcess.o : ChildProcess.hs
ChildProcess.o : top/util/Object.hi
ChildProcess.o : top/concurrency/Concurrency.hi
ChildProcess.o : ./Interaction.hi
ChildProcess.o : ./WatchDog.hi
ChildProcess.o : top/util/Maybes.hi
ChildProcess.o : top/util/ExtendedPrelude.hi
ChildProcess.o : top/concurrency/ThreadWait.hi
ChildProcess.o : ./SIMClasses.hi
ChildProcess.o : top/util/Debug.hi
EventBroker.o : EventBroker.hs
EventBroker.o : top/concurrency/Concurrency.hi
EventBroker.o : top/util/Dynamics.hi
EventBroker.o : ./SIMClasses.hi
EventBroker.o : ./Interaction.hi
EventBroker.o : top/util/Debug.hi
InfoBus.o : InfoBus.hs
InfoBus.o : top/concurrency/Concurrency.hi
InfoBus.o : top/util/Dynamics.hi
InfoBus.o : ./EventBroker.hi
InfoBus.o : top/util/Object.hi
InfoBus.o : ./SIMClasses.hi
InfoBus.o : ./Interaction.hi
InfoBus.o : top/util/Debug.hi
Observer.o : Observer.hs
Observer.o : top/concurrency/Concurrency.hi
Observer.o : ./Interaction.hi
Observer.o : top/util/Object.hi
Observer.o : ./SIMClasses.hi
Observer.o : ./WatchDog.hi
Observer.o : ./ChildProcess.hi
Observer.o : top/util/Debug.hi
Interpreter.o : Interpreter.hs
Interpreter.o : top/concurrency/Concurrency.hi
Interpreter.o : ./ChildProcess.hi
Interpreter.o : ./SIMClasses.hi
Interpreter.o : top/util/Object.hi
Interpreter.o : top/util/Debug.hi
Dispatcher.o : Dispatcher.hs
Dispatcher.o : top/util/Dynamics.hi
Dispatcher.o : top/concurrency/Concurrency.hi
Dispatcher.o : ./Interaction.hi
Dispatcher.o : ./ChildProcess.hi
Dispatcher.o : ./Interpreter.hi
Dispatcher.o : ./SIMClasses.hi
Dispatcher.o : ./EventBroker.hi
Dispatcher.o : top/util/Object.hi
Dispatcher.o : top/util/Debug.hi
Signal.o : Signal.hs
Signal.o : top/concurrency/Concurrency.hi
Signal.o : ./Interaction.hi
Signal.o : ./EventBroker.hi
Signal.o : ./SIMClasses.hi
Signal.o : ./InfoBus.hi
Signal.o : ./ChildProcess.hi
Signal.o : top/concurrency/ThreadWait.hi
Signal.o : top/util/Object.hi
Signal.o : top/util/Debug.hi
Collector.o : Collector.hs
Collector.o : top/concurrency/Concurrency.hi
Collector.o : ./SIMClasses.hi
Collector.o : ./InfoBus.hi
Collector.o : top/util/Object.hi
Collector.o : ./ChildProcess.hi
Collector.o : top/util/Debug.hi
Collector.o : top/concurrency/ThreadWait.hi
RegularExpression.o : RegularExpression.hs
RegularExpression.o : top/util/Dynamics.hi
RegularExpression.o : top/util/Debug.hi
Expect.o : Expect.hs
Expect.o : top/util/ExtendedPrelude.hi
Expect.o : top/util/Object.hi
Expect.o : top/util/Maybes.hi
Expect.o : top/util/Dynamics.hi
Expect.o : top/util/Debug.hi
Expect.o : top/concurrency/Concurrency.hi
Expect.o : ./Interaction.hi
Expect.o : ./Listener.hi
Expect.o : ./ChildProcess.hi
Expect.o : ./Interpreter.hi
Expect.o : ./SIMClasses.hi
Expect.o : ./Collector.hi
Expect.o : ./RegularExpression.hi
SIM.o : SIM.hs
SIM.o : top/concurrency/Concurrency.hi
SIM.o : ./Interaction.hi
SIM.o : ./InterActor.hi
SIM.o : ./Controller.hi
SIM.o : ./EventStream.hi
SIM.o : ./EventLoop.hi
SIM.o : top/concurrency/Event.hi
SIM.o : top/util/Dynamics.hi
SIM.o : ./SIMClasses.hi
SIM.o : ./InfoBus.hi
SIM.o : ./Signal.hi
SIM.o : ./Collector.hi
SIM.o : ./Observer.hi
SIM.o : ./Dispatcher.hi
SIM.o : ./ChildProcess.hi
SIM.o : top/util/Debug.hi
# DO NOT DELETE: End of Haskell dependencies
