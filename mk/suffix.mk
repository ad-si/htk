OBJSHS = $(patsubst %.hs,%.o,$(SRCS))
OBJSC = $(patsubst %.c,%.o,$(SRCSC))
OBJS = $(OBJSHS) $(OBJSC)
HIFILES = $(patsubst %.hs,%.hi,$(SRCS))

# The following gmake-3.77ism prevents gmake deleting all the
# object files once it has finished with them, so remakes
# actually work.
.SECONDARY : $(OBJS) $(HIFILES)

depend : $(SRCS) 
	$(DEPEND) $(HCSYSLIBS) -i$(HCDIRS) $(SRCS)

lib    : $(LIB)

$(LIB) : $(OBJS)
	if $(MUSTMAKE) $@ $? ; then $(RM) $@ ; $(AR) -r $@ $^ ; fi

.SUFFIXES : .o .hi .hc .hs .c

.o.hi:
	@:

.hs.o:
	if $(MUSTMAKE) $@ $? ; then mv $*.hi $*.hi.old ; cp $*.hi.old $*.hi ; $(RM) $@ ; set -e ; $(HC) -c $< $(HCFLAGS) ; set +e ; STATUS=$$? ; if diff $*.hi $*.hi.old ; then rm $*.hi ; mv $*.hi.old $*.hi ; else rm $*.hi.old; fi; exit $$STATUS ; fi

.c.o:
	if $(MUSTMAKE) $@ $? ; then $(RM) $@ ; $(CC) $(CFLAGS) -c $< -o $@ ; fi


.cc.o:
	if $(MUSTMAKE) $@ $? ; then $(RM) $@ ; $(CC) $(CFLAGS) -c $< -o $@ ; fi

clean:
	echo $(MAKE_VERSION)
	$(RM) -f $(OBJS) $(LIB) $(patsubst %.o,%.hi,$(OBJS))
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) clean && ) echo Clean finished


