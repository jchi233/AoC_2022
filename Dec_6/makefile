#
# compile with -r8 or set MPI_DOUBLE to MPI_FLOAT
#
EXECUTABLE = john.x
#
FLAGS  = -O0 -g -w
FCOMPL = mpic++
LIBS   = -lgfortran
#
.SUFFIXES: .F90
.F90.o:
	$(FCOMPL) -c $(FLAGS) $< 
#
FOBJECTS = attempt_1.o
#
OBJECTS = $(FOBJECTS) $(COBJECTS)
#
APPLIC:	$(OBJECTS)
	$(FCOMPL) $(FLAGS) -o $(EXECUTABLE) $(OBJECTS) $(LIBS)
#
clean:
	rm -f $(OBJECTS)
#
