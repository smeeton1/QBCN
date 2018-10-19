CC = gfortran
CFLAGS = 
LIB = 
path?= obj/
BINDIR?= mod

OBJECTS = $(path)/graph.o          \
	  $(path)/jump.o          \
	  $(path)/density.o  \
	  $(path)/LineTest.o \
	  $(path)/Dran.o
	  
$(path)/%.o : %.F90
	$(CC) $(CFLAGS) -c $<

Dran: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LIB)
	
	
graph.o:
jump.o:
density.o:
LineTest.o: density.o
Dran.o:LineTest.o density.o jump.o graph.o

rm mod/*.mod

clean: 
	rm Dran $(path)/*.o 
