CC = gfortran
CFLAGS = -g
LIB = 
path= obj
BINDIR= mod

OBJECTS = $(path)/walker.o \
	  $(path)/measure.o \
	  $(path)/rules.o  \
	  $(path)/walk_test.o  
	  
$(path)/%.o : src/%.F90
	$(CC) $(CFLAGS) -c $< -o $@ -J$(BINDIR)

Cwlk: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LIB)
	
	
walker.o:
measure.o:
rules.o: walker.o
walk_test.o: walker.o measure.o rules.o


#rm mod/*.mod

clean: 
	rm Cwlk $(path)/*.o 
