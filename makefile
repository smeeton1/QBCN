CC = gfortran
CFLAGS = -g
LIB = 
path= obj
BINDIR= mod

OBJECTS = $(path)/walker.o \
	  $(path)/measure.o \
	  $(path)/rules.o  \
	  $(path)/walk_test.o 
	  
OBJECTS1 = $(path)/walker.o \
	   $(path)/measure.o \
	   $(path)/rules.o  \
	   $(path)/1_node_ai.o
	   
OBJECTS2 = $(path)/walker.o \
	   $(path)/measure.o \
	   $(path)/rules.o  \
	   $(path)/ai.o

All: Cwlk Ai1l Ai6l
	   
$(path)/%.o : src/%.F90
	$(CC) $(CFLAGS) -c $< -o $@ -J$(BINDIR)

Cwlk: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LIB)

Ai1l: $(OBJECTS1)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS1) $(LIB)
	
Ai6l: $(OBJECTS2)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS2) $(LIB)
	
walker.o:
measure.o: walker.o
rules.o: walker.o measure.o 
walk_test.o: walker.o measure.o rules.o
1_node_ai.o: walker.o measure.o rules.o
ai.o: walker.o measure.o rules.o

#rm mod/*.mod

clean: 
	rm Cwlk Ai1l Ai6l $(path)/*.o 
