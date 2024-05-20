DFLAGS=-g -O
OBJ=hems_visualizer.o microservice.o


hems_visualizer: $(OBJ)
	ldc2 $(DFLAGS) $^ -of $@
	
%.o: %.d
	ldc2 $(DFLAGS) -c $< -of$@
