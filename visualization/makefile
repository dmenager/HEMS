DFLAGS=-g -O
BIN=hems_visualizer
OBJ=hems_visualizer.o microservice.o process_range.o

.PHONY: clean all

all: $(BIN)

clean:
	rm -- $(OBJ) $(BIN)

$(BIN): $(OBJ)
	ldc2 $(DFLAGS) $^ -of $@
	
%.o: %.d
	ldc2 $(DFLAGS) -c $< -of$@
