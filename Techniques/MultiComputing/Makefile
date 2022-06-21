TARGET = STM_Basics
FLAGS = -threaded

$(TARGET): $(TARGET).hs
	ghc $(FLAGS) $(TARGET).hs

run: $(TARGET)
	./$(TARGET) +RTS -N2

clear:
	rm -rdf *.hi *.o $(TARGET)
