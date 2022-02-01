TARGET = wordsfreq

$(TARGET).out: $(TARGET).hs
	ghc $(TARGET).hs -o $(TARGET).out

run: $(TARGET).out
	./$(TARGET).out

clear:
	rm -rf *.hi *.out *.o
	clear && exa
