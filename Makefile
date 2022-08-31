HC := haskell-compiler
OUTPUT := apstats
INPUT := $(OUTPUT).hs
LIB_NAME := APStats

exec:
	$(HC) $(INPUT)

clean:
	rm -rf *.o *.hi $(LIB_NAME)/*.o $(LIB_NAME)/*.hi
	rm -rf $(OUTPUT)
