NAME=main
OUTNAME=flp21-fun

all: $(NAME).hs
	ghc $(NAME).hs -o $(OUTNAME) -Wall

clean:
	rm -f $(NAME).hi $(NAME).o

cleanall:
	rm -f $(NAME).hi $(NAME).o $(OUTNAME)
