#Project name: FLP 2021/2022 – funkcionální projekt: Haskell
#Login: xklemr00
#Author: Richard Klem
#Year: 2022

NAME=main
OUTNAME=flp21-fun

all: run

run:
	./$(OUTNAME)

install: compile

compile: clean
	ghc -Wall -o $(OUTNAME) $(NAME).hs

clean:
	rm $(OUTNAME) *.o *.hi

zip:
	zip -r flp-fun-xklemr46.zip Makefile *.hs doc/ tests/
