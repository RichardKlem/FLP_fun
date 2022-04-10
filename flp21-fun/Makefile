#Project name: FLP 2021/2022 – funkcionální projekt: Haskell
#Login: xklemr00
#Author: Richard Klem
#Year: 2022

MAIN=Main
LIB=Lib
UTILS=Utils
TYPES=Types
OUTNAME=flp21-fun

all: compile

run: help

test:
	chmod +x test.sh
	./test.sh

help:
	./$(OUTNAME) -h

compile: clean
	ghc -Wall -o $(OUTNAME) src/$(LIB).hs src/$(UTILS).hs src/$(TYPES).hs app/$(MAIN).hs && make clean

cleanall:
	rm -f $(OUTNAME) src/*.o src/*.hi app/*.o app/*.hi *.zip

clean:
	rm -f src/*.o src/*.hi app/*.o app/*.hi

zip:
	zip -r flp-fun-xklemr00.zip Makefile src/ app/ test/
