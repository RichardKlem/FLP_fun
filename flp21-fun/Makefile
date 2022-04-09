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

run:
	./$(OUTNAME)

install: compile

compile: clean
	ghc -Wall -o $(OUTNAME) src/$(LIB).hs src/$(UTILS).hs src/$(TYPES).hs app/$(MAIN).hs && make clean

cleanall:
	rm -f $(OUTNAME) src/*.o src/*.hi app/*.o app/*.hi

clean:
	rm -f src/*.o src/*.hi app/*.o app/*.hi

zip:
	zip -r flp-fun-xklemr46.zip Makefile *.hs doc/ tests/
