#Formální informace
 - **Předmět**: Funkcionální a logické programování<br>
 - **Název projektu**: funkcionální projekt: Haskell<br>
 - **Varianta**: 4 PLG-2-NKA<br>
 - **Akademický rok**: 2021/2022<br>
 - **Autor**: Richard Klem<br>
 - **E-mail**: xklemr00@stud.fit.vutbr.cz<br>
 - **Datum**: 10.4.2022<br>
 
# Překlad a spuštění
Přeložit projekt lze pomocí příkazu `make`, nebo `make compile`.
Program nabízí 4 přepínače:
 - `-h` vypíše nápovědu,
 - `-i` vypíše načtenou pravou lineární gramatiku,
 - `-1` vypíše pravou regulární gramatiku převedenou ze vstupní PLG,
 - `-2` vypíše nedeterministický konečný automat převedený ze vstupní PLG

a 2 volby zadání vstupu:
 - za přepínačem následuje cesta k souboru, kde je uložen vstup ve správném formátu,
 - nebo za přepínačem nenásleduje žádný další znak, a pak je vstupní PLG načtena ze standartního vstupu.

Příklad spustění:
```
./flp21-fun -1 testfile01
```
```
./flp21-fun -2
A, B
a
A
A->aB
B->#
```

# Testy
Je možné spustit sadu regresních testů, které mi sloužily pro kontrolu, zda jsem při vývoji nerozbil něco, co již bylo správně.
Nesloužily tedy k nějakému Test Driven Development způsobu vývoje.
Spuštění testů:
```
make test
```

# Speciální chování programu
Program automaticky dopočítá z pravidel případně chybějící neterminály a terminály.
Dále provádí lehkou minimalizaci automatu v případě, že máme sémanticky shodná pavidla.
Např. pravidla `A->aaA` a `A->aaaA`, pak lze dva vygenerované stavy sloučit.