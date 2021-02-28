
all: Haskinator

out:
	mkdir out

Haskinator: Haskinator.hs Oraculo.hs
	ghc -main-is Haskinator -outputdir out Haskinator.hs -o Haskinator
	
clean:
	if test -d "out"; then rm -rd "out"; fi
	if test -f "Haskinator"; then rm "Haskinator"; fi
