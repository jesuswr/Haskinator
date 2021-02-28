
all: Haskinator

Haskinator: Haskinator.hs Oraculo.hs out
	ghc -main-is Haskinator -outputdir out Haskinator.hs -o Haskinator
	
out:
	mkdir out

clean:
	if test -d "out"; then rm -rf "out"; fi
	if test -f "Haskinator"; then rm "Haskinator"; fi
