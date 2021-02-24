

Haskinator:
	ghc -main-is Haskinator Haskinator.hs -o Haskinator


clean:
	rm Haskinator
	rm *.hi
	rm *.o