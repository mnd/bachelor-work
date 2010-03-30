

all: Integer.hs Complex.hs Main.hs
	../process.awk Integer.hs Complex.hs > Dynamic.hs
	ghc --make Integer.hs Complex.hs Dynamic.hs Main.hs
