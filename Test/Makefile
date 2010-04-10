

all: Integer.hs Complex.hs Main.hs dynamic.gen ../process.hs
	ghc --make ../process.hs
	cpp dynamic.gen | grep -v '^#' | ../process  > Dynamic.hs
	ghc --make Integer.hs Complex.hs Dynamic.hs Main.hs
