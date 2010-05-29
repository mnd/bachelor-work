all: library.hs Main.hs dynamic.gen ../process.hs
	ghc --make ../process.hs
	cpp dynamic.gen | grep -v '^#' | ../process  > Simple.hs
	ghc --make library.hs Simple.hs SimpleTemplate.hs Main.hs
