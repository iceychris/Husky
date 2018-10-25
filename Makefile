PROJECT=husky

all: build

regen:
	cabal2nix . > default.nix
	cabal2nix --shell . > shell.nix

build: regen
	nix-build release.nix

shell: regen
	nix-shell shell.nix

run:
	./result/bin/$(PROJECT)

clean:
	rm -f default.nix shell.nix result 
