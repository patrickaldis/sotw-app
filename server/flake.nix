{
  description = "A test package for using the servant library";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            serverProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc928";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell = {
                name = "servant-shell";
                tools = {
                  # cabal = { };
                  # hlint = "latest";
                  haskell-language-server = "latest";
                };

                # Non-Haskell shell tools go here
                buildInputs = with pkgs; [ nixpkgs-fmt git-crypt docker-compose ];
              };
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.serverProject.flake {
          # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
          # crossPlatforms = p: [p.ghcjs];
        };
      in flake // {
        # Built by `nix build .`
        packages.default = flake.packages."Server:exe:Server";
      });
}

