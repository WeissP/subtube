{
  description = "A very basic flake";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    sqitchNix.url =
      "github:NixOS/nixpkgs/46a1cd0c97578127286ee672ee1d85e30a18e847";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
    # coddRepo.url = "github:mzabani/codd";
  };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  outputs = inputs@{ self, flake-utils, haskellNix, devshell, nixpkgs, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            subtube = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc948";
            };
          })
          devshell.overlays.default
        ];
        sqitchPkgs = import inputs.sqitchNix { inherit system; };
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.subtube.flake {
          # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
          # crossPlatforms = p: [p.ghcjs];
        };
        shellWithToml = tomls:
          pkgs.subtube.shellFor {
            exactDeps = false;
            withHoogle = true;
            inputsFrom = [
              (pkgs.devshell.mkShell {
                name = "subtube";
                imports = map pkgs.devshell.importTOML
                  ([ ./env_config/common.toml ] ++ tomls);
                commands = [{
                  name = "sqitch";
                  package = sqitchPkgs.sqitchPg;
                  category = "DB";
                }];
              })
            ];
            tools = {
              cabal = "latest";
              fourmolu = "latest";
              hlint = "latest";
              cabal-fmt = "latest";
              ghcid = "latest";
              haskell-language-server = "latest";
            };
          };
      in flake // {
        # Built by `nix build .`
        packages.default = flake.packages."subtube:exe:subtube-api-server";
        packages.test = flake.packages."subtube:test:subtube-test";
        devShells = {
          default = shellWithToml [ ];
          frontend = shellWithToml [ ./env_config/frontend.toml ];
          backend = shellWithToml [ ./env_config/backend.toml ];
        };
      });
}
