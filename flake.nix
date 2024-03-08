{
  description = "subtube";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
  };

  outputs = inputs@{ self, flake-utils, devshell, nixpkgs, ... }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              subtube = hfinal.callCabal2nix "subtube" ./. { };
            };
        };
        subtube = final.haskellPackages.subtube;
      };
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay devshell.overlays.default ];
          };
          ghcVersion = "ghc948";
          hspkgs = pkgs.haskell.packages.${ghcVersion};
          devShells.shellFor = hspkgs.shellFor {
            packages = p: [ p.subtube ];
            withHoogle = true;
          };
        in {
          devShells.default = pkgs.devshell.mkShell {
            name = "subtube";
            imports = [ (pkgs.devshell.importTOML ./devshell.toml) ];
            packagesFrom = [ devShells.shellFor ];
            commands = [
              (let cabal = pkgs.cabal-install;
              in {
                name = cabal.pname;
                help = cabal.meta.description;
                package = cabal;
                category = "tools";
              })
              (let hls = hspkgs.haskell-language-server;
              in {
                name = hls.pname;
                help = hls.meta.description;
                package = hls;
                category = "tools";
              })
            ];
            packages = [ hspkgs.hlint hspkgs.fourmolu hspkgs.cabal-fmt ];
          };
          packages = rec {
            default = subtube;
            subtube = pkgs.subtube;
          };
        };
    in { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;

}
