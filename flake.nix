# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "nix flake for aoc2022";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellpackages = pkgs.haskell.packages.ghc92;

        jailbreakunbreak = pkg:
          pkgs.haskell.lib.dojailbreak (pkg.overrideattrs (_: { meta = { }; }));

        packagename = "aoc2022";
      in {
        packages.${packagename} =
          haskellpackages.callcabal2nix packagename self rec {
            # dependency overrides go here
            unique = jailbreakunbreak haskellpackages.unique;
          };

        defaultpackage = self.packages.${system}.${packagename};

        devshell = pkgs.mkshell {
          buildinputs = with pkgs; [
            haskellpackages.haskell-language-server
            ghcid
            cabal-install
            haskellpackages.hp2pretty
            haskellpackages.ghcprofview
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
