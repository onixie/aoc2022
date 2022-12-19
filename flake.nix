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

        haskellPackages = pkgs.haskell.packages.ghc92.extend (hself: hsuper: rec{
          gloss-rendering = pkgs.haskell.lib.doJailbreak hsuper.gloss-rendering;
        }
        );

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "aoc2022";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            Unique = jailbreakUnbreak haskellPackages.Unique;
            gloss  = jailbreakUnbreak haskellPackages.gloss;
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            ghcid
            cabal-install
            haskellPackages.hp2pretty
            haskellPackages.ghcprofview
            libGL
            libGLU
            freeglut
            mesa
            glfw
            xorg.libX11
            xorg.libXinerama
            xorg.libXcursor
            xorg.libXi
            xorg.libXrandr
            xorg.libXxf86vm
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
