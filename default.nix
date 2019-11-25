{ pkgs ? import <nixpkgs> (import (fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz)) }:

let
  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [
      # these extras will provide additional packages
      # ontop of the package set.  E.g. extra-deps
      # for stack packages. or local packages for
      # cabal.projects
    ];
    modules = [
      (let
        staticLibs = [
          (pkgs.gmp6.override { withStatic = true; })
          (pkgs.libffi.overrideAttrs (oldAttrs: {
            dontDisableStatic = true;
            configureFlags = (oldAttrs.configureFlags or []) ++ [
              "--enable-static"
              "--disable-shared"
            ];
          }))
        ];

        withFullyStatic = {
          configureFlags = [
              "--disable-executable-dynamic"
              "--disable-shared"
              "--ghc-option=-optl=-pthread"
              "--ghc-option=-optl=-static"
            ] ++ map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
          };
      in {
        packages.unfog.components.exes.unfog = withFullyStatic;
      })
    ];
  };

in pkgSet.config.hsPkgs // { _config = pkgSet.config; }
