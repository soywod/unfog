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
      # specific package overrides would go here
      # example:
      #  packages.cbors.package.ghcOptions = "-Werror";
      #  packages.cbors.patches = [ ./one.patch ];
      #  packages.cbors.flags.optimize-gmp = false;
      # It may be better to set flags in stack.yaml instead
      # (`stack-to-nix` will include them as defaults).
    ];
  };

in pkgSet.config.hsPkgs // { _config = pkgSet.config; }
