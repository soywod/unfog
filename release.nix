let
  unfog = import ./default.nix;
  pkgs = import <nixpkgs> {};
  haskellNix = import (fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);

  # pkgsNative = import <nixpkgs> haskellNix;
  # pkgsLinux = import <nixpkgs> (haskellNix // { crossSystem = pkgs.lib.systems.examples.musl64; });
  # pkgsWindows = import <nixpkgs> (haskellNix // { crossSystem = pkgs.lib.systems.examples.mingwW64 ;});
  pkgsx86_64 = import <nixpkgs> (haskellNix // { crossSystem = pkgs.lib.systems.examples.x86_64-embedded ;});

  # native = unfog { pkgs = pkgsNative; };
  # crossLinux = unfog { pkgs = pkgsLinux; };
  # crossWindows = unfog { pkgs = pkgsWindows; };
  crossx86_64 = unfog { pkgs = pkgsx86_64; };

in {
  # unfogNative = native.unfog.components.exes.unfog;
  # unfogLinux = crossLinux.unfog.components.exes.unfog;
  # unfogWindows = crossWindows.unfog.components.exes.unfog;
  unfogx86_64 = crossx86_64.unfog.components.exes.unfog;
}
