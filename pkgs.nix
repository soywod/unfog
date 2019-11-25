{
  extras = hackage:
    { packages = { unfog = ./unfog.nix; }; };
  resolver = "lts-14.11";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }