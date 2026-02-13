{ config
, lib
, pkgs
, ...
}: let
  #lfi = (builtins.getFlake "github:yaitskov/literal-flake-input").packages.x86_64-linux.literal-flake-input;
  lfi = (builtins.getFlake "path:/home/don/pro/haskell/my/literal-flake-input/literal-flake-input").packages.x86_64-linux.literal-flake-input;
in (import ./flake-lfi.nix lfi) { inherit config; inherit lib; inherit pkgs; }
