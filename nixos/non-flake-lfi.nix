{ config
, lib
, pkgs
, ...
}: let
  #literal-flake-input-router = (builtins.getFlake "github:yaitskov/literal-flake-input").packages.x86_64-linux.vpn-router;
  lfi = (builtins.getFlake "path:/home/don/pro/haskell/my/literal-flake-input/literal-flake-input").packages.x86_64-linux.vpn-router;
in (import ./flake-lfi.nix lfi) { inherit config; inherit lib; inherit pkgs; }
