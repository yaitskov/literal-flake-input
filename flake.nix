# nix develop --profile .ndc --command true
# nix develop ./.ndc
{
  description = "VPN bypass";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/ec57e782751e30b7b97907a4b14a7b584e14f3ec";
    flake-utils.url = "github:numtide/flake-utils";
    uphack = {
      url = "github:yaitskov/upload-doc-to-hackage";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, uphack }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcName = "ghc9122";
        packageName = "literal-flake-input";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.${ghcName};
        inherit (pkgs.haskell.lib) dontHaddock;
      in {
        packages.${packageName} = dontHaddock (haskellPackages.callCabal2nix packageName self rec {});
        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = [ haskellPackages.haskell-language-server ] ++ (with pkgs; [
            ghcid
            cabal-install
            pandoc
            openssl
            (import uphack { inherit pkgs; })
          ]);
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
          shellHook = ''
            export PS1='N$ '
            echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
          '';
        };
        devShell = self.devShells.${system}.default;
      });
}
