# nix develop --profile .ndc --command true
# nix develop ./.ndc
{
  description = "Literal Flake Inputs";
  inputs = {
    c = { url = https://lficom.me/static/false; flake = false; };
    nixpkgs.url = "github:NixOS/nixpkgs/bc16855ba53f3cb6851903a393e7073d1b5911e7";
    flake-utils.url = "github:numtide/flake-utils";
    uphack = {
      url = "github:yaitskov/upload-doc-to-hackage";
      flake = false;
    };
    hnix-store = {
      url = "git+https://github.com/yaitskov/hnix-store.git?ref=cryptonite-ghost&submodules=1";
      flake = false;
    };
    hnix = {
      url = "github:yaitskov/hnix/rip-crytonite";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, uphack, c, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcName = "ghc9122";
        packageName = "literal-flake-input";
        hnix-overlay = final: prev: {
          hnix-store-json =
            dontCheck
              (final.callCabal2nix "hnix-store-json" "${inputs.hnix-store}/hnix-store-json" { });
          hnix-store-tests =
            final.callCabal2nix "hnix-store-tests" "${inputs.hnix-store}/hnix-store-tests" { };
          hnix-store-nar =
            final.callCabal2nix "hnix-store-nar" "${inputs.hnix-store}/hnix-store-nar" { };
          hnix-store-core =
            final.callCabal2nix "hnix-store-core" "${inputs.hnix-store}/hnix-store-core" { };
          hnix-store-remote =
            dontHaddock
              (final.callCabal2nix "hnix-store-remote" "${inputs.hnix-store}/hnix-store-remote" { });
          hnix = dontCheck (final.callCabal2nix "hnix" inputs.hnix { });
        };
        mkStatic = pkName:
          let
            pkgs = import nixpkgs {
              inherit system;
              crossSystem = "x86_64-unknown-linux-musl";
              overlays = [
                (final: prev: {
                  haskell = prev.haskell // {
                    compiler = prev.haskell.compiler // {
                      ${ghcName} = prev.haskell.compiler.${ghcName}.override {
                        enableRelocatedStaticLibs = true;
                        enableShared = false;
                        enableDwarf = false;
                      };
                    };
                  };
                })
              ];
            };
            staticExtraLibs = [
              "--ghc-option=-optl=-static"
              "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgs.numactl.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${pkgs.zlib.static}/lib"
              "--extra-lib-dirs=${pkgs.libelf.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            ];

            compressElf = drv:
              drv.overrideAttrs(oa: {
                postInstall = (oa.postInstall or "") + ''
                  ${pkgs.upx}/bin/upx -9 $out/bin/literal-flake-input
                  ${pkgs.upx}/bin/upx -9 $out/bin/e
                '';
              });

            assertStatic = drv:
              drv.overrideAttrs(oa: {
                postInstall = (oa.postInstall or "") + ''
                  for b in $out/bin/*
                  do
                    if ldd "$b"
                    then
                      echo "ldd succeeded on $b, which may mean that it is not statically linked"
                      exit 1
                    fi
                  done
                '';});

            makeStatic = drv:
              drv.overrideAttrs(oa:
                { configureFlags = (oa.configureFlags or []) ++ staticExtraLibs;
                  disallowGhcReference = false;
                  disallowedRequisites = [];
                });

            haskellPackagesO = pkgs.haskell.packages.${ghcName};
            inherit (pkgs.haskell.lib) dontCheck justStaticExecutables;
            haskellPackages = haskellPackagesO.override (old:
              {
                overrides = builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
                  [ (final: prev: { vector = dontCheck prev.vector; })
                    hnix-overlay
                  ];
              });
          in
            assertStatic (compressElf (assertStatic (makeStatic (justStaticExecutables
              (haskellPackages.callCabal2nix pkName self rec {})))));
        ## dynamic
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs.haskell.lib) dontHaddock dontCheck;
        haskellPackages = pkgs.haskell.packages.${ghcName}.extend(hnix-overlay);
      in {
        # haskellProjects.default = {
        #   packages = {
        #     hnix.source = inputs.hnix;
        #   };
        # };
        packages.${packageName} =
          if (import c { inherit pkgs; }).static then
            mkStatic packageName
          else
            dontHaddock (haskellPackages.callCabal2nix packageName self rec {});
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

        nixosModules.default = import ./nixos/flake-lfi.nix (self.packages.${system}.${packageName});
      });
}
