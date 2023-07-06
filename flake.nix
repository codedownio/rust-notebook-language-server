{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject = compiler-nix-name:
              final.haskell-nix.hix.project {
                src = gitignore.lib.gitignoreSource ./.;
                evalSystem = "x86_64-linux";
                inherit compiler-nix-name;
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        # lsp-types = pkgs.haskell.packages.ghc8107.callPackage ./lsp-types.nix {};

        flake = compiler-nix-name: (pkgs.hixProject compiler-nix-name).flake {};
        flakeStatic = compiler-nix-name: (pkgs.pkgsCross.musl64.hixProject compiler-nix-name).flake {};

        allVersions = with pkgs.lib; (
          concatMap (name: [
            (nameValuePair name (flake name).packages."rust-notebook-language-server:exe:rust-notebook-language-server")
            (nameValuePair "${name}-static" (flakeStatic name).packages."rust-notebook-language-server:exe:rust-notebook-language-server")
          ]) [
            # "ghc902"
            "ghc945"
          ]
        );

        allVersionsAttrset = pkgs.lib.listToAttrs allVersions;

      in
        rec {
          packages = allVersionsAttrset // (rec {
            inherit (pkgs) cabal2nix;
            # inherit lsp-types;

            default = allVersionsAttrset.ghc945;
            defaultStatic = allVersionsAttrset.ghc945-static;

            # No GMP (we test the dynamic builds to make sure GMP doesn't end up in the static builds)
            verify-no-gmp = pkgs.writeShellScriptBin "verify-no-gmp.sh" ''
              echo "Checking for libgmp in ${default}/bin/rust-notebook-language-server"
              (echo "$(ldd ${default}/bin/rust-notebook-language-server)" | grep libgmp) && exit 1
            '';

            all = with pkgs.lib; pkgs.linkFarm "rust-notebook-language-server-all" (
              map (x: {
                inherit (x) name;
                path = x.value;
              }) allVersions
            );
          });

          inherit flake;

          nixpkgsPath = pkgs.path;
        }
    );

  # nixConfig = {
  #   # This sets the flake to use the IOG nix cache.
  #   # Nix should ask for permission before using it,
  #   # but remove it here if you do not want it to.
  #   extra-substituters = ["https://cache.iog.io"];
  #   extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  #   allow-import-from-derivation = "true";
  # };
}
