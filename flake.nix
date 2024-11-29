{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        compiler-nix-name = "ghc963";

        overlays = [
          haskellNix.overlay

          # Set enableNativeBignum flag on compiler
          (final: prev: {
            haskell-nix = let
              shouldPatch = name: compiler: prev.lib.hasPrefix compiler-nix-name name;

              overrideCompiler = name: compiler: (compiler.override {
                enableNativeBignum = true;
              });
            in
              prev.lib.recursiveUpdate prev.haskell-nix {
                compiler = prev.lib.mapAttrs overrideCompiler (prev.lib.filterAttrs shouldPatch prev.haskell-nix.compiler);
              };
          })

          # Configure hixProject
          (final: prev: {
            hixProject = compiler-nix-name:
              final.haskell-nix.hix.project {
                src = gitignore.lib.gitignoreSource ./.;
                evalSystem = system;
                inherit compiler-nix-name;

                modules = [{
                  packages.rust-notebook-language-server.components.exes.rust-notebook-language-server.dontStrip = true;
                }];
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        flake = compiler-nix-name: (pkgs.hixProject compiler-nix-name).flake {};
        flakeStatic = compiler-nix-name: (pkgs.pkgsCross.musl64.hixProject compiler-nix-name).flake {};

        packageForGitHub = rnls: pkgs.runCommand "rust-notebook-language-server-${rnls.version}" {} ''
          name="rust-notebook-language-server-${rnls.version}-x86_64-linux"

          mkdir -p $out
          cp ${rnls}/bin/rust-notebook-language-server $out/$name

          tar -czvf $name.tar.gz $name
        '';

      in
        {
          packages = (rec {
            inherit (pkgs) cabal2nix;

            default = (flake compiler-nix-name).packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            defaultStatic = (flakeStatic compiler-nix-name).packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            packaged = packageForGitHub defaultStatic;

            # No GMP (we test the dynamic builds to make sure GMP doesn't end up in the static builds)
            verify-no-gmp = pkgs.writeShellScriptBin "verify-no-gmp.sh" ''
              echo "Checking for libgmp in ${default}/bin/rust-notebook-language-server"
              (echo "$(ldd ${default}/bin/rust-notebook-language-server)" | grep libgmp) && exit 1

              exit 0
            '';
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
