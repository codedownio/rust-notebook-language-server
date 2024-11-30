{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
  inputs.nixpkgsMaster.url = "github:NixOS/nixpkgs/master";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs, nixpkgsMaster }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        compiler-nix-name = "ghc966";

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
                  packages.rust-notebook-language-server.components.exes.rust-notebook-language-server.dontStrip = false;
                }];
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        flake = (pkgs.hixProject compiler-nix-name).flake {};
        flakeStatic = (pkgs.pkgsCross.musl64.hixProject compiler-nix-name).flake {};

        packageForGitHub = rnls: pkgs.runCommand "rust-notebook-language-server-${rnls.version}" {} ''
          name="rust-notebook-language-server-${rnls.version}-x86_64-linux"

          mkdir -p $out
          cp ${rnls}/bin/rust-notebook-language-server $out/$name

          tar -czvf $name.tar.gz $name
        '';

      in
        {
          packages = (rec {
            inherit (pkgs) cabal2nix stack;

            default = flakeStatic.packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            dynamic = flake.packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            packaged = packageForGitHub default;

            # No GMP (we test the dynamic builds to make sure GMP doesn't end up in the static builds)
            verify-no-gmp = pkgs.writeShellScriptBin "verify-no-gmp.sh" ''
              echo "Checking for libgmp in ${dynamic}/bin/rust-notebook-language-server"
              (echo "$(ldd ${dynamic}/bin/rust-notebook-language-server)" | grep libgmp) && exit 1

              exit 0
            '';

            nixpkgsPath = let
              pkgsMaster = import nixpkgsMaster { inherit system; };
            in
              pkgsMaster.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgsMaster.path}";
          });

          inherit flake;
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
