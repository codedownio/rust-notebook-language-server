{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/9c5956641f45b6b02607e318485aad01c18e65b0"; # Was master
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        compiler-nix-name = "ghc9122";

        overlays = [
          haskellNix.overlay

          # Set enableNativeBignum flag on compiler
          (import ./nix/overlays/native-bignum.nix { inherit compiler-nix-name; })

          # Configure hixProject
          (import ./nix/overlays/hix-project.nix { inherit compiler-nix-name gitignore system; })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        flake = (pkgs.hixProject compiler-nix-name).flake {};
        flakeStatic = (pkgs.pkgsCross.musl64.hixProject compiler-nix-name).flake {};
        flakeDarwin = (pkgs.pkgsCross.aarch64-darwin.hixProject compiler-nix-name).flake {};
        flakeAarch64Linux = (pkgs.pkgsCross.aarch64-multiplatform.hixProject compiler-nix-name).flake {};
        flakeStaticAarch64Linux = (pkgs.pkgsCross.aarch64-multiplatform-musl.hixProject compiler-nix-name).flake {};

        packageForGitHub' = systemToUse: rnls: pkgs.runCommand "rust-notebook-language-server-${rnls.version}-${systemToUse}" {} ''
          name="rust-notebook-language-server-${rnls.version}-${systemToUse}"

          mkdir -p to_zip
          cp -r ${rnls}/* to_zip
          mkdir -p $out
          tar -czvf $out/$name.tar.gz -C to_zip .
        '';

      in
        {
          devShells = {
            default = pkgs.mkShell {
              NIX_PATH = "nixpkgs=${pkgs.path}";
              buildInputs = with pkgs; [
                haskell.compiler.ghc9122

                pcre
                zlib
              ];
            };
          };

          packages = (rec {
            inherit (pkgs) cabal2nix stack;

            default = static;

            static = flakeStatic.packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            dynamic = flake.packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            darwin = flakeDarwin.packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            aarch64Linux = let
              executable = flakeAarch64Linux.packages."rust-notebook-language-server:exe:rust-notebook-language-server";
            in pkgs.callPackage ./nix/package-bundled.nix {
              binaryDrv = executable;
              binaryName = "rust-notebook-language-server";
            };
            staticAarch64Linux = flakeStaticAarch64Linux.packages."rust-notebook-language-server:exe:rust-notebook-language-server";

            grandCombinedGithubArtifacts = pkgs.symlinkJoin {
              name = "rust-notebook-language-server-grand-combined-artifacts";
              paths = [
                (packageForGitHub' "x86_64-linux" self.packages.x86_64-linux.static)
                (packageForGitHub' "aarch64-linux" aarch64Linux)
                (packageForGitHub' "x86_64-darwin" self.packages.x86_64-darwin.dynamic)
                (packageForGitHub' "aarch64-darwin" self.packages.aarch64-darwin.dynamic)
              ];
            };

            # No GMP (we test the dynamic builds to make sure GMP doesn't end up in the static builds)
            verify-no-gmp = pkgs.writeShellScriptBin "verify-no-gmp.sh" ''
              echo "Checking for libgmp in ${dynamic}/bin/rust-notebook-language-server"
              (echo "$(ldd ${dynamic}/bin/rust-notebook-language-server)" | grep libgmp) && exit 1

              exit 0
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
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
