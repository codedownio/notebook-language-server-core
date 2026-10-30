{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/7993f34ee47e28a3d607a525aadc6c94b73be728";

  # Matches the proxies' pin, so the dependency closure is shared.
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/1dde0b46d3daa5bf0ff3ea6332d579a1943b4a0c";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # This ships no binary; the haskell.nix project exists so CI can run the test
  # suite from .#tests without a writable ~/.stack. `nix develop` still has stack.
  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        compiler-nix-name = "ghc9124";

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
          inherit (haskellNix) config;
        };

        flake = (pkgs.haskell-nix.hix.project {
          src = gitignore.lib.gitignoreSource ./.;
          evalSystem = system;
          inherit compiler-nix-name;
          modules = [ ./nix/os-string-module.nix ];
        }).flake {};

        tests = flake.packages."notebook-language-server-core:test:notebook-language-server-core-test";

      in
        {
          packages = {
            inherit tests;
            default = tests;

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          };

          devShells.default = pkgs.mkShell {
            NIX_PATH = "nixpkgs=${pkgs.path}";
            buildInputs = with pkgs; [
              stack
              # Matches the resolver in stack.yaml, which sets system-ghc.
              haskell.compiler.ghc9124

              # pcre-light, and zlib for the usual transitive dependents.
              pcre
              zlib
            ];
          };

          inherit flake;
        }
    );
}
