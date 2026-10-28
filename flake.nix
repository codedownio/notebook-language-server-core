{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/7993f34ee47e28a3d607a525aadc6c94b73be728";

  # A shell, not a build. This package ships no binary -- the proxies that depend on it build
  # it themselves through haskell.nix -- so all it needs is a toolchain for stack to use, the
  # same one CI runs the tests with.
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
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
        }
    );
}
