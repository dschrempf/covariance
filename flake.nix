{
  description = "Covariance estimation development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            # haskell-overlay = (
            #   selfn: supern: {
            #     haskellPackages = supern.haskellPackages.override {
            #       overrides = selfh: superh: {};
            #     };
            #   }
            # );
            # overlays = [ ];
            pkgs = import nixpkgs {
              inherit system;
            };
            covariance = pkgs.haskellPackages.callCabal2nix "covariance" ./. rec {};
            covariance-dev = pkgs.haskell.lib.doBenchmark covariance;
          in
            {
              packages.covariance = covariance;

              defaultPackage = covariance;

              devShell = pkgs.haskellPackages.shellFor {
                packages = _: [ covariance-dev ];
                buildInputs = with pkgs; [
                  # See https://github.com/NixOS/nixpkgs/issues/59209.
                  bashInteractive
                  haskellPackages.cabal-install
                  haskellPackages.haskell-language-server
                ];
                doBenchmark = true;
                withHoogle = true;
              };
            }
      );
}
