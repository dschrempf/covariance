{
  description = "Covariance estimation development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";

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
        hpkgs = pkgs.haskell.packages.ghc921;
        hlib = pkgs.haskell.lib;
        covariance = hpkgs.callCabal2nix "covariance" ./. rec { };
        covariance-dev = hlib.doBenchmark covariance;
      in
      {
        packages.covariance = covariance;

        defaultPackage = covariance;

        devShell = hpkgs.shellFor {
          packages = _: [ covariance-dev ];
          buildInputs = with pkgs; [
            # See https://github.com/NixOS/nixpkgs/issues/59209.
            bashInteractive
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}
