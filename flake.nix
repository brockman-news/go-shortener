{
  description = "Really simple URL shortener";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";
  };

  outputs = { self, nixpkgs }: let
    supportedSystems = [ "x86_64-linux" "aarcg64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
  in rec {
    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      package = pkgs.haskellPackages.callPackage ./default.nix {};
    in {
      default = package;
      go-shortener = package;
    });

    devShell = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      package = pkgs.haskellPackages.callPackage ./default.nix {};
    in package.env.overrideAttrs (old: old // {
      buildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
    }));

    nixosModules.default = { config, lib, pkgs, ... }:
    let
      cfg = config.services.go-shortener;
    in with lib; {
      options.services.go-shortener = {
        enable = mkEnableOption "go-shortener";
        port = mkOption { type = types.port; };
        keyLength = mkOption { type = types.int; default = 8; };
        endpoint = mkOption { type = types.str; };
      };

      config = mkIf (cfg.enable) {
        services.redis.servers.go-shortener.enable = true;
        services.redis.package = pkgs.valkey;

        systemd.services.go-shortener = {
          description = "Simple URL shortener";
          wantedBy = [ "multi-user.target" ];
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
          serviceConfig = {
            Type = "simple";
            Restart = "always";
            ExecStart = "${packages.${config.nixpkgs.hostPlatform.system}.go-shortener}/bin/go-shortener";
            DynamicUser = true;
          };
          environment = {
            PORT = toString cfg.port;
            KEY_LENGTH = toString cfg.keyLength;
            ENDPOINT = toString cfg.endpoint;
          };
        };
      };
    };
  };
}
