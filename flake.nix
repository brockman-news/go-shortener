{
  description = "Really simple URL shortener";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
  in {
    overlays.default = self: super: {
      go-shortener = self.haskellPackages.callPackage ./default.nix {};
    };

    packages = forAllSystems (system: {
      default = (import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      }).go-shortener;
    });

    devShell = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      package = pkgs.haskellPackages.callPackage ./default.nix {};
    in package.env.overrideAttrs (old: old // {
      buildInputs = [ pkgs.cabal-install pkgs.cabal2nix pkgs.haskellPackages.ormolu ];
    }));

    nixosModules.default = { config, lib, pkgs, ... }:
    let
      cfg = config.services.go-shortener;
    in with lib; {
      options.services.go-shortener = {
        enable = mkEnableOption "go-shortener";
        port = mkOption { type = types.port; };
        keyLength = mkOption { type = types.int; default = 8; };
        endpoint = mkOption { type = types.str; default = 8888; };
      };

      config = mkIf (cfg.enable) {
        nixpkgs.overlays = [ self.overlays.default ];
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
            ExecStart = "${pkgs.go-shortener}/bin/go-shortener";
            DynamicUser = true;
            SupplementaryGroups= "redis-go-shortener";
          };
          environment = {
            PORT = toString cfg.port;
            KEY_LENGTH = toString cfg.keyLength;
            ENDPOINT = toString cfg.endpoint;
            REDIS_SOCKET = config.services.redis.servers.go-shortener.unixSocket;
          };
        };
      };
    };
  };
}
