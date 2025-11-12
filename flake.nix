{
  description = "A small server for hosting materials";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    typix = {
      url = "github:loqusion/typix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      typix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        sbcl' = pkgs.sbcl.withPackages (
          ps: with ps; [
            hunchentoot
            spinneret
            cl-fad
            cl-ppcre
            split-sequence
            serapeum
            anaphora
          ]
        );
      in
      {
        packages.default = pkgs.writeShellScriptBin "matfyz-server" ''
          ${sbcl'}/bin/sbcl --eval "(push #p\"./\" asdf:*central-registry*)" \
                            --eval "(asdf:load-system :matfyz)" \
                            --eval "(matfyz:start-server)" \
                            --eval "(loop (sleep 1))"
        '';

        packages.typst = import ./typst.nix {
          typixLib = typix.lib.${system};
          inherit pkgs;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sbcl'
          ];
        };
      }
    );
}
