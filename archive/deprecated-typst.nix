{ pkgs, typixLib }:
let
  inherit (pkgs) lib;
  inherit (lib) strings;
  fs = lib.fileset;

  unstable_typstPackages = [
    {
      name = "touying";
      version = "0.6.1";
      hash = "sha256-bTDc32MU4GPbUbW5p4cRSxsl9ODR6qXinvQGeHu2psU=";
    }
    {
      name = "touying";
      version = "0.5.3";
      hash = "sha256-T5RsYTxtmptxYFyi3Xn2ueMYUzUQPXq3ndwBHEnwnwo=";
    }
    {
      name = "tiaoma";
      version = "0.3.0";
      hash = "sha256-WkbmsZaVbLUgoToI966vR+92DF0KMBMUMKilegpJvWA=";
    }
    {
      name = "numbly";
      version = "0.1.0";
      hash = "sha256-j3kolGf2gAJiXGymYpmtWiOJbtOvjQz8xxVjxKmjDQw=";
    }
  ];

  # TODO: clean source
  src = ./.;
  commonArgs = {
    typstOptsString = "--format pdf --input store-path=\"$out\"";
    inherit src unstable_typstPackages;
  };

  buildTypstProjects =
    paths:
    lib.attrsets.listToAttrs (
      map (name: {
        name = baseNameOf (dirOf name);
        value = typixLib.buildTypstProject (
          commonArgs
          // {
            # typix wants typst source as a relative path
            typstSource = (strings.removePrefix ((toString ./.) + "/") (toString name));
          }
        );
      }) paths
    );

  typstFiles = path: fs.fileFilter (file: file.hasExt "typ") path;

  basic-rust-files = typstFiles ./basic-rust;

  basic-rust-labs = builtins.filter (path: strings.hasPrefix "lab-" (baseNameOf (dirOf path))) (
    fs.toList basic-rust-files
  );

  basic-rust-lectures = builtins.filter (
    path: !(strings.hasPrefix "lab-" (baseNameOf (dirOf path)))
  ) (fs.toList basic-rust-files);
in
{
  # TODO: add an output that combines all
  basic-rust = {
    lectures = buildTypstProjects basic-rust-lectures;
    labs = buildTypstProjects basic-rust-labs;
  };
}
