{ lib, pkgs, ... }:

{
  mkIf = p: v: if p then v else "";
  mkNilOr = v: v': if v == null then "nil" else v';
  mkAlist = v: ''
        '(${
          toString (
            lib.mapAttrsToList (key: val: ''
              ("${key}" "${val}")
            '') v
          )
        }
    )
  '';
  mkList = v: '''(${toString (map (x: ''"${x}"'') v)})'';
  mkBoolean = v: if v then "t" else "nil";
  mkElispConfig =
    {
      name,
      config,
      elispPackages ? [ ],
      summary ? "No description provided",
      earlyInit ? null,
      addToInitEl ? true,
    }:
    let
      pkg = pkgs.emacsPackages.trivialBuild {
        pname = name;
        version = "0.1.0";
        src = pkgs.writeText "${name}.el" ''
          ;;; ${name}.el --- ${summary} -*- lexical-binding: t -*-

          ${config}
          (provide '${name})'';
        propagatedBuildInputs = elispPackages;
      };
    in
    {
      extraPackages = epkgs: elispPackages ++ [ pkg ];
      extraConfig = ''
        ${if addToInitEl then "(require '${pkg.pname})" else ""} 
      '';
    }
    // (lib.optionalAttrs (earlyInit != null) {
      init = {
        enable = true;
        earlyInit = earlyInit;
      };
    });
}
