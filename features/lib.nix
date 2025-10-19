{ pkgs, lib, ... }:

{
  _module.args.ordenada-lib = rec {
    transposeChild = child: parent: value: {
      ${parent} = {
        ${child} = value;
      };
    };
    transpose =
      attrs:
      (lib.pipe attrs [
        (lib.mapAttrsToList (parent: (lib.mapAttrsToList (transposeChild parent))))
        (lib.flatten)
        (lib.foldAttrs (item: acc: item // acc) { })
      ]);
    getClassModules =
      class: modules:
      lib.foldr (
        item: acc: acc ++ (if builtins.hasAttr class item then item.${class}.imports else { })
      ) [ ] (lib.mapAttrsToList (name: value: value) modules);
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
        pkg = pkgs.emacsPackages.melpaBuild {
          pname = name;
          version = "0.1.0";
          src = pkgs.writeText "${name}.el" ''
            ;;; ${name}.el --- ${summary} -*- lexical-binding: t -*-

            ${config}
            (provide '${name})'';
          packageRequires = elispPackages;
          ignoreCompilationError = true;
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
          inherit earlyInit;
        };
      });
  };
}
