{
  lib,
  inputs,
  pkgs,
  ...
}:

{
  _module.args.ordenada-lib = rec {
    base16 = inputs.base16.lib { inherit lib pkgs; };
    nix-rice = inputs.nix-rice.lib.nix-rice;
    types = {
      fnOrAttrs = lib.mkOptionType {
        name = "Function or attribute set";
        description = "Type that is either a function returning an attribute set or an attribute set.";
        check = x: builtins.isFunction x || builtins.isAttrs x;
      };
    };
    mkKeybindings =
      bindings: optionalArgs:
      if (builtins.isFunction bindings) then (bindings optionalArgs) else bindings;
    mkEnableTrueOption =
      name:
      lib.mkOption {
        default = true;
        example = true;
        description = "Whether to enable ${name}.";
        type = lib.types.bool;
      };
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
        item: acc: acc ++ lib.optionals (builtins.hasAttr class item) item.${class}.imports
      ) [ ] (lib.mapAttrsToList (name: value: value) modules);
    mkNyxtLispConfig =
      pkgs:
      {
        name,
        config ? "",
        lispPackages ? [ ],
      }:
      let
        cfg = pkgs.writeText "${name}.lisp" ''
          (in-package :nyxt-user)
          ${config}
        '';
      in
      {
        config = ''
          (define-nyxt-user-system-and-load nyxt-user/${name}
          ${
            lib.optionalString (lispPackages != [ ]) ''
              :depends-on (${toString lispPackages})
            ''
          } :config-directory "${builtins.dirOf cfg}/"
            :components ("${builtins.baseNameOf cfg}"))
        '';
      };
    mkElispConfig =
      pkgs:
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
    lisp = {
      toList = v: '''(${toString (map (x: ''"${x}"'') v)})'';
      toBoolean = v: if v then "t" else "nil";
      toNilOr = v: v': if v == null then "nil" else v';
      toAlist = v: ''
            '(${
              toString (
                lib.mapAttrsToList (key: val: ''
                  ("${key}" "${val}")
                '') v
              )
            }
        )
      '';
    };
    elisp = {
      inherit (lisp)
        toAlist
        toBoolean
        toNilOr
        toList
        ;
    };
  };
}
