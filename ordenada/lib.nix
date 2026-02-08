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
    getFnOrAttrsValue =
      fnOrAttrSet: optionalArgs:
      if (builtins.isFunction fnOrAttrSet) then (fnOrAttrSet optionalArgs) else fnOrAttrSet;
    attrsToFlags =
      {
        separator ? "=",
      }:
      attrs:
      builtins.concatStringsSep " " (
        lib.mapAttrsToList (name: value: "--${name}${separator}${toString value}") attrs
      );
    mkEnableTrueOption =
      name:
      lib.mkOption {
        default = true;
        example = true;
        description = "Whether to enable ${name}.";
        type = lib.types.bool;
      };
    mkLiteral = value: {
      _type = "literal";
      inherit value;
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
    str = {
      capitalize = s: (lib.toUpper (lib.substring 0 1 s)) + (lib.substring 1 (lib.stringLength s - 1) s);
    };
    wm = {
      mkWindowRule =
        config: app: commands:
        let
          appId = if lib.isString app then null else app.app-id or null;
          appTitle = if lib.isString app then app else app.title or app.app;

          wmEnabled = wm: config.ordenada.features.${wm}.enable or false;

          swayCommands =
            (lib.optional (commands.layout or null == "floating") "floating enable")
            ++ (lib.optional (commands.layout or null == "tiling") "floating disable")
            ++ (lib.optional (commands.width or null != null) "resize set width ${toString commands.width} px");

          aerospaceCommands =
            (lib.optional (commands.layout or null != null) "layout ${commands.layout}")
            ++ (lib.optional (commands.width or null != null) "resize width ${toString commands.width}");

          # Sway-specific window rule
          swayRule = {
            wayland.windowManager.sway.config.window.commands = map (cmd: {
              command = cmd;
              criteria = if appId != null then { app_id = appId; } else { title = ".*${appTitle}.*"; };
            }) swayCommands;
          };

          # Aerospace-specific window rule
          aerospaceRule = {
            programs.aerospace.settings.on-window-detected = [
              {
                "if" =
                  if appId != null then { "app-id" = appId; } else { "window-title-regex-substring" = appTitle; };
                run = aerospaceCommands;
              }
            ];
          };
        in
        lib.mkMerge [
          (lib.mkIf (wmEnabled "sway") swayRule)
          (lib.mkIf (wmEnabled "aerospace") aerospaceRule)
        ];
    };
    lisp = rec {
      toVal =
        v:
        if builtins.isAttrs v then
          toAlist v
        else if builtins.isList v then
          toList v
        else if builtins.isString v then
          ''"${v}"''
        else if builtins.isInt v then
          toString v
        else if builtins.isBool then
          toBoolean v
        else
          v;
      toVal' =
        v:
        if builtins.isAttrs v then
          toAlist' v (_: v: toVal' v)
        else if builtins.isList v then
          toList' v toVal'
        else if builtins.isString v then
          ''"${v}"''
        else if builtins.isInt v then
          toString v
        else if builtins.isBool v then
          toBoolean v
        else
          v;
      toList' = v: f: toString (map f v);
      toList = v: "'(${toList' v toVal'})";
      toBoolean = v: if v then "t" else "nil";
      toNilOr = v: v': if v == null then "nil" else v';
      toAlist' =
        v: f:
        toString (
          lib.mapAttrsToList (name: val: ''
            ("${name}" ${f name val})
          '') v
        );
      toAlist = v: "'(${toAlist' v (_: v: toVal' v)})";
    };
    elisp = {
      inherit (lisp)
        toAlist
        toAlist'
        toBoolean
        toNilOr
        toList
        toList'
        toVal
        toVal'
        ;
    };
    darwin = rec {
      mkAgent =
        {
          type ? "home",
        }:
        config: label: options:
        let
          configKey = if (type == "home") then "config" else "serviceConfig";
          agentOptions = builtins.removeAttrs options [ "config" ];
          agentConfig = options.config or { };
        in
        lib.mkMerge [
          (lib.mkIf (type == "home") { enable = true; })
          agentOptions
          {
            "${configKey}" =
              with config.ordenada.features;
              lib.mkMerge [
                {
                  RunAtLoad = true;
                  Label = "org.ordenada.${type}.${label}";
                  StandardErrorPath = "${userInfo.homeDirectory}/Library/Logs/ordenada/${type}/${label}/error.log";
                  StandardOutPath = "${userInfo.homeDirectory}/Library/Logs/ordenada/${type}/${label}/output.log";
                }
                agentConfig
              ];
          }
        ];
      mkHomeAgent = mkAgent { type = "home"; };
      mkSystemAgent = mkAgent { type = "system"; };
    };
  };
}
