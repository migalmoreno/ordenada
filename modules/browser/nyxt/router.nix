{
  inputs,
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "nyxt"
    "router"
  ];
  options = {
    routers = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      description = "A list of nx-router routers.";
      example = [
        {
          name = "google";
          type = "redirector";
          route = ".*google.com.*";
          redirect = "https://search.atlas.engineer";
        }
      ];
      default = [ ];
    };
    showBlockBanner = ordenada-lib.mkEnableTrueOption "showing a banner upon navigating to a blocked route";
    extraConfig = lib.mkOption {
      type = lib.types.lines;
      description = "Extra configuration to be put in nx-router config file.";
      default = '''';
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      xdg.dataFile."nyxt/extensions/nx-router".source = inputs.nx-router;
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-nx-router";
        config =
          with config.ordenada.features.nyxt.router; # lisp
          ''
            (define-configuration router:blocker
              ((router:block-banner-p ${ordenada-lib.lisp.toBoolean showBlockBanner})))

            (define-configuration router:router-mode
              ((router:routers
                (list
                 ${
                   toString (
                     map (router: ''
                       (make-instance 'router:${router.type}
                         ${lib.optionalString (builtins.hasAttr "name" router) ''
                           :name "${router.name}"
                         ''} :route "${router.route}"
                         ${lib.optionalString (builtins.hasAttr "redirect" router) ''
                           :redirect ${ordenada-lib.lisp.toVal router.redirect}
                         ''}
                         ${
                           toString (
                             lib.mapAttrsToList (n: v: '':${n} "${v}"'') (
                               lib.removeAttrs router [
                                 "type"
                                 "name"
                                 "route"
                                 "redirect"
                               ]
                             )
                           )
                         })
                     '') routers
                   )
                 }))))

            (defmethod nyxt:on-signal-load-finished
                :around ((mode nyxt/mode/history:history-mode) url)
              (call-next-method mode (router:trace-url url)))

            (defmethod nyxt/mode/bookmark:bookmark-current-url
                :around (&optional (buffer (current-buffer)))
              (setf (url buffer) (router:trace-url (url buffer)))
              (call-next-method buffer))

            (define-command copy-url ()
              "Save nx-router traced URL to clipboard."
              (let ((url (render-url (router:trace-url (url (current-buffer))))))
                (copy-to-clipboard url)
                (echo "~a copied to clipboard." url)))

            (define-configuration web-buffer
              ((default-modes `(router:router-mode ,@%slot-value%))))

            ${extraConfig}
          '';
        lispPackages = [ "nx-router" ];
      };
    };
}
