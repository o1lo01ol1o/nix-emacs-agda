# nix-emacs-agda

## Overview

Provides a nix shell containing an `emacs` overlay and any `agda` packages specified in shell nix.  It provides the `standard-library` and `cubical` by default.

## Cachix support

[Install cachix](https://app.cachix.org/cache/nix-emacs-agda#pull) and then use it download any prebuilt derivations available.

```bash
cachix use nix-emacs-agda
```
to download current builds instead of building them locally.



## Installing Doom Emacs with Nix


``Nixpkgs`` maintains a set of ``agda`` libraries that can be added to a
derivation managed by the nix package manager,
see [here](https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/agda.section.md)
for details.
The file ``shell.nix`` in our repository contains a derivation that will add ``emacs``, ``agda``, the ``agda standard library``,
and ``cubical agda`` to your local nix store and subsequently to a local shell environment by adding these locations to your ``PATH``.

However, because user configurations for ``emacs`` are mutable,
it will not (easily) manage your (emacs configuration) dot-files,
so we will use the underlying ``emacs`` provided by ``nixpkgs`` but install ``doom emacs`` normally in your local user's environment.

1. Install ``doom emacs`` (or whichever text editor you prefer)
   via the method described for your operating system [as here](https://github.com/hlissner/doom-emacs)
   
   (If you are on Windows with NixOS on WSL2 then you are a linux
   user for the rest of the installation and should do everything in a termial inside NixOS.)


2. Install ``Nix`` (*not* ``NixOS``) using following the guidance
   `on the official site <https://nixos.org/download.html#nix-install-linux>`_.
   We install the single-user version for linux
   (compare this with what is written on the official website):

   .. code::

      sh <(curl -L https://nixos.org/nix/install) --no-daemon

   If you are on MacOS this will be different, and if you are on Windows using NixOS
   then this should also be exactly what you need.

3. In the root directory of this project enter a `nix-shell`: 

   ```bash 
   nix-shell
   ```
4. Run doom from within the `nix-shell`:  
   ```bash 
   doom run
   ```

5. ``doom`` should run if configured correctly.  Using doom or your favorite editor, edit ``.doom.d/init.el``
   and *replace*  ``;; agda`` in the ``;; lang`` section with ``(agda +local)`` to tell doom to use the ``agda-mode``
   version specified by the local environment.
   Once the file is saved, sync ``doom`` from within the ``nix-shell`` that was loaded above:

   ```bash 

      doom sync
      ```

8. You can now load the agda source code in this by starting doom from the nix-shell:

   ```bash 

      doom run .
      ```


