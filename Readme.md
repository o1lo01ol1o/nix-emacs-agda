# nix-emacs-agda

## Overview

Provides a nix shell containing an `emacs` overlay and any `agda` packages specified in shell nix.

## Cachix support

[Install cachix](https://app.cachix.org/cache/nix-emacs-agda#pull) and then use it download any prebuilt derivations available.

```bash
cachix use nix-emacs-agda
```
to download current builds instead of building them locally.