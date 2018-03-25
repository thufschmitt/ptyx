> Sur les crédences, au salon vide : nul ptyx \
> Aboli bibelot d'inanité sonore,\
> Car le Maître est allé puiser des pleurs au Styx\
> Avec ce seul objet dont le Néant s'honore.

<cite>Stéphane Mallarmé, Sonnet en -yx.</cite>

# ptyx

A type-system for the Nix language.

[![Build Status](https://travis-ci.org/regnat/ptyx.svg?branch=master)](https://travis-ci.org/regnat/ptyx)

Ptyx is an experimental implementation of a
[type-system](http://docdro.id/BWWJov0) for [Nix](https://nixos.org/nix).
It aims at adding some type-safety to the language in the least intrusive way
possible. For this purpose, the design of the type system follows two main
principles:

- The type-system must embrace as much as possible the existing patterns
  (provided enough type annotations) instead of enforcing new easier-to-type
  patterns,
- Non-annotated code must typecheck with little to no modifications (although
  with very few static guaranties).
  This is made possible thanks to the use of a gradual type-system and the
  convention that non-annotated variables will be given the unknown type.

## Current status

The project is still in early stages and not usable yet.

Implemented:

- [X] Parsing of type-annotations
- [X] Typing of the core language
- [X] Recursive types
- [ ] Lists
- [ ] Records
- [ ] Gradual typing
- [ ] Clean error messages

## How to build

### With stack

Assuming you have `stack` installed, just run `stack install` to install the
`ptyx` executable in `~/.local/bin/`

### With Nix

The project also comes with a
[stack2nix](https://github.com/input-output-hk/stack2nix)-generated nix file
(which is hopefully up-to-date with the `stack.yaml` file). To use it, just run
`nix-build` from the root of the repository. The executable should be in
`./result/bin/ptyx`
