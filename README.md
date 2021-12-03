---
title: "Advent of Code 2021"
subtitle: "solutions, literally in Haskell"
author: "Johan Hidding"
---
[![Entangled badge](https://img.shields.io/badge/entangled-Use%20the%20source!-%2300aeff)](https://entangled.github.io/)
[![Github badge](https://img.shields.io/badge/github-clone%20me-%44ee55ff)](https://github.com/jhidding/aoc2021/)

# Advent of Code 2021
This year I will publish my solutions for Advent of Code in Haskell, using Entangled to do **Literate Programming**.

## Advent of Code
[Advent of Code](https://www.adventofcode.com) is an anual coding challenge keeping nerds off the street for the entire merry month of decemeber. This is officially the best way to learn a new programming language or improve on your existing skills.

{.warning}
> ### Spoiler warning
> If you're still trying to solve AOC2021, this site contains spoilers.

## Entangled
[Entangled](https://entangled.github.io) is a tool for Literate Programming. My challenge for this years' Advent of Code is to create a set of beautifull solutions, that are completely documented in a literate form. The idea is that the code you see here is the complete solution to a problem. Think of Entangled as a content-management system for code blocks in your Markdown documents. The code blocks are assembled into compilable code, while changes are also tracked back to your markdown files. This means you can still debug and work with your favourite IDE.

## Instructions
To run this code, I recommend installing Haskell using the [GHCUp installer](https://www.haskell.org/ghcup/). Run all solutions:

```bash
cabal run x2021 -- -a
```

## Generic remarks
All solutions use the `RIO` library to replace the standard `Prelude`. This saves a long list of standard imports and is much better suited to modern Haskell practices. Most of the input parsing is done through `Megaparsec`, for which I have a submodule that contains some common types and functions, see the [boilerplate section](#appendix-boiler-plate).

## License
This code is licensed under the Apache v2 license, see `LICENSE` file in this repository.

