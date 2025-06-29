# PhysiSpell

This is the starting code for an Embedded Domain-Specific Language (EDSL) for physics simulations, something I am making for my undergraduate thesis. It's built in Haskell and lets you define physics behaviors ("runes" for atomic effects and "spells" for composed actions) in a structured and modular way. This project is scoped for classical Newtonian mechanics so nothing is too extravagent sadly. 

## What's here?

* The main library files (`src/`) include a bunch of modules for different stuff. 
* A little executable (`app/Main.hs`) that runs a simple simulation.
* Tests in (`test/`) to sanity-check via unit-tests so everything’s working.

## How do you run this?

Make sure you've got [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then:

```bash
stack build
stack test
stack exec physics-edsl > {some csv name}.csv
```

## What's the main simulation?

The example in `Main.hs` simulates a simple gravitational setup of two bodies. Energy and momentum conservation is there and the values are plausible.