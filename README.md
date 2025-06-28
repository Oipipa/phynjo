# PhysiSpell

So, this is an Embedded Domain-Specific Language (EDSL) for physics simulations, something I made for my undergraduate thesis. Basically, it's built in Haskell and lets you define physics behaviors ("runes" for atomic effects and "spells" for composed actions) in a structured and modular way.

## What's here?

* The main library files (`src/`) include modules for:

  * **Components.hs** (stuff like particles or bodies)
  * **Literal.hs** (state definitions)
  * **Transition.hs** (changes to state)
  * **Action.hs** (basic actions)
  * **Process.hs** (composing actions in parallel or sequence)
  * **Rune.hs** (atomic physics laws)
  * **Spell.hs** (composing runes into more complex physics)
  * **Constraint.hs** (constraints and conservation laws)

* A little executable (`app/Main.hs`) that runs a simple simulation of a **three-body gravitational system** (like planets orbiting each other, simplified).

* Tests in (`test/`) to sanity-check everything’s working.

## How do you run this?

Make sure you've got [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then:

```bash
stack build
stack test
stack run
```

## What's the main simulation?

The example in `Main.hs` simulates a simple gravitational three-body system for one timestep using both Euler and Leapfrog integration methods, and prints out the resulting positions and momenta.
