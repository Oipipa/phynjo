# physics-spells

An Embedded-DSL in Haskell for composing and running discrete “spells” and numerical integrators on physical systems.  Supports:

- **Boolean‐literal DSL** for high‐level processes (runes → actions → processes).  
- **Numeric DSL** for 1D position & momentum integration (drift, kick, leapfrog).  
- **Unit‐checked quantities** via `Numeric.Units.Dimensional`.  
- **Composable forces**: built-in `Gravity`, `Spring`, `Drag` plus arbitrary `Custom` fields.  
- **Examples**: Hooke oscillator, two‐body gravity, drag + free-fall.

---

## Installation

```bash
git clone https://github.com/Oipipa/physics-spells.git
cd physics-spells
stack build
````

To run the primary executable:

```bash
stack run -- exe:physics-edsl
```

This will print a CSV trace to stdout; redirect into a file:

```bash
stack run -- exe:physics-edsl > traj.csv
```

---

## Project layout

```
src/
  Action.hs, Process.hs, Rune.hs, Spell.hs, Constraint.hs
  Literal.hs, Transition.hs
  Components.hs, UnitLiteral.hs, System.hs, System/SystemForces.hs
  NState.hs, ScalarLiteral.hs
  Physics/
    DriftNR.hs, GravNR.hs, EulerNR.hs, LeapfrogNR.hs
    Force.hs, ForceNR.hs, Leapfrog1D.hs, ForceDSL.hs
app/
  Main.hs            # primary CSV‐tracing example
test/
  ActionSpec.hs, ProcessSpec.hs, …
  Physics/…Spec.hs
```

* **Boolean DSL**:

  * `Rune` ↔ `Action` ↔ `Process`
  * `Spell` combines `Rune`s into high‐level spells on `Literal`

* **Unit literals**:

  * `UnitLiteral` provides type‐safe maps of `Quantity u Double`
  * `System` & `SystemForces` bridge unit‐checked to raw SI

* **Numeric DSL**:

  * `NRune` (single‐tick on `NState`) → `NSpell` (sequential/parallel)
  * `DriftNR`, `GravNR`, `ForceNR`, `Leapfrog1D`

* **Force combinators**:

  * `Force` datatype: `Gravity`, `Spring`, `Drag`, `Custom`
  * `(<+>)` and `scaleF` for combining/scaling forces
  * `ForceDSL.addForces`: wraps into velocity‐Verlet step

---

## Example Simulations

Change:

```
  main-is:
    examples/example{x}.hs
```

according to your needs (there's example simulations in the `example/` directory).

1. Example01 setup: We have two point masses (5 kg and 1 kg) starting 10 m apart, at rest. We apply pure Newtonian attraction (Gravity `gConst = 1`) and step forward with a leapfrog integrator. 
2. Example02 setup:  We have a 1D two-body Hooke's simulator
3. Example03 setup: 1-D Newtonian two-body “free-fall” or “mutual‐attraction” test. We've got: Two point masses (5 kg and 1 kg) sitting 10 m apart on a line. They start at rest (zero initial momentum). We apply only the built-in Gravity force (with our chosen G constant) and step forward with the same leapfrog integrator.
4. Example04 setup: 2-D orbit scenario with a sun and a planet.

---

## Testing

Run the full test suite:

```bash
stack test
```

Each module has an `…Spec.hs` in `test/…` covering its behavior.