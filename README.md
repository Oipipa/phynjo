To run this project, make sure you have stack: 

```
stack build
stack test
stack run -- exe:physics-edsl > stdout_file.extension
```

You can change which example to run in `physics-edsl.cabal` somewhere around:

```
executable physics-edsl
  hs-source-dirs:
    app
  main-is:
    examples/example09.hs <------ HERE!!!
```

This project is basically somewhat of an answer to the age-old question: 

> *Could we treat every step of a classical-mechanics simulation as nothing more than a lambda-calculus reduction?*

In other words: if **all** updates are pure functions, can we assemble them with ordinary function composition and still end up with a useful, reasonably fast physics engine?

I decided to try the abstract this stuff in Haskell because Haskell is already “λ-calculus with types”.  Everything you’ll see in the codebase follows the same pattern:

```
step :: Δt -> world -> world
```

where `world` is an immutable value and `step` is a lambda that rewrites that value. The final result was an EDSL that I call "Phynjo". It managed to do a few things well: 

1. I can rebuild an entire scheme (Euler → leap-frog → Forest–Ruth) by changing NumericRule nodes; the interpreter never changes. Energy drift plots that I made by passing the simulation data to LLMs (namely, Chatgpt's 'analysis' features) confirm: Euler drifts linearly, leap-frog stays bounded, Forest–Ruth is flat to machine precision for $10^5$ steps. No bugs surfaced when I changed gravity for a spring—only the force lambda changed, which was convenient.
2. Running the derived equations with leap-frog preserves energy to within 0.01 % over 20 000 steps which is the same accuracy as the hand-coded equations. I changed the potential term (+k/2 (θ₁−θ₂)²) and re-ran without touching any numeric code.

There were still disappointments, though. For starters, every map lookup is O(log n); at 5000 bodies the gravity step dominates. A mutable Vector keyed by Int would be faster, but would break the “everything pure” elegance unless I push it behind ST. Barnes–Hut could be a next step if this project were to be optimized for demanding performances. In any case, the current Map approach is clearly not scalable beyond a few thousand bodies. There's also the issue in the rigid-body kick where I renormalise the orientation every half-step. It works, but the corrective factor introduces a small non-symplectic
error; energy grows at $10^{-4}$ per orbit. A proper exponential map
update (or a Lie–group integrator) would fix this, but again, a possible to-do.

Also the CAS is laughably incompetent (go figure as that wasn't the priority here) for generating readable equations. No pattern simplification means the symbolic mass-matrix grows bulky (deep nest of sums) and the simplifier pass is slow (seconds) for anything beyond 3-DOF. It's good enough for the demos I include here, but a real CAS would be required for ten degrees of freedom. 

Either way, before the numerical approaches, the first step was to define 'flags' (like events and such).

I began with collisions.  A contact sensor is really just a Boolean: “is the distance negative?”
So I defined

```haskell
newtype Literal = Literal (Set Component)
```

and wrote a small record

```haskell
data EventRule = EventRule
  { erDomain :: Set Component           -- the slice I’m allowed to touch
  , erStep   :: Literal -> Literal      -- pure flag update
  , erEvents :: Int -> Literal -> Phenomenon
  }
```

`erStep` only rewrites flags in `erDomain`; the rest of the set is left intact.  Because it is pure I can unit-test a rule with one line: feed it a literal, look at the result, compare to expectation.

Boolean rules need to be put together, sometimes sequentially, sometimes in parallel, so I introduced

```haskell
data EventWorkflow
  = ERun EventRule
  | ESeq EventWorkflow EventWorkflow
  | EPar EventWorkflow EventWorkflow
```

`ESeq` is function composition over time; `EPar` is parallel evaluation provided the two domains are disjoint.  Both constructors lower to a generic `Process` AST.  One interpreter walks that AST each tick; the interpreter never cares whether the leaves came from contact sensors or numeric forces.

The numeric side is the same idea but with `NState`, a record that holds two maps, `q` and `p`.  An atomic update is

```haskell
data NumericRule = NumericRule
  { nrDomain :: Set Component
  , nrStep   :: Double -> NState -> NState
  }
```

Kinematic drift and Newtonian gravity are like \~15-line lambdas that fill such a record.  Because they are pure, I can write QuickCheck properties like “energy change is `O(dt²)`” and they run deterministically.

Those lambdas are composed with the **same** three constructors:

```haskell
WRun drift
WSeq kick drift              -- Euler
WSeq drift½ (WSeq kick drift½)  -- leap-frog
```

The interpreter for numeric workflows is identical in shape to the Boolean one.

The point is that an integrator is nothing more than an AST of lambdas. Running the simulation is ordinary β-reduction carried out by the interpreter.

Anyway, I'll be walking through how each of the core modules work in the following sections.

### `EventWorkflow.hs`

`EventWorkflow` is one of the modules for the Boolean layer.  Each `EventRule` is like a small chip: it consumes the current flag set, possibly flips a bit or two, and possibly emits an event.  A useful simulation, however, needs many chips working together.  I considered dropping straight into `Action` composition, but that loses the semantic difference between “do these two rules **now**” and “do one, then on the next tick do the other”.  The workflow datatype keeps that distinction explicit.

```haskell
data EventWorkflow
  = ERun EventRule
  | ESeq EventWorkflow EventWorkflow
  | EPar EventWorkflow EventWorkflow
```

`ERun` is the trivial wrapper; `ESeq` and `EPar` are the thing.

Sequential composition is just function composition over time.  If I have a state `(tick, flags)` and I give it to `w1`, the result becomes the input to `w2`.  The interpreter does exactly that and nothing more; it does not go inside the rules.

It's worth noting that parallel composition needs a safety check: two branches must not update the same flag.  That requirement is enforced at run time by computing the union of `erDomain`s.  If the intersection is non-empty the interpreter throws.

`toProcess` lowers the workflow to the generic `Process` defined earlier. The translation is structural.  `ERun` becomes `PAct (ruleToAction r)`, `ESeq` becomes `PSeq`, and so on.  That means the only interpreter I need to maintain is `applyProcessWorld / Phen`, and Boolean workflows piggy-back on it automatically.

So, as an example, suppose I want a contact sensor that, once it sees the flag `onGround`, waits one tick and then clears it.  I write two rules:

```haskell
setRule    = … -- sets onGround when penetration detected
clearRule  = … -- clears onGround unconditionally
```

Then

```haskell
sensorWF = ESeq (ERun setRule) (ERun clearRule)
```

gives me exactly one-tick hysteresis.  If I add a second, unrelated sensor I can combine them safely:

```haskell
fullWF = EPar sensorWF otherSensorWF
```

because their domains are disjoint.

### `EventRule.hs` 

The Boolean part of the language needed a truly atomic unit of behaviour: “look at a handful of flags right now, decide what those flags should be on the next tick, possibly announce that something happened.”  That atomic unit is `EventRule`.

```haskell
data EventRule = EventRule
  { erDomain :: Set Component
  , erStep   :: Literal -> Literal
  , erEvents :: Int -> Literal -> Phenomenon
  }
```

*`erDomain`* is the whole key to composability.  If I write a rule for a door latch (`latchOpen`, `latchClosed`) I can guarantee no other rule touches those two flags by declaring them here.  Later, when I run two rules in parallel, the interpreter checks that the domains are disjoint before it merges their results.  Without this field I would end up silently overwriting someone else’s flag.

*`erStep`* is completely pure: given only the slice of flags in the domain it produces the slice for the next tick.  Because it is pure, QuickCheck and unit tests can nail the behaviour down with no IO and no global state.  I deliberately excluded the global tick from `erStep`; the tick lives in `erEvents` because it matters only for time-stamping events.

*`erEvents`* is also pure.  Many rules don’t emit anything, so they just return `emptyPhen`.  When a rule does emit, it receives the same domain slice and the integer tick and can build any `Phenomenon` it wants—typically via the helper `epsilon component tick`.

Embedding a rule into the engine is handled by `ruleToAction`.  The function literally splits the full world literal into two pieces (`domain` and `outside`), runs `erStep` on the first, pastes the result back together with the second, and increments the tick.  That is all.  The phenomenon function is forwarded directly.  After the conversion an `EventRule` behaves like any other `Action`, which means the generic `Process` interpreter can execute it without knowing anything about domains or events.

In the grand scheme of things, `EventRule` is a data record of two lambdas and one set, but it is the very important pivot that lets Boolean logic plug into the exact same interpreter infrastructure as the numeric rules.  That uniformity pays off every time I add a new subsystem: I implement a pure function, declare its domain, and the rest of the engine will schedule it correctly.

### `NumericRule.hs` 

`NumericRule` is the atom of the continuous half of the DSL.  The idea makes the bold assumption that every bit of classical mechanics, no matter how complicated, can be written as a **pure function** that takes a timestep `dt` and an `NState` and returns a new `NState`.  Once that function is wrapped in a record together with the set of bodies it may touch, the interpreter can schedule it alongside any other rule.

```haskell
data NumericRule = NumericRule
  { nrDomain :: Set Component
  , nrStep   :: Double -> NState -> NState
  }
```

`nrDomain` enforces locality.  If I compose two rules in parallel and they try to update the same momentum entry, the merge step in `NumericWorkflow` will detect the overlap and abort.  That single set keeps the rest of the engine honest.

`nrStep` is kept completely unconstrained.  A rule may read both `q` and `p`, update one or both, allocate new maps, or reuse them.  The only promise is purity: the same inputs yield the same outputs.  That design made unit testing straightforward; I can build a tiny `NState`, call the function, and check an exact result.

Writing a rule is therefore just writing a lambda.  For example, the drift rule is

```haskell
nrStep = \dt st ->
  foldr
    (\(c,m) acc ->
       let q0 = lookupPos c acc
           p0 = lookupMom c acc
       in insertPos c (q0 + dt * p0 / m) acc)
    st
    masses
```

No monads, no mutation—just ordinary functional code.

The helper

```haskell
applyRule :: NumericRule -> Double -> NState -> NState
applyRule NumericRule{nrStep} = nrStep
```

exists mostly for readability in tests.  It hides the record selector and keeps call-sites short.

### `NumericWorkflow.hs` — design notes

The Boolean side had `EventWorkflow`; I needed the same idea for the numeric layer—a data structure that lets me put together a handful of `NumericRule`s into a single stepper without writing a bespoke interpreter for every integrator.  That became `NumericWorkflow`.

```haskell
data NumericWorkflow
  = WRun NumericRule
  | WSeq NumericWorkflow NumericWorkflow
  | WPar NumericWorkflow NumericWorkflow
```

`WRun` is the leaf: “apply this one rule”.
`WSeq a b` means “run `a`, then feed the result to `b`”, so it advances the world by *two* sub-steps.
`WPar a b` runs both branches on the *same* input state and merges the outputs in a single sub-step provided those branches touch disjoint bodies.

The interpreter is about forty lines.  The sequential case is trivial recursion.  The parallel case is slightly subtle: both branches read the same `NState`, so they must write to disjoint momentum/position maps or the merge would be ambiguous.  I check that with

```haskell
let da = workflowDomain a
    db = workflowDomain b
in  if Set.null (da `Set.intersection` db)
        then mergeStates …
        else error "NumericWorkflow: parallel overlap"
```

`mergeStates` unions the two `SLit` maps; because the domains are disjoint, `Map.union` is safe.

With that interpreter in place the higher-level integrator functions become simple AST builders.  The leap-frog constructor returns

```haskell
WSeq (WRun driftHalf) (WSeq (WRun kickFull) (WRun driftHalf))
```

and Forest–Ruth is just `foldr1 WSeq (map mkSlice coeffs)`. 

### The CAS: `SymbolicPhysics.SymbolicD`

I'd like to point out that the goal here was not to build a full computer-algebra system but to capture *just enough* structure to derive Euler–Lagrange equations automatically.

The datatype is a straightforward expression tree:

```haskell
data Expr
  = Var  String
  | Const Double
  | Neg  Expr
  | Add  Expr Expr
  | Sub  Expr Expr
  | Mul  Expr Expr
  | Div  Expr Expr
  | Pow  Expr Expr
  | Sin  Expr | Cos Expr | Tan Expr
  | Exp  Expr | Log Expr
```

Anything beyond this would require me putting some extra effort which is obviously outrageous.

Anyway, for everyday work I wrote smart constructors `var`, `constant`, `add` etc.—thin wrappers that save parentheses when building expressions manually.  They don’t simplify; the simplifier runs in a separate pass.

`deriv` is probably the most important of the module (in the context of this project at least).  It is a direct transcription of the textbook rules, case by case, including the chain rule for composite functions and a special case for `Pow u (Const n)` so that `x^3` produces the familiar `3 x^2` instead of the more general `x^3 (2 log x + 3/x)` formula.  That saves me from writing a rewrite pass later.

I decided to add a simplifier early on because the derivative of even moderately simple Lagrangian balloons in size. I settled on three passes which were enough for the context of my project: 

1. Distribute multiplication over sums so terms align.
2. Local constant folding and flattening of nested `Add` and `Mul`.  A set
   of helper functions collects all additive or multiplicative children in
   a list, sums or multiplies constants, and rebuilds.
3. Combine like terms by inserting them into a `Map Expr Double`; this
   reduces `x + x` to `2 x` and eliminates zero coefficients.

`eval` is a plain interpreter from `Expr` to `Double`.  Unbound variables
default to zero so I can partially evaluate an expression easily.  In most
cases I pass a complete environment anyway.

What I do not implement os symbolic integration, pattern-based trig simplification, `sqrt`, piecewise functions.  For the examples I care about—double pendulum, spring–mass, rigid body in spherical coordinates— plain algebraic differentiation is enough.

The payoff shows up in `Physics.Lagrangian`.  There I build expressions for kinetic and potential energies, give them to `deriv` twice, simplify, and out drops an explicit update rule that I convert into a `NumericRule`.  I can tweak the symbolic Lagrangian and immediately get a new simulator.