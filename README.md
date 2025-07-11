This is a functional EDSL built on top of haskell for modeling problems in classical mechanics, rigid body mechanics and rotational kinematics.

![](results/samples/1.gif)

To run this project, make sure you have stack:

```md
stack build
stack test
stack run -- exe:physics-edsl > stdout_file.extension
```

You can change which example to run in physics-edsl.cabal somewhere around:

```md
executable physics-edsl
  hs-source-dirs:
    app
  main-is:
    examples/example09.hs <------ HERE!!!
```

Implementation notes [here](README.pdf). 