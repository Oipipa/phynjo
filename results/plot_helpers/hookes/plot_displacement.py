import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def displacement(PATH):
    df = pd.read_csv(PATH)
    m1 = 1.0
    m2 = 1.0 
    k  = 10.0
    L0 = 1.0 
    df["r"] = np.abs(df["x1"] - df["x2"])
    
    plt.figure()
    plt.plot(df["t"], df["r"], label="r(t)")
    plt.axhline(L0, linestyle="--", label="rest length L0")
    plt.xlabel("time  [s]")
    plt.ylabel("separation r  [m]")
    plt.title("Relative displacement of the two masses")
    plt.legend()
    plt.grid(True)
    plt.show()
