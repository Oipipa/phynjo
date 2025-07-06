import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def energy(PATH):
    df = pd.read_csv(PATH)
    m1 = 1.0
    m2 = 1.0 
    k  = 10.0
    L0 = 1.0 
    df["r"] = np.abs(df["x1"] - df["x2"])
    df["KE"]     = ((df["p1"]**2 / (2 * m1)) + (df["p2"]**2 / (2 * m2)))
    df["PE"]     = 0.5 * k * (df["r"] - L0)**2
    df["Tot_E"]  = df["KE"] + df["PE"]
    plt.figure()
    plt.plot(df["t"], df["Tot_E"])
    plt.xlabel("time  [s]")
    plt.ylabel("total energy  [J, arb. units]")
    plt.title("Energy conservation check")
    plt.grid(True)
    plt.show()
