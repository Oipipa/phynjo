import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def additional_artifact(PATH):
    R = 0.02  
    m = 0.0027
    I = 2/3 * m * R**2 

    df = pd.read_csv(PATH)
    t = df['t'].values 
    w = df['wz'].values 

    L_rot = I * w 

    plt.figure(figsize=(8,5), dpi=100)
    plt.plot(t, L_rot, '.', label='L_rot data', alpha=0.7)
    plt.xlabel('Time t (s)', fontsize=14)
    plt.ylabel('Rotational Angular Momentum L_rot (kg·m²/s)', fontsize=14)
    plt.title('L_rot vs t with Linear Fit', fontsize=16)
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.tight_layout()
    plt.show()
