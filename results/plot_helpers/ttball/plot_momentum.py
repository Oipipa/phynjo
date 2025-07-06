import numpy as np, pandas as pd, matplotlib.pyplot as plt

def plot_momentum(PATH):
    m = 0.0027

    df = pd.read_csv(PATH)
    t = df['t']
    px = m * df['vx']
    py = m * df['vy']

    plt.figure(figsize=(10,6), dpi=100)
    plt.plot(t, px, label='pₓ', linewidth=2)
    plt.plot(t, py, label='pᵧ', linewidth=2)
    plt.xlabel('Time (s)', fontsize=14)
    plt.ylabel('Momentum (kg·m/s)', fontsize=14)
    plt.title('Linear Momentum Components', fontsize=16)
    plt.legend(fontsize=12)
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.tight_layout()
    plt.show()
