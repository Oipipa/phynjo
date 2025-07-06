import numpy as np, pandas as pd, matplotlib.pyplot as plt

def plot_trajectory(PATH):
    df = pd.read_csv(PATH)
    x, y = df['x'], df['y']
    plt.figure(figsize=(10,6), dpi=100)
    plt.plot(x, y, linewidth=2)
    plt.gca().set_aspect('equal', 'box')
    plt.xlabel('x (m)', fontsize=14)
    plt.ylabel('y (m)', fontsize=14)
    plt.title('Ball Trajectory', fontsize=16)
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.tight_layout()
    plt.show()
