import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_rotation_artifacts(PATH):
    R = 0.02 
    m = 0.0027
    I = 2/3 * m * R**2  

    # Load your data
    df = pd.read_csv(PATH)
    t  = df['t'].values
    x  = df['x'].values
    y  = df['y'].values
    vx = df['vx'].values
    vy = df['vy'].values
    w  = df['wz'].values
    L_rot = I * w
    L_orb = m * (x * vy - y * vx)
    L_tot = L_rot + L_orb
    dt = np.diff(t, prepend=t[0])
    tau = np.zeros_like(L_tot)
    tau[1:] = np.diff(L_tot) / dt[1:]
    def nice(ax):
        ax.grid(True, linestyle='--', alpha=0.5)
        ax.tick_params(labelsize=12)
        ax.title.set_fontsize(16)
        ax.xaxis.label.set_fontsize(14)
        ax.yaxis.label.set_fontsize(14)
    plt.figure(figsize=(10,6), dpi=100)
    ax = plt.gca()
    ax.plot(t, L_orb, label='Orbital $L_{orb}$', linewidth=2)
    ax.plot(t, L_rot, label='Rotational $L_{rot}$', linewidth=2)
    ax.plot(t, L_tot, '--', label='Total $L_{tot}$', linewidth=2)
    ax.set_xlabel('Time (s)')
    ax.set_ylabel('Angular Momentum (kg·m²/s)')
    ax.set_title('Angular Momentum vs Time')
    ax.legend(fontsize=12)
    nice(ax)
    plt.tight_layout()
    plt.show()

    # Plot Torque
    plt.figure(figsize=(10,6), dpi=100)
    ax = plt.gca()
    ax.plot(t, tau, linewidth=2)
    ax.set_xlabel('Time (s)')
    ax.set_ylabel('Torque τ (N·m)')
    ax.set_title('Instantaneous Torque vs Time')
    nice(ax)
    plt.tight_layout()
    plt.show()
