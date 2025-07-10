import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

CSV_PATH = "../data/doublePendulum.csv" 
L1 = 1.0 
L2 = 1.0 
m1 = 1.0 
m2 = 1.0 
g = 9.81

df = pd.read_csv(CSV_PATH)
required_cols = {"θ1", "ω1", "θ2", "ω2", "t"}
if not required_cols.issubset(df.columns):
    missing = ", ".join(required_cols - set(df.columns))
    raise ValueError(f"Missing column(s) in CSV: {missing}")

t = df["t"].to_numpy()
θ1 = df["θ1"].to_numpy()
ω1 = df["ω1"].to_numpy()
θ2 = df["θ2"].to_numpy()
ω2 = df["ω2"].to_numpy()

x1 = L1 * np.sin(θ1)
y1 = -L1 * np.cos(θ1)

x2 = x1 + L2 * np.sin(θ2)
y2 = y1 - L2 * np.cos(θ2)

x1_dot = L1 * ω1 * np.cos(θ1)
y1_dot = L1 * ω1 * np.sin(θ1)

x2_dot = x1_dot + L2 * ω2 * np.cos(θ2)
y2_dot = y1_dot + L2 * ω2 * np.sin(θ2)

T = 0.5 * m1 * (x1_dot ** 2 + y1_dot ** 2) + \
    0.5 * m2 * (x2_dot ** 2 + y2_dot ** 2)

V = m1 * g * y1 + m2 * g * y2

E_total = T + V

def format_plot(ax, title, xlabel, ylabel):
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.3)

fig1, ax1 = plt.subplots()
ax1.plot(t, θ1, label="θ1")
ax1.plot(t, θ2, label="θ2")
format_plot(ax1, "Angular positions vs time", "Time [s]", "Angle [rad]")
ax1.legend()

fig2, ax2 = plt.subplots()
ax2.plot(t, ω1, label="ω1")
ax2.plot(t, ω2, label="ω2")
format_plot(ax2, "Angular velocities vs time", "Time [s]", "Angular velocity [rad/s]")
ax2.legend()

fig3, ax3 = plt.subplots()
ax3.plot(x1, y1, label="Mass 1")
ax3.plot(x2, y2, label="Mass 2")
ax3.scatter([0], [0], marker="+", zorder=5, label="Pivot")
ax3.set_aspect("equal", "box")
format_plot(ax3, "Trajectories in the plane", "x [m]", "y [m]")
ax3.legend()

fig4, ax4 = plt.subplots()
ax4.plot(t, T, label="Kinetic T")
ax4.plot(t, V, label="Potential V")
ax4.plot(t, E_total, label="Total E")
format_plot(ax4, "Energy vs time", "Time [s]", "Energy [J]")
ax4.legend()

fig5, ax5 = plt.subplots()
ax5.plot(t, E_total - E_total[0])
format_plot(ax5, "Total-energy deviation", "Time [s]", "ΔE [J]")

plt.show()
