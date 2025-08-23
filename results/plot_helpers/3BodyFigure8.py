import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# ------------------ Hard-coded paths ------------------
INPUT_CSV = "../data/3BodyFigure8.csv"
OUTDIR    = "../plots/3BodyFigure8"

os.makedirs(OUTDIR, exist_ok=True)

# ------------------ Helpers ------------------
def autocorr_fft(x):
    n = len(x)
    x = x - np.mean(x)
    f = np.fft.rfft(x, n=2*n)
    p = f * np.conjugate(f)
    ac = np.fft.irfft(p)[:n]
    ac /= ac[0]
    return ac

def savefig(path):
    plt.savefig(path, dpi=300, bbox_inches="tight")
    plt.close()

# ------------------ Load data ------------------
df = pd.read_csv(INPUT_CSV)

t = df["t"].to_numpy()
x1, y1 = df["x1"].to_numpy(), df["y1"].to_numpy()
x2, y2 = df["x2"].to_numpy(), df["y2"].to_numpy()
x3, y3 = df["x3"].to_numpy(), df["y3"].to_numpy()
vx1, vy1 = df["vx1"].to_numpy(), df["vy1"].to_numpy()
vx2, vy2 = df["vx2"].to_numpy(), df["vy2"].to_numpy()
vx3, vy3 = df["vx3"].to_numpy(), df["vy3"].to_numpy()

r12, r23, r31 = df["r12"].to_numpy(), df["r23"].to_numpy(), df["r31"].to_numpy()
E_given = df["E"].to_numpy()

# ------------------ Derived quantities ------------------
v1 = np.sqrt(vx1**2 + vy1**2)
v2 = np.sqrt(vx2**2 + vy2**2)
v3 = np.sqrt(vx3**2 + vy3**2)

# Energies (G = m_i = 1)
K = 0.5*(v1**2 + v2**2 + v3**2)
U = -(1.0/r12 + 1.0/r23 + 1.0/r31)
relE = (E_given - E_given[0]) / abs(E_given[0])

# Center of mass and linear momentum
Rx = (x1 + x2 + x3)/3.0
Ry = (y1 + y2 + y3)/3.0
Rcm = np.sqrt(Rx**2 + Ry**2)

Px = vx1 + vx2 + vx3
Py = vy1 + vy2 + vy3
Pmag = np.sqrt(Px**2 + Py**2)

# Angular momentum components if present; otherwise compute Lz
if {"Lx","Ly","Lz"}.issubset(df.columns):
    Lx, Ly, Lz = df["Lx"].to_numpy(), df["Ly"].to_numpy(), df["Lz"].to_numpy()
else:
    Lz = x1*vy1 - y1*vx1 + x2*vy2 - y2*vx2 + x3*vy3 - y3*vx3
    Lx = np.zeros_like(Lz)
    Ly = np.zeros_like(Lz)

# Autocorrelation for period estimate
dt = float(np.median(np.diff(t)))
ac_x1 = autocorr_fft(x1)
lags = np.arange(len(ac_x1))*dt
T_est = np.nan
min_idx = int(1.0/dt) if dt > 0 else 1  # skip tiny lags
for i in range(min_idx+1, len(ac_x1)-1):
    if ac_x1[i-1] < ac_x1[i] and ac_x1[i] > ac_x1[i+1]:
        T_est = i*dt
        break

# ------------------ Plots ------------------

# 1) Trajectories (equal aspect ratio)
plt.figure(figsize=(6,6))
plt.plot(x1, y1, label="Body 1")
plt.plot(x2, y2, label="Body 2")
plt.plot(x3, y3, label="Body 3")
plt.gca().set_aspect("equal", adjustable="box")
plt.xlabel("x")
plt.ylabel("y")
plt.title("Three-Body 'Figure-Eight' Trajectories")
plt.legend()
savefig(os.path.join(OUTDIR, "figure8_trajectories.png"))

# 2) Pairwise distances
plt.figure()
plt.plot(t, r12, label="r12")
plt.plot(t, r23, label="r23")
plt.plot(t, r31, label="r31")
plt.xlabel("t")
plt.ylabel("Distance")
plt.title("Pairwise Distances vs Time")
plt.legend()
savefig(os.path.join(OUTDIR, "figure8_distances.png"))

# 3) Relative energy error (from provided E column)
plt.figure()
plt.plot(t, relE)
plt.xlabel("t")
plt.ylabel("(E(t)-E(0))/|E(0)|")
plt.title("Relative Energy Error")
savefig(os.path.join(OUTDIR, "figure8_energy_error.png"))

# 4) Kinetic and potential energy vs time
plt.figure()
plt.plot(t, K, label="Kinetic K(t)")
plt.plot(t, U, label="Potential U(t)")
plt.xlabel("t")
plt.ylabel("Energy")
plt.title("Kinetic and Potential Energy vs Time")
plt.legend()
savefig(os.path.join(OUTDIR, "figure8_KU.png"))

# 5) Linear momentum magnitude
plt.figure()
plt.plot(t, Pmag)
plt.xlabel("t")
plt.ylabel("||P(t)||")
plt.title("Linear Momentum Magnitude vs Time")
savefig(os.path.join(OUTDIR, "figure8_linear_momentum.png"))

# 6) Center-of-mass drift
plt.figure()
plt.plot(t, Rcm)
plt.xlabel("t")
plt.ylabel("||R_CM(t)||")
plt.title("Center-of-Mass Drift")
savefig(os.path.join(OUTDIR, "figure8_com_drift.png"))

# 7) Angular momentum components
plt.figure()
plt.plot(t, Lx, label="Lx")
plt.plot(t, Ly, label="Ly")
plt.plot(t, Lz, label="Lz")
plt.xlabel("t")
plt.ylabel("Angular momentum")
plt.title("Angular Momentum Components vs Time")
plt.legend()
savefig(os.path.join(OUTDIR, "figure8_angular_momentum.png"))

# 8) Body speeds
plt.figure()
plt.plot(t, v1, label="|v1|")
plt.plot(t, v2, label="|v2|")
plt.plot(t, v3, label="|v3|")
plt.xlabel("t")
plt.ylabel("Speed")
plt.title("Body Speeds vs Time")
plt.legend()
savefig(os.path.join(OUTDIR, "figure8_speeds.png"))

# 9) Autocorrelation of x1 with estimated period marker
plt.figure()
plt.plot(lags, ac_x1, label="ACF(x1)")
if not np.isnan(T_est):
    plt.axvline(T_est, linestyle="--", label=f"T ≈ {T_est:.3f}")
plt.xlim(0, min(t[-1]-t[0], 20))
plt.xlabel("Lag (time)")
plt.ylabel("Autocorrelation")
plt.title("Autocorrelation of x1 (Period Estimate)")
plt.legend()
savefig(os.path.join(OUTDIR, "figure8_acf_x1.png"))

print("Saved PNGs in:", os.path.abspath(OUTDIR))
