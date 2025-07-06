from ttball import plot_trajectory
from ttball import plot_momentum 
from ttball import rotational
from ttball import rotational_angular_momentum

PATH = '../data/ttball.csv'

plot_trajectory.plot_trajectory(PATH)
plot_momentum.plot_momentum(PATH)
rotational.plot_rotation_artifacts(PATH)
rotational_angular_momentum.additional_artifact(PATH)