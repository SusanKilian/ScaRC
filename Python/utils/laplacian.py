import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

X = np.arange(-6, 10, 0.25)
Y = np.arange(-8.5, 7.5, 0.25)
X, Y = np.meshgrid(X, Y)
Z = np.sin(X/2+2)+np.cos(Y/2+5)+10-X/8+Y/4

fig = plt.figure()
ax = Axes3D(fig)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap=cm.viridis)
cset = ax.contour(X, Y, Z, cmap=plt.get_cmap('rainbow'))

plt.show()
