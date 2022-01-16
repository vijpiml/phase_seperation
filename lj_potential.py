import numpy as np
import matplotlib.pyplot as plt

r = np.linspace(0.9, 2.6, 10000)
v = 4*((1/r)**12 - (1/r)**6)
v1 = np.empty((10000,))*0
plt.plot(r, v)
plt.plot(r, v1)
plt.xlabel('r')
plt.ylabel('V')
plt.show()
