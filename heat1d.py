import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation

l = 0.1
n = 10
T0 = 0
T1s = 40
T2s = 40
dx = l/10
alpha = 0.0001
t_final = 60
dt = 0.1

x = np.linspace(dx/2, l-dx/2, n)
T = np.ones(n)*T0
dTdt = np.empty(n)
t = np.arange(0, t_final, dt)

T_list = []
for j in range(1, len(t)):
    for i in range(1, n-1):
        dTdt[i] = alpha*(-(T[i]-T[i-1])/dx**2 +(T[i+1]-T[i])/dx**2)
    dTdt[0] = alpha*(-(T[0]-T1s)/dx**2 +(T[1]-T[0])/dx**2)
    dTdt[n-1] = alpha*(-(T[n-1]-T[n-2])/dx**2 +(T2s-T[n-1])/dx**2)
    T = T + dTdt*dt
    T_list.append(T)

fig, ax = plt.subplots()

line, = ax.plot([], [])
ax.axis([0, l, 0, 50])

def animate(i):
    line.set_xdata(x)
    line.set_ydata(T_list[i])
    return line,



ani = animation.FuncAnimation(fig, animate, interval=100, blit=True)
plt.show()

