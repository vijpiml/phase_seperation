import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation
import pandas as pd
import seaborn as sns
from tqdm import tqdm

# Initial conditions
Lx = 256
Ly = 256
dx = 1  # dx = dy
dt = 0.1
t_final = 5
# psi at time t=0 which is small values
psi0 = np.random.uniform(low=-0.1, high=0.1, size=(Lx, Ly))

t = np.arange(0, t_final, dt)
x = np.arange(0, Lx, 1)
y = np.arange(0, Ly, 1)
psi = np.ones((Lx, Ly))*psi0
dpsidt = np.empty((Lx, Ly))

# finite difference method with periodic boundary condition (PBC)

t_list = []
x_list = []
y_list = []
psi_list = []

for _ in tqdm(range(len(t))):
    for i in range(len(x)):
        w = i-1  # west
        e = i+1  # east
        if w == -1:
            w = w + Lx
        if e == Lx:
            e = e - Lx

        for j in range(len(y)):
            n = j-1  # north
            s = j+1  # south
            if n == -1:
                n = n + Ly
            if s == Ly:
                s = s - Ly

            dpsidt[i][j] = psi[i][j] - psi[i][j]**3 + (psi[e][j] + psi[w][j] + psi[i][n] + psi[i][s]
                                                       - 4*psi[i][j])/dx**2

            t_list.append(_*dt)
            x_list.append(i)
            y_list.append(j)
            psi_list.append(psi[i][j])

    psi = psi + dpsidt*dt

df = pd.DataFrame({'time': t_list, 'x': x_list, 'y': y_list, 'psi': psi_list})
df['psi'] = df['psi'].map(lambda x: -1 if x < 0 else 1)

df_list = []
for i in t:
    df_list.append(df[df['time'] == i])

fig = plt.figure()
ax = plt.axes(xlim=(0, 28), ylim=(0, 28))


def init():
    sns.heatmap(np.zeros((28, 28)), square=True, cbar=False)
    ax.set_title(f't = {0.0}')
    ax.axis('off')


def animate(i):
    data = df_list[i]
    sns.heatmap(data.pivot(index='x', columns='y',
                           values='psi'), square=True, cbar=False)
    ax.set_title(f't = {np.round(t[i], 2)}')
    ax.invert_yaxis()
    ax.axis('off')


anim = animation.FuncAnimation(fig, animate, init_func=init,
                               frames=len(df_list), repeat=False)


FFwriter = animation.FFMpegWriter(fps=10)
anim.save('animation.mp4', writer = FFwriter)
print('.........THE END............')
#plt.show()
