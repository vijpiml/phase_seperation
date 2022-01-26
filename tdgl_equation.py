import numpy as np
import pandas as pd
import time

# Initial conditions
np.random.seed(1)
Lx = 256
Ly = 256
dx = 1  # dx = dy
dt = 0.1
t_final = 501
psi0 = np.random.uniform(low=-0.1, high=0.1, size=(Lx, Ly))  # psi at time t=0 which is small values

t = np.arange(0, t_final, dt)
x = np.arange(0, Lx, 1)
y = np.arange(0, Ly, 1)
psi = np.ones((Lx, Ly)) * psi0
dpsidt = np.empty((Lx, Ly))

# finite difference method with periodic boundary condition (PBC)
t_list = []
x_list = []
y_list = []
psi_list = []

snapshots = [i for i in range(0, 501, 5)]
print('......Running.......')
start = time.time()
for _ in range(len(t)):
    if _ == t_final:
        end1 = time.time()
        print(f'estimated time to finish the simulation = {((end1 - start)*100)/60} mins')
    for i in range(len(x)):
        w = i - 1  # west
        e = i + 1  # east
        if w == -1:
            w = w + Lx
        if e == Lx:
            e = e - Lx

        for j in range(len(y)):
            n = j - 1  # north
            s = j + 1  # south
            if n == -1:
                n = n + Ly
            if s == Ly:
                s = s - Ly

            dpsidt[i][j] = psi[i][j] - psi[i][j]**3 + (psi[e][j] + psi[w][j] +
                                                       psi[i][n] + psi[i][s] -
                                                       4 * psi[i][j]) / dx**2

            if _*dt in snapshots:
                t_list.append(_ * dt)
                x_list.append(i)
                y_list.append(j)
                psi_list.append(psi[i][j])

    psi = psi + dpsidt * dt

df = pd.DataFrame({'time': t_list, 'x': x_list, 'y': y_list, 'psi': psi_list})
df['psi'] = df['psi'].map(lambda x: -1 if x < 0 else 1)    
df.to_csv('tdgl_500.csv', index=False)

print('done! congratulations!')
