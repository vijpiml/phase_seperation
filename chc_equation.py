import numpy as np
import pandas as pd
import time

#Initial conditions
np.random.seed(1)
Lx = 128
Ly = 128
dx = 1  #dx = dy
dt = 0.01
t_final = 501
psi0 = np.random.uniform(low=-0.3, high=0.7, size=(Lx, Ly))  # psi at time t=0 which is small values
t = np.arange(0, t_final, dt)
x = np.arange(0, Lx, 1)
y = np.arange(0, Ly, 1)
psi = np.ones((Lx, Ly)) * psi0
phi = np.empty((Lx, Ly))
d2psi = np.empty((Lx, Ly))
d2phi = np.empty((Lx, Ly))

#calculating psi
t_list = []
x_list = []
y_list = []
psi_list = []

snapshots = [i for i in range(0, 501, 5)]
print('****************RUNING***************')
start = time.time()
for _ in range(len(t)):
    if _ == t_final:
        end1 = time.time()
        print(f'estimated time to finish the simulation = {((end1 - start)*100)/60} mins')

    #Boundary conditions (periodic)
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

            d2psi[i][j] = (psi[e][j] + psi[w][j] + psi[i][s] + psi[i][n] -
                           4 * psi[i][j]) / dx**2
            phi[i][j] = psi[i][j]**3 - psi[i][j] - d2psi[i][j]
            d2phi[i][j] = (phi[e][j] + phi[w][j] + phi[i][s] + phi[i][n] -
                           4 * phi[i][j]) / dx**2

            if _ * dt in snapshots:
                t_list.append(_ * dt)
                x_list.append(i)
                y_list.append(j)
                psi_list.append(psi[i][j])

    psi = psi + d2phi * dt

df = pd.DataFrame({'time': t_list, 'x': x_list, 'y': y_list, 'psi': psi_list})
df['psi'] = df['psi'].map(lambda x: -1 if x < 0 else 1)
df.to_csv('chc_128x128_500.csv', index=False)

print('done! Congratulation!')
