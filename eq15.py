import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation
import pandas as pd
from tqdm import tqdm

#Initial conditions
Lx = 256
Ly = 256
dx = 1    #dx = dy
dt = 0.1
t_final = 600
psi0 = np.random.uniform(low=-0.1, high=0.1, size=(256, 256)) # psi at time t=0 which is small values

t = np.arange(0, t_final, dt)
x = np.arange(0, 256, 1)
y = np.arange(0, 256, 1)
psi = np.ones((256, 256))*psi0
dpsidt = np.empty((256, 256))

#finite difference method with periodic boundary condition (PBC)
print('Calculating Psi.......')
for _ in tqdm(range(t_final)):
    for i in range(len(x)):
        w = i-1       #west
        e = i+1       #east
        if w == -1:
            w = w + Lx
        if e == Lx:
            e = e - Lx
            
        for j in range(len(y)):
            n = j-1    #north
            s = j+1    #south
            if n == -1:
                n = n + Ly
            if s == Ly:
                s = s - Ly
                
            dpsidt = psi[i][j] - psi[i][j]**3 + (psi[e][j] + psi[w][j] + psi[i][n] + psi[i][s]
                                                 - 4*psi[i][j])/dx**2
            
    psi = psi + dpsidt*dt
    
# storing result in dataframe and save as csv file
t_list = []
x_list = []
y_list = []
psi_list = []

print('Storing data.......')
for _ in tqdm(t):
    for i in x:
        for j in y:
            t_list.append(_)
            x_list.append(i)
            y_list.append(j)
            psi_list.append(psi[i][j])
            
df = pd.DataFrame({'Time':t_list, 'X':x_list, 'Y':y_list, 'Psi':psi_list})
df.to_csv('eq15.csv', index=False)
print('Data is saved as csv file')
