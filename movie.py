import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation
import seaborn as sns


class Movie:
    def __init__(self, data, fps, filename):
        self.data = data
        self.fps = fps
        self.filename = filename
        self.data_list = []

    def make_movie(self):
        t = self.data['time'].unique().tolist()
        for i in t:
            self.data_list.append(self.data[self.data['time'] == i])
        fig = plt.figure()
        xlim = (0, self.data_list[0].shape[0])
        ylim = (0, self.data_list[0].shape[1])
        ax = plt.axes(xlim=xlim, ylim=ylim)

        def init():
            sns.heatmap(self.data_list[0].pivot(index='x', columns='y', values='psi'), square=True,
                        cbar=False)
            ax.set_title(f't = {0.0}')
            ax.axis('off')

        def animate(i):
            sns.heatmap(self.data_list[i].pivot(index='x', columns='y', values='psi'), square=True,
                        cbar=False)
            ax.set_title(f't = {np.round(t[i], 2)}')
            ax.invert_yaxis()
            ax.axis('off')

        anim = animation.FuncAnimation(fig, animate, init_func=init, frames=len(self.data_list),
                                       repeat=False, interval=3000)

        FFwriter = animation.FFMpegWriter(fps=self.fps)
        anim.save(self.filename, writer = FFwriter)
