import matplotlib.pyplot as plt
import seaborn as sns

class Plot:
    def __init__(self, data, snapshots, filename, dpi):
        self.data = data
        self.snapshots = snapshots
        self.filename = filename
        self.dpi = dpi

        size = int(len(self.snapshots)/2)

        fig, ax = plt.subplots(size, 2, figsize=(15, 15))
        ax = ax.ravel()

        for i, j in enumerate(self.snapshots):
            df_i = self.data[self.data['time']==j]
            sns.heatmap(df_i.pivot(index='x', columns='y', values='psi'), ax=ax[i], 
                        square=True, cbar=False)
            ax[i].set_title(f't = {j}')
            ax[i].axis('off')
            ax[i].invert_yaxis()
        plt.savefig(self.filename, dpi=dpi)