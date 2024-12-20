import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

from matplotlib.patches import FancyBboxPatch

df = pd.read_excel('./data/goniometer.xlsx')

dfFiltered = df[df['rater'] == 'manual']
mesaureAvg = dfFiltered.groupby('id')['y'].mean().reset_index()
mesaureAvg.rename(columns={'y':'manAvg'}, inplace=True)
dfMod = df.merge(mesaureAvg[['id', 'manAvg']], on='id', how='left')
dfMod.sort_values(['manAvg', 'id', 'time'], ascending=[True, True, True], inplace=True)
dfMod.reset_index(inplace=True)
dfMod['rank'] =  (dfMod.index+1)
dfMod['rank'] = dfMod['manAvg'].rank(method='dense', ascending=True).astype(int)
dfMod['chartOrder'] = (dfMod.index // 6) + 1
dfMod['orderStagger'] = ((dfMod['chartOrder']) + (dfMod['time']-2)*0.1)# - 0.2
dfMod['orderFlat'] = dfMod['chartOrder'] #- 0.2
dfMod['delta'] = dfMod['y'] - dfMod['manAvg']
dfMod['markCol'] = dfMod['rater'].apply(lambda x: '#2A90FF' if x == 'electro' else '#C42AFF')

byRange = dfMod.groupby(['chartOrder', 'rater'], as_index=False)['y'].agg(['min', 'max', 'mean'])
baseMan = byRange[byRange['rater'] == 'manual'].groupby('chartOrder').first().reset_index()
baseMan.rename(columns={'mean':'meanMan'}, inplace=True)
byRange = byRange.merge(baseMan[['chartOrder', 'meanMan']], on='chartOrder', how='left')
byRange['diff'] =  byRange['mean'] - byRange['meanMan']

byR = dfMod.groupby(['chartOrder', 'rater'], as_index=False)['y'].mean()

pivot = byR.pivot(index='chartOrder', columns='rater', values='y')
pivot['diff'] = pivot['electro'] - pivot['manual']

pivot.reset_index(inplace=True)

fig = plt.figure(figsize=(7, 9))
gs = fig.add_gridspec(1,2, width_ratios=[1,4],wspace=0)
ax = gs.subplots(sharey=True)

yTicks = np.arange(0,29,2)

for _, row in pivot.iterrows():
    
    lineCol = '#FFA72A' if row['electro'] > row['manual'] else '#34CB83'
    elMark = '>' if row['electro'] > row['manual'] else '<'
    
    ax[1].plot(
        [row['electro'], row['manual']],
        [row['chartOrder'], row['chartOrder']],  
        color=lineCol,    
        linewidth=1   
    )
    ax[1].plot(row['manual'], row['chartOrder'], marker='|', color=lineCol)
    ax[1].plot(row['electro'], row['chartOrder'], marker=elMark, markerfacecolor='#ffffff', markeredgecolor=lineCol)

ax[0].set_xlim([-6, 8])
ax[1].set_xlim([-20, 20])

ax[0].spines['top'].set_visible(False)
ax[0].spines['right'].set_visible(False)

ax[1].spines['top'].set_visible(False)
ax[1].spines['right'].set_visible(False)

ax[1].spines['left'].set_color('#ababab')



ax[0].scatter(dfMod['delta'], dfMod['orderStagger'], marker='x', color=dfMod['markCol'], s=10, alpha=0.75)


ax[0].set_yticks(yTicks)
ax[0].grid(True, linestyle='-', linewidth=15, alpha=0.15, axis='y')
ax[1].set_yticks(yTicks)
ax[1].grid(True, linestyle='-', linewidth=15, alpha=0.15, axis='y')

ax[0].set_ylabel('Subject')
ax[0].set_xlabel('Intra-Measure\nVariation')
ax[1].set_xlabel('Inter-Measure\nVariation')
ax[1].tick_params(axis='y', length=0)
ax[0].tick_params(axis='y', length=0)
ax[0].set_yticklabels([])
ax[0].set_xticks([-5, 0, 5])
ax[1].set_xticks([-15, -10, -5, 0, 5, 10, 15, 20])

figAx =  fig.add_axes([0.1, 0.1, 0.9, 0.9], zorder=10)  
figAx.set_axis_off() 

bbox = FancyBboxPatch(
    (0.15, 0.775),
    0.15, 0.055,            
    boxstyle='round,pad=0.009', 
    edgecolor='#efefef',
    facecolor='#fff',
    zorder=2,
)
figAx.add_patch(bbox)

figAx.text(0.16, 0.785, 'x', verticalalignment='center', color='#2A90FF')
figAx.text(0.16, 0.815, 'x', verticalalignment='center', color='#C42AFF')
figAx.text(0.18, 0.785, '- Electro', verticalalignment='center', color='#232323')
figAx.text(0.18, 0.815, '- Manual', verticalalignment='center', color='#232323')

bbox = FancyBboxPatch(
    (0.65, 0.15),
    0.21, 0.055,            
    boxstyle='round,pad=0.009', 
    edgecolor='#efefef',
    facecolor='#fff',
    zorder=2,
)
figAx.add_patch(bbox)

figAx.text(0.66, 0.195, 'Electro < Manual', verticalalignment='center', color='#34CB83')
figAx.text(0.66, 0.16, 'Electro > Manaul', verticalalignment='center', color='#FFA72A')

figAx.text(0.04, 0.93, 'Electro-Goniometers Consistently Produce\nLower Values Across the Measurement Range', fontsize=15, horizontalalignment='left')
figAx.text(0.04, 0.9, 'with similar variation', fontsize=12, horizontalalignment='left')

figAx.text(0.01, -0.09, 'data: Eliasziw et al., 1994', fontsize=9, horizontalalignment='left', color='#ababab')
figAx.text(0.85, -0.05, 'Andrew Staroscik', fontsize=12, horizontalalignment='center', color='#ababab')
figAx.text(0.85, -0.065, 'Wonderful-Wednesday', fontsize=8, horizontalalignment='center', color='#ababab')
figAx.text(0.85, -0.08, 'Nov 2024', fontsize=8, horizontalalignment='center', color='#ababab')

fig.savefig('./measures.png', dpi=300)

	