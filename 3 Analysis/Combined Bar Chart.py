##set up
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Combined_bar_charts PNOM 2019.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##subset into the separate plots
ex1 = dat[dat['Experiment'] == 1] #standard instructions
ex2 = dat[dat['Experiment'] == 2] #response deadline
ex3 = dat[dat['Experiment'] == 3] #Delayed JOL
ex4 = dat[dat['Experiment'] == 4] #Pooled

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(15,15)

##Make the subplots
ax1 = fig.add_subplot(2, 2, 1)
ax2 = fig.add_subplot(2, 2, 2)
ax3 = fig.add_subplot(2, 2, 3)
ax4 = fig.add_subplot(2, 2, 4)
fig.subplots_adjust(hspace = .275)
fig

#Standard instructions
#subset by task
j1 = ex1[ex1['Task'] == 'JOL']
r1 = ex1[ex1['Task'] == 'Recall']

#get all the things to plug into the plots
#separate out averages and conf interval
j1_average = j1['Average']
r1_average = r1['Average']

j1_conf = j1['diff2']
r1_conf = r1['diff2']

ind = np.arange(len(j1_average))  # the x locations for the groups
width = 0.35 #bar width 

rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'dodgerblue', edgecolor = 'k',
                label ='JOL')

rects2 = ax1.bar(ind + width/2, r1_average, width, yerr = r1_conf, capsize = 3, color = 'midnightblue', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax1.set_title('Experiment 1: Concurrent JOLs', fontsize = 20, fontweight = 'bold')
ax1.set_ylabel('Mean % JOL/Recall', fontsize = 18, fontweight = 'bold')
ax1.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('B', 'F', 'S', 'U'), fontsize = 16)
ax1.legend(fontsize = 16)
ax1.set_ylim([0,100])

#Response Deadline
#subset by task
j2 = ex2[ex2['Task'] == 'JOL']
r2 = ex2[ex2['Task'] == 'Recall']

#get all the things to plug into the plots
#separate out averages and conf interval
j2_average = j2['Average']
r2_average = r2['Average']

j2_conf = j2['diff2']
r2_conf = r2['diff2']

ind = np.arange(len(j2_average))  # the x locations for the groups
width = 0.35 #bar width 

rects3 = ax2.bar(ind - width/2, j2_average, width, yerr = j2_conf, capsize = 3, color = 'dodgerblue', edgecolor = 'k',
                label ='JOL')

rects4 = ax2.bar(ind + width/2, r2_average, width, yerr = r2_conf, capsize = 3, color = 'midnightblue', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax2.set_title('Experiment 2: Response Deadline', fontsize = 20, fontweight = 'bold')
ax2.set_ylabel('Mean % JOL/Recall', fontsize = 18, fontweight = 'bold')
ax2.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
ax2.set_xticks(ind)
ax2.set_xticklabels(('B', 'F', 'S', 'U'), fontsize = 16)
ax2.legend(fontsize = 16)
ax2.set_ylim([0,100])

#Delayed JOL
#subset by task
j3 = ex3[ex3['Task'] == 'JOL']
r3 = ex3[ex3['Task'] == 'Recall']

#get all the things to plug into the plots
#separate out averages and conf interval
j3_average = j3['Average']
r3_average = r3['Average']

j3_conf = j3['diff2']
r3_conf = r3['diff2']

ind = np.arange(len(j3_average))  # the x locations for the groups
width = 0.35 #bar width 

rects5 = ax3.bar(ind - width/2, j3_average, width, yerr = j3_conf, capsize = 3, color = 'dodgerblue', edgecolor = 'k',
                label ='JOL')

rects6 = ax3.bar(ind + width/2, r3_average, width, yerr = r3_conf, capsize = 3, color = 'midnightblue', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax3.set_title('Experiment 3: Immediate JOLs', fontsize = 20, fontweight = 'bold')
ax3.set_ylabel('Mean % JOL/Recall', fontsize = 18, fontweight = 'bold')
ax3.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
ax3.set_xticks(ind)
ax3.set_xticklabels(('B', 'F', 'S', 'U'), fontsize = 16)
ax3.legend(fontsize = 16)
ax3.set_ylim([0,100])

#Pooled
#subset by task
j4 = ex4[ex4['Task'] == 'JOL']
r4 = ex4[ex4['Task'] == 'Recall']

#get all the things to plug into the plots
#separate out averages and conf interval
j4_average = j4['Average']
r4_average = r4['Average']

j4_conf = j4['diff2']
r4_conf = r4['diff2']

ind = np.arange(len(j4_average))  # the x locations for the groups
width = 0.35 #bar width 

rects7 = ax4.bar(ind - width/2, j4_average, width, yerr = j4_conf, capsize = 3, color = 'dodgerblue', edgecolor = 'k',
                label ='JOL')

rects8 = ax4.bar(ind + width/2, r4_average, width, yerr = r4_conf, capsize = 3, color = 'midnightblue', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax4.set_title('Experiment 4: Delayed JOLs', fontsize = 20, fontweight = 'bold')
ax4.set_ylabel('Mean % JOL/Recall', fontsize = 18, fontweight = 'bold')
ax4.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
ax4.set_xticks(ind)
ax4.set_xticklabels(('B', 'F', 'S', 'U'), fontsize = 16)
ax4.legend(fontsize = 16)
ax4.set_ylim([0,100])

##save figure
fig.savefig('Combined_bar_chart2.png', dip = 10000)
