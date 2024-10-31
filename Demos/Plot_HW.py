from cProfile import label
import numpy as np
import pandas as pd
import csv
import re
import os
import scipy.stats as sps
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from scipy.io import savemat, loadmat
import datetime
from collections import defaultdict
import scienceplots
from ast import literal_eval
# import plottools



def OrderData(time, tput):
    Time=[]; Tput=[]
    for i in range(len(time)):
        Time.append(np.mean(time[i]))
        Tput.append(np.mean(tput[i]))
    Order_time=sorted(Time)
    Indexes = sorted(range(len(Time)),key=Time.__getitem__)
    Order_tput=[]
    for i in Indexes:
        Order_tput.append(Tput[i])
    return Order_time, Order_tput

def findMiddle(list):
  l = len(list)
  if l/2:
    return (list[l/2-1]+list[l/2])/2.0
  else:
    return list[(l/2-1)/2]
            

if __name__ == "__main__":


    mainDir='c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/Output/'

    files = os.listdir(mainDir)
    files_logs = [i for i in files if i.endswith('.csv')]
    
    dataset = pd.read_csv(mainDir+files_logs[0])
    
     
            
    # # #------------------------ Graphic Analysis -------------------------#
 
    DPI = 250
    plt.rc('text', usetex=True)
    DPI = 250


    SMALL_SIZE = 8
    MEDIUM_SIZE = 16
    BIGGER_SIZE = 12

    plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
    plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
    

    
 
    with plt.style.context(['science', 'ieee', 'std-colors']):
            
            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(4,4), dpi=DPI) #, constrained_layout=True)
            ax1.set_prop_cycle(color=['darkred', 'indianred', 'indigo'],marker = ['o','*', 'o'], alpha=[0.95, 0.4, 0.2]) #linewidth=2
            for i in range(0, len(dataset)):
                
                df = dataset[['test_type', 'data_pct', 'flops', 'Power_CPU','timeLine']]
                
                
                fflops = literal_eval(df.flops[i])
                ddata = literal_eval(df.data_pct[i])
                df.tail()
                ax1.semilogx(fflops, ddata, linewidth=2)

            #ax1.set(**pparam2)
            ax1.legend(prop={'size': 12})
            ax1.set_ylabel(r'Dataset Proc. $(\%)$', fontsize=12)  
            ax1.set_xlabel(r'Flop/s', fontsize=12)  
            ax1.xaxis.set_label_coords(.5, -.055)
            ax1.yaxis.set_label_coords(-0.225, 0.5)
            
                
            
                
            #meanColor = ['k', 'navy'] 
            ax2.set_prop_cycle(color=['darkred', 'darkred','indianred' , 'indianred', 'indigo','indigo','darkred', 'darkred'],marker = ['o','o','*','*', '*','*','o','o'], alpha=[1, 1,0.4,0.4, 0.2, 0.2,0.2,0.2]) #linewidth=2
                
            for i in range(0, len(dataset)):
                
                df = dataset[['test_type', 'data_pct', 'flops', 'Power_CPU','timeLine']]

       
                power = literal_eval(df.Power_CPU[i])
                
                meanPW = np.mean(power)
                sumPW = np.mean(power)
                
                ttime = literal_eval(df.timeLine[i])
                
                sumT = np.mean(ttime)/3600
                
                energy = round(sumPW*sumT,2)
              
                
                
                if df.test_type[i] == 'GLRT':
                    test = 'GLRT+CA-CFAR'
                elif df.test_type[i] == 'AR1':
                    test = r'AR($1$)'
                elif df.test_type[i] == 'AR1MoC':
                    test = r'AR($1$)-MoC'
                    
                elif df.test_type[i] == 'AR6MoC':
                    test = r'AR($6$)-MoC'
                
                x =ttime[int(len(ttime) / 2) - 1]
                y = sumPW
                
                #list(set(df.test_type))[0]
                df.tail()
                meanColor = ['k', 'navy', 'green', 'red'] 
                meanMarker = ['o','*', '*', 'o']
               
                ax2.plot(ttime, power, linewidth=2, label=r'%s'%test) # test append(s)
                ax2.plot(x, y)
                
                
                ax2.axhline(y=np.nanmean(power), color = meanColor[i], label=r'%s [W]'%np.round(meanPW,2))
            

            # ax2.set(**pparam2)
            ax2.legend(prop={'size': 5})
              
            ax2.yaxis.set_label_position("right")
            ax2.set_xlabel(r'time [s]', fontsize=12)  
            ax2.xaxis.set_label_coords(.5, -.055)
            ax2.yaxis.set_label_coords(-0.15, 0.5)
            #ax2.set_xlim([0, 250])
            ax2.yaxis.set_tick_params(which='both',labelleft=False, labelright=True)
            ax2.set_ylabel(r'Power (W)',labelpad=-725, fontsize=12)
      
            #plt.ylabel('Your label here', labelpad=-725, fontsize=18)


            ax2.legend(loc='lower center', bbox_to_anchor=(0, 1), fancybox=True, shadow=True, ncol=2)
            
          
    fig.subplots_adjust(bottom=0.2)
    path='c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/plots/'
 
    fig.savefig(path + 'kthHW.png', dpi=DPI)
    fig.tight_layout()
    plt.show()      
