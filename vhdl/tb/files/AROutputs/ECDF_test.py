from __future__ import division
import numpy as np

from matplotlib import pyplot as plt
import scipy.io as io
import os
import re
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Rectangle
import numpy as np
import time
from numpy import zeros,sqrt, mean,linspace,concatenate, cumsum
import scipy.io as sio
from matplotlib.widgets  import RectangleSelector
import matplotlib.patches as patches
import scienceplots
from matplotlib.collections import LineCollection
import matplotlib.pyplot as plt
from scipy import stats
from scipy.stats import norm
from statsmodels.distributions.empirical_distribution import ECDF
from numpy import inf
import pandas as pd


#create CDF definition
def ecdf(data):
    n = len(data)
    x = np.sort(data)
    y = np.arange(1.0, n+1) / n
    return x, y

#Using +-1.5x IQR method for defining outliers
def outliers_iqr(ys):
    quartile_1, quartile_3 = np.percentile(ys, [25, 90])
    iqr = quartile_3 - quartile_1
    lower_bound = quartile_1 - (iqr * 1.5)
    upper_bound = quartile_3 + (iqr * 1.5)

    return  np.where((ys < lower_bound)), np.where((ys > upper_bound))


def generate_plot1(ax, df):
    dat = df.values.T[0]

    s= ECDF(dat)   
   
    
    x  = s.x 
    x[x == -inf] = 0; x[x == inf] = 1 
    y = s.y
    y[y == -inf] = 0; y[y == inf] = 1 
    


    ax.plot(x, y, marker='.', linestyle='none') 
    ax.axvline(x.mean(), color='gray', linestyle='dashed', linewidth=2) #Add mean

   
    outliers= outliers_iqr(df.values) 

    #highlight the outliers area in the CDF plot
    for outl in outliers:
        vals = df.values[outl]
        if vals.size>0:
            ax.axvspan(np.min(vals),np.max(vals),alpha=0.5,color='red')



    ax.set_xlabel('Magnitude')



def generate_plot(ax, df):
    dat = df.values.T[0]
    s= ECDF(dat)   # cdf computation

    
    x  = s.x 
    x[x == -inf] = 0; x[x == inf] = 1 
    y = s.y
    y[y == -inf] = 0; y[y == inf] = 1 
    
    ax.plot(x, y, marker='.', linestyle='none') 
    ax.axvline(x.mean(), color='gray', linestyle='dashed', linewidth=2) #Add mean


    outliers= outliers_iqr(df.values) 

    #highlight the outliers area in the CDF plot
    for outl in outliers:
        vals = df.values[outl]
        if vals.size>0:
            ax.axvspan(np.min(vals),np.max(vals),alpha=0.5,color='red')


    ax.set_xlabel('Magnitude')
    ax.set_ylabel('$\hat{F}(y)$')
    ax.legend(('Pixel', 'mean', 'Quartiles'), loc='lower right')
    




if __name__ == "__main__":
  
    
    path = '/home/gwebber/ChangeDetection/vhdl/tb/files/AROutputs/AR1_Outputs_Changes/'
      
        
    files = os.listdir(path+'/')
    files_images = [i for i in files if i.endswith('.txt')]
    #files_images = sorted(files_images, key=lambda s: int(re.search(r'\d+', s).group()[-2:]))
    
    
    
    n = 500; W =500

    im= []
    for m in range(len(files_images)):
       dct = {}
       with open(path +files_images[m]) as f:
           for index, line in enumerate(f):
               for token in line.split():
                   dct[token] = index+1
       a = list(dct.keys())[0]
       s = a.split(',')
       res = []
       for i in range(len(s)):
           nFloat = re.findall(r"-?\d\.\d+[Ee][+\-]\d\d?", s[i])
           nZero  = re.findall(r"[-+]?(?:\d*\.\d+|\d+)", s[i])
           if len(nFloat) != 0:
               res.append(float(nFloat[0]))    
           elif len(nZero) != 0:
               res.append(float(nZero[0])) 
       im.append(np.array(res))
    
    # image reconstruction
    REC = []
    for k in range(len(im)):
        IM = im[k].reshape(-1, n, n)
        Im = []
        for j in range(0,len(IM),int(W/n)):
            a=[]
            for l in range(int(W/n)):
                a.append(IM[j+l])
            Im.append(np.hstack((a)))
        REC.append(np.vstack((Im)))
        
    print(len(files_images))
    
    idx = 0
        
    print(files_images[idx])
    
    
    resARAvgVecT1= REC[idx].ravel()
    
    # fig = plt.figure('AR1')
    
    
    # ax = fig.add_subplot(1, 1, 1)
    # plt.imshow(REC[idx], cmap = plt.cm.gray)
    # ax.set_title("Test Detection")
    # plt.axis("off")
    
   
    
    # plt.show() 
    
    
    ############################## NoChanges
    
    
    path = '/home/gwebber/ChangeDetection/vhdl/tb/files/AROutputs/AR1_Outputs_NoChanges/'
      
        
    files = os.listdir(path+'/')
    files_images = [i for i in files if i.endswith('.txt')]
    files_images = sorted(files_images, key=lambda s: int(re.search(r'\d+', s).group()[-2:]))
    
    
    
    n = 500; W =500

    im= []
    for m in range(len(files_images)):
       dct = {}
       with open(path +files_images[m]) as f:
           for index, line in enumerate(f):
               for token in line.split():
                   dct[token] = index+1
       a = list(dct.keys())[0]
       s = a.split(',')
       res = []
       for i in range(len(s)):
           nFloat = re.findall(r"-?\d\.\d+[Ee][+\-]\d\d?", s[i])
           nZero  = re.findall(r"[-+]?(?:\d*\.\d+|\d+)", s[i])
           if len(nFloat) != 0:
               res.append(float(nFloat[0]))    
           elif len(nZero) != 0:
               res.append(float(nZero[0])) 
       im.append(np.array(res))
    
    # image reconstruction
    REC = []
    for k in range(len(im)):
        IM = im[k].reshape(-1, n, n)
        Im = []
        for j in range(0,len(IM),int(W/n)):
            a=[]
            for l in range(int(W/n)):
                a.append(IM[j+l])
            Im.append(np.hstack((a)))
        REC.append(np.vstack((Im)))
        
    print(len(files_images))
    
    idx = 1 # 4,3,1
         
    print(files_images[idx])
     
    # fig = plt.figure('AR1')
    
    
    # ax = fig.add_subplot(1, 1, 1)
    # plt.imshow(REC[idx], cmap = plt.cm.gray)
    # ax.set_title("Test Detection")
    # plt.axis("off")
    
   
    
    # plt.show() 
    
    
    resARAvgVecC1= REC[idx].ravel()


a = resARAvgVecC1
b = resARAvgVecT1

fig, axes = plt.subplots(nrows = 1, ncols = 2, figsize=(10,5))


generate_plot(axes[0],pd.DataFrame({"pixel" : a}))
generate_plot1(axes[1],pd.DataFrame({"pixel" : b}))


axes[0].set_title('clutter-clutter: 9AR(1)')
axes[1].set_title('target-clutter: 9AR(1)')


fig.tight_layout()

plt.show()