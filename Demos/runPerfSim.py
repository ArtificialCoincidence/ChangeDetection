from __future__ import division
import numpy as np
import time
from matplotlib import pyplot as plt
import array
import scipy.io
import scipy.io as io
import pyprind
import time
import psutil
import datetime
import pyprind
import time
import psutil
import re
import os
import signal
import subprocess
from datetime import datetime




def kthSim(test_type, N, K, lags):
    
    


    
    campaignRuntime = 'Runtime_%s_N_%d_K_%d'%(test_type, N, K)
    campaignPower = '%s_N_%d_K_%d'%(test_type, N, K)
    path = 'c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/Output/logs/'
    
    if lags == 6:
        nroPairs = 2
    elif lags == 1:
        nroPairs = 6
    
    cmdBAT = 'python3 PowerMeasure.py %s' %campaignPower
    proBAT = subprocess.Popen(cmdBAT, stdin=subprocess.PIPE, stdout=subprocess.PIPE, shell=True, preexec_fn=os.setsid)
    time.sleep(20)
    
    bar = pyprind.ProgBar(nroPairs, monitor=True, title=campaignPower)
    for s in range(nroPairs):
 
        tt = lags
        
        
        name_file = path+campaignRuntime+'.txt'
        with open(name_file, 'a') as f:
            pathTest = 'c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/'
            os.chdir(pathTest)
            com = 'echo "main" | ghci Demo-MC_AR%d.hs'%tt  
           
            
            START = time.time()
            os.system(com)
            END = time.time()
            dt = datetime.now()
            dt.microsecond   
            
            timeAR1 = dt
            Nt = 0; timeCFAR = 0

            f.write(str(test_type)+';'+'N_'+str(N)+';'+'K_'+str(K)+';'+'pair_'+str(s)+';'+'HT_runtime_'+str(timeAR1)+';'+'cfar_runtime_'+str(timeCFAR)+';'+'Threshold_op_'+str(Nt)+'\n')
            bar.update()
    
    #os.killpg(os.getpgid(proBAT.pid), signal.SIGTERM)
                
            

if __name__ == "__main__":
    
    test_type = ['AR1MoC', 'AR6MoC']
    test_type  = test_type[1]
    lags = 6  # 1 for 'AR1MoC'/ and 6 for 'AR6MoC'
    
    K = 7
    N = 500
    
    
    kthSim(test_type, N, K, lags)
        
    
    
        
        
        
        