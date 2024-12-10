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
    path = './PowerConsumptionLogs/'
    
    if lags == 9:
        nroPairs = 4
    elif lags == 1:
        nroPairs = 4
    elif lags == 3:
        nroPairs = 4
    elif lags == 6:
        nroPairs = 4
        
    campaign = ['S1', 'K1', 'F1','AF1']
    
    cmdBAT = 'python3 PowerMeasure.py %s' %campaignPower
    proBAT = subprocess.Popen(cmdBAT, stdin=subprocess.PIPE, stdout=subprocess.PIPE, shell=True, preexec_fn=os.setsid)
    time.sleep(20)
    
    bar = pyprind.ProgBar(nroPairs, monitor=True, title=campaignPower)
    for s in range(nroPairs):
 
        
        
        name_file = path+campaignRuntime+'.txt'
        with open(name_file, 'a') as f:
            pathTest = '/home/marcello-costa/workspace/Demos/Executables/'
         
            os.chdir(pathTest)
            if test_type == 'AR1MoC':
               com = './%s_AR1 %s_AR1.1.txt +RTS -N%s' % (campaign[s], campaign[s], lags)
                
            elif test_type == 'AR3MoC' or 'AR6MoC' or 'AR9MoC':
                com = './%s_MC%sAR1 %s_MC%sAR1.1.txt +RTS -N%s' % (campaign[s], lags, campaign[s], lags, lags)
           
            
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
    
    idx = 3
    
    test_type = ['AR1MoC', 'AR3MoC', 'AR6MoC','AR9MoC']
    lags = [1,3,6,9]  # 1 for 'AR1MoC'/ and 6 for 'AR6MoC'
    
    K = 9
    N = 500
    
    
    kthSim(test_type[idx], N, K, lags[idx])
        
    
    
        
        
        
        