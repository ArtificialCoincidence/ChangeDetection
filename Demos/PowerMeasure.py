from torch.multiprocessing import Pool, set_start_method
import os
import numpy as np
import torch
import time
import psutil
import datetime
import re
#from concurrent.futures import ProcessPoolExecutor
import signal
import subprocess
import sys
from subprocess import call    


def get_power(campaign, Time):
    
    path1 = 'c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/Output/logs/'
    nameFile = 'Power_%s.txt'%campaign
    id_temp = path1+nameFile
       

    command = 'powerstat -D 1 60'
    
    #command = 'powerstat -d 0 1 920' # 1200 = 20 min | 960 = 16 min # 2400  #BATERRY
    
    os.system('echo "PosDoc&&23" | sudo -S %s | tee %s' % (command, id_temp))
    
    
     

if __name__ == '__main__':

    campaign = sys.argv[1]

    nowTime = datetime.datetime.now()
    date_time = nowTime.strftime("%d/%m/%Y %H:%M:%S.%f")[:-3]
    get_power(campaign, date_time)
    


    

