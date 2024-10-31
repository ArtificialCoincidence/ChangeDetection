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



def merge(dicts):
    result = defaultdict(list)
    for d in dicts:
        for key, value in d.items():
            result[key].append(value)
    return result


def time2sec(T):
   h=int(T[0])*3600; min=int(T[1])*60; seg=float(T[2])
   res = h+min+seg
   
   return res

def timeLine(time):
    count=0; timeSec=[]
    for i in range(len(time)):
        timeSec.append(time[i]-time[0])
    return timeSec

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


def get_Flops(file, Test_type): 
    
    df = pd.read_csv(file, delimiter= ';', index_col=False)

    CNT=[]; Pair = []; time_HT=[]; time_cfar=[]; FLOPS=[]; FLOP=[]; Flops = 0; flop =0

    Flops = 0

    for i in range(0, len(df)):
        data=df[i:i+1].values.tolist()[0]
               
        pair = int(re.findall(r"[-+]?(?:\d*\.\d+|\d+)", data[3])[0])
        
        pair = ((pair+1)/24)*100
        
        Cnt = int(re.findall(r"[-+]?(?:\d*\.\d+|\d+)", data[6])[0])
        runTime_HT = float(re.findall(r"[-+]?(?:\d*\.\d+|\d+)", data[4])[0])
        runTime_cfar = float(re.findall(r"[-+]?(?:\d*\.\d+|\d+)", data[5])[0])
        
        if Test_type == 'AR1MoC':
            N=7*7*(500*500) # estimated complexity (spatial filtering) + linear compexity of AR lags -- anomaly detec is sublinear
            num_ops = 2*N   # number of images (sub-images)
        elif Test_type == 'AR6MoC': 
            N=7*7*(500*500)
            num_ops = 6*N
            

        flops = (num_ops/runTime_HT)#/10**9
        Flops += flops
        flop += num_ops
        
        CNT.append(Cnt); Pair.append(pair); time_HT.append(runTime_HT); time_cfar.append(runTime_cfar); FLOPS.append(Flops); FLOP.append(flop)

    return Pair, time_HT, time_cfar, FLOPS, FLOP
        


def get_Power(file): 
    
    pair = int(re.findall(r"[-+]?(?:\d*\.\d+|\d+)", file)[-1]) + 1
    RES = open(file).readlines()
    
    POWER=[]; ttime =[]
    for i in range(len(RES)):
        if len(re.findall(r'[ \t]', RES[i])) >= 40 and len(re.findall(r'[-+]?(?:\d*\.\d+|\d+)', RES[i]))==19:
            RES2 = re.findall(r'[-+]?(?:\d*\.\d+|\d+)', RES[i])
            ttime.append(time2sec(RES2[0:3]))
            POWER.append(float(RES2[14]))
            
            
            
    return POWER, timeLine(ttime), pair




def SaveLogs(campaign, Test, pwr, FLOPS):
    
    
    
    pathOut = 'c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/Output/'
    nameFile=pathOut+'Campaign_%s.csv' %campaign
    
    dicList = []
    energy = 0
    keys = ['test_type', 'data_pct', 'flops', 'Power_CPU','timeLine']
    for i in range(len(pwr)):
        PW = pwr[i][0]
        test = pwr[i][2]
        runTime = pwr[i][1]
        Flops = FLOPS[i][3]
        data = FLOPS[i][0]
        dicts = {}
        keys = ['test_type', 'data_pct', 'flops', 'Power_CPU','timeLine']
        values = [test, data, Flops, PW, runTime]
        for k in range(len(keys)):
            dicts[keys[k]] = values[k]
        dicList.append(dicts)
    dic = merge(dicList)
    pd.DataFrame(dic).to_csv(nameFile)
    

if __name__ == "__main__":

    mainDir='c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/Output/logs/'
    
    files = os.listdir(mainDir)
    
    files_logs = [i for i in files if i.endswith('.txt')]
    files_logs = sorted(files_logs, key=lambda x:x[-20:])

    
    campaign = 'kth24'
    

    FLOPS=[]; proc=[]; pwr=[]; Test_type = []
    for i in range(len(files_logs)):
        test_type = files_logs[i].split("_")[1]
       

        if files_logs[i].find('Runtime_%s'%test_type) != -1:
            
            [Pair, time_HT, time_cfar, flops, flop] = get_Flops(mainDir+files_logs[i],test_type)
            tot_time = [sum(x) for x in zip(time_HT,time_cfar)]
            pct_cfar = sum(time_cfar)/sum(tot_time); pct_ht=sum(time_HT)/sum(tot_time)
            res = (Pair, time_HT, time_cfar, flops, flop, test_type,tot_time, pct_ht, pct_cfar)
            FLOPS.append(res)
        
        
        elif files_logs[i].find('Power_%s'%test_type) != -1:
            [power, ttime, pair] = get_Power(mainDir+files_logs[i])
            res = (power, ttime, test_type, pair)
            pwr.append(res)
            
    
    SaveLogs(campaign, Test_type, pwr, FLOPS)
