from __future__ import division
import numpy as np
from matplotlib import pyplot as plt
import cv2
import os
import scipy.io as sio
from skimage.morphology import disk
from skimage.filters.rank import median
from scipy.spatial.distance import cdist
from scipy.special import erfcinv
import re
from matplotlib import pyplot as plt
import array
import scipy.io
import scipy.io as io
import math
from ast import literal_eval



def Classifier(ICD,tp,pfa, TestType, pair):

            radius = 12
            if TestType == 'CD_demo':
              Imc=np.array(ICD)*1
            th=pfa
            ImAMax = Imc.max(axis=0).max(axis=0)
            (thresh, im_bw) = cv2.threshold(Imc, th, ImAMax, cv2.THRESH_BINARY)
            im_bwN = im_bw

            #Morphologic operators
            kernel1= cv2.getStructuringElement(cv2.MORPH_RECT,(3,3))
            kernel2= cv2.getStructuringElement(cv2.MORPH_RECT,(5,5))
            erosion = cv2.erode(im_bwN,kernel1,iterations = 1)
            dilation1 = cv2.dilate(erosion,kernel1,iterations = 1)
            dilation2 = cv2.dilate(dilation1,kernel2,iterations = 1)
            Imc=dilation2


            u8 =Imc.astype(np.uint8)
            nb_components, output, stats, centroids0 = cv2.connectedComponentsWithStats(u8, connectivity=8)
            sizes = stats[1:, -1]

            centroids0 = centroids0[1:]
            centroids=[]
            for i in range(len(centroids0)):
                if sizes[i]>30 and sizes[i]<800:
                    centroids.append(centroids0[i])
            centroids = np.array(centroids)
            num_Objects = len(centroids)
            centro = centroids[np.argsort(centroids[:, 0])]

            if num_Objects > 0:
                outlines = []
                detected = [] 
                for k in range(len(tp)):
                    for m in range(len(centro)):
                        if (np.abs(centro[m][0] - tp[k][0]) < radius) and (np.abs(centro[m][1] - tp[k][1]) < radius):
                            detected.append(m)
                        else:
                            outlines.append(m)
      
                detected_dict = {i:detected.count(i) for i in detected}
                outlines_dict = {i:outlines.count(i) for i in outlines}
                detected_idx = list(detected_dict.keys())
                outlines_idx = list(outlines_dict.keys())
                outlines_idx = [ x for x in outlines_idx if not x in detected_idx]
                potential_targets = num_Objects
                detected_targets= len(detected_idx) 
                if detected_targets > len(tp):
                    detected_targets = len(tp)
                    falseAlarm = np.abs(potential_targets - len(tp))
                else:
                    falseAlarm = np.abs(potential_targets - detected_targets)
            else:
                detected_targets = 0
                falseAlarm = 0

            mission_targets = len(tp)

            if pair==0 or pair==1 or pair==2 or pair==3 or pair==4 or pair==5:
              start=400; end = 1000; Start=-400
            
            f, ax = plt.subplots()
            for k in outlines_idx:
                circleOutlines = plt.Circle((centro[k][0], centro[k][1]+Start), 15, color='r', fill=False)
                ax.add_patch(circleOutlines)
                ax.imshow(Imc[start:end], cmap='gray', interpolation='nearest')
            for j in detected_idx:
                circleDetected = plt.Circle((centro[j][0], centro[j][1]+Start), 15, color='g', fill=False)
                ax.imshow(Imc[start:end], cmap='gray', interpolation='nearest')
                ax.add_patch(circleDetected)
            plt.axis("off")
            
            # path='/home/marcello-costa/workspace/Demo1/class/'
            path = 'c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/class/'
            f.savefig(path + 'Class_%s_%s.png'%(TestType, pair), dpi=350,bbox_inches='tight',transparent=True, pad_inches=0)
            f.tight_layout()
            
            
            plt.show()

            return detected_targets, falseAlarm, mission_targets


if __name__ == "__main__":
    
    
    # path = '/home/marcello-costa/workspace/Demo1/Out/'
    path = 'c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/Out/'
    
    files = os.listdir(path+'/')
    files_images = [i for i in files if i.endswith('.txt')]
    files_images = sorted(files_images, key=lambda s: int(re.search(r'\d+', s).group()))
    
    n = 500; W =500
    lags = 1
    pfa = 0.6 # or 0.45 to 0.7 best average PD/Pfa compromises over all dataset
    
    
    # .dat to dict of arrays
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
        

    test_type = ['CD_demo']
    test_type  = test_type[0] 
    pair = 0
 
    
    
    # pathTargetsPosition = '/home/marcello-costa/workspace/Demo1/SampleData/data/targets/'
    pathTargetsPosition = 'c:/Users/webbe/OneDrive/Documents/KTH/ESDP/ChangeDetection/Demos/SampleData/data/targets/'
    
    # pad for original size 3k x 2k
    if pair==0 or pair==1 or pair==2 or pair==3 or pair==4 or pair==5:
      tp=sio.loadmat(pathTargetsPosition+'S1.mat'); tp=tp['S1']; tp = np.fliplr(tp)
      OUT=np.pad(REC[0], ((450,0),(350,0)), mode='constant')
      ICD=np.pad(OUT, ((0,2050),(0,850)), mode='constant')
      
         
    # classification
    [detected_targets, falseAlarm, mission_targets]=Classifier(ICD,tp,pfa, test_type, pair)

    # Detection summary
    print('Pair:', pair, ' ', 'Test:',test_type )
    print('Detected Target:', detected_targets)
    print('False Alarms:', falseAlarm)
    
