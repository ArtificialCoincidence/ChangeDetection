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




def ImageReconstructionSeq(path, n, W):
  
  files = os.listdir(path+'/')
  files_images = [i for i in files if i.endswith('.txt')]
  files_images = sorted(files_images, key=lambda s: int(re.search(r'\d+', s).group()))
  

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
  
  return REC[0]


def ImageReconstructionPar(path, n, W):
  
  files = os.listdir(path+'/')
  files_images = [i for i in files if i.endswith('.txt')]
  files_images = sorted(files_images, key=lambda s: int(re.search(r'\d+', s).group()))
  
  
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
      
  res1 = np.concatenate((REC[0],REC[1]), axis=0, out=None)
  res2 = np.concatenate((REC[2],REC[3]), axis=0, out=None)

  return np.concatenate((res1,res2), axis=1, out=None)




def Classifier(ICD,tp,pfa, TestType, pair):

            radius = 12
            Imc=np.array(ICD)*10
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
            elif pair==6 or pair==7 or pair==8 or pair==9 or pair==10 or pair==11:
              start=200; end = 800; Start=-200
              
            elif pair==12 or pair==13 or pair==14 or pair==15 or pair==16 or pair==17:
              start=2070; end = 2570 ; Start=-2070
              
            elif pair==18 or pair==19 or pair==20 or pair==21 or pair==22 or pair==23:
              start=2330; end = 2830 ; Start=-2330
            
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
            
            path='./Plots/'
            f.savefig(path + 'Class_%s_%s.png'%(TestType, pair), dpi=350,bbox_inches='tight',transparent=True, pad_inches=0)
            f.tight_layout()
            
            
            plt.show()

            return detected_targets, falseAlarm, mission_targets


if __name__ == "__main__":
  
    idtest = 1
    test = ['Seq', 'ImageSplitting', 'ParDai', 'SeqDai']
    
    idlag = 3
    Lag = [1, 2, 3, 6]
    
    idPair=0
    Pair = ['S1', 'K1','F1','AF1']
    
    
    if test[idtest] == 'Seq':
      path = '/home/marcello-costa/workspace/ChangeDetection/Out/%s/Lag%s/Seq/'%(Pair[idPair],Lag[idlag])
      TestName = test[idtest]+'_'+'MC'+str(Lag[idlag])+'AR1'
      
      if Pair[idPair] == 'S1':
        pair = 0
        pfa =  1.3     
        
      elif Pair[idPair] == 'K1':
        pair = 6
        pfa = 1.23 
        
      elif Pair[idPair] == 'F1':
        pair = 12
        pfa = 1.95
        
      elif Pair[idPair] == 'AF1':
        pair = 18
        pfa = 1.08
 
      n=500; W= 500
      resf = ImageReconstructionSeq(path, n, W)
 
      
    elif test[idtest] == 'ImageSplitting':
      path = './Out/%s/Lag%s/ImageSplitting/'%(Pair[idPair],Lag[idlag])
      TestName = test[idtest]+'_'+'MC'+str(Lag[idlag])+'AR1'
      
      if Pair[idPair] == 'S1':
        pair = 0
        pfa =  1.3     
        
      elif Pair[idPair] == 'K1':
        pair = 6
        pfa = 1.23 
        
      elif Pair[idPair] == 'F1':
        pair = 12
        pfa = 1.95
        
      elif Pair[idPair] == 'AF1':
        pair = 18
        pfa = 1.08
      
      
      n=250; W= 250
      resf = ImageReconstructionPar(path, n, W)
      
      
    elif test[idtest] == 'ParDai':
      path = '/home/marcello-costa/workspace/ChangeDetection/Out/%s/Lag%s/ParDai/'%(Pair[idPair],Lag[idlag])
      TestName = test[idtest]+'_'+'MC'+str(Lag[idlag])+'AR1'
      
      if Pair[idPair] == 'S1':
        pair = 0
        pfa =  1.3     
        
      elif Pair[idPair] == 'K1':
        pair = 6
        pfa = 1.23 
        
      elif Pair[idPair] == 'F1':
        pair = 12
        pfa = 1.95
        
      elif Pair[idPair] == 'AF1':
        pair = 18
        pfa = 1.08
 
      n=500; W= 500
      resf = ImageReconstructionSeq(path, n, W)
      
    elif test[idtest] == 'SeqDai':
      path = '/home/marcello-costa/workspace/ChangeDetection/Out/%s/Lag%s/SeqDai/'%(Pair[idPair],Lag[idlag])
      TestName = test[idtest]+'_'+'MC'+str(Lag[idlag])+'AR1'
      
      if Pair[idPair] == 'S1':
        pair = 0
        pfa =  1.3     
        
      elif Pair[idPair] == 'K1':
        pair = 6
        pfa = 1.23 
        
      elif Pair[idPair] == 'F1':
        pair = 12
        pfa = 1.95
        
      elif Pair[idPair] == 'AF1':
        pair = 18
        pfa = 1.08
 
      n=500; W= 500
      resf = ImageReconstructionSeq(path, n, W)
      
      
      
        
      
    pathTargetsPosition = './SampleData/data/targets/'
    
    # pad for original size 3k x 2k
    if pair==0 or pair==1 or pair==2 or pair==3 or pair==4 or pair==5:
      tp=sio.loadmat(pathTargetsPosition+'S1.mat'); tp=tp['S1']; tp = np.fliplr(tp)
      OUT=np.pad(resf, ((450,0),(350,0)), mode='constant')
      ICD=np.pad(OUT, ((0,2050),(0,1150)), mode='constant')
      
      #start1=250; end1 = 750; start2=350; end2 = 850 # K1 
    elif pair==6 or pair==7 or pair==8 or pair==9 or pair==10 or pair==11:
      tp=sio.loadmat(pathTargetsPosition+'K1.mat'); tp=tp['K1']; tp = np.fliplr(tp)
      OUT=np.pad(resf, ((250,0),(350,0)), mode='constant')
      ICD=np.pad(OUT, ((0,2250),(0,1150)), mode='constant')
      
      
      
    elif pair==12 or pair==13 or pair==14 or pair==15 or pair==16 or pair==17:
      tp=sio.loadmat(pathTargetsPosition+'F1.mat'); tp=tp['F1']; tp = np.fliplr(tp)
      OUT=np.pad(resf, ((2050,0),(1100,0)), mode='constant')
      ICD=np.pad(OUT, ((0,450),(0,400)), mode='constant')
      
    elif pair==18 or pair==19 or pair==20 or pair==21 or pair==22 or pair==23:
      tp=sio.loadmat(pathTargetsPosition+'AF1.mat'); tp=tp['AF1']; tp = np.fliplr(tp)
      OUT=np.pad(resf, ((2350,0),(950,0)), mode='constant')
      ICD=np.pad(OUT, ((0,150),(0,550)), mode='constant')

    print(ICD.shape)
    
   
     
    # fig = plt.figure('test')
    # #plt.suptitle('Binarizada')
    
    # ax = fig.add_subplot(1, 1, 1)
    # plt.imshow(ICD, cmap = plt.cm.gray)
    # ax.set_title("Test Detection")
    # plt.axis("off")
    
   
    
    # plt.show() 
      
         
    # # #classification
    [detected_targets, falseAlarm, mission_targets]=Classifier(ICD,tp,pfa, TestName, pair)

    # Detection summary
    print('Pair:', pair, ' ', 'Test:',TestName)
    print('Detected Target:', detected_targets)
    print('False Alarms:', falseAlarm)
    
