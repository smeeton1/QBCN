import sys
import numpy 
import cmath 
import math 
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import scipy.integrate as integrate
import statistics
import scipy.sparse as sps


f=open("cycle.txt","r")

for i in range(0,5):
  node=[]
  nodec=[]
  step=f.readline()
  #print(step)
  norm=f.readline()
  #print(norm)
  for j in range(0,10):
    data=f.readline()
 #   print(data)
    node.append(float(data))
    nodec.append(-0.1)
  t_f=f.readline()
  hold=t_f.split()
  col=[]
  for tf in hold:
    if tf == 'T':
      col.append('g')
    else:
      col.append('r')
  #print(hold)
  #print(col)
  x_tick=range(1,11)
  y_tick=numpy.arange(-0.1,1,0.1)
  
  plt.figure(i)
  plt.bar(x_tick, node, align='center', alpha=0.5)
  plt.bar(x_tick, nodec, align='center', alpha=0.5,color=col)
  plt.xticks(x_tick, x_tick)
  plt.yticks(y_tick, y_tick)
  plt.ylabel('Probablity')
  plt.xlabel('node')
  imgname='step_%d.eps'%(i)
  plt.savefig(imgname)
  del node
  del col
  del nodec
  



f.close()