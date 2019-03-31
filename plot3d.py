import sys
import numpy 
import cmath 
import math 
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import statistics
import scipy.sparse as sps


f=open("cycle.txt","r")
node=[[] for i in range(0,5)]#numpy.zeros(10*5)#
nodec=[[] for i in range(0,5)]
col=[[] for i in range(0,5)]
for i in range(0,5):

  step=f.readline()
  #print(step)
  norm=f.readline()
  #print(norm)
  for j in range(0,10):
    data=f.readline()
 #   print(data)
    node[i].append(float(data))
    nodec[i].append(-0.1)
  t_f=f.readline()
  hold=t_f.split()

  for tf in hold:
    if tf == 'T':
      col[i].append('g')
    else:
      col[i].append('r')
  #print(hold)
  #print(col)
x_tick=numpy.arange(1,11)
y_tick=numpy.arange(0,5)
_xx, _yy = numpy.meshgrid(x_tick, y_tick)
x, y = _xx.ravel(), _yy.ravel()
z_tick=numpy.arange(-0.1,1.1,0.1)
z_pos=numpy.zeros_like(x)
z=numpy.ones_like(x)
#print(node[1])
#print(z_pos)
#node=node.ravel() 
#print(node)
fig=plt.figure()
ax1=fig.add_subplot(111, projection='3d')
for i in range(0,5):
 c = plt.cm.jet(i/5)
 ax1.bar(x_tick,node[i],  zs=y_tick[i], zdir='y',align='center',color='b')
 ax1.bar(x_tick,nodec[i],  zs=y_tick[i], zdir='y',align='center',  alpha=0.5,color=col[i])
ax1.set_xticks(x_tick)
ax1.set_yticks(y_tick)
ax1.set_zticks(z_tick)
ax1.set_ylabel('steps')
ax1.set_xlabel('node')
ax1.set_zlabel('probablity')
imgname='3d.eps'
plt.savefig(imgname)
#plt.show()
del node
del col
del nodec
  



f.close()