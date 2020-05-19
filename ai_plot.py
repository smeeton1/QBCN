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


f=open("ai_1o_P.dat","r")
data=[]
y=range(0,1000)
for i in range(0,1000):
    #if f.readline() !=
  data.append(float(f.readline()))

m1=round(min(data)-0.01,2)
m2=round(max(data)+0.01,2)
y_tick=numpy.arange(m1,m2,0.1)
print(m1,m2)

plt.scatter(y,data)
plt.yticks(y_tick, y_tick)
plt.savefig('aierr.jpg')
