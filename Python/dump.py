#me -*- coding: utf-8 -*-

import sys, os
import re
import math
import glob
from pylab import *
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import matplotlib.font_manager as fnt
from matplotlib import *
from matplotlib.ticker import LinearLocator, FormatStrFormatter
import numpy as np



def read_quantity(name):
   ''' read indicated quantity from corresponding save-directory'''

   quan = []
   found = False
   dump_name = "dump/%s" %(name)
   #print ("reading from %s" %dump_name)

   if os.path.exists(dump_name) :

      found = True

      f  = open(dump_name ,'r')
      input  = f.readlines()
      f.close()

      for line in input:
        line, null = line.split ("\n")
        value = line.split (",")
        quan.append(float(value[0]))
   
   return (found,quan)


def plot_quantity(name, quan, nx, nz, dx, dz):
   ''' 3D-plot of specified quantity '''



   xp = []
   for ix in range(nx):
      x = ix*dx + dx/2
      xp.append(x)
      
   zp = []
   for iz in range(nz):
      z = iz*dz + dz/2
      zp.append(z)

   xp, zp = np.meshgrid(xp, zp)
   val = []

   max_val = -10000.0
   min_val =  10000.0
   for iz in range(nz):
      line = []
      for ix in range(nx):
         pos = iz * nx + ix
         max_val = max(max_val, quan[pos])
         min_val = min(min_val, quan[pos])
         line.append(quan[pos])
      val.append(line)


   #---- First subplot
   fig = plt.figure()
   ax = fig.add_subplot(1, 1, 1, projection='3d')

   #print 'max, min:', max_val, min_val
   surf = ax.plot_surface(xp, zp, val, rstride=1, cstride=1, cmap=cm.jet, antialiased=False)
   #surf = ax.plot_surface(xp, zp, val, rstride=8, cstride=8, cmap=cm.jet)
   ax.set_zlim(min_val-0.01, max_val+0.01)
   #if "err" in name:
   #   ax.set_zlim(min_val-0.01, 0.201)
   #else:
   #   ax.set_zlim(min_val-0.01, 1.501)
   ax.set_xlabel('x')
   ax.set_ylabel('y')
   ax.set_zlabel('z')
   ax.set_zlim(0.0, 0.025)
   ax.zaxis.set_major_locator(LinearLocator(10))
   ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
   fig.colorbar(surf, shrink=0.5, aspect=5)
   
   picture = "pictures/error/3d/%s.png" %(name)
   #print 'plotting picture ', picture
   savefig(picture)
   plt.close(fig)
   #show()

   #---- Second subplot
   fig2 = plt.figure()
   ax2 = fig2.add_subplot(1, 1, 1, projection='3d')
   ax2.plot_wireframe(xp, zp, val, rstride=1, cstride=1, cmap=cm.jet)
   ax2.set_zlim(min_val-0.01, max_val+0.01)
   #if "err" in name:
   #   ax.set_zlim(min_val-0.01, 0.201)
   #else:
   #   ax.set_zlim(min_val-0.01, 1.501)
   ax2.set_xlabel('x')
   ax2.set_ylabel('y')
   ax2.set_zlabel('z')
   ax2.set_zlim(0.0, 0.025)

   picture = "pictures/error/3d_wf/%s.png" %(name)
   #print 'plotting picture ', picture
   savefig(picture)
   plt.close(fig2)
   #show()

nx0 = int(sys.argv[1])
nz0 = nx0

for name in glob.glob("dump/err*"): # generator, search immediate subdirectories 

    nx=nx0
    nz=nz0
    if "level2" in name:
        nx=nx0/2
        nz=nz0/2
    elif "level3" in name:
        nx=nx0/4
        nz=nz0/4
    elif "level4" in name:
        nx=nx0/8
        nz=nz0/8

    dx = 0.8/nx
    dz = 0.8/nz

    name = name[5:]
    print name, nx, nz, dx, dz
    (found, quan) = read_quantity(name)

    plot_quantity(name, quan, nx, nz, dx, dz)
