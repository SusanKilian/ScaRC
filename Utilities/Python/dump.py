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
   #ax.set_zlim(0.0, 0.025)
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
   #ax2.set_zlim(min_val-0.01, max_val+0.01)
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


def plot_exact(name, nx, nz, dx, dz):
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

   exact = []
   rhs = []

   for iz in range(nz):
      for ix in range(nx):
         x = ix*dx + dx/2
         z = iz*dz + dz/2
         x2 = math.pow(x,2)
         x4 = math.pow(x,4)
         z2 = math.pow(z,2)
         z4 = math.pow(z,4)
         exact.append((x2-x4)*(z4-z2))
         rhs.append(2*((1-6*x2)*z2*(1-z2)+(1-6*z2)*x2*(1-x2)))
   print 'exact:',len(exact), exact
   print 'rhs:',len(rhs), rhs

   #---- First subplot
   fig = plt.figure()
   ax = fig.add_subplot(1, 1, 1, projection='3d')

   surf = ax.plot_surface(xp, zp, exact, rstride=1, cstride=1, cmap=cm.jet, antialiased=False)
   ax.set_xlabel('x')
   ax.set_ylabel('y')
   ax.set_zlabel('z')
   ax.zaxis.set_major_locator(LinearLocator(10))
   ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
   fig.colorbar(surf, shrink=0.5, aspect=5)
   
   picture = "pictures/error/3d/%s_exact.png" %(name)
   savefig(picture)
   plt.close(fig)
   #show()

   #---- Second subplot
   fig2 = plt.figure()
   ax2 = fig2.add_subplot(1, 1, 1, projection='3d')

   surf = ax2.plot_surface(xp, zp, rhs, rstride=1, cstride=1, cmap=cm.jet, antialiased=False)
   ax2.set_xlabel('x')
   ax2.set_ylabel('y')
   ax2.set_zlabel('z')
   ax2.zaxis.set_major_locator(LinearLocator(10))
   ax2.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
   fig2.colorbar(surf, shrink=0.5, aspect=5)
   
   picture = "pictures/error/3d/%s_rhs.png" %(name)
   savefig(picture)
   plt.close(fig2)
   #show()


nx0 = int(sys.argv[1])
name = sys.argv[2]

nz0 = nx0

for name in glob.glob("dump/%s*" %name): # generator, search immediate subdirectories 

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

    dx = 1.0/nx
    dz = 1.0/nz

    name = name[5:]
    print name, nx, nz, dx, dz
    (found, quan) = read_quantity(name)

    #plot_exact(name, nx, nz, dx, dz)
    plot_quantity(name, quan, nx, nz, dx, dz)
