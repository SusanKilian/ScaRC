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



def read_quantity(base, name, plot_type, cpres, ite):
   ''' read indicated quantity from corresponding save-directory'''

   quan = []
   found = False
   if plot_type == 'dump':
      dump_name = "%s/%s/%s_%3.3d" %(base, plot_type, name, ite)
   else:
      dump_name = "%s/%s/%s_%s_%3.3d" %(base, plot_type, name, cpres, ite)
   print ("trying to read from %s" %dump_name)

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


def plot_quantity(base, name, plot_type, cpres, quan, ite, nx, nz, dx, dz):
   ''' 3D-plot of specified quantity '''

   if plot_type == 'dump':
      plot_name = "%s/%s_%3.3d.png" %(plot_type, name, ite)
   else:
      plot_name = "%s/%s_%s_%3.3d.png" %(plot_type, cpres, name, ite)
   print ("trying to plot to %s" %plot_name)

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

   val = np.array(val)

   #---- First subplot
   fig = plt.figure()
   ax = fig.add_subplot(1, 1, 1, projection='3d')
   #print 'max, min:', max_val, min_val
   surf = ax.plot_surface(xp, zp, val, rstride=1, cstride=1, cmap=cm.jet, antialiased=False)
   #surf = ax.plot_surface(xp, zp, val, rstride=8, cstride=8, cmap=cm.jet)
   #ax.set_zlim(min_val-0.01, max_val+0.01)
   ax.set_zlim(-10.0, 1500.0)
   #ax.set_xlabel('x')
   #ax.set_ylabel('y')
   ax.set_zlabel('z')
   ax.set_xticklabels([])
   ax.set_yticklabels([])
   #ax.set_zticks([0])
   #ax.zaxis.set_major_locator(LinearLocator(10))
   #ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
   #fig.colorbar(surf, shrink=0.5, aspect=5)
   ax.view_init(24, -69)
   plt.show()
   picture = plot_name
   print 'Plotting picture ', plot_name, min_val, max_val
   savefig(picture)
   plt.close(fig)

   #---- Second subplot
   plot_wireframe = False
   if (plot_wireframe):
      fig2 = plt.figure()
      ax2 = fig2.add_subplot(1, 1, 1, projection='3d')
      ax2.plot_wireframe(xp, zp, val, rstride=1, cstride=1, cmap=cm.jet)
      #ax2.set_zlim(min_val-0.01, max_val+0.01)
      ax2.set_xlabel('x')
      ax2.set_ylabel('y')
      ax2.set_zlabel('z')
      picture = "%wf%s" %(plot_name)
      savefig(picture)
      plt.close(fig2)
      #show()

#base = '../../VisualStudio/Cases'
base = '/Users/susannekilian/GIT/github/01_ScaRC/Verification/Develop/MGM'

nx = 32
nz = 32

dx = 0.8/nx
dz = 0.8/nz

nit = 1

#names = ['h1', 'h2', 'h']
#or ite in range(nit):
#  for name in names:
#     (found, quan) = read_quantity(base, name, 'dump', ' ', ite+1)
#     #if found: plot_quantity(base, name, quan, ite+1, nx, nz, dx, dz)

#names = ['mgm1', 'scarc1','uscarc1', 'glmat1','uglmat1']
names = ['mgm32']
for ite in range(nit):
   for name in names:
      (found, quan) = read_quantity(base, name, 'pressure', 'h1', ite+1)
      if found: plot_quantity(base, name, 'pressure', 'h1', quan, ite+1, nx, nz, dx, dz)
      (found, quan) = read_quantity(base, name, 'pressure', 'h2', ite+1)
      if found: plot_quantity(base, name, 'pressure', 'h2', quan, ite+1, nx, nz, dx, dz)
      (found, quan) = read_quantity(base, name, 'pressure', 'h', ite+1)
      if found: plot_quantity(base, name, 'pressure', 'h', quan, ite+1, nx, nz, dx, dz)
