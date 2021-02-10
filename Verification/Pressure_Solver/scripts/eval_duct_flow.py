#me -*- coding: utf-8 -*-

#####################################################################################
# Initial import
#####################################################################################
import os.path

import sys
import math
from pylab import *
from matplotlib import *
import matplotlib.pyplot as plt
import matplotlib.font_manager as fnt


def read_csv(chid, time, quan, nquan, title, isim, qerr, qite, zoom_x1, zoom_x2): 

    time_sim  = []
    quan_sim  = []

    found1 = False
    found2 = False

    b1=0
    b2=0

    for iq in range(nquan):
       quan_sim.append([])

    chid_sim = chid[isim]
    name_chid= "%s_devc.csv" % (chid_sim)

    if not os.path.isfile(name_chid): 
       print 'Not found:  %s' %name_chid
       return 0

    print 'Processing: %s' %name_chid
    chid_in  = open(name_chid ,'r')
    chid_input  = chid_in.readlines()
    chid_in.close()

    num=0
    start = 2

    for line in chid_input:

        if start == 1:
           line, null = line.split ("\n")
           titles = line.split (",")
        
        if (start > 0):
            start -= 1
            continue
        num=num+1

        line, null = line.split ("\n")
        values = line.split (",")

        t = float(values[0])
        if t>zoom_x1 and not found1:
           b1 = num
           found1 = True
        if t>zoom_x2 and not found2:
           b2 = num
           found2 = True
              
        time_sim.append(t)

        for iq in range(nquan):
           q = float(values[iq+1])
           quan_sim[iq].append(q)

    # compute statistics

    t1=1
    t2=num/2
    t3=num-1

    print num, t1, t2, t3

    max1_err  = max(quan_sim[qerr][t1:t2])
    max2_err  = max(quan_sim[qerr][t2:t3])
    mean1_err = numpy.mean(quan_sim[qerr][t1:t2])
    mean2_err = numpy.mean(quan_sim[qerr][t2:t3])

    print '%20s: max1: %7.1e, max2: %7.1e, mean1: %7.1e, mean2: %7.1e' %(titles[qerr+1], max1_err, max2_err, mean1_err, mean2_err)

    max1_ite  = max(quan_sim[qite][t1:t2])
    max2_ite  = max(quan_sim[qite][t2:t3])
    mean1_ite = numpy.mean(quan_sim[qite][t1:t2])
    mean2_ite = numpy.mean(quan_sim[qite][t2:t3])

    print '%20s: max1: %8d, max2: %8d, mean1: %8d, mean2: %8d' %(titles[qite+1], max1_ite, max2_ite, mean1_ite, mean2_ite)

    ##print 'chid_sim=',chid_sim,': time_sim=',time_sim[0:5]
    time.append(time_sim)
    quan.append(quan_sim)
    title.append(titles)

    return  (b1, b2)


def plot_csv(chid, time, quan, iquan, title, nsim, name, b1, b2, zoom_x1, zoom_x2, zoom_y1, zoom_y2):

    quantity = '%s'%(title[0][iquan+1].strip('\"'))

    fig = plt.figure (facecolor='w')
    ax = fig.add_subplot(111)

    linewidths = [0.75,0.75, 0.75, 0.75, 0.75, 0.75, 0.75]
    markers = ["s","D","o","v","h","^","<","s","D","o","v","h","^","<","s","D","o","v","h","^","<","s"]
    colors  = ["r","g","b","m","c","orange","g","b","r","m","k","b","r","g","m","c","k","b","r","c","g"]
    colors  = ["g","m","b","b","r","c","g","m","k","b","r","g","m","c","k","b","r","c","g"]
    linestyles = ['-', '-', '-', '-','-', '-', '-', '--','-', '-', '-', '--']

    legsize = fnt.FontProperties(size=11)

    index  = []
    pstart = []
    pend   = []
    legend = []

    pstart0=1
    pend0=len(time[1])

    #pstart0=b1
    #pend0  =b2

    step = (pend0 - pstart0)/(nsim+1)
 
    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.15, box.width, box.height * 0.89])

    pstart0 += step/2
    print pstart0, pend0, step
    for isim in range(nsim):
      mark =  (pstart0, 5000)
      if 'glmat' in chid[isim] and 'SCARC' in name: continue
      if 'fft' in chid[isim] and len(chid[isim]) <=17 and 'SCARC' in name: continue
      ax.plot (time[isim], quan[isim][iquan],'-r', linewidth=linewidths[isim], linestyle=linestyles[isim], markevery=mark, marker=markers[isim], color = colors[isim])
      pstart0 += step

      if 'default' in chid[isim]:
          legend.append('fft')
      elif 'glmat' in chid[isim]:
          legend.append('glmat')
      elif 'uglmat' in chid[isim]:
          legend.append('uglmat')
      elif 'scarc' in chid[isim]:
          legend.append('scarc')
      elif 'uscarc' in chid[isim]:
          legend.append('uscarc')
      elif 'mgm_expol' in chid[isim]:
          legend.append('mgm_expol')
      elif 'mgm_simple' in chid[isim]:
          legend.append('mgm_simple')


    ax.legend(legend, prop=legsize, loc="lower center", 
              bbox_to_anchor=(0.50, -0.3), fancybox=True, shadow=True, ncol=3)

    ax.grid(True)
    #ax.set_title('%s meshes'%(top),fontsize=30)

    #ax.set_xlabel('Time [s]',fontsize=16)
    #ax.set_ylabel(r'%s' %title[0][iquan+1].strip('\"'),fontsize=16)


    ax.set_xlim(tstart,tend)
    ax.set_xlabel(r'Time [s]',fontsize=16)
    yname='none'
    if  'U-VEL' in quantity or 'u-vel' in quantity:
       yname = r'U-Velocity [m/s]'
       ax.set_ylim(-0.001,0.004)
    elif  'V-VEL' in quantity or 'v-vel' in quantity:
       yname = r'V-Velocity [m/s]'
    elif  'W-VEL' in quantity or 'w-vel' in quantity:
       yname = r'W-Velocity [m/s]'
       ax.set_ylim(-0.001,0.004)
    elif  'flow_in' in quantity:
       yname = r'flow-in [m/s]'
       #ax.set_ylim(-0.001,0.004)
    elif  'flow_out' in quantity:
       yname = r'flow-out [m/s]'
       #ax.set_ylim(-0.001,0.004)
    elif  'PRESSURE' in quantity or 'pres' in quantity:
       yname = r'Pressure [Pa]'
       ax.set_ylim(-0.4, 0.20)
       #ax.set_xlim(zoom_x1, zoom_x2)
       #ax.set_ylim(zoom_y1, zoom_y2)
    elif  'Vel-err' in quantity or 'error' in quantity:
       yname = r'Velocity Error [m/s]'
       ax.set_yscale('log')
       #ax.set_ylim(1E-6,0.1)
       ax.set_ylim(1E-17,0.01)
    elif 'Pres-ite' in quantity or 'iter' in quantity:
       yname = r'Pressure iterations'
       #ax.set_ylim(-1,160)
       ax.set_ylim(0,100)
    elif  'ScaRC-ite' in quantity or 'scarc-iter' in quantity:
       yname = r'ScaRC Iterations'
    elif  'ScaRC-rate' in quantity or 'scarc-rate' in quantity:
       yname = r'ScaRC convergence rate'
    elif  'ScaRC-res' in quantity or 'scarc-res' in quantity:
       yname = r'ScaRC Residual'

    ax.set_ylabel(r'%s' %yname,fontsize=16)


    labels = ax.get_xticklabels() + ax.get_yticklabels()
    for label in labels:
        label.set_size(11)

    output = "pictures/%s/%s.png" % (name,quantity)
    savefig(output)
    plt.close(fig)


tstart= 0.0
tend  = 60.0

b1=0
b2=0

name = 'duct_flow'
nquan=4

qerr=2
qite=3

zoom_x1 = 1.24
zoom_x2 = 1.33

zoom_y1 = -0.17
zoom_y2 =  0.11


chid=[
      '%s_uglmat' %name,
      '%s_uscarc' %name,
      '%s_mgm_expol' %name,
      '%s_mgm_simple' %name,
      ]


nsim = len(chid)
if nsim == 0: sys.exit('no chids defined')

if not os.path.exists("pictures/%s" %name): os.makedirs("pictures/%s" %name)
   
time = []
quan = []
title = []

for isim in range(nsim):
   (b1, b2) = read_csv(chid, time, quan, nquan, title, isim, qerr, qite, zoom_x1, zoom_x2)


print '---------------> Printing to %s, b1=%4d, b2=%4d' %(name, b1, b2)
for iquan in range(nquan):
   plot_csv(chid, time, quan, iquan, title, nsim, name, b1, b2, zoom_x1, zoom_x2, zoom_y1, zoom_y2)
   




