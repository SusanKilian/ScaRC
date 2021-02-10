#me -*- coding: utf-8 -*-

#####################################################################################
# Initial import
#####################################################################################
import sys
from pylab import *
from matplotlib import *
import matplotlib.pyplot as plt
import matplotlib.font_manager as fnt
import matplotlib


site = []
sres = []
scon = []

def read_csv(chid, time, v1, v2, v3, v4,  i):

    time0  =[]
    v10  =[]
    v20  =[]
    v30  =[]
    v40  =[]

    chid0 = chid[i]
    name_chid= "%s_devc.csv" % (chid0)
    print 'reading %s'%name_chid

    chid_in  = open(name_chid ,'r')
    chid_input  = chid_in.readlines()
    chid_in.close()

    num=0
    start = 3

    for line in chid_input:

        if (start > 0):
            start -= 1
            continue
        num=num+1

        line, null = line.split ("\n")
        quan = line.split (",")

        t = float(quan[0])
        time0.append(t)

        v = float(quan[1])
        v10.append(v)

        v = float(quan[2])
        v20.append(v)

        v = float(quan[3])
        v30.append(v)

        v = float(quan[4])
        v40.append(v)

    time.append(time0)
    v1.append(v10)
    v2.append(v20)
    v3.append(v30)
    v4.append(v40)


def plot_csv(case, chid, time, quan, name, tstart, tend):


    fig = plt.figure (facecolor='w')
    ax = fig.add_subplot(111)

    markers = ["s","D","o","v","h","^","<"]
    colors  = ["k","r","b","g","c","m","y","k","r","b","g","c","m","y"]
    linestyles  = ["-","-.","--",":","-","-.","--",":","-","-.","--",":"]

    legsize = fnt.FontProperties(size=8)

    clen = len(chid)
    nsim = len(time)

    index = []
    legend = []

    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.15, box.width, box.height * 0.85])

    for i in range(nsim):
       if name[0] == "s" and "cg" not in chid[i]: continue
       #ax.plot (time[i], quan[i],'-r', linewidth=0.2, marker=markers[i], color = colors[i])
       ax.plot (time[i], quan[i], linewidth=1.0, linestyle = linestyles[i], color = colors[i])
       legend.append(chid[i])

    ax.legend(legend, prop=legsize, loc="lower center", 
              bbox_to_anchor=(0.50, -0.25), fancybox=True, shadow=True, ncol=2)

    ax.grid(True)
    ax.set_title('%s '%(case),fontsize=25)

    ax.set_xlabel('Time [s]',fontsize=16)
    ax.set_ylabel(r'%s'%name,fontsize=16)

    ax.set_xlim(tstart,tend)
    if name == 'pres': ax.set_ylim(-0.3, 0.2)

    labels = ax.get_xticklabels() + ax.get_yticklabels()
    for label in labels:
        label.set_size(13)

    output = "pictures/%s_%s.png" % (case, name)
    print 'Printing ', output
    savefig(output)
    #show()


#nmeshes = int(sys.argv[1])
#obst    = sys.argv[2]
#tstart = 0.1
#tend   = 1.00

case = 'dancing_eddies'
nmeshes = 4
tstart = 0.0
tend = 2.0

chid=[]
chid.append('%s_uglmat' %case)
chid.append('%s_uscarc' %case)
chid.append('%s_mgm1' %case)
chid.append('%s_mgm2' %case)
chid.append('%s_mgm5' %case)
chid.append('%s_mgm10' %case)
chid.append('%s_mgm20' %case)
chid.append('%s_mgm50' %case)
#chid.append('%s_mgm_expol' %case)


nsim = len(chid)

values = ['pres','error','iter','cpu']

time = []
v1 = []
v2 = []
v3 = []
v4 = []

for i in range(nsim):
   read_csv(chid, time, v1, v2, v3, v4, i)

plot_csv(case, chid, time, v1, values[0], tstart, tend)
plot_csv(case, chid, time, v2, values[1], tstart, tend)
plot_csv(case, chid, time, v3, values[2], tstart, tend)
plot_csv(case, chid, time, v4, values[3], tstart, tend)


