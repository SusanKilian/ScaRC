from pylab import savefig
import matplotlib.pyplot as plt
import matplotlib.font_manager as fnt


site = []
sres = []
scon = []


def read_csv(path, chid, time, v1, v2, v3, v4, i):

    time0 = []
    v10 = []
    v20 = []
    v30 = []
    v40 = []

    chid0 = chid[i]
    name_chid = "%s/%s_steps.csv" % (path, chid0)
    print('reading %s' % name_chid)

    chid_in = open(name_chid, 'r')
    chid_input = chid_in.readlines()
    chid_in.close()

    num = 0
    start = 2

    for line in chid_input:

        if (start > 0):
            start -= 1
            continue
        num = num+1

        line, null = line.split("\n")
        quan = line.split(",")

        t = float(quan[0])
        time0.append(t)

        v = quan[1]
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

    fig = plt.figure(facecolor='w')
    ax = fig.add_subplot(111)

    # markers = ["s", "D", "o", "v", "h", "^", "<"]
    colors = ["r", "r", "b", "g", "c", "m", "k", "b", "g", "k", "b", "g", "k"]
    linestyles = ["-", "-.", ":", "--", "-", "-.", ":", "--", "-", "-.", ":"]

    legsize = fnt.FontProperties(size=8)

    # clen = len(chid)
    nsim = len(chid)
    legend = []

    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height*0.15,
                     box.width, box.height*0.85])

    for i in range(nsim):
        # print('=====================> ', i)
        # print(time[i][0:4])
        # print(quan[i][0:4])
        # print(linestyles[i])
        # print(colors[i])
        ax.plot(time[i], quan[i], linewidth=1.0, linestyle=linestyles[i],
                color=colors[i])
        legend.append(chid[i])

    ax.legend(legend, prop=legsize, loc="lower center",
              bbox_to_anchor=(0.50, -0.25), fancybox=True, shadow=True, ncol=3)

    ax.grid(True)
    ax.set_xlabel('Time [s]', fontsize=16)
    ax.set_ylabel(r'%s' % name, fontsize=16)

    ax.set_xlim(0.0, tend)
    # ax.set_xlim(0.0, 5)

    if 'Step' in name:
        ax.set_ylim(0., 0.006)
    elif 'CPU' in name:
        ax.set_ylim(0., 70000.0)
    # ax.set_ylim(0., 400.0)

    labels = ax.get_xticklabels() + ax.get_yticklabels()
    for label in labels:
        label.set_size(13)

    output = "pictures/%s_%s.png" % (case, name)
    print('Printing ', output)
    savefig(output)
    plt.close()
    # show()


# nmeshes = int(sys.argv[1])
# obst    = sys.argv[2]
# tstart = 0.1
# tend   = 1.00

base = '/home/susanne/Tank/Samba/GIT/A_ScaRC/'
folder = 'Validation/Tunnel_Ventilation/FDS_Input_Files/Lunarc'
path = base + folder
case = 'Wu_Bakar_Tunnel_A'
nmeshes = 30
tstart = 0.0
tend = 165.0

chid = []
chid2 = []

chid.append('%s' % case)
chid.append('%s_fine' % case)
# chid.append('%s_scarc_xmean_add_06' % case)
# chid.append('%s_scarc_xmean_add_06_low' % case)
chid.append('%s_scarc_xmean_add_06_verylow' % case)
chid.append('%s_scarc_cgamg2_one' % case)
chid.append('%s_scarc_cgamg4_one' % case)
chid.append('%s_scarc_cgamg8_one' % case)
chid.append('%s_scarc_cgamg2_low' % case)
chid.append('%s_scarc_cgamg4_low' % case)
chid.append('%s_scarc_cgamg8_low' % case)

chid2.append('default')
chid2.append('fine')
# chid2.append('add_06')
# chid2.append('add_06_low')
chid2.append('add_06_verylow')
chid2.append('cgamg2_one')
chid2.append('cgamg4_one')
chid2.append('cgamg8_one')
chid2.append('cgamg2_low')
chid2.append('cgamg4_low')
chid2.append('cgamg8_low')


nsim = len(chid)

values = ['Step_Size', 'Simulation_Time', 'CPU_Time']

time = []
v1 = []
v2 = []
v3 = []
v4 = []

for i in range(nsim):
    read_csv(path, chid, time, v1, v2, v3, v4, i)

# print('v1====================')
# print(v1[0][0:4], v1[0][0:4], v1[0][0:4])
# print('v3====================')
# print(v3[0][0:4], v3[0][0:4], v3[0][0:4])
# print('v4====================')
# print(v4[0][0:4], v4[0][0:4], v4[0][0:4])
# print('ready====================')

plot_csv(case, chid2, v3, v2, values[0], tstart, tend)
plot_csv(case, chid2, v3, v4, values[2], tstart, tend)
