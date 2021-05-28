import os
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np


def read_quantity(scarc, direc, chid, name, ite):
    ''' read indicated quantity from corresponding save-directory'''

    quan = []
    found = False
    dump_name = "%s/%s/dump/%s_%s_%3.3d" % (scarc, direc, chid, name, ite)
    print("trying to read from %s" % dump_name)

    if os.path.exists(dump_name):

        found = True

        f = open(dump_name, 'r')
        input = f.readlines()
        f.close()

        for line in input:
            line, null = line.split("\n")
            value = line.split(",")
            quan.append(float(value[0]))

    return (found, quan)


def plot_quantity(scarc, chid, name, quan, ite, nx, nz, dx, dz):
    ''' 3D-plot of specified quantity '''



    xp = []
    for ix in range(nx+2):
        x = ix*dx + dx/2
        xp.append(x)

    zp = []
    for iz in range(nz+2):
        z = iz*dz + dz/2
        zp.append(z)

    xp, zp = np.meshgrid(xp, zp)
    val = []

    max_val = -10000.0
    min_val = 10000.0
    for iz in range(nz+2):
        line = []
        for ix in range(nx+2):
            pos = iz * (nx+2) + ix
            max_val = max(max_val, quan[pos])
            min_val = min(min_val, quan[pos])
            line.append(quan[pos])
        val.append(line)

    val = np.array(val)

    # ---- First subplot
    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1, projection='3d')
    # print 'max, min:', max_val, min_val
    surf = ax.plot_surface(xp, zp, val, rstride=8, cstride=8, cmap=cm.jet)
    # ax.set_zlim(min_val-0.01, max_val+0.01)
    ax.set_zlim(min_val*0.9, max_val*1.1)
    # ax.set_xlabel('x')
    # ax.set_ylabel('y')
    ax.set_zlabel('z')
    ax.set_xticklabels([])
    ax.set_yticklabels([])
    ax.set_title("%s_%3.3d" % (name,ite))
    # ax.set_zticks([0])
    # ax.zaxis.set_major_locator(LinearLocator(10))
    # ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
    # fig.colorbar(surf, shrink=0.5, aspect=5)
    ax.view_init(24, -69)
    # plt.show()
    plot_name = "%s/Python/insep/plot/solid/%s_%s_%3.3d.png" % (scarc, chid, name, ite)
    print('Plotting picture ', plot_name, min_val, max_val)
    plt.savefig(plot_name)
    plt.close(fig)

    # ---- Second subplot
    plot_wireframe = True
    if (plot_wireframe):
        fig2 = plt.figure()
        ax2 = fig2.add_subplot(1, 1, 1, projection='3d')
        ax2.plot_wireframe(xp, zp, val, rstride=1, cstride=1, cmap=cm.jet)
        ax2.set_zlim(min_val-0.01, max_val+0.01)
        ax2.set_xlabel('x')
        ax2.set_ylabel('y')
        ax2.set_zlabel('z')
        ax2.set_title("wf_%s_%3.3d" % (name, ite))
        plot_name = "%s/Python/insep/plot/wireframe/%s_%s_%3.3d.png" % (scarc, chid, name, ite)
        print('Plotting picture ', plot_name, min_val, max_val)
        # plt.show()
        plt.savefig(plot_name)
        plt.close(fig2)
        # show()


chid = 'insep_mixed'
scarc = '/home/susanne/Tank/Samba/GIT/B_FDS/01_ScaRC'
direc =  '/Verification/Pressure_Solver'

nx = 8
nz = 8

dx = 0.8/nx
dz = 0.8/nz

nit = 6

names = ['PRES', 'HP']
for ite in range(nit):
    for name in names:
        (found, quan) = read_quantity(scarc, direc, chid, name, ite+1)
        if found:
            plot_quantity(scarc, chid, name, quan, ite+1, nx, nz, dx, dz)
