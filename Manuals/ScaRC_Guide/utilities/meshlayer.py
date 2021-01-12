import math
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection, Line3DCollection

def plotSlope():
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax._axis3don = False
    ax.view_init(20, -90)
    deltaH = np.array([0,0,0.01])

    z = 0.0
    points = []
    # create a coordinate array for the main slope 
    # bottom 2 cell coordinates
    for y in range(9):
        for x in range(17):
            points.append([float(x), float(y), z])

    # remaining top right lowest coordinates
    for x in range(8):
        points.append([float(x+17), 8.0, z])

    # top 2 cells
    for y in range(9):
        for x in range(17):
            points.append([float(x+8), float(y+9), z])
    points = np.array(points)

    # helper to create a point at current z
    def point(x,y):
        return [float(x),float(y),z]
    
    # thick blue and red arrows
    def arrow(p1,m,p2):
        p1 = np.array(p1) # start
        p2 = np.array(p2) # end
        m = np.array(m) # middle

        lineFact = 0.2
        arrowFact = 0.7

        dir = m - p1
        dir /= math.sqrt(dir[0]*dir[0] + dir[1]*dir[1]) 
        arrowW = dir * arrowFact
        arrowWi = np.array([arrowW[1], arrowW[0], 0])
        lineWi = dir*lineFact
        lineW = np.array([lineWi[1], lineWi[0], 0])
        leftArrow = [p1, p1+arrowW+arrowWi, p1+arrowW+lineW, m+lineW+lineWi, m-lineW+lineWi, p1+arrowW-lineW, p1+arrowW-arrowWi, p1]

        dir = m - p2
        dir /= max(abs(dir[0]), abs(dir[1])) 
        arrowW = dir * arrowFact
        arrowWi = np.array([arrowW[1], arrowW[0], 0])
        lineWi = dir*lineFact
        lineW = np.array([lineWi[1], lineWi[0], 0])
        rightArrow = [p2, p2+arrowW+arrowWi, p2+arrowW+lineW, m+lineW+lineWi, m-lineW+lineWi, p2+arrowW-lineW, p2+arrowW-arrowWi, p2]
        return [leftArrow+deltaH, rightArrow+deltaH]


    # layer 0 (lowest) -------------------------------------------------------------
    whiteFill = (1,1,1,0.1) # colors for fill areas
    shadedArea = (1.0,0.957,0.886,0.9)
    obstacle = (0,0,0,0.3)
    
    edges = np.array([ # large slope boundary
        [point(0,0), point(8,0), point(8,8), point(0,8)],
        [point(8,0), point(16,0), point(16,8), point(8,8)],
        [point(8,8), point(16,8), point(16,16), point(8,16)],
        [point(16,8), point(24,8), point(24,16), point(16,16)],
    ])

    faces = Poly3DCollection(edges, linewidths=2, edgecolors='k')
    faces.set_facecolor(whiteFill)
    ax.add_collection3d(faces)
    faces = Poly3DCollection([edges[3]], linewidths=2, edgecolors='k')
    faces.set_facecolor(shadedArea)
    ax.add_collection3d(faces)
    
    streamZ = 0.05
    z += streamZ
    streams0 = np.concatenate([ # the blue stream lines in lowest layer
        arrow(point(6.5,4), point(8,4), point(9.5,4)), 
        arrow(point(6.5,2), point(22,2), point(22,10)),
        arrow(point(6.5,6), point(9.5,6), point(9.5,10.5)),
        arrow(point(12,6.5), point(12,8), point(12,10)),
        arrow(point(14,6.5), point(20,6.5), point(20,10)),
        arrow(point(14.5,10), point(16,10), point(18,10)),
    ])
    faces = Poly3DCollection(streams0)
    blue = (0,0,1,1)
    faces.set_facecolor(blue)
    ax.add_collection3d(faces)

    ax.scatter(points[:,0], points[:,1], points[:,2], s=0)
    
    center = point(20,12) # transfer lines will start here

    layer = 0.4 # distance between single layers

    # layer 1 --------------------------------------------------------------------------------------------------------------------------------------------
    innerLineWidth = 1.0
    z += layer-streamZ
    linesDown1 = np.array([[point(18,10), center], [point(22,10), center], [point(18,14), center], [point(22,14), center]]) # 4 transfer lines to layer 0
    ax.add_collection3d(Line3DCollection(linesDown1, colors='k', linewidths=innerLineWidth))

    # divide each grid element into 4 squares
    points += [0,0,layer]
    edges += [0,0,layer]
    faces = Poly3DCollection(edges, linewidths=2, edgecolors='k')
    faces.set_facecolor(whiteFill)
    ax.add_collection3d(faces)
    faces = Poly3DCollection([[point(0,0), point(4,0), point(4,4), point(0,4)]])
    faces.set_facecolor(shadedArea)
    ax.add_collection3d(faces)
    faces = Poly3DCollection([edges[3]], edgecolors='k')
    faces.set_facecolor(shadedArea)
    ax.add_collection3d(faces)

    lines1 = np.array([[point(4,0), point(4,8)], [point(12,0), point(12,16)], [point(20,8), point(20,16)], [point(0,4), point(16,4)], [point(8,12), point(24,12)]])
    ax.add_collection3d(Line3DCollection(lines1, colors='k', linewidths=innerLineWidth))
    
    z += streamZ
    streams1 = np.concatenate([ # the read stream lines
        arrow(point(6.5,4), point(8,4), point(9.5,4)), 
        arrow(point(12,6.5), point(12,8), point(12,10)),
        arrow(point(14.5,12), point(16,12), point(18,12)),
    ])
    faces = Poly3DCollection(streams1)
    red = (1,0,0,1)
    faces.set_facecolor(red)
    ax.add_collection3d(faces)

    ax.scatter(points[:,0], points[:,1], points[:,2], s=0)
    center = point(2,2)

    # layer 2 --------------------------------------------------------------------------------------------------------------------------------------------
    innerLineWidth = 0.6
    z += layer - streamZ
    linesDown2 = np.array([[point(1,1), center], [point(3,1), center], [point(3,3), center], [point(1,3), center]])
    ax.add_collection3d(Line3DCollection(linesDown2, colors='k', linewidths=innerLineWidth))

    points += [0,0,layer]
    edges += [0,0,layer]
    faces = Poly3DCollection(edges, linewidths=2, edgecolors='k')
    faces.set_facecolor(whiteFill)
    ax.add_collection3d(faces)

    faces = Poly3DCollection([[point(0,0), point(4,0), point(4,4), point(0,4)]])
    faces.set_facecolor(shadedArea)
    ax.add_collection3d(faces)
    edges2 = np.array([
        [point(12,2), point(14,2), point(14,4), point(12,4)],
    ])
    faces = Poly3DCollection(edges2, edgecolors='k', linewidths=innerLineWidth)
    faces.set_facecolor(shadedArea)
    ax.add_collection3d(faces)

    newLines2 = np.array([
        [point(2,0), point(2,8)],
        [point(6,0), point(6,8)],
        [point(10,0), point(10,16)],
        [point(14,0), point(14,16)],
        [point(18,8), point(18,16)],
        [point(22,8), point(22,16)],
        [point(0,2), point(16,2)],
        [point(0,6), point(16,6)],
        [point(8,10), point(24,10)],
        [point(8,14), point(24,14)],
    ])
    lines2 = np.concatenate([lines1 + [0,0,layer], newLines2])
    ax.add_collection3d(Line3DCollection(lines2, colors='k', linewidths=innerLineWidth))

    z += streamZ
    streams2 = np.concatenate([
        arrow(point(6.5,4), point(8,4), point(9.5,4)), 
        arrow(point(12,6.5), point(12,8), point(12,10)),
        arrow(point(14.5,12), point(16,12), point(18,12)),
    ])
    faces = Poly3DCollection(streams2, linewidths=innerLineWidth)
    faces.set_facecolor(red)
    ax.add_collection3d(faces)

    ax.scatter(points[:,0], points[:,1], points[:,2], s=0)

    center = point(13,3)

    # layer 3 (top) --------------------------------------------------------------------------------------------------------------------------------------
    innerLineWidth = 0.3
    z += layer - streamZ

    linesDown3 = np.array([[point(12.5,2.5), center], [point(13.5,2.5), center], [point(13.5,3.5), center], [point(12.5,3.5), center]])
    ax.add_collection3d(Line3DCollection(linesDown3, colors='k', linewidths=innerLineWidth))

    points += [0,0,layer]
    edges += [0,0,layer]
    faces = Poly3DCollection(edges, linewidths=2, edgecolors='k')
    faces.set_facecolor(whiteFill)
    ax.add_collection3d(faces)

    edges2 += [0,0,layer]
    faces = Poly3DCollection(edges2, edgecolors='k', linewidths=innerLineWidth)
    faces.set_facecolor(shadedArea)
    ax.add_collection3d(faces)

    edges3 = np.array([
        [point(3,3), point(5,3), point(5,5), point(3,5)],
    ])
    faces = Poly3DCollection(edges3, edgecolors='k', linewidths=innerLineWidth)
    faces.set_facecolor(obstacle)
    ax.add_collection3d(faces)

    newLines3 = np.array([
        [point(1,0), point(1,8)],
        [point(3,0), point(3,8)],
        [point(5,0), point(5,8)],
        [point(7,0), point(7,8)],
        [point(9,0), point(9,16)],
        [point(11,0), point(11,16)],
        [point(13,0), point(13,16)],
        [point(15,0), point(15,16)],
        [point(17,8), point(17,16)],
        [point(19,8), point(19,16)],
        [point(21,8), point(21,16)],
        [point(23,8), point(23,16)],
        [point(0,1), point(16,1)],
        [point(0,3), point(16,3)],
        [point(0,5), point(16,5)],
        [point(0,7), point(16,7)],
        [point(8,9), point(24,9)],
        [point(8,11), point(24,11)],
        [point(8,13), point(24,13)],
        [point(8,15), point(24,15)],
    ])
    lines3 = np.concatenate([lines2 + [0,0,layer], newLines3])

    ax.add_collection3d(Line3DCollection(lines3, colors='k', linewidths=innerLineWidth))

    z += streamZ
    streams3 = np.concatenate([
        arrow(point(6.5,4), point(8,4), point(9.5,4)), 
        arrow(point(12,6.5), point(12,8), point(12,10)),
        arrow(point(14.5,12), point(16,12), point(18,12)),
    ])
    faces = Poly3DCollection(streams3)
    faces.set_facecolor(red)
    ax.add_collection3d(faces)

    ax.scatter(points[:,0], points[:,1], points[:,2], s=0)

plotSlope()
plt.show()

