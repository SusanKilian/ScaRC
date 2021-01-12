import math
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection, Line3DCollection

def plotSlope(level):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax._axis3don = False
    ax.view_init(20, -90)
    deltaH = np.array([0,0,0.01])

    # helper to create a point at current z
    def point(x,y): return [float(x),float(y),z]
    
    z = 0.0
    points = []
    # create a coordinate array for the main slope
    # coarse cell = 8x8
    # medium cell = 16+16
    # fine cell = 32x32
    # bottom 2 cell coordinates
    for y in range(33):
        for x in range(65):
            points.append(point(x, y))

    # remaining top right lowest coordinates
    for x in range(32):
        points.append(point(x+65, 64.0))

    # top 2 cells
    for y in range(33):
        for x in range(65):
            points.append(point(x+32, y+33))
    points = np.array(points)

    # for thick blue and red arrows
    red = (1,0,0,1)
    blue = (0,0,1,1)
    def arrow(p1,m,p2):
        p1 = np.array(p1) # start
        p2 = np.array(p2) # end
        m = np.array(m) # middle

        lineFact = 0.6
        arrowFact = 1.5

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


    # layer 0 (lowest) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    whiteFill = (1,1,1,0.1) # colors for fill areas
    shadedArea = (1.0,0.957,0.886,0.99)
    obstacle = (0,0,0,0.3)
    
    edges = np.array([ # large slope boundary
        [point(0,0), point(32,0), point(32,32), point(0,32)],
        [point(32,0), point(64,0), point(64,32), point(32,32)],
        [point(32,32), point(64,32), point(64,64), point(32,64)],
        [point(64,32), point(96,32), point(96,64), point(64,64)],
    ])
    
    faces = Poly3DCollection(edges, linewidths=2, edgecolors='k')
    faces.set_facecolor(whiteFill)
    ax.add_collection3d(faces)

    if level == 3:
        cell0 = np.array([ # first refinement cell
            [point(0,0), point(4,0), point(4,4), point(0,4)],
        ])
        center = point(2,2)

        faces = Poly3DCollection(cell0, edgecolors='k')
        faces.set_facecolor(shadedArea)
        ax.add_collection3d(faces)
    
    # divide each grid element into 5x8 squares
    innerLineWidth = 1.0
    gridLines = []
    for y in range(7):
        gridLines.append([point(0, (y+1)*4), point(64, (y+1)*4)])
    for y in range(7):
        gridLines.append([point(32, (y+9)*4), point(96, (y+9)*4)])
    for x in range(7):
        gridLines.append([point((x+1)*4, 0), point((x+1)*4, 32)])
    for x in range(7):
        gridLines.append([point((x+1)*4+32, 0), point((x+1)*4+32, 64)])
    for x in range(7):
        gridLines.append([point((x+1)*4+64, 32), point((x+1)*4+64, 64)])
    gridLines = np.array(gridLines)
    ax.add_collection3d(Line3DCollection(gridLines, colors='k', linewidths=innerLineWidth))

    obstruction = np.array([
        [point(12,12), point(20,12), point(20,20), point(12,20)],
    ])
    faces = Poly3DCollection(obstruction, edgecolors='k', linewidths=innerLineWidth)
    faces.set_facecolor(obstacle)
    ax.add_collection3d(faces)

    streamZ = 0.0
    z += streamZ
    streams0 = np.concatenate([ # the blue stream lines in lowest layer
        arrow(point(28,4), point(92,4), point(92,35)), 
        arrow(point(60,28), point(68,28), point(68,35)), 
        arrow(point(28,14), point(32,14), point(36,14)), 
        arrow(point(28,28), point(36,28), point(36,35)), 
        arrow(point(50,28), point(50,32), point(50,35)), 
        arrow(point(60,46), point(62,46), point(68,46)), 
    ])
    faces = Poly3DCollection(streams0)
    faces.set_facecolor(blue)
    ax.add_collection3d(faces)

    ax.scatter(points[:,0], points[:,1], points[:,2], s=0)
    
    center = point(2,2) # transfer lines will start here

    if level == 3:
        layer = 0.6 # distance between single layers
    else:
        layer = 0.3

    # layer 1 (middle) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    z += layer - streamZ
    points += [0,0,layer]
    edges += [0,0,layer]

    innerLineWidth = 0.6

    if level == 3:
        linesDown1 = np.array([[point(1,1), center], [point(3,1), center], [point(3,3), center], [point(1,3), center]])
        ax.add_collection3d(Line3DCollection(linesDown1, colors='k', linewidths=innerLineWidth))

        faces = Poly3DCollection(edges, linewidths=2, edgecolors='k')
        faces.set_facecolor(whiteFill)
        ax.add_collection3d(faces)
        cell1 = cell0 + [0, 0, layer]
        faces = Poly3DCollection(cell1)
        faces.set_facecolor(shadedArea)
        ax.add_collection3d(faces)

    # divide each grid element into 16x16 squares
    gridLines2 = gridLines + [0,0,layer]
    gridLines = []
    for y in range(8):
        gridLines.append([point(0, y*4+2), point(64, y*4+2)])
    for y in range(8):
        gridLines.append([point(32, y*4+34), point(96, y*4+34)])
    for x in range(8):
        gridLines.append([point(x*4+2, 0), point(x*4+2, 32)])
    for x in range(8):
        gridLines.append([point(x*4+34, 0), point(x*4+34, 64)])
    for x in range(8):
        gridLines.append([point(x*4+66, 32), point(x*4+66, 64)])
    gridLines = np.array(gridLines)
    gridLines2 = np.concatenate((gridLines2, gridLines))
    if level == 3:
        ax.add_collection3d(Line3DCollection(gridLines2, colors='k', linewidths=innerLineWidth))

    obstruction2 = obstruction + [0, 0, layer]
    if level == 3:
        faces = Poly3DCollection(obstruction2, edgecolors='k', linewidths=innerLineWidth)
        faces.set_facecolor(obstacle)
        ax.add_collection3d(faces)
    
    if level == 3:
        cell2 = np.array([ # first refinement cell
            [point(92,60), point(94,60), point(94,62), point(92,62)],
        ])
        center = point(93,61)
        faces = Poly3DCollection(cell2, edgecolors='k')
        faces.set_facecolor(shadedArea)
        ax.add_collection3d(faces)

    z += streamZ
    streams1 = np.concatenate([ # the red stream lines in middle layer
        arrow(point(28,18), point(32,18), point(36,18)), 
        arrow(point(46,28), point(46,32), point(46,36)), 
        arrow(point(60,50), point(62,50), point(68,50)), 
    ])
    if level == 3:
        faces = Poly3DCollection(streams1)
        faces.set_facecolor(red)
        ax.add_collection3d(faces)

    # layer 2 (top) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    z += layer - streamZ
    points += [0,0,layer]
    edges += [0,0,layer]

    if level == 3:
        linesDown2 = np.array([[point(92.5,60.5), center], [point(93.5,60.5), center], [point(93.5,61.5), center], [point(91.5,61.5), center]])
        ax.add_collection3d(Line3DCollection(linesDown2, colors='k', linewidths=innerLineWidth))

    faces = Poly3DCollection(edges, linewidths=2, edgecolors='k')
    faces.set_facecolor(whiteFill)
    ax.add_collection3d(faces)

    if level == 3:
        cell3 = cell2 + [0, 0, layer]
        faces = Poly3DCollection(cell3)
        faces.set_facecolor(shadedArea)
        ax.add_collection3d(faces)

    # divide each grid element into 32x32 squares
    innerLineWidth = 0.4
    gridLines3 = gridLines2 + [0,0,layer]
    gridLines = []
    for y in range(16):
        gridLines.append([point(0, y*2+1), point(64, y*2+1)])
    for y in range(16):
        gridLines.append([point(32, y*2+33), point(96, y*2+33)])
    for x in range(16):
        gridLines.append([point(x*2+1, 0), point(x*2+1, 32)])
    for x in range(16):
        gridLines.append([point(x*2+33, 0), point(x*2+33, 64)])
    for x in range(16):
        gridLines.append([point(x*2+65, 32), point(x*2+65, 64)])
    gridLines = np.array(gridLines)
    gridLines3 = np.concatenate((gridLines3, gridLines))
    ax.add_collection3d(Line3DCollection(gridLines3, colors='k', linewidths=innerLineWidth))

    obstruction3 = obstruction2 + [0, 0, layer]
    faces = Poly3DCollection(obstruction3, edgecolors='k', linewidths=innerLineWidth)
    faces.set_facecolor(obstacle)
    ax.add_collection3d(faces)

    streams2 = streams1 + [0,0,layer]
    faces = Poly3DCollection(streams2)
    faces.set_facecolor(red)
    ax.add_collection3d(faces)
    if level != 3:
        streams3 = streams0 + [0,0,2*layer]
        faces = Poly3DCollection(streams3)
        faces.set_facecolor(blue)
        ax.add_collection3d(faces)


plotSlope(2)
plt.show()