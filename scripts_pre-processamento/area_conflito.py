import numpy as np
import cv2 as cv
from tkinter.filedialog import askopenfilename

inicio = (1,1,1,1)
points = []
i = 0

# mouse callback function
def draw_circle(event,x,y,flags,param):
    global inicio, points, i
    if event == cv.EVENT_LBUTTONDOWN:
        inicio = (x,y)

    if event == cv.EVENT_LBUTTONUP:
        i+=1
        print([inicio[0],inicio[1],x,y])
        #cv.rectangle(frame,(inicio[0],inicio[1]),(x,y),(255,0,0),2)
        cv.line(frame,(inicio[0],inicio[1]),(x,y),(255,0,0),5) 
        cv.putText(frame,'P'+str(i), (inicio[0],inicio[1]-3), cv.FONT_HERSHEY_SIMPLEX, 0.5,(255,0,0),1)
        inicio = (inicio[0],inicio[1],x,y)
        points.append([inicio[0],inicio[1],x,y])

# Create a black image, a window and bind the function to window

def get_euler_distance(pt1, pt2):
    return ((pt1[0] - pt2[0])**2 + (pt1[1] - pt2[1])**2)**0.5

def calculate_4_vertice_parallelogram(pt1, pt2, pt3):
    pt4 = []
    m2 = (pt2[1] - pt3[1])
    m22 = (pt2[0] - pt3[0])
    pt4.append(pt1[0]-m22)
    pt4.append(pt1[1]-m2)
    return pt4

def converter_point(p,M):
    px = (M[0][0]*p[0] + M[0][1]*p[1] + M[0][2]) / ((M[2][0]*p[0] + M[2][1]*p[1] + M[2][2]))
    py = (M[1][0]*p[0] + M[1][1]*p[1] + M[1][2]) / ((M[2][0]*p[0] + M[2][1]*p[1] + M[2][2]))
    return (int(px), int(py))

filename = askopenfilename()
aux = 0
n_frame = 0
video = cv.VideoCapture(filename)

if not video.isOpened():
    print("Could not open video")
    exit()


status, frame = video.read()
cv.namedWindow('video')

cv.setMouseCallback('video',draw_circle)
while(1):
    cv.imshow('video',frame)

    if cv.waitKey(1) & 0xFF == 27:
         break



while(1):
    
    #point_4 = calculate_4_vertice_parallelogram([points[0][0], points[0][1]], [points[1][0], points[1][1]], [points[1][2], points[1][3]])
    #src_pts = np.array([[points[0][0], points[0][1]], [points[1][0], points[1][1]], [points[1][2], points[1][3]], [point_4[0], point_4[1]]], dtype=np.int32)
    #pts = src_pts.reshape((-1,1,2))
    #cv.polylines(frame,[pts],True,(255,0,0),5)
    #cv.imshow('video',frame)

    #src_pts = np.array([[points[0][0], points[0][1]], [points[1][0], points[1][1]], [points[1][2], points[1][3]], [point_4[0], point_4[1]]], dtype=np.float32)
    #points =  [[50, 376, 315, 170], [315, 170, 1033, 344], [1033, 344, 884, 626], [884, 626, 49, 382]] 
    src_pts = np.array([[points[0][0], points[0][1]], [points[1][0], points[1][1]], [points[2][0], points[2][1]], [points[3][0], points[3][1]]], dtype=np.float32)

    width = int(get_euler_distance(src_pts[0], src_pts[1]))
    height = int(get_euler_distance(src_pts[0], src_pts[3]))
    rows,cols,ch = frame.shape

    ajuste_perspective = 120
    #dst_pts = np.array([[500, 500],   [width, 500],  [width, height], [500, height]], dtype=np.float32)
    dst_pts = np.array([[ajuste_perspective, ajuste_perspective],   [width, ajuste_perspective],  [width, height], [ajuste_perspective, height]], dtype=np.float32)

 

    M = cv.getPerspectiveTransform(src_pts, dst_pts)
    warp = cv.warpPerspective(frame, M, (width+ajuste_perspective,height+ajuste_perspective), cv.INTER_LINEAR, borderMode=cv.BORDER_CONSTANT, borderValue=(0,0,0,0))
    imgOutput = cv.resize(warp, (cols,rows))

    cv.imshow('video_warp',warp)
    cv.imshow('video_warp2',imgOutput)
    #cv.imshow('video_warp2',warp2)
    if cv.waitKey(1) & 0xFF == 27:
         cv.imwrite('output/video.jpg',warp)
         break

print(points)
print([[points[0][0], points[0][1],points[1][0], points[1][1]],
      [points[1][0], points[1][1],points[2][0], points[2][1]], [points[2][0], points[2][1],points[3][0], points[3][1]], 
      [points[3][0], points[3][1],points[0][0], points[0][1]]])
p = (295, 106)
p_after = converter_point(p,M)
print(p_after)
#print(cv.perspectiveTransform(p , M[: dst_pts]	))
#print(cv.perspectiveTransform(np.array([[50, 50]], dtype = "float32"), M[:dst_pts]))
#print(np.matmul(np.array([[295, 106]], dtype = "float32") , M))



       



