# -*- coding: utf-8 -*-
"""
Author : Rishabh Samdarshi

Excercise set 2 for W P Carey MSBA 2019-2020 Cohort
This code creates the functions for calculating multiple distances like manhattan, euclidean and minkowski

"""


a,b,c,d = [float (x) for x in input(" Enter coordinates (a,b) (c,d) separated by a space in form a b c d \n").split()]   
 
manhattanDistance = lambda p,q,r,s:  abs(p-r)+ abs(q-s)
euclideanDistance = lambda p,q,r,s: ((p-r)**2 + (q-s)**2)**0.5
minkowskiDistance = lambda p,q,r,s: ((abs(p-r))**3 + (abs(q-s))**3)**(1/3)
 
manDis = manhattanDistance(a,b,c,d)
eucDis = euclideanDistance(a,b,c,d)
minkDis = minkowskiDistance(a,b,c,d) 
 
print ( " The manhattan distnce is :" , manDis, 
      "\n The euclidean distance is :" , eucDis,"\n The minkowski distance is :", minkDis)


"""
Now for the sake of an excercise we do this again taking the input as a list

"""

coordinateOne = [float (x) for x in input("Enter the x and y coordinate of the first point separated by spaces \n").split()]
coordinateTwo = [float (x) for x in input("Enter the x and y coordinate of the second point separated by spaces\n").split()]




manDis = manhattanDistance(coordinateOne[0],coordinateOne[1],coordinateTwo[0],coordinateTwo[1])
eucDis = euclideanDistance(coordinateOne[0],coordinateOne[1],coordinateTwo[0],coordinateTwo[1])
minkDis = minkowskiDistance(coordinateOne[0],coordinateOne[1],coordinateTwo[0],coordinateTwo[1])


"""

The above examples were for 2 dimensions. Will try to make it more generic for multiple dimensions

"""

dimension = [int(x) for x in input(" Enter the number of dimensions that you wish to calculate the distance for \n")]

xDim = [float (x) for x in input("Enter the x and y coordinate of the first point separated by spaces \n").split()]
if len(xDim) > dimension:
    xDim = xDim[:dimension]
    raise Exception('Items exceeds the maximum allowed length set, taking the limited dimension only ')
    
yDim = [float (x) for x in input("Enter the x and y coordinate of the first point separated by spaces \n").split()]
if len(xDim) > dimension:
    xDim = xDim[:dimension]
    raise Exception('Items exceeds the maximum allowed length set, taking the limited dimension only ')

manDisMultiDim = sum(list(map(lambda x,y: (abs(x)-abs(y)),xDim,yDim)))
eucDisMultiDim = sum(list(map(lambda x,y: (abs(x)-abs(y))**2,xDim,yDim)))**0.5
minkDisMultiDim = sum(list(map(lambda x,y: (abs(x)-abs(y))**3,xDim,yDim)))**(1/3)






    