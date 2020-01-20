# -*- coding: utf-8 -*-
"""
Created on Sun Jan 19 16:47:17 2020

@author: Rishabh Samdarshi

Description : 
The code can take any user rating data in JSON format and can create 
recommendations on the basis of nearest neighbour

"""

print ()

from operator import itemgetter

# definie class similarity
class similarity:
    
    # Class instantiation 
    def __init__ (self, ratingP, ratingQ):
        self.ratings1 = ratingP
        self.ratings2 = ratingQ

    # Minkowski Distance between two vectors
    def minkowski(self, r):
    
        # calcualte minkowski distance
        distance = 0       
        for k in (set(self.ratings1.keys()) & set(self.ratings2.keys())):
            p = self.ratings1[k]
            q = self.ratings2[k]
            distance += pow(abs(p - q), r)
    
        # return value of minkowski distance
        return pow(distance,1/r)

    # Pearson Correlation between two vectors
    def pearson(self):
        
        #Following code finds the total number of common ratings

        n = 0 

        for i in set(self.ratings1):
            if i in set(self.ratings2):
       	        n += 1
        
        # The following code assigns a value of -2 if no common ratings exist
        if n== 0:
        	return -2

         
        
        # using a SINGLE for loop to calculate the partial sums
        # in the computationally efficient form of the pearson correlation   

        sumOfR1 = 0
        sumOfR2 =0
        sumOfR1R2 = 0 
        sumOfR1sq = 0
        sumOfR2sq = 0 
        for k in (set(self.ratings1.keys()) & set(self.ratings2.keys())):
            sumOfR1 +=  self.ratings1[k]
            sumOfR1sq +=  self.ratings1[k]**2
            sumOfR2 +=  self.ratings2[k]
            sumOfR2sq +=  self.ratings2[k]**2
            sumOfR1R2 += self.ratings1[k]*self.ratings2[k]


        # calcualting the numerator term for pearson correlation
        # using relevant partial sums
        
        
        pearsonNumerator = sumOfR1R2 - (sumOfR1*sumOfR2)/n

        # calcualting the denominator term for pearson correlation
        # using relevant partial sums

        pearsonDenominator	= (((sumOfR1sq- (sumOfR1**2/n)))**0.5)*((sumOfR2sq- (sumOfR2**2/n)))**0.5
        
        # error checking for denominator==0 condition
        # returning -2 if denominator==0

        if pearsonDenominator == 0:
        	return -2

        # calcualting the pearson correlation 
        # using the numerator and deonomminator
        # and returning the pearson correlation

        pearsonCorrelation = pearsonNumerator/pearsonDenominator

        return pearsonCorrelation

# user ratings - this is the same data as we used in the User Recommendation Lecture
songData = {"Angelica": {"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0},
         "Bill":{"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0},
         "Chan": {"Blues Traveler": 5.0, "Broken Bells": 1.0, "Deadmau5": 1.0, "Norah Jones": 3.0, "Phoenix": 5, "Slightly Stoopid": 1.0},
         "Dan": {"Blues Traveler": 3.0, "Broken Bells": 4.0, "Deadmau5": 4.5, "Phoenix": 3.0, "Slightly Stoopid": 4.5, "The Strokes": 4.0, "Vampire Weekend": 2.0},
         "Hailey": {"Broken Bells": 4.0, "Deadmau5": 1.0, "Norah Jones": 4.0, "The Strokes": 4.0, "Vampire Weekend": 1.0},
         "Jordyn":  {"Broken Bells": 4.5, "Deadmau5": 4.0, "Norah Jones": 5.0, "Phoenix": 5.0, "Slightly Stoopid": 4.5, "The Strokes": 4.0, "Vampire Weekend": 4.0},
         "Sam": {"Blues Traveler": 5.0, "Broken Bells": 2.0, "Norah Jones": 3.0, "Phoenix": 5.0, "Slightly Stoopid": 4.0, "The Strokes": 5.0},
         "Veronica": {"Blues Traveler": 3.0, "Norah Jones": 5.0, "Phoenix": 4.0, "Slightly Stoopid": 2.5, "The Strokes": 3.0}
        }


''' 
The similarityGenerator function generates a sorted tuple of similarity scores

args: 
    userName(string) : Name of the user for whom similarity has to be found
    DataDictName : The JSON object from where the data has to be sourced
    
function Calls: 
    Pearson (class similarity)

returns: 
    (list of tuples) : Element 0 is the Name of neighbor
                       Element 1 is the degree of similarity
    
'''

def similarityGenerator(userName, DataDictName):
	userSimilarities = []
	for key in DataDictName.keys():
		if key == userName:
			continue
		else:
			userYRatings = DataDictName[key]
			userXRatings = DataDictName[userName]
			similarityObject = similarity(userXRatings,userYRatings)
			userSimilarities.append((key, similarityObject.pearson()))
			sortedUserSimilarities = sorted(userSimilarities, key=itemgetter(1), reverse = True)
	return sortedUserSimilarities


''' 
The recommendations function generates recommendation on the basis of closest
neighbour

args: 
    userName(string) : Name of the user for whom similarity has to be found
    DataDictName : The JSON object from where the data has to be sourced

function calls : similarityGenerator

returns: 
    (list of tuples) : Element 0 is the Name of neighbor
                       Element 1 is the degree of similarity
    
'''


def recommendations (userName, DataDictName):
    nearestUserName = similarityGenerator(userX,songData)[0][0]
    recos = []
    for key in set(DataDictName[nearestUserName]):
        if key not in set(DataDictName[userName]): 
            recos.append((key, DataDictName[nearestUserName][key]))
    sortedRecos = sorted(recos, key=itemgetter(1), reverse = True)
    return sortedRecos       




# for whom are we making recommendations?
userX = input("Who do you want to build recommendations for?")
userXSortedRecos = recommendations(userX,songData)

print ("Recommendations for", userX)
print ("--------------------------")
print ()
print (userXSortedRecos)