# COGS543 Assignments - Ali Kaan Sungur
# This solution can be uploaded to Github.

#------------------------------------------------------------------------
# ASSIGNMENT 2
#------------------------------------------------------------------------

import random
# Random entities
#entities = []
#setSize = 8
#for i in range(1,setSize):
#    entities.append('G' + str(i))

entities = ['G385', 'G386', 'G387', 'G388', 'G389', 'G390', 'G391']

# Arities
zeroPlace = ['SUE', 'MARY', 'BILL', 'JOHN']
onePlace = ['JEALOUS', 'RED', 'KOALA', 'BED', 'RUNS']
twoPlace = ['CHEATS', 'LOVES', 'HATES', 'CHASES', 'LIKES']

def randomElement(set):
    return set[random.randint(0,len(set)-1)]

def popRandomElement(set):
    return set.pop(random.randint(0,len(set)-1))

def randomSubset(set):
    copySet = set.copy()
    randomSubsetSize = random.randint(1,len(copySet))
    subset = []
    for i in range(1,randomSubsetSize):
        randomEntity = popRandomElement(copySet)
        subset.append(randomEntity)
    return subset

def randomTuppleSubset(set):
    cartesian = []
    for elementOut in set:
        for elementIn in set:
            if elementIn != elementOut:
                comb1 = (elementIn, elementOut)
                comb2 = (elementOut, elementIn)
                if (comb1 not in cartesian) and (comb2 not in cartesian):
                    cartesian.append((elementIn, elementOut))
    return randomSubset(cartesian)

#------------------------------------------------------------------------
# ASSIGNMENT 3
#------------------------------------------------------------------------

def assignZeroPlaces(zeroPlaces, entities):
    copyZeroPlaces = zeroPlaces.copy()
    zeroPlaceMap = {}
    while copyZeroPlaces:
        zeroPlaceMap[popRandomElement(copyZeroPlaces)] = randomElement(entities)
    return zeroPlaceMap

def assignOnePlaces(onePlaces, entities):
    copyOnePlaces = onePlaces.copy()
    onePlaceMap = {}
    while copyOnePlaces:
        onePlaceMap[popRandomElement(copyOnePlaces)] = randomSubset(entities)
    return onePlaceMap

def assignTwoPlaces(twoPlaces, entities):
    copyTwoPlaces = twoPlaces.copy()
    twoPlaceMap = {}
    while copyTwoPlaces:
        twoPlaceMap[popRandomElement(copyTwoPlaces)] = randomTuppleSubset(entities)
    return twoPlaceMap

def printMap(map):
    for key, value in map.items():
        print(key + ': ' + str(value))

print('\nThe domain of entities: ' + str(entities))
print()

zeroPlaceMap = assignZeroPlaces(zeroPlace, entities)
print('0-place names')
printMap(zeroPlaceMap)
print()

onePlaceMap = assignOnePlaces(onePlace, entities)
print('1-place names')
printMap(onePlaceMap)
print()

twoPlaceMap = assignTwoPlaces(twoPlace, entities)
print('2-place names')
printMap(twoPlaceMap)
print()

#------------------------------------------------------------------------
# ASSIGNMENT 4
#------------------------------------------------------------------------

vocabulary = {}
vocabulary.update(zeroPlaceMap)
vocabulary.update(onePlaceMap)
vocabulary.update(twoPlaceMap)

#def zeroPlaceTransform(key):
#    def func(arg):
#        return arg == zeroPlaceMap[key]
#    return func

#def onePlaceTransform(key):
#    def func(arg):
#        return arg in onePlaceMap[key]
#    return func

#def twoPlaceTransform(key):
#    def func(arg):
#        return arg in twoPlaceMap[key]
#    return func

#def functionalizeVocabularyV1():
#    functionalVocabulary = {}
#    for key, value in vocabulary.items():
#        if key in zeroPlaceMap:     functionalVocabulary[key] = zeroPlaceTransform(key)
#        elif key in onePlaceMap:    functionalVocabulary[key] = onePlaceTransform(key)
#        elif key in twoPlaceMap:    functionalVocabulary[key] = twoPlaceTransform(key)
#    return functionalVocabulary

def generalizedTransform(key):
    def func(arg):
        return arg in vocabulary[key]
    return func

def functionalizeVocabularyV2():
    print('Generating functions from the vocabulary.')
    functionalVocabulary = {}
    for key, value in vocabulary.items():
        functionalVocabulary[key] = generalizedTransform(key)
    print('Transformation complete')
    return functionalVocabulary

functions = functionalizeVocabularyV2()


print('\nCorrect case testing:')
correctTupple = vocabulary['SUE']
print("functions['SUE']('" + str(correctTupple) + "') is " + str(functions['SUE'](correctTupple)))

correctTupple = vocabulary['RUNS'][0]
print("functions['RUNS']('" + str(correctTupple) + "') is " + str(functions['RUNS'](correctTupple)))

correctTupple = vocabulary['LOVES'][0]
print("functions['LOVES']('" + str(correctTupple) + "') is " + str(functions['LOVES'](correctTupple)))

print('\nAll the functions:')
print(functions)
