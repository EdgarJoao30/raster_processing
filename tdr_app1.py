import ee
from random import randint
from ee import batch
import pandas as pd
import csv

## Initialize (a ee python thing)
ee.Initialize()

## load files
roi_app1 = ee.FeatureCollection('ft:1d80zsjZAW6wudXeTBOxg-akjEauImi3nzO504Kty')
VB = ee.Image('users/gcarrasco/VB')
ML_1 = ee.Image('users/gcarrasco/ML_1')
ML_2 = ee.Image('users/gcarrasco/ML_2')
ML_3 = ee.Image('users/gcarrasco/ML_3')
SL = ee.Image('users/gcarrasco/SL')
UM = ee.Image('users/gcarrasco/UM')
LI = ee.Image('users/gcarrasco/LI')
collection_rgb = ee.ImageCollection([VB,ML_1,ML_2,ML_3,SL,UM,LI])
collection_m = ee.ImageCollection('users/edgarmanrique30/comunities')
geometry = ee.FeatureCollection('ft:1O-lP3K4Po6HV2MNvLwPheszU_lnGihdKCxaBbuuT')
geometry = geometry.geometry()
#defining functions
def __maskvaluesmulti__(img):
	mask = img.neq(0)
	return img.mask(mask)
def __maskvaluesrgb__(img):
	mask = img.neq(255)
	return img.mask(mask)
def addndvi(img):
	ndvi = img.expression('(NIR-RED)/(NIR+RED)', {
		'NIR': img.select('NIR'),
		'RED': img.select('red_m')
		}).rename('NDVI')
	return img.addBands(ndvi)
def __clip_col__(img): return img.clip(geometry)
def __renameBandsRGB__ (img):return img.rename(['red','green','blue','mask'])
def __renameBandsMulti__(img):return img.rename(['red_m','green_m','Edge Red','NIR'])

#masking values
collection_rgb = collection_rgb.map(__maskvaluesrgb__)
collection_m = collection_m.map(__maskvaluesmulti__)
#clipping extent
collection_rgb = collection_rgb.map(__clip_col__)
collection_m = collection_m.map(__clip_col__)
#renaming bands
collection_rgb = collection_rgb.map(__renameBandsRGB__)
collection_m = collection_m.map(__renameBandsMulti__)
#converting collection to single images
collection_rgb = collection_rgb.median()
collection_m = collection_m.median()
#getting a new image with all the bands
image = collection_rgb.select(['red', 'green', 'blue']).addBands(collection_m, ['red_m','green_m','Edge Red','NIR'])
image = addndvi(image)
#training samples for sequoia images
polygons= ee.FeatureCollection(roi_app1,"geometry");
classifier = ee.Classifier.randomForest(20)
#specifying bands to use 
bands = ['red', 'green', 'blue', 'red_m','green_m','Edge Red','NIR']

pylist = []

text_file = open("Output.txt", "w")


#for loop
for n in range(2):
	print("iteration", n)
	lista = ee.List([])
	for i in range(10):
		print i
		seed = randint(0,999)

		polygons = polygons.randomColumn('random',seed)

		regionsOfInterest = image.select(bands).sampleRegions(polygons,['class', 'random'],2)

		training1 = regionsOfInterest.filterMetadata('random', 'less_than', 0.8).filterMetadata('random', 'not_less_than', 0)
		testing1 = regionsOfInterest.filterMetadata('random', 'less_than', 1).filterMetadata('random', 'not_less_than', 0.8)

		trainingClassifier1 = classifier.train(training1, 'class',bands)
		validation1 = testing1.classify(trainingClassifier1)
		errorMatrix1 = validation1.errorMatrix('class', 'classification')

		#print('accuracy1: ',errorMatrix1.accuracy())
		#print("accuracy 1 is ", errorMatrix1.accuracy())


		training2 = regionsOfInterest.filterMetadata('random', 'less_than', 1).filterMetadata('random', 'not_less_than', 0.8)
		training22 = regionsOfInterest.filterMetadata('random', 'less_than', 0.6).filterMetadata('random', 'not_less_than', 0)
		training2 = training2.merge(training22)
		testing2 = regionsOfInterest.filterMetadata('random', 'less_than', 0.8).filterMetadata('random', 'not_less_than', 0.6)

		trainingClassifier2 = classifier.train(training2, 'class',bands)
		validation2 = testing2.classify(trainingClassifier2)
		errorMatrix2 = validation2.errorMatrix('class', 'classification')

		#print('accuracy2: ',errorMatrix2.accuracy())
		#print("accuracy 2 is ", errorMatrix2.accuracy())
	
		training3 = regionsOfInterest.filterMetadata('random', 'less_than', 1).filterMetadata('random', 'not_less_than', 0.6)
		training33 = regionsOfInterest.filterMetadata('random', 'less_than', 0.4).filterMetadata('random', 'not_less_than', 0)
		training3 = training2.merge(training33)
		testing3 = regionsOfInterest.filterMetadata('random', 'less_than', 0.6).filterMetadata('random', 'not_less_than', 0.4)

		trainingClassifier3 = classifier.train(training3, 'class',bands)
		validation3 = testing3.classify(trainingClassifier3)
		errorMatrix3 = validation3.errorMatrix('class', 'classification')

		#print('accuracy3: ',errorMatrix3.accuracy())
		#print("accuracy 3 is ", errorMatrix3.accuracy())

		training4 = regionsOfInterest.filterMetadata('random', 'less_than', 1).filterMetadata('random', 'not_less_than', 0.4)
		training44 = regionsOfInterest.filterMetadata('random', 'less_than', 0.2).filterMetadata('random', 'not_less_than', 0)
		training4 = training4.merge(training44)
		testing4 = regionsOfInterest.filterMetadata('random', 'less_than', 0.4).filterMetadata('random', 'not_less_than', 0.2)

		trainingClassifier4 = classifier.train(training4, 'class',bands)
		validation4 = testing4.classify(trainingClassifier4)
		errorMatrix4 = validation4.errorMatrix('class', 'classification')

		#print('accuracy4: ',errorMatrix4.accuracy())
		#print("accuracy 4 is ", errorMatrix4.accuracy())

		training5 = regionsOfInterest.filterMetadata('random', 'less_than', 1).filterMetadata('random', 'not_less_than', 0.2)
		testing5 = regionsOfInterest.filterMetadata('random', 'less_than', 0.2).filterMetadata('random', 'not_less_than', 0)

		trainingClassifier5 = classifier.train(training5, 'class',bands)
		validation5 = testing5.classify(trainingClassifier5)
		errorMatrix5 = validation5.errorMatrix('class', 'classification')

		#print('accuracy5: ',errorMatrix5.accuracy())
		#print("accuracy 5 is ", errorMatrix5.accuracy().getInfo())

		suma = ee.Number(errorMatrix1.accuracy()).add(ee.Number(errorMatrix2.accuracy())).add(ee.Number(errorMatrix3.accuracy())).add(ee.Number(errorMatrix4.accuracy())).add(ee.Number(errorMatrix5.accuracy()))
		mean = suma.divide(5)
		lista = lista.insert(i,mean)

	lista = lista.getInfo()
	print(lista)
	pylist.append(lista)
print(pylist)

for item in pylist:
  text_file.write("%s\n" % item)

text_file.close()
#df = DataFrame(lista, columns = pts[0])
#df.to_csv('~/Desktop/test.csv')
#out = batch.Export.table.toDrive(lista, description='lista_test_app1')
#process = batch.Task.start(out)



