from os import listdir
from os import makedirs
from os.path import isfile, join, exists
from random import randint
import errno
import os
import xlwt

mypath = "/Users/ben/Desktop/python fiddling/traces-4-22-2014"
listOfLogs = []
listOfFilenames = [] #For recreating the social state again at a later date!


global totalNumberOfExperiment1Sets
global totalNumberOfExperiment2Sets
global totalNumberOfExperiment3Sets
totalNumberOfExperiment1Sets = 0
totalNumberOfExperiment2Sets = 0
totalNumberOfExperiment3Sets = 0

#How much should we do for each character...
experimentsPerCharacter = dict([('Chloe', 0), 
	('Doug', 50), 
	('Edward', 0), 
	('Lil', 0), 
	('Mave', 0), 
	('Monica', 1), 
	('Naomi', 0), 
	('Oswald', 0), 
	('Simon', 50), 
	('Zack', 0), 
	])

global bookExperiment1
global sheetExperiment1
global bookExperiment2
global sheetExperiment2
global bookExperiment3
global sheetExperiment3


#Just some basic prep work to set up the three spreadsheets we'll be using for our experiment.
def setupSpreadsheets():
	#Set up the spreadsheet for experiment 1
	global bookExperiment1
	bookExperiment1 = xlwt.Workbook()
	global sheetExperiment1
	sheetExperiment1 = bookExperiment1.add_sheet("Sheet 1")
	sheetExperiment1.write(0, 0, "EXPERIMENT 1")
	sheetExperiment1.write(0, 1, "BASELINE")
	sheetExperiment1.write(0, 2, "Trace1")
	sheetExperiment1.write(0, 3, "Trace1 Rating")
	sheetExperiment1.write(0, 4, "Trace2")
	sheetExperiment1.write(0, 5, "Trace2 Rating")
	sheetExperiment1.write(0, 6, "Trace3")
	sheetExperiment1.write(0, 7, "Trace3 Rating")
	sheetExperiment1.write(0, 8, "Trace4")
	sheetExperiment1.write(0, 9, "Trace4 Rating")
	sheetExperiment1.write(0, 10, "Trace5")
	sheetExperiment1.write(0, 11, "Trace5 Rating")

	#Set up the spreadsheet for experiment 2
	global bookExperiment2
	bookExperiment2 = xlwt.Workbook()
	global sheetExperiment2
	sheetExperiment2 = bookExperiment2.add_sheet("Sheet 1")
	sheetExperiment2.write(0, 0, "EXPERIMENT 2")
	sheetExperiment2.write(0, 1, "DIVERSITY RATING")
	sheetExperiment2.write(0, 2, "Trace1")
	sheetExperiment2.write(0, 3, "Trace2")
	sheetExperiment2.write(0, 4, "Trace3")
	sheetExperiment2.write(0, 5, "Trace4")
	sheetExperiment2.write(0, 6, "Trace5")
	sheetExperiment2.write(0, 7, "Trace6")
	sheetExperiment2.write(0, 8, "Trace7")
	sheetExperiment2.write(0, 9, "Trace8")
	sheetExperiment2.write(0, 10, "Trace9")
	sheetExperiment2.write(0, 11, "Trace10")

	#Set up the spreadsheet for experiment 3
	global bookExperiment3
	bookExperiment3 = xlwt.Workbook()
	global sheetExperiment3
	sheetExperiment3 = bookExperiment3.add_sheet("Sheet 1")
	sheetExperiment3.write(0, 0, "EXPERIMENT 3")
	sheetExperiment3.write(0, 1, "Trace1")
	sheetExperiment3.write(0, 2, "Trace1 Rating")
	sheetExperiment3.write(0, 3, "Trace2")
	sheetExperiment3.write(0, 4, "Trace2 Rating")
	sheetExperiment3.write(0, 5, "Trace3")
	sheetExperiment3.write(0, 6, "Trace3 Rating")
	sheetExperiment3.write(0, 7, "Trace4")
	sheetExperiment3.write(0, 8, "Trace4 Rating")
	sheetExperiment3.write(0, 9, "Trace5")
	sheetExperiment3.write(0, 10, "Trace5 Rating")
	sheetExperiment3.write(0, 11, "Trace6")
	sheetExperiment3.write(0, 12, "Trace6 Rating")
	sheetExperiment3.write(0, 13, "Trace7")
	sheetExperiment3.write(0, 14, "Trace7 Rating")
	sheetExperiment3.write(0, 15, "Trace8")
	sheetExperiment3.write(0, 16, "Trace8 Rating")
	sheetExperiment3.write(0, 17, "Trace9")
	sheetExperiment3.write(0, 18, "Trace9 Rating")
	sheetExperiment3.write(0, 19, "Trace10")
	sheetExperiment3.write(0, 20, "Trace10 Rating")


#END setupSpreadsheets

#Checks to see if a directory exists -- creates it if it doesn't.
def doesPathExist(path):
	try:
		os.mkdir(path)
	except OSError as exception:
		if exception.errno != errno.EEXIST:
			raise

#END doesPathExist

#Given a character file of gamelyzer logs, parse it out and set it up for the experiments that we'll be running.
def createExperimentFilesByCharacterFile(characterFileName):

	#It will be useful to know the name of the character that we are working with - figure it out
	#from the name of the input file.
	print "Dealing with: " + characterFileName
	hyphenIndex = characterFileName.find("-")
	actualCharacterName = characterFileName[hyphenIndex+1:len(characterFileName)]
	periodIndex = actualCharacterName.find(".")
	actualCharacterName = actualCharacterName[0:periodIndex]
	print "CHARACTER NAME: " + actualCharacterName

	#Open up the character file to start reading information
	fullPath = mypath + "/" + characterFileName
	infile = open(fullPath, 'rU')

	#Some things to help us store the ID
	playTraceLog = ""
	logCount = 0
	
	#Go through each line in input file, and we're going to store it in a big list that we'll
	#reference when producing the actual files for the experiment.
	for line in infile:
		playTraceLog += line
		#Check to see if we've reached the end of a single log (one file will have MANY logs)
		if line.strip() == ":log_end":
			listOfLogs.append(playTraceLog) # We have a complete log; append it to our list
			logCount += 1
			playTraceLog = "" #Clean this out for the next log to add.

	#print "uhhhhhh"
	experiment1Setup(listOfLogs, actualCharacterName)
	print "finished experiment1"
	experiment2Setup(listOfLogs, actualCharacterName)
	experiment3Setup(listOfLogs, actualCharacterName)
	print "finished experiment2"

	#Clean this out for the next character file that we'll be looking at.
	del listOfLogs[:]

	infile.close()

#END createExperimentFilesByCharacterFile

#getFilenameFromLog this will parse out the filename from the first line of the passed in log file
#and return it
def getFilenameFromLog(gameLog):
	openingQuoteIndex = gameLog.find('"')
	closingQuoteIndex = gameLog.find('"', openingQuoteIndex+1)
	fileNameTemp = gameLog[openingQuoteIndex:closingQuoteIndex]
	fileNameFinal = fileNameTemp.split("-")[2]
	#Right now fileName is of the form: game-797-1350380261_86638881.xml -- we'd like to strip away everything that isn't
	#the actual fileName

	print "Opening quote: " + str(openingQuoteIndex) + " Closing Quote: " + str(closingQuoteIndex) + " fileName: " + fileNameTemp
	print "Final: " + fileNameFinal
	return fileNameFinal

#END getFilenameFromLog

#Experiment 1 contains a baseline log and several other logs. For each of the other logs, a likert scale number will be given to
#represent how 'close' to the baseline it is. 7 Means VERY different. 1 means NOT very different.
def experiment1Setup(gameLogs, characterName):
	randomIndexes = [] #Will store the random indexes representing the 'baseline' and the logs to compare it against.
	
	print "start of exp1"

	directory = createExperimentDirectory("experiment1", characterName)

	print "got past the directory stage... the directory is " + directory

	#Create numForExperiment1 files that contain a 'baseline' trace and 5 other traces.
	#They are guaranteed to all come from the same campaign, and to have all seen an ending of some kind.
	#At present, there are no features distinguishing the baseline vs. the non baseline. By convention, the baseline 
	#is the first log file.
	for j in range(0, experimentsPerCharacter[characterName]):

		global totalNumberOfExperiment1Sets
		global sheetExperiment1
		totalNumberOfExperiment1Sets = totalNumberOfExperiment1Sets + 1
		sheetExperiment1.write(totalNumberOfExperiment1Sets, 0, characterName + str(j) + "-exp1")

		#Actually create the file
		exp1OutFile = open(directory + "/" + characterName + str(j) +  "-exp1", 'w')
		exp1OutFileENGLISH = open(directory + "/" + characterName + str(j) +  "-exp1-english", 'w')

		#Find 6 random numbers, <= the number of logs we have for this character.
		#These random numbers represent the IDs of the logs we'll be using for one 'round' of the experiment.
		randomIndexes = getListOfRandomIndexes(6, gameLogs)

		fileNameListInfoString = "*** Experiment 1, "+ characterName + ", sample set " + str(j) + " ***"
		listOfFilenames.append(fileNameListInfoString)
		#Now lets actually print the logs represented by the discovered indexes to a file.
		baselineFound = False
		nonBaselineLogsSeen = 0
		for logID in randomIndexes:
			filename = getFilenameFromLog(gameLogs[logID])
			listOfFilenames.append(filename)
			exp1OutFile.write(gameLogs[logID])
			englishSummary = "*** " + filename + " ***\n"
			englishSummary += getEnglishSummaryFromLog(gameLogs[logID]) 
			exp1OutFileENGLISH.write(englishSummary)
			if baselineFound == False:
				baselineFound = True
				#We are dealing with the baseline! Add it to the thingy
				sheetExperiment1.write(totalNumberOfExperiment1Sets, 1, filename)
			else:
				sheetExperiment1.write(totalNumberOfExperiment1Sets, nonBaselineLogsSeen*2 + 2, filename)
				sheetExperiment1.write(totalNumberOfExperiment1Sets, nonBaselineLogsSeen*2 + 3, " ")
				nonBaselineLogsSeen += 1

			#print listOfLogs[logID]

		#Clean up for the next pass.
		del randomIndexes[:]
		exp1OutFile.close()
		exp1OutFileENGLISH.close()

#END experiment1Setup

#Experiment 2 presents some amount of logs in a file. The task is to pick a number that represents the 'spread' of these 
#logs -- a high number means there is a lot of variation between them, a small number means there is a small amount of variation.
#put another way: we rate how diverse the experiments are.
def experiment2Setup(gameLogs, characterName):
	randomIndexes = [] #Will store the random indexes representing the 'baseline' and the logs to compare it against.
	directory = createExperimentDirectory("experiment2", characterName)

	print "start of exp2"

	for j in range(0, experimentsPerCharacter[characterName]):

		global totalNumberOfExperiment2Sets
		global sheetExperiment2
		totalNumberOfExperiment2Sets = totalNumberOfExperiment2Sets + 1
		sheetExperiment2.write(totalNumberOfExperiment2Sets, 0, characterName + str(j) + "-exp2")

		#actually create the file
		exp2OutFile = open(directory + "/" + characterName + str(j) + "-exp2", 'w')
		exp2OutFileENGLISH = open(directory + "/" + characterName + str(j) +  "-exp2-english", 'w')

		randomIndexes = getListOfRandomIndexes(10, gameLogs)
		print randomIndexes

		fileNameListInfoString = "*** Experiment 2, "+ characterName + ", sample set " + str(j) + " ***"
		listOfFilenames.append(fileNameListInfoString)
		numLogsLookedAt = 0
		for logID in randomIndexes:
			#print "log id is " + str(logID) + " and length of game logs is " + str(len(gameLogs))
			filename = getFilenameFromLog(gameLogs[logID])
			listOfFilenames.append(filename)
			exp2OutFile.write(gameLogs[logID])
			englishSummary = "*** " + filename + " ***\n"
			englishSummary += getEnglishSummaryFromLog(gameLogs[logID]) 
			exp2OutFileENGLISH.write(englishSummary)
			sheetExperiment2.write(totalNumberOfExperiment2Sets, numLogsLookedAt + 2, filename)
			numLogsLookedAt += 1

		#clean up for the next pass
		del randomIndexes[:]
		exp2OutFile.close()
		exp2OutFileENGLISH.close()

#END experiment2Setup

#Experiment 3 involves there being a collection of traces, and each trace being marked for 'outlier-ness'
def experiment3Setup(gameLogs, characterName):
	randomIndexes = []
	directory = createExperimentDirectory("experiment3", characterName)

	print "start of exp3"

	for j in range(0, experimentsPerCharacter[characterName]):

		global totalNumberOfExperiment3Sets
		global sheetExperiment3
		totalNumberOfExperiment3Sets = totalNumberOfExperiment3Sets + 1
		sheetExperiment3.write(totalNumberOfExperiment3Sets, 0, characterName + str(j) + "-exp3")

		#actually create the file
		exp3OutFile = open(directory + "/" + characterName + str(j) + "-exp3", 'w')
		exp3OutFileENGLISH = open(directory + "/" + characterName + str(j) +  "-exp3-english", 'w')

		randomIndexes = getListOfRandomIndexes(10, gameLogs)
		print randomIndexes

		fileNameListInfoString = "*** Experiment 3, "+ characterName + ", sample set " + str(j) + " ***"
		listOfFilenames.append(fileNameListInfoString)
		numLogsLookedAt = 0
		for logID in randomIndexes:
			filename = getFilenameFromLog(gameLogs[logID])
			listOfFilenames.append(filename)
			exp3OutFile.write(gameLogs[logID])
			englishSummary = "*** " + filename + " ***\n"
			englishSummary += getEnglishSummaryFromLog(gameLogs[logID]) 
			exp3OutFileENGLISH.write(englishSummary)
			sheetExperiment3.write(totalNumberOfExperiment3Sets, numLogsLookedAt * 2 + 1, filename)
			sheetExperiment3.write(totalNumberOfExperiment3Sets, numLogsLookedAt * 2 + 2, " ")
			numLogsLookedAt += 1

		#clean up for next pass
		del randomIndexes[:]
		exp3OutFile.close()
		exp3OutFileENGLISH.close()

#END experiment3Setup


#This is a super simplistic boiling down of a game log into something that is understandable in English.
#There isn't really a great way to get this information, but,hey, this is going to be my pretty dumb, straightforward attempt.
def getEnglishSummaryFromLog(gameLog):
	lineByLine = gameLog.split("\n")
	theStory = ""
	print "<-> WE'RE GONNA BREAK IT DOWN YO. Length is: " + str(len(lineByLine))
	for singleLine in lineByLine:
		print singleLine
		if singleLine.find("log_start") != -1:
			continue
		elif singleLine.find("log_end") != -1:
			continue
		elif singleLine == "":
			continue
		else:
			goodPartIndex = singleLine.find("([:")
			goodPartOfString = singleLine[goodPartIndex:]
			print "good part of string is: " + goodPartOfString
			workingString = goodPartOfString.replace(":", "")
			workingString = workingString.replace("(", "")
			workingString = workingString.replace(")", "")
			workingString = workingString.replace("[", "")
			workingString = workingString.replace("]", "")
			valuedComponents = workingString.split()
			print "even better string is: " + workingString
			gameSummary = createGameSummaryFromValuedComponents(valuedComponents[0], valuedComponents[1], valuedComponents[2], valuedComponents[3], valuedComponents[4] )
			print gameSummary
			theStory += gameSummary
			#We are dealing with an actual line of action

	return theStory
#END getEnglishSummaryFromLog

def createGameSummaryFromValuedComponents(initiator, intent, gameName, gameID, responder):
	print "HOWDY"
	niceOrMean = ""
	if intent == "buddyUp" or intent == "friendsStart":
		niceOrMean = "nice to"
	if intent == "buddyDown" or intent == "friendsStop":
		niceOrMean = "mean to"
	if intent == "enemiesStart":
		niceOrMean = "to become enemies with"
	if intent == "enemiesStop":
		niceOrMean = "to make peace with"
	if intent == "romanceUp":
		niceOrMean = "romantic towards"
	if intent == "datingStart":
		niceOrMean = "to try to start dating"
	if intent == "datingStop":
		niceOrMean = "to stop dating"
	if intent == "romanceDown":
		niceOrMean = "UNromantic towards"
	if intent == "coolUp":
		niceOrMean = "to impress"
	if intent == "coolDown":
		niceOrMean = "to UNimpress"

	returnString = initiator + " did something " + niceOrMean + " " + responder + " (" + gameName + "-" + gameID + ")\n"
	return returnString
#END createGamesummaryFromValuedComponents

#Super simple thing to help make sure that the directories we want to store things in actually exist.
def createExperimentDirectory(experimentName, characterName):
	#Printing to a directory is more challenging than you would think!
	#Create the directory if it doesn't exist!
	print "inside of making special directory thing"
	directory = "/Users/ben/Desktop/python fiddling/"
	directory = join(directory, experimentName + "/")
	doesPathExist(directory)
	directory = join(directory, characterName)
	doesPathExist(directory)
	return directory

#END createExperimentDirectory

#Find 6 random numbers, <= the number of logs we have for this character.
#These random numbers represent the IDs of the logs we'll be using for one 'round' of an experiment.
def getListOfRandomIndexes(numIndexesDesired, gameLogs):
	randomIndexes = []
	numAdded = 0
	for i in range(0, numIndexesDesired):
		successfullyAdded = False
		randomIndex = randint(0, len(gameLogs) - 1)
		while not successfullyAdded:
			if randomIndex not in randomIndexes:
				randomIndexes.append(randomIndex)
				successfullyAdded = True
				numAdded += 1
				#print "Now we have " + str(numAdded) + " added! Neat! "
			else:
				#print "we got " + str(randomIndex) + "but we can't put it in randomIndexes cause of " + str(randomIndexes)
				randomIndex = randint(0, len(gameLogs) - 1)

	return randomIndexes

#END getListOfRandomIndexes


setupSpreadsheets()

onlyfiles = [ f for f in listdir(mypath) if isfile(join(mypath,f)) ]

for i in xrange(0,len(onlyfiles)):
	#we are looping through each of the files now. For each one, we'll want to copy certain things out of them and blah blah blah, you know?
	createExperimentFilesByCharacterFile(onlyfiles[i])

#Create a file to store all of the playtraces that we have been using.
allFilenamesFile = open("/Users/ben/Desktop/python fiddling/fileNames", 'w')

print "Here is every single file that was used in this experiment"
for awesomeFileName in listOfFilenames:
	#print awesomeFileName
	allFilenamesFile.write(awesomeFileName + "\n")
allFilenamesFile.close()

bookExperiment1.save("/Users/ben/Desktop/python fiddling/experiment1Data.xls")
bookExperiment2.save("/Users/ben/Desktop/python fiddling/experiment2Data.xls")
bookExperiment3.save("/Users/ben/Desktop/python fiddling/experiment3Data.xls")

	#outputFile = open("./testOutput-" + actualCharacterName, 'w')

