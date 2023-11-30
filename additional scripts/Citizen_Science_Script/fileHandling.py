
import os
import json
from pathlib import Path
import cam2imageGUI as GUI


## Returns a list of all files in a folder and (recursively) all its subfolders. 
# If a list is given as the pattterns argument only files ending on the pattern will be returned (e.g., only table files of the json, txt, csv, xlsx format if patterns = [".json", ".csv", ".xlsx", ".txt"])
def listFilesInFolder(folderPath: str, patterns = None) -> list:  
    #patterns = [".json", ".csv", ".xlsx", ".txt"]
    filepaths = []
    if not patterns == None:
        for pattern in patterns:
            folder = Path(folderPath) 
            for filename in map(str, list(folder.rglob("*"))): 
                if filename.endswith(pattern):
                    filepaths.append(filename)
    else:
        folder = Path(folderPath) 
        for filename in map(str, list(folder.rglob("*"))): 
            filepaths.append(filename)

    return filepaths   


## Coosing a folder for saving files
def outputFolder():
    while True:
        saveDir = GUI.chooseSavingFolder()  # The user has to choose where to save the results
        if os.path.isdir(saveDir):  # Checking if chosen folder is valid (if not user has to choose again)
            return str(Path(saveDir))  # Path module is called to make sure formating is correct
        elif saveDir == None:  # This is the case if the user clicks on "Cancel"
            print("Operation canceled. Nothing was saved. Crashing now.")
            saveDir = "placeholder"
            return saveDir
        else:
            GUI.invalidFolder()
            pass
    

## Takes a list of filepaths, creates a dict with the filenames as keys and the filepaths as values ##      
def prepSVGsForConversion(filePaths):
    namePathDict = {}
    for filePath in filePaths:
        try:
            namePathDict[Path(filePath).stem] = Path(filePath)
        except Exception as error:
            print("There was an error with ", str(filePath), ": ", str(error))
    
    return namePathDict


## Renames a file (specified by filePath parameter), by adding a Prefix (prefix parameter)
# Returns the original filename, the new filename and the name of the directory where the renaming was done
def addPrefix(filePath, prefix):
    dirName = os.path.dirname(filePath)
    originalFileName = str(os.path.basename(filePath))
    newFileName = prefix + originalFileName
    newFilePath = os.path.join(dirName, newFileName) 
    os.rename(filePath, newFilePath)

    return originalFileName, newFileName, dirName  


## Function to split a file that contains data from several CAMs into several files (one file per CAM) 
# Returns the number of files that were successfully created and the number of files of which the creation failed
def splitFile(file, prefix, savefolder=None):  
    if savefolder == None:
        savefolder = outputFolder()
    filesCreated = 0
    filesFailed = 0
    lines = open(file).readlines()
    for line in lines:
        try:
            CAM = json.loads(line)  # Returning json file as a python dict
            creator = str(CAM["creator"])
            idCAM = str(CAM["idCAM"])
            nameCAM = creator if not idCAM == "" else "noCreator"
            fileWritten = False
            i = 0
            while not fileWritten:
                index = str(i) if i > 0 else ""
                filename = (nameCAM + str(prefix) + index + ".txt")
                savePath = os.path.join(savefolder, filename)
                if os.path.isfile(savePath):
                    i += 1
                else: 
                    with open(savePath, 'w') as f:
                        f.write(line)
                    fileWritten = True
            filesCreated += 1
        except:
            filesFailed += 1
       
    return filesCreated, filesFailed
