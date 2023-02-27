import spacy
import pandas as pd


### Config Variables
nlp = spacy.load('en_core_web_lg')
LOADFILE = "path where you have stored the file downloaded from the CAM-app\\filename.txt"  # e.g."C:\\CAMs\\analysis\\summarizedWords.txt"
SAVEFILE = "path where the output of this script should be saved\\filename for output.txt"  # e.g."C:\\CAMs\\analysis\\distanceMatrix.txt". ATTENTION: EXISTING FILES of the same name and location are OVERWRITTEN (if not specified otherwise below!


### Data preparation
rawData = pd.read_csv(LOADFILE, delimiter="\t").iloc[:, 0].str.cat(others=None, sep=" ", na_rep=None, join='left') # reads in the file specified by LOADFILE, converting it to a single string of words separated by a blankspace
tokens = nlp(rawData)

##cleans up data olny for concepts in the model
# Defined as a function to be used on multiple datasets if necessary 
def cleanData(data):
    cleanTokens = []
    for token in tokens:
        if not token.is_oov:
            cleanTokens.append(token) 
    return cleanTokens

### determining string distances
## Creates a matrix of word similarities
def calcDistanceMatrix(cleanTokens):
    df = pd.DataFrame(columns=cleanTokens, index = cleanTokens) # builds al empty pandas dataframe with rows and columns named after the words to be analyzed
    for token in cleanTokens:
        similiarities = []
        for token2 in cleanTokens:
            similiarities.append(token.similarity(token2))
        df[token] = similiarities
    return df

### Exporting a .txt file to/with the location specified by the SAVEFILE variable
#export DataFrame to text file
def saveFile(distanceMatrix): 
    with open(SAVEFILE, 'w') as f:  # overwrites existing files of the same name and path. If you want to change that: change line to with open(SAVEFILE, 'x') as f:
        distMatString = distanceMatrixDF.to_string(header=True, index=True)
        f.write(distMatString)

### Calling the functions defined above for cleaning data, calculating a distance matrix and saving it to a file 
cleanTokens = cleanData(tokens)  
distanceMatrixDF = calcDistanceMatrix(cleanTokens)
saveFile(distanceMatrixDF)

#### Printing the distance matrix (for testing/debug only)
#print(distanceMatrixDF)