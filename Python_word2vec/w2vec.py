import spacy
import pandas as pd
### Config Variables
#inputLanguage = "german"
nlp = spacy.load('en_core_web_lg')
LOADFILE = "D:\\Coding\\CAM\\Teststuff\\summarizedWords.txt"
SAVEFILE = "D:\\Coding\\CAM\\Teststuff\\distanceMatrix.txt"  # change according to your needs
#print(pd.read_csv(LOADFILE, delimiter="\t").iloc[:, 0])  # for debug
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

cleanTokens = cleanData(tokens)
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

distanceMatrixDF = calcDistanceMatrix(cleanTokens)

#### Printing the Matrix (for testing/debug only)
print(distanceMatrixDF)
### Exporting a .txt file to/with the location specified by the SAVEFILE variable
#export DataFrame to text file
with open(SAVEFILE, 'w') as f:  # overwrites existing files of the same name and path. If you want to change that: change line to with open(SAVEFILE, 'x') as f:
    distMatString = distanceMatrixDF.to_string(header=True, index=True)
    f.write(distMatString)