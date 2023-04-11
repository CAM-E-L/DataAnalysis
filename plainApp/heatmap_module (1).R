library("tidyverse")
#library("ggplot2")
library(dplyr)
#library("jsonlite")
#library('magrittr')
#library('gmodels')
#library('psych')
library(purrr)
library(stats)
library(heatmaply)

library(psych)

######## Code for setting up data etc. I've copied from your file ########

# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

########################################
# load packages
########################################
library(shiny)
# library(shinyWidgets)
library(shinyjs)


# library(shinycssloaders) %>% withSpinner(color="#0dc5c1")

library(tidyverse)


library(rjson) # write JSON files


library(igraph)

library(sortable)

########################################
# Daten
########################################
setwd("..")
# create files
source("./www/functions_CAM/create_CAMfiles.R", encoding = "utf-8")
source("./www/functions_CAM/create_ValenceFiles.R", encoding = "utf-8")
# > fix Valence data
source("./www/functions_CAM/fix_ValenceData.R", encoding = "utf-8")


# draw CAMs
source("./www/functions_CAM/draw_CAM.R", encoding = "utf-8")

# compute network indicators
source("./www/functions_CAM/compute_indicatorsCAM.R", encoding = "utf-8")


# helper functions for protocol
source("./www/functions_CAM/protocolFunctions.R", encoding = "utf-8")


# create wordlist
source("./www/functions_CAM/create_wordlist2.R", encoding = "utf-8")



########################################
# Daten
########################################
setwd("plainApp")
dir()


# protocol <- jsonlite::read_json(path = "protocol.txt")
protocol <- rjson::fromJSON(file = "protocol.txt")



CAMfiles = list()
# readLines("CAM_nodes_raw.txt")


CAMfiles[[1]] <- vroom::vroom(file = "CAM_nodes_raw.txt", delim = "\t",
                              show_col_types = FALSE)


CAMfiles[[1]] <- vroom::vroom(file = "CAM_nodes_clean.txt", delim = "\t",
                              show_col_types = FALSE)


CAMfiles[[2]] <- vroom::vroom(file = "CAM_connectors_raw.txt", delim = "\t",
                              show_col_types = FALSE)
CAMfiles[[3]] <- vroom::vroom(file = "CAM_merged_raw.txt", delim = "\t",
                              show_col_types = FALSE)


CAMfiles[[1]]$text_summarized <- CAMfiles[[1]]$text
tmp_out <- overwriteTextNodes(protocolDat = protocol, nodesDat = CAMfiles[[1]])
CAMfiles[[1]]$text_summarized <- tmp_out[[1]]$text_summarized




unique(CAMfiles[[1]]$text_summarized)
# CAMfiles[[1]]$text_summarized <- tolower(CAMfiles[[1]]$text_summarized)
###############################
####  New Code starts HERE ####
###############################

###### CONFIG ######
showOnlySignificant <- TRUE #If TRUE, only those concepts are shown in the heatmap, that correlate significantly with at least one other variable
levelOfSignicifance <- 0.05
useSummarized <- FALSE #If TRUE, heatmap/correlation is calculated based on summarized concepts, if false, raw (not-summarized) concepts are used
binaryCorr <- FALSE #If TRUE, Phi correlation is used and the co-occurrence of concepts is analyzed dichotomously (i.e. concept A and B occur together in same CAM), if FALSE, number of occurences of a concept within a CAM are counted and Pearson correlations calculated
interactiveDots <- TRUE #Only important for interactive heatmap. Uses dots in the heatmap which vary in size according to p value
####################

### Input data ###
CAMsConsInput <- CAMfiles[[1]]





################
# Functions for analyzing correlation of occurence of concepts
################

### create (and return) data frame of concepts per CAM (column) from CAM-data
listConcepts <- function(datCAM = CAMfiles[[1]], useSummarized = TRUE, removeSuffix = TRUE){

  if(removeSuffix) {
    datCAM$text_summarized <-
      str_remove(string = datCAM$text_summarized , pattern = "_positive$|_negative$|_neutral$|_ambivalent$")

  }

  conceptList <- list()
  for(i in unique(datCAM$CAM)) {
    if (useSummarized == TRUE) {
      concepts <- unlist(datCAM[datCAM$CAM == i, "text_summarized"])

    } else {
      concepts <- unlist(datCAM[datCAM$CAM == i, "text"])
    }
    conceptList[[i]] <- concepts
  }
  conceptsDF <- data.frame(lapply(conceptList, "length<-", max(lengths(conceptList))))
  rownames(conceptsDF) <- NULL
  return(conceptsDF)
}


conceptsDF <- listConcepts(datCAM = CAMfiles[[1]], useSummarized = TRUE)
head(conceptsDF)

### Takes a data frame the concepts mentioned in the CAMs (one CAM one column)
### Returns a data frame with all concepts mentioned in more than one CAM (columns "concept") and the number of CAMS where it occurs (column "numCAMoccurences")
# concepts = conceptsDF
# oderFrequency = FALSE

countDuplicates <- function(concepts = conceptsDF, oderFrequency = FALSE){
  allCons <- c()

  for(i in 1:ncol(concepts)) {       # for-loop over columns
    CAMcons <- unlist(concepts[ , i], use.names=FALSE)
    allCons <- append(allCons,unique(CAMcons))
  }

  duplicateCons <- names(table(allCons))[table(allCons) >=2]


  duplicateDF = matrix(nrow = length(duplicateCons), ncol = 2)
  for(i in 1:length(duplicateCons)) {
    duplicateDF[i,] = c(duplicateCons[i], sum(allCons==duplicateCons[i], na.rm=TRUE))
  }
  colnames(duplicateDF) = c("concept","numCAMoccurences")
  duplicateDF <- as.data.frame(duplicateDF)
  duplicateDF$numCAMoccurences <- as.numeric(duplicateDF$numCAMoccurences)


  if( oderFrequency ) {
    duplicateDF <- duplicateDF[order(duplicateDF$numCAMoccurences, decreasing = TRUE),]
  }

  return(duplicateDF)
}


duplicateDF <- countDuplicates(concepts = conceptsDF, oderFrequency = TRUE)
head(duplicateDF)



## Creating a Table with binary encoding for duplicate concepts. I.e., each concept mentioned more than once is a column, each CAM is a row.
# If the concept is mentioned in the respective CAM, cell is 1, if not cell is 0
# conDF = conceptsDF
# duplDF = duplicateDF
binaryEncoding <- function(conDF = conceptsDF, duplDF = duplicateDF) {
  binaryDF <- data.frame(matrix(nrow = ncol(conDF), ncol = length(duplDF$concept))) # Create an empty dataframe where each column is going to represent a concept and each row a CAM
  colnames(binaryDF) <- duplDF$concept #Name the columns of the dataframe according to the list of concepts occuring more than once
  binaryDF[is.na(binaryDF)] <- 0

  rownames(binaryDF) <- colnames(conDF)

  for(r in 1:nrow(binaryDF)){
    binaryDF[rownames(binaryDF)[r],] <- as.numeric(duplDF$concept %in% conDF[, rownames(binaryDF)[r]])
  }

  return(binaryDF)
}

testOut <- binaryEncoding(conDF = conceptsDF, duplDF = duplicateDF)
head(testOut); dim(testOut)

## Creating a Table with binary encoding for duplicate concepts. I.e., each concept mentioned more than once is a column, each CAM is a row.
# If the concept is mentioned in the respective CAM, cell is 1, if not cell is 0

# conDF = conceptsDF
# duplDF = duplicateDF
metricEncoding <- function(conDF = conceptsDF, duplDF = duplicateDF) {
  metricDF <- data.frame(matrix(nrow = 0, ncol = length(duplDF$concept))) # Create an empty dataframe where each column is going to represent a concept and each row a CAM
  colnames(metricDF) <- duplDF$concept #Name the columns of the dataframe according to the list of concepts occuring more than once
  #rownames(binaryDF) <- duplDF$concept
  CAMs <- colnames(conDF)
  for(CAM in CAMs) {
    metricVec <- c() #Create an empty vector that is going to be filled with the number of occurence of concepts in the respective CAM
    for(concept in duplDF$concept) { #For each concept count how often it is mentioned in the respective CAM
      metricVec <- append(metricVec, length(which(conDF[[CAM]] == concept)))
    }
    metricDF[nrow(metricDF) + 1,] = metricVec #Append the vector as a row to binaryDF
    rownames(metricDF)[nrow(metricDF)] <- CAM #Name the respective row by the ID of the CAM it represents
  }

  return(metricDF)
}

testOut <- metricEncoding(conDF = conceptsDF, duplDF = duplicateDF)
head(testOut); dim(testOut)



## Calculates Phi coefficient and p-value (chi-square) and returns both in a vector
phiCoefficient <- function(var1, var2) {
  crosstab <- table(factor(var1, levels=c(0,1)), factor(var2, levels=c(0,1)))
  chisqTest <- chisq.test(crosstab, correct = FALSE)
  sig <- chisqTest$p.value
  a <- crosstab[1, 1]
  b <- crosstab[1, 2]
  c <- crosstab[2, 1]
  d <- crosstab[2, 2]
  ad <- a * d
  bc <- b * c
  phi <- (ad - bc) / sqrt((a + b) * (c + d) * (a + c) * (b + d))

  if(round(x = phi, digits = 5) != psych::phi(t = crosstab, digits = 5)){
    print("possible ERROR in phi computation")
  }
  return(c(phi, sig))
}



## Calculates pearson coefficient and p-value and returns both in a vector
pearsonCoefficient <- function(var1, var2) {
  correl <- cor.test(var1, var2)
  sig <- correl$p.value
  #print(typeof(correl$estimate))
  r <- correl$estimate
  return(c(r, sig))
}


conList = duplicateDF
inputTable =  binaryDf
conRow =  conList$concept[24]; conList$concept[24]
conCol =  conList$concept[19]; conList$concept[19]
CorrFUNC = phiCoefficient
CorrFUNC = pearsonCoefficient

CorrFUNC(inputTable[[conRow]], inputTable[[conCol]])


cbind(inputTable[[conRow]], inputTable[[conCol]])
cor(cbind(inputTable[[conRow]], inputTable[[conCol]]))
psych::phi(table(factor(inputTable[[conRow]], levels=c(0,1)), factor(inputTable[[conCol]], levels=c(0,1))))
lsa::cosine(cbind(inputTable[[conRow]], inputTable[[conCol]]))

# CorrFUNC = phiCoefficient



## Builds a list that contains two Dataframes: one that contains the correlation coefficients for the pairwise co-occurence of all concepts and one the respective p-values
correlationTable <- function(conList = duplicateDF, inputTable = binaryDf, CorrFUNC) {
  corrTabDF <- data.frame(matrix(nrow = 0, ncol = length(conList$concept))) # Create an empty dataframe where each column and each row is going to represent a concept
  sigTabDF <- data.frame(matrix(nrow = 0, ncol = length(conList$concept))) # Create an empty dataframe where each column and each row is going to represent a concept
  colnames(corrTabDF) <- colnames(sigTabDF) <- conList$concept
  for(conRow in conList$concept) {
    corrList <- list() #Create an empty list that is going to be filled with the correlation coefficients
    sigList <- list() #Create an empty list that is going to be filled with the p-values
    i <- 1
    for(conCol in conList$concept) {
      newCell <- CorrFUNC(inputTable[[conRow]], inputTable[[conCol]])
      corrList[[i]] <- newCell[1]
      sigList[[i]] <- newCell[2]
      i <- i+1
    }
    corrTabDF[nrow(corrTabDF) + 1,] <- corrList #Append the list as a row to corrTabDF
    sigTabDF[nrow(sigTabDF) + 1,] <- sigList #Append the list as a row to sigTabDF
    rownames(corrTabDF)[nrow(corrTabDF)] <- conRow #Name the respective row by the ID of the CAM it represents
    rownames(sigTabDF)[nrow(sigTabDF)] <- conRow #Name the respective row by the ID of the CAM it represents
  }
  return(list(corrTabDF, sigTabDF))
}


## Calling functions defined above in oder to calculate correlation coefficients and p-values for all concept pairings
conceptsDF <- listConcepts(CAMsConsInput, useSummarized = TRUE, removeSuffix = TRUE)
duplicateDF <- countDuplicates(concepts = conceptsDF, oderFrequency = TRUE)
binaryDf <- binaryEncoding(conDF = conceptsDF, duplDF = duplicateDF)
# metricDF <- metricEncoding(conceptsDF, duplicateDF)
CorrSigTables <- correlationTable(conList = duplicateDF,
                                  inputTable =  binaryDf, # metricDF
                                  CorrFUNC = ifelse(binaryCorr, phiCoefficient, pearsonCoefficient))
names(CorrSigTables) <- c('coefficient', 'p')




## Extracting separate matrices for correlation coefficients and p values and filtering out rows/columns with NA only ##
coefficientMatrix <- as.matrix(CorrSigTables[["coefficient"]])
coefficientMatrix <- coefficientMatrix[rowSums(!is.na(coefficientMatrix))>0, colSums(!is.na(coefficientMatrix))>0]
pMatrix <- as.matrix(CorrSigTables[["p"]])
pMatrix <- pMatrix[rowSums(!is.na(pMatrix))>0, colSums(!is.na(pMatrix))>0]

## Filter matrices such that only concepts with at least one significant correlation (apart from autocorrelation) remain
if(showOnlySignificant == TRUE) {
  coefficientMatrix <- coefficientMatrix[(rowSums(pMatrix <= levelOfSignicifance) > 1), (colSums(pMatrix <= levelOfSignicifance) > 1)]
  pMatrix <- pMatrix[(rowSums(pMatrix <= levelOfSignicifance) > 1), (colSums(pMatrix <= levelOfSignicifance) > 1)]
}

## Creating a combined matrix with coefficients and p values for output
coefficientVec <- paste0(round(coefficientMatrix,3))
pVec <- paste0(round(pMatrix,3))
combinedMatrix <- matrix(paste0(coefficientVec, " (p=", pVec, ")"), ncol = ncol(coefficientMatrix))
colnames(combinedMatrix) <- rownames(combinedMatrix) <- colnames(coefficientMatrix)


### Preparing interactive heatmap (see comments below, if this causes problems in shiny)
if(interactiveDots){
  nodeType = "scatter"
} else {
  nodeType = "heatmap"
}



coefficientMatrix <- coefficientMatrix[str_detect(string = rownames(coefficientMatrix), pattern = "^ea|^risk|^benefits"),
                  str_detect(string = rownames(coefficientMatrix), pattern = "^ea|^risk|^benefits")]

interactiveHeatmap <- heatmaply_cor(
  coefficientMatrix,
  node_type = nodeType,
  point_size_mat = -log10(pMatrix),
  point_size_name = "-log10(p-value)",
  xlab = "Concepts",
  ylab = "Concepts",
  k_col = 2,
  k_row = 2,
  label_names = c("x", "y", "Correlation")
)



#######################################
##### Output (tables and heatmap) #####
interactiveHeatmap #Is an html object. If interactive heatmap causes problems in shiny, uncomment the following two lines
#regularHeatmap <- heatmap(coefficientMatrix)
#regularHeatmap
coefficientMatrix
pMatrix
combinedMatrix
#######################################



