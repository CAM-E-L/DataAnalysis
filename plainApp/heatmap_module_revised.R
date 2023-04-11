

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

library(stats)
# library(sortable)


#library("ggplot2")
# library(dplyr)
#library("jsonlite")
#library('magrittr')
#library('gmodels')
#library('psych')
# library(purrr)
# library(stats)
library(heatmaply)

library(psych)

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




#unique(CAMfiles[[1]]$text_summarized)
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

tmp_listConcepts <- listConcepts(datCAM = CAMsConsInput, useSummarized = TRUE, removeSuffix = TRUE)
head(tmp_listConcepts)




### Takes a data frame the concepts mentioned in the CAMs (one CAM one column)
### Returns a data frame with all concepts mentioned in more than one CAM (columns "concept") and the number of CAMS where it occurs (column "numCAMoccurences")
countDuplicates <- function(concepts = conceptsDF, oderFrequency = FALSE){
  allCons <- c()

  for(i in 1:ncol(concepts)) {       # for-loop over columns
    CAMcons <- unlist(concepts[ , i], use.names=FALSE)
    allCons <- append(allCons,unique(CAMcons))
  }

  duplicateCons <- names(table(allCons))[table(allCons) >=2]


  duplicateDF <- matrix(nrow = length(duplicateCons), ncol = 2)
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

tmp_countDuplicates <- countDuplicates(concepts = tmp_listConcepts, orderFrequency = TRUE)
head(tmp_countDuplicates)

## Creating a Table with binary encoding for duplicate concepts. I.e., each concept mentioned more than once is a column, each CAM is a row.
# If the concept is mentioned in the respective CAM, cell is 1, if not cell is 0
binaryEncoding <- function(conDF = conceptsDF, duplDF = duplicateDF) {
  binaryDF <- data.frame(matrix(nrow = ncol(conDF), ncol = length(duplDF$concept))) # Create an empty dataframe where each column is going to represent a concept and each row a CAM
  colnames(binaryDF) <- duplDF$concept #Name the columns of the dataframe according to the list of concepts occurring more than once
  binaryDF[is.na(binaryDF)] <- 0

  rownames(binaryDF) <- colnames(conDF)

  for(r in 1:nrow(binaryDF)){
    binaryDF[rownames(binaryDF)[r],] <- as.numeric(duplDF$concept %in% conDF[, rownames(binaryDF)[r]])
  }

  return(binaryDF)
}


tmp_binaryEncoding <- binaryEncoding(conDF = tmp_listConcepts, duplDF = tmp_countDuplicates)
head(tmp_binaryEncoding)



## Creating a Table with metric encoding for duplicate concepts. I.e., each concept mentioned more than once is a column, each CAM is a row.
# If the concept is mentioned in the respective CAM, cell is X, whereby X is the number of occurrence of concepts in the respective CAM if not cell is 0
# conDF = conceptsDF
# duplDF = duplicateDF
metricEncoding <- function(conDF = conceptsDF, duplDF = duplicateDF) {
  metricDF <- data.frame(matrix(nrow = 0, ncol = length(duplDF$concept))) # Create an empty dataframe where each column is going to represent a concept and each row a CAM
  colnames(metricDF) <- duplDF$concept #Name the columns of the dataframe according to the list of concepts occuring more than once
  #rownames(binaryDF) <- duplDF$concept
  CAMs <- colnames(conDF)
  for(CAM in CAMs) {
    metricVec <- c() #Create an empty vector that is going to be filled with the number of occurrence of concepts in the respective CAM
    for(concept in duplDF$concept) { #For each concept count how often it is mentioned in the respective CAM
      metricVec <- append(metricVec, length(which(conDF[[CAM]] == concept)))
    }
    metricDF[nrow(metricDF) + 1,] = metricVec #Append the vector as a row to binaryDF
    rownames(metricDF)[nrow(metricDF)] <- CAM #Name the respective row by the ID of the CAM it represents
  }

  return(metricDF)
}

tmp_metricEncoding <- metricEncoding(conDF = tmp_listConcepts, duplDF = tmp_countDuplicates)
head(tmp_metricEncoding)
# tmp_metricEncoding[rownames(tmp_metricEncoding) %in% rownames(tmp_metricEncoding)[tmp_metricEncoding$`risks for health` >= 2], ]
sum(tmp_metricEncoding >= 2)


## Calculates Phi coefficient and p-value (chi-square) and returns both in a vector
phiCoefficient <- function(var1, var2) {
  crosstab <- table(factor(var1, levels=c(0,1)), factor(var2, levels=c(0,1)))
  chisqTest <- stats::chisq.test(crosstab, correct = FALSE)
  sig <- chisqTest$p.value
  a <- crosstab[1, 1]
  b <- crosstab[1, 2]
  c <- crosstab[2, 1]
  d <- crosstab[2, 2]
  ad <- a * d
  bc <- b * c
  phi <- (ad - bc) / sqrt((a + b) * (c + d) * (a + c) * (b + d))

  ## return earlier is phi is nan (divide by 0)
  if(is.nan(x = phi)){
    # print(phi)
    # print(crosstab)
    return(c(NA, NA))
  }

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


## Calculates Cohens Kappa
cohensKappa <- function(var1, var2) {

  outCohen <- irr::kappa2(cbind(var1, var2))

  sig <- outCohen$p.value
  r <- outCohen$value

  return(c(r, sig))
}



## Builds a list that contains two Dataframes: one that contains the correlation coefficients for the pairwise co-occurence of all concepts and one the respective p-values
# args
# conList = duplicateDF
# inputTable =  binaryDf
# CorrFUNC = cohensKappa # pearsonCoefficient, phiCoefficient
# considerValence = TRUE
# datNodes = CAMfiles[[1]]

correlationTable <- function(conList = duplicateDF,
                             inputTable = binaryDf,
                             CorrFUNC,
                             considerValence = TRUE,
                             datNodes = NULL) {

  if(considerValence){
    ## update inputTable df by valence ratings
    datNodes$text_summarized <-
      str_remove(string = datNodes$text_summarized , pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
    datNodes$value <- ifelse(test = datNodes$value == 10, yes = 0, no = datNodes$value)

    for(r in 1:nrow(inputTable)){
      tmp_CAMid <- str_remove(string = rownames(inputTable)[r], pattern = "^X")
      tmp_CAMid <- str_replace_all(string = tmp_CAMid, pattern = "\\.", replacement = "-")

      tmp_datNodes <- datNodes[datNodes$CAM == tmp_CAMid,]

      for(c in 1:ncol(inputTable)){
        if(inputTable[r,c] == 1){
          tmp_value <- tmp_datNodes$value[tmp_datNodes$text_summarized %in% colnames(inputTable)[c]]

          if (mean(tmp_value) > 0){
            inputTable[r,c] <- "positive"
          }else if (mean(tmp_value) < 0){
            inputTable[r,c] <- "negative"
          } else {
            inputTable[r,c] <- "neutral"
          }
        }
      }
    }

    # error message if no overlapping words
    # for(c in 1:ncol(inputTable)){
    #   inputTable[,c] <- factor(inputTable[,c], levels=c("positive","negative","neutral"))
    # }
  }



  ## set up data frames
  mat_est <- matrix(data = NA, nrow = length(conList$concept), ncol = length(conList$concept))
  mat_p <- matrix(data = NA, nrow = length(conList$concept), ncol = length(conList$concept))

  for(r in 1:length(conList$concept)) {
    # print(r)
    for(c in 1:length(conList$concept)) {
      # print(c)

      if(r == c){
        mat_est[r, c] <- 1  # add estimate (main diagonal always 1)
        mat_p[r, c] <- 0 # add p-value
      }else if(r < c){
        newCell <- CorrFUNC(var1 = inputTable[, r],
                            var2 = inputTable[, c])
        # add estimate (phi or corr)
        mat_est[r, c] <- newCell[1]
        mat_est[c, r] <- newCell[1]
        # add p-value
        mat_p[r, c] <- newCell[2]
        mat_p[c, r] <- newCell[2]
      }
    }
  }

  rownames(mat_est) <- conList$concept
  colnames(mat_est) <- conList$concept

  rownames(mat_p) <- conList$concept
  colnames(mat_p) <- conList$concept


  return(list(mat_est, mat_p))
}


## Calling functions defined above in oder to calculate correlation coefficients and p-values for all concept pairings
conceptsDF <- listConcepts(CAMsConsInput, useSummarized = TRUE,
                           removeSuffix = TRUE)
duplicateDF <- countDuplicates(concepts = conceptsDF, oderFrequency = TRUE)
binaryDf <- binaryEncoding(conDF = conceptsDF, duplDF = duplicateDF)
# metricDF <- metricEncoding(conceptsDF, duplicateDF)


CorrSigTables <- correlationTable(conList = duplicateDF,
                                  inputTable =  binaryDf, # metricDF
                                  CorrFUNC = phiCoefficient, considerValence = FALSE,
                                  datNodes = CAMfiles[[1]])



CorrSigTables <- correlationTable(conList = duplicateDF,
                                  inputTable =  binaryDf, # metricDF
                                  CorrFUNC = cohensKappa, considerValence = TRUE,
                                  datNodes = CAMfiles[[1]])
names(CorrSigTables) <- c('coefficient', 'p')




## Extracting separate matrices for correlation coefficients and p values and filtering out rows/columns with NA only ##
coefficientMatrix <- as.matrix(CorrSigTables[["coefficient"]])
coefficientMatrix <- coefficientMatrix[rowSums(!is.na(coefficientMatrix))>1,
                                       colSums(!is.na(coefficientMatrix))>1]
pMatrix <- as.matrix(CorrSigTables[["p"]])
pMatrix <- pMatrix[rowSums(!is.na(pMatrix))>1,
                   colSums(!is.na(pMatrix))>1]

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


interactiveHeatmap <- heatmaply_cor(
  coefficientMatrix,
  node_type = nodeType,
  point_size_mat = -log10(pMatrix),
  point_size_name = "-log10(p-value)",
  xlab = "Concepts",
  ylab = "Concepts",
  k_col = 2,
  k_row = 2,
  label_names = c("x", "y", "Correlation"),
  width = 1000,
  height = 1000
)



#######################################
##### Output (tables and heatmap) #####
interactiveHeatmap #Is an html object. If interactive heatmap causes problems in shiny, uncomment the following two lines


interactiveHeatmap  %>% plotly::layout(height=800,width=800)

regularHeatmap <- heatmap(coefficientMatrix)
regularHeatmap
coefficientMatrix
pMatrix
combinedMatrix
#######################################



heatmap(x = coefficientMatrix, col = brewer.pal(n = 5, name = "Paired"))
legend(x = "bottomright", legend = c("very low", "low", "medium", "high","very high"),
       cex = 0.6, fill =  brewer.pal(n = 5, name = "Paired"))


tmp <- coefficientMatrix
rownames(tmp) <- NULL
colnames(tmp) <- NULL


cosine_sim <- as.dist(tmp) # create similarity  using euclidean distance
cluster1 <- hclust(cosine_sim, method = "ward.D2") # Ward's method
plot(cluster1)



cosine_sim <- as.dist(cor(variables)) # create similarity  using euclidean distance
cluster1 <- hclust(cosine_sim, method = "ward.D2") # Ward's method
plot(cluster1)


