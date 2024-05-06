# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()


########################################
# please define!
#> put everything in the "data" folder (your data set and protocol if you have one)
########################################
CAMdataset <- "Lars_2024_IndiviualCAMs.txt"
# "Fenn_2023_SAIstudy_subset.txt"
# "Fenn_2023_CAMtools.txt"

protocolDataset <- "CAMspiracy_protocol.txt" #  # protocol.txt
consider_Protocol <- FALSE


########################################
# load packages
########################################
# for deploying Shiny App online
# remotes::install_version("rsconnect", "0.8.29")
# see issue: https://github.com/rstudio/rsconnect/issues/926

library(shiny)

# library(shinyWidgets)
library(shinyjs)

library(shinyvalidate)

# library(shinycssloaders) %>% withSpinner(color="#0dc5c1")

library(tidyverse)
library(lubridate)

library(magrittr)

library(rjson) # write JSON files
library(jsonlite) # read JSON files



library(igraph)

library(sortable)

library(vroom)
library(xlsx)


library(irr)


library(stargazer)


library(kableExtra) # APA 7 tables

## for heatmap
library(stats)
library(heatmaply)
library(plotly)
library(RColorBrewer)


library(tm)
library(stopwords) # old function for spell checking

library(visNetwork)
library(wordcloud)


library(moments)

library(psych)
library(rempsyc) # APA tables with nice_table()
library(flextable) # dependency of rempsyc
library(officer) # landscape mode for docx export

library(Cairo) # save CAMs as .png file

library(ggcorrplot)
# library(qdap, include.only = c('syn')) # include multiple functions
# library(qdapDictionaries, include.only = c('key.syn'))
# library(qdap)
########################################
# load functions
########################################
setwd("../www/functions_CAM")
dir()


for(i in 1:length(dir())){
  # print(dir()[i])
  source(dir()[i], encoding = "utf-8")
}
rm(i)


########################################
# get session info (version of R and loaded packages)
########################################
# devtools::session_info()



########################################
# load data
########################################
setwd("../../additional scripts/data")
dir()

### load CAM files
# individual
suppressMessages(read_file(CAMdataset) %>%
                   # ... split it into lines ...
                   str_split('\n') %>% first() %>%
                   discard(function(x) x == '') %>%
                   discard(function(x) x == '\r') %>%
                   # ... filter empty rows ...
                   discard(function(x) x == '')) -> dat_CAM

raw_CAM <- list()
for(i in 1:length(dat_CAM)){
  raw_CAM[[i]] <- jsonlite::fromJSON(txt = dat_CAM[[i]])
}
rm(i)


### if protocol considered
if(consider_Protocol){
  text <- readLines(protocolDataset, warn = FALSE)
  text <- readLines(textConnection(text, encoding="UTF-8"), encoding="UTF-8")

  if (testIfJson(file = text)) {
    protocol <- rjson::fromJSON(file = protocolDataset)
  } else{
    print("Invalid protocol uploaded")
  }
}




setwd("..")


########################################
# create CAM files, draw CAMs
########################################
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE)

### if protocol considered
if(consider_Protocol){
  CAMfiles[[1]] <- CAMfiles[[1]][CAMfiles[[1]]$CAM %in% protocol$currentCAMs,]
  CAMfiles[[2]] <- CAMfiles[[2]][CAMfiles[[2]]$CAM %in% protocol$currentCAMs,]
  CAMfiles[[3]] <- CAMfiles[[3]][CAMfiles[[3]]$CAM.x %in% protocol$currentCAMs,]


  tmp_out <- overwriteTextNodes(protocolDat = protocol,
                                nodesDat = CAMfiles[[1]])
  CAMfiles[[1]] <- tmp_out[[1]]
  tmp_out[[2]]
}



### draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all",
                     plot_CAM = FALSE,
                     useCoordinates = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

plot(CAMdrawn[[1]], edge.arrow.size = .7,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
     vertex.size = 10, vertex.label.cex = .9)





vec_duration <- c()
for(c in unique(CAMfiles[[1]]$CAM)){
  print(c)

  tmp_diffConcepts <- CAMfiles[[1]][CAMfiles[[1]]$CAM == c,"dateConceptCreated"] -
    CAMfiles[[1]][CAMfiles[[1]]$CAM == c,"dateCAMcreated"]
  tmp_diffConnectors <- CAMfiles[[2]][CAMfiles[[2]]$CAM == c,"dateConnectorCreated"] -
    CAMfiles[[2]][CAMfiles[[2]]$CAM == c,"dateCAMcreated"]

  print(max(c(tmp_diffConcepts, tmp_diffConnectors)))

  vec_duration[[c]] <- max(c(tmp_diffConcepts, tmp_diffConnectors))
}



sort(table(CAMfiles[[1]]$text))
tmp_micro <- c("Ökologische Nachhaltigkeit", "Wirtschaftswachstum")

tmp_Indicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn,
                                        micro_degree =  tmp_micro,
                                        micro_valence = tmp_micro,
                                        micro_centr_clo = tmp_micro,
                                        micro_transitivity = tmp_micro,
                                        largestClique = FALSE)

tmp_neighborhoodIndicators <- compute_neighborhoodIndicatorsCAM(drawn_CAM = CAMdrawn, weightSecondOrder = .5,
                                  consideredConcepts = c("Ökologische Nachhaltigkeit", "Wirtschaftswachstum"),
                                  sliceCAMbool = FALSE,
                                  removeConnectionCAM = c("Ökologische Nachhaltigkeit", "Wirtschaftswachstum"),
                                  removeNodeCAM = NULL)

plot(tmp_neighborhoodIndicators$mean_1_ÖkologischeNachhaltigkeit, tmp_neighborhoodIndicators$mean_1_Wirtschaftswachstum
)

tmp_neighborhoodIndicators$mean_1_ÖkologischeNachhaltigkeit - tmp_neighborhoodIndicators$mean_1_Wirtschaftswachstum


CAMwordlist <- create_wordlist(
  dat_nodes = CAMfiles[[1]],
  dat_merged = CAMfiles[[3]],
  useSummarized = TRUE,
  order = "frequency",
  splitByValence = TRUE,
  comments = TRUE,
  raterSubsetWords = NULL,
  rater = FALSE
)











dat_nodes <- CAMfiles[[1]]
dat_nodes$text <-   dat_nodes$text_summarized
sum(stringr::str_detect(string = dat_nodes$text, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")) < nrow(dat_nodes)

# CAMfiles[[1]]$text
# tmp_text <- unique(x = tmp_text)
# wordsOut <- sample(x = tmp_text, size = 11, replace = FALSE)
# wordsOut
# CAMwordlist <- create_wordlist(
#   dat_nodes = CAMfiles[[1]],
#   dat_merged = CAMfiles[[3]],
#   useSummarized = TRUE,
#   order = "frequency",
#   splitByValence = TRUE,
#   comments = TRUE,
#   raterSubsetWords = wordsOut,
#   rater = TRUE
# )


length(wordsOut); nrow(CAMwordlist)
CAMwordlist

CAMwordlist$comment_1
CAMwordlist$sd_valence
