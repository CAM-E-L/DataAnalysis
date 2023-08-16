# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()


########################################
# please define!
#> put everything in the "data" folder (your data set and protocol if you have one)
########################################
CAMdataset <- "Fenn_2023_CAMtools.txt"
# "Fenn_2023_SAIstudy_subset.txt"
# "Fenn_2023_CAMtools.txt"

protocolDataset <- "protocol_Fenn_2023_CAMtools.txt" #  # protocol.txt
consider_Protocol <- TRUE






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

read_file(CAMdataset) %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') -> dat_CAM


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
  tmp_out <- overwriteTextNodes(protocolDat = protocol,
                                nodesDat = CAMfiles[[1]])
  CAMfiles[[1]] <- tmp_out[[1]]
}


### draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all",
                     plot_CAM = FALSE,
                     useCoordinates = TRUE,
                     relvertexsize = 3,
                     reledgesize = 1)

plot(CAMdrawn[[1]], edge.arrow.size = .7,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
     vertex.size = 10, vertex.label.cex = .9)


tmp_microIndicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn,
                                             micro_degree = NULL,
                                             micro_valence = NULL,
                                             micro_centr_clo = NULL,
                                             micro_transitivity = NULL,
                                             largestClique = FALSE)




centralConcepts <- c("negative aspects", "positive aspects")

slicedCAMs_combined <- sliceAllCAMs_combined(CAMfilesList = CAMfiles,
                                             drawnCAMs = CAMdrawn,
                                             connectionToRemove = NULL,
                                             nodeToRemove = "Covid-19",
                                             centralConceptsSubgraphs = centralConcepts,
                                             plot = FALSE)




slicedCAMs_seperated <- sliceAllCAMs_seperated(slicedCAMs = slicedCAMs_combined,
                                               centralConceptsSubgraphs = centralConcepts,
                                               plot = FALSE)
names(slicedCAMs_seperated)



tmp_merged <- slicedCAMs_combined[[3]][slicedCAMs_combined[[3]]$CAM.x %in% unique(slicedCAMs_combined[[3]]$CAM.x)[1], ]
tmp_nodes <- slicedCAMs_combined[[1]][slicedCAMs_combined[[1]]$CAM %in% unique(slicedCAMs_combined[[1]]$CAM)[1], ]


tmp_c12 <- draw_CAM(dat_merged = tmp_merged,
                         dat_nodes = tmp_nodes, ids_CAMs = "all",
                         plot_CAM = FALSE,
                         useCoordinates = TRUE,
                         relvertexsize = 3,
                         reledgesize = 1)

plot(tmp_c12[[1]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn[[names(tmp_c12)[1]]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)

names(CAMdrawn_c12)




CAMdrawn_c12 <- draw_CAM(dat_merged = slicedCAMs_combined[[3]],
                        dat_nodes = slicedCAMs_combined[[1]], ids_CAMs = "all",
                        plot_CAM = TRUE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)




tmp_merged <- slicedCAMs_seperated[[3]][slicedCAMs_seperated[[3]]$CAM.x %in% unique(slicedCAMs_seperated[[3]]$CAM.x)[1], ]
tmp_nodes <- slicedCAMs_seperated[[1]][slicedCAMs_seperated[[1]]$CAM %in% unique(slicedCAMs_seperated[[1]]$CAM)[1], ]

CAMdrawn_c1_1 <- draw_CAM(dat_merged = tmp_merged,
                        dat_nodes = tmp_nodes, ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)



CAMdrawn_c1 <- draw_CAM(dat_merged = slicedCAMs_seperated[[3]],
                        dat_nodes = slicedCAMs_seperated[[1]], ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)

CAMdrawn_c2 <- draw_CAM(dat_merged = slicedCAMs_seperated[[6]],
                        dat_nodes = slicedCAMs_seperated[[4]], ids_CAMs = "all",
                        plot_CAM = FALSE,
                        useCoordinates = TRUE,
                        relvertexsize = 3,
                        reledgesize = 1)



plot(CAMdrawn[[names(CAMdrawn_c1)[2]]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn_c12[[2]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn_c1[[2]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)
plot(CAMdrawn_c2[[2]], edge.arrow.size = .3,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = 0.1,
     vertex.size = 10, vertex.label.cex = .9)


###########
tmp_microIndicators_c1 <- compute_indicatorsCAM(drawn_CAM = CAMdrawn_c1,
                                                micro_degree = NULL,
                                                micro_valence = NULL,
                                                micro_centr_clo = NULL,
                                                micro_transitivity = NULL,
                                                largestClique = FALSE)


tmp_microIndicators_c2 <- compute_indicatorsCAM(drawn_CAM = CAMdrawn_c2,
                                                micro_degree = NULL,
                                                micro_valence = NULL,
                                                micro_centr_clo = NULL,
                                                micro_transitivity = NULL,
                                                largestClique = FALSE)


getDescriptives(dataset = tmp_microIndicators_c1, nameAPAtable = NULL)
getDescriptives(dataset = tmp_microIndicators_c2, nameAPAtable = NULL)



tmp_microIndicators_c12 <- rbind(tmp_microIndicators_c1, tmp_microIndicators_c2)
tmp_microIndicators_c12$group <- rep(centralConcepts, each = nrow(tmp_microIndicators_c1))

p_meanValence <- ggstatsplot::ggwithinstats(
  data = tmp_microIndicators_c12,
  x = group,
  y = mean_valence_macro,
  type = "parametric", ## type of statistical test
  xlab = "Central Concepts", ## label for the x-axis
  ylab = "Mean Valence", ## label for the y-axis
  title = "Comparison of mean valence between choosen central concepts"
) + ## modifying the plot further
  ggplot2::scale_y_continuous(
    limits = c(-3, 3),
    breaks = -3:3
  )
p_meanValence
