# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()


########################################
# please define!
#> put everything in the "data" folder (your data set and protocol if you have one)
########################################
CAMdataset <- "Fenn_2023_CAMtools.txt" # Fenn_2023_SAIstudy_subset.txt
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
  print(dir()[i])
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

setwd("..")


########################################
# create CAM files, draw CAMs
########################################
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE)

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
