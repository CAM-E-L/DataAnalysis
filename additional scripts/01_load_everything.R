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

# library(qdap)
library(qdap, include.only = c('syn')) # include multiple functions
library(qdapDictionaries)


library(kableExtra) # APA 7 tables

## for heatmap
library(stats)
library(heatmaply)
library(plotly)
library(RColorBrewer)


library(tm)
library(visNetwork)
library(wordcloud)

########################################
# functions
########################################
setwd("../www/functions_CAM")
dir()


for(i in 1:length(dir())){
  print(dir()[i])
  source(dir()[i], encoding = "utf-8")
}


########################################
# get session info
########################################
devtools::session_info()



########################################
# load data # !!!
########################################
setwd("../../additional scripts/data")
dir()
myDataset <- "SAI study.txt" # "example_CAMEL_subset_broken.txt"
read_file(myDataset) %>%
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

# raw_CAM[[1]]
