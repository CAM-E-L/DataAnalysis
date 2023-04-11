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
CAMfiles[[2]] <- vroom::vroom(file = "CAM_connectors_raw.txt", delim = "\t",
                              show_col_types = FALSE)
CAMfiles[[3]] <- vroom::vroom(file = "CAM_merged_raw.txt", delim = "\t",
                              show_col_types = FALSE)

CAMfiles[[1]]$text_summarized <- CAMfiles[[1]]$text
tmp_out <- overwriteTextNodes(protocolDat = protocol, nodesDat = CAMfiles[[1]])
# CAMfiles[[1]] <- tmp_out[[1]]
# tmp_out[[1]]$text_summarized
# CAMfiles[[1]]$text_summarized














tmp <- str_subset(string = CAMfiles[[1]]$text_summarized, pattern = "_", negate)
sort(table(tmp))
length(tmp) / nrow(CAMfiles[[1]])


word(string = CAMfiles[[1]]$text)
cat(CAMfiles[[1]]$text[str_count(string = CAMfiles[[1]]$text,
                                 pattern = "\\S+") == 1])
######################
# tmp_nodes_out <- CAMfiles[[1]]
# tmp_nodes_out$text <- tmp_nodes_out$text_summarized
# tmp_nodes_out$text_summarized <- NULL
#
# vroom::vroom_write(x = tmp_nodes_out, file = "CAM_nodes_raw_upload.txt")



CAMwordlist <- create_wordlist(
  dat_nodes = CAMfiles[[1]],
  dat_merged = CAMfiles[[3]],
  order = "alphabetic",
  splitByValence = TRUE,
  comments = FALSE
)

head(CAMwordlist)

vroom::vroom_write(x = CAMwordlist, "aa.txt")
xlsx::write.xlsx2(x = CAMwordlist, file = "CAMwordlist.xlsx", row.names = FALSE)
