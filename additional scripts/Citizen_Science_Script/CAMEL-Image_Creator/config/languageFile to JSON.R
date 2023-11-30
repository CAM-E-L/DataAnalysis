# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

############################################################################
# load packages, functions, data
############################################################################

################
# packages
################
library(jsonlite)
library(xlsx)

################
# data
################
dir()
dat_european <- xlsx::read.xlsx2(file = "languageFile.xlsx",
                        sheetIndex = 1, encoding = "UTF-8")
dat_chinese <- read.csv(file = "languageFile_chinese.csv")

# dat$chinese <- stringi::stri_trans_general(dat$chinese, "zh")

# dat_european$chinese <- dat_chinese$chinese
# Encoding(x = dat_european$german) <- "UTF-8"
# Encoding(x = dat_european$chinese) <- "Chinese"
# Sys.setlocale(locale = "Chinese")
# write(toJSON(dat_european), "languageFile.json")


write(toJSON(dat_chinese[, c("X_identifier", "X_location", "english", "chinese")]), "languageFile_chinese.json")
options("encoding" = "UTF-8")
write(toJSON(dat_european[, c("X_identifier", "X_location",
                              "english",
                              "german",
                              "french",
                              "spanish")]), "languageFile_european.json")



