########################################
# Script to automatically check "non-sense" CAMs (CAMs which include no )
#> Remarks: currently implemented for German
#> https://camgalaxy.github.io/?ShowResearcherButtons=true&fullScreen=false
########################################

# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


if(!exists(x = "CAMfiles") && !exists(x = "CAMdrawn")){
  cat('\n
      please run script "aa_start_load_draw" before running this script
      \n')
}


########################################
# load dictionaries
########################################
### open-office dictionaries:
# see: https://extensions.openoffice.org/en/project/german-de-de-frami-dictionaries
setwd("data dictionaries")
### english
dict_english <- readr::read_table(file = "en_frami.txt", col_names = TRUE)

dict_english <- str_subset(string = dict_english$words, pattern = "/")
dict_english <- str_remove_all(string = dict_english, pattern = "/.*$")
dict_english <- str_subset(string = dict_english, pattern = "^[[:alpha:]]{2,}")
dict_english <- c(dict_english, "markets", "statistical", "Processing",
                  "components", "models", "Sustainable", "Anthropocene", "Sustainability",
                  "Renewable", "geoengineering", "BECCS", "Fearful")
dict_english <- tolower(x = dict_english)
dict_english <- dict_english[nchar(x = dict_english) >= 5]
dict_english <- c(dict_english, "the", "are")
dict_english <- unique(dict_english)

# nchar(x = dict_english[1:10])
# dict_english[1:10]

### german
dict_german <- readr::read_table(file = "de_frami.txt", col_names = TRUE)

dict_german <- str_subset(string = dict_german$words, pattern = "/")
dict_german <- str_remove_all(string = dict_german, pattern = "/.*$")
dict_german <- str_subset(string = dict_german, pattern = "^[[:alpha:]]{2,}")
dict_german <- c(dict_german, "globale", "KOGNITIVE", "Modelle", "Nutzeneffekte", "Faktoren", "Soziale")
dict_german <- tolower(x = dict_german)
dict_german <- dict_german[nchar(x = dict_german) >= 5]
dict_german <- unique(dict_german)
setwd("..")





########################################
# create aggregated CAM
########################################
## the function draw_CAM creates a list
length(x = CAMdrawn)
## every element in the list is an igraph object and has so called vertex (and edge) attributes
vertex.attributes(graph = CAMdrawn[[1]])
## writing a for loop + using the %in% operator you can check if the text of drawn concepts are existing words
V(CAMdrawn[[1]])$label



vec_fakeCAMs <- rep(x = FALSE, times = length(CAMdrawn))
vec_IDs_fakeCAMs <- rep(x = NA, times = length(CAMdrawn))
for(i in 1:length(CAMdrawn)){
  tmpSingleWords <- stringr::str_split(string = V(CAMdrawn[[i]])$label , pattern = " ", simplify = TRUE)
  tmpSingleWords <- tmpSingleWords[tmpSingleWords != ""]
  tmpSingleWords <- unique(tmpSingleWords)
  tmpSingleWords <- tolower(tmpSingleWords)

  if(any(tmpSingleWords %in% dict_german)){
    cat('\nI am a real CAM - words found:\n')
    cat(tmpSingleWords[tmpSingleWords %in% dict_german])

  }else{
    print("I am a fake CAM")
    vec_fakeCAMs[i] <- TRUE
    vec_IDs_fakeCAMs[i] <- names(CAMdrawn)[i]
  }
}


vec_IDs_fakeCAMs <- vec_IDs_fakeCAMs[!is.na(vec_IDs_fakeCAMs)]
length(vec_IDs_fakeCAMs)


plot(CAMdrawn[[vec_IDs_fakeCAMs[4]]], edge.arrow.size = .7,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
     vertex.size = 10, vertex.label.cex = .9)
