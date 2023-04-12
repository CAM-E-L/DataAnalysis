# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()


############################################################################
# Pakete, Daten, Funktionen laden
############################################################################
################
# Pakete
################
# This code relies on the pacman, tidyverse and jsonlite packages
require(pacman)
p_load('tidyverse', 'jsonlite', 'magrittr', 'xlsx',
       'stargazer', 'psych', 'igraph', 'Cairo', 'visNetwork', 'tm')
# library(stopwords)



################
# Daten
################
### read CAM

# '02_jatos_results_20220108121141.txt'
read_file('jatos_results_20221215131103.txt') %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') -> dat_CAM


raw_CAM <- list()
for(i in 1:length(dat_CAM)){
  raw_CAM[[i]] <- jsonlite::fromJSON(txt = dat_CAM[[i]])
}
rm(i)


################
# sub functions
################
setwd("functions_CAM")
source("create_CAMfiles.R", encoding="utf-8")
source("compute_indicatorsCAM.R", encoding="utf-8")
source("draw_CAM.R", encoding="utf-8")
source("create_wordlist.R", encoding="utf-8")
source("summaryFunctions.R", encoding="utf-8")
source("aggregate_CAMs.R", encoding="utf-8")

source("checkSpelling_words.R", encoding="utf-8")

source("helperFunctions.R", encoding="utf-8")
setwd("..")



############################################################################
# run code -> CAMs
############################################################################
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE)
## Anzahl Knoten pro CAM:
table(CAMfiles[[1]]$CAM)
## Verteilung Valenz
barplot(table(CAMfiles[[1]]$value))
# > 10 = ambivalent
## Häufigste Konzepte
sort(table(CAMfiles[[1]]$text))



set.seed(123)
a <- create_wordlist(dat_nodes = CAMfiles[[1]],
                            dat_merged = CAMfiles[[3]],
                            order = "alphabetic",
                            splitByValence = TRUE,
                            comments = TRUE,
                     raterSubsetWords = sample(x = unique(CAMfiles[[1]]$text), size = 3),
                            rater=FALSE)
a



### draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     useCoordinates = TRUE,
                     relvertexsize = 3,
                     reledgesize = 1)
plot(CAMdrawn[[1]])
plot(induced_subgraph(CAMdrawn[[1]], ego(CAMdrawn[[1]], 1, "ea1d471e-834d-4042-b550-1e5c5bfe6933")[[1]]))
# plot(CAMdrawn[[3]])





### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn,
                                       micro_degree = c("Bedingungsloses Grundeinkommen"),
                                       micro_valence = c("Bedingungsloses Grundeinkommen"),
                                       micro_centr_clo = c("Bedingungsloses Grundeinkommen"))
head(CAMindicators)
## mean valence
hist(CAMindicators$mean_valence_macro)
summary(CAMindicators$mean_valence_macro)


# write.xlsx2(x = CAMindicators, file = "CAMindicators.xlsx")


CAMfiles[[1]]$text_summarized <- CAMfiles[[1]]$text

CAMfiles[[1]]$text_summarized[str_detect(string = CAMfiles[[1]]$text_summarized, pattern = "a")] <- "aaa"
CAMfiles[[1]]$text_summarized[str_detect(string = CAMfiles[[1]]$text_summarized, pattern = "b")] <- "bbb"
CAMfiles[[1]]$text_summarized[str_detect(string = CAMfiles[[1]]$text_summarized, pattern = "c")] <- "ccc"

CAMfiles[[1]] <- rename_identicalTerms(dat_nodes = CAMfiles[[1]], drawn_CAM = CAMdrawn)

### aggregated CAM
sel_ids <- unique(CAMfiles[[1]]$CAM)
CAMaggregated <- aggregate_CAMs(dat_merged = CAMfiles[[3]], dat_nodes = CAMfiles[[1]],
                                ids_CAMs = sel_ids)

plot(CAMaggregated[[2]], vertex.size=diag(CAMaggregated[[1]]) / max(diag(CAMaggregated[[1]]))*20, edge.arrow.size=0.01)
plot(CAMaggregated[[2]], vertex.size=(abs(V(CAMaggregated[[2]])$value)+1)*5, edge.arrow.size=0.01)


g = CAMaggregated[[2]]
g2 = simplify(CAMaggregated[[2]])
# plot(g2, edge.arrow.size=0.01,
#      vertex.size=diag(CAMaggregated[[1]]) / max(diag(CAMaggregated[[1]]))*20)

E(g2)$weight = sapply(E(g2), function(e) {
  length(all_shortest_paths(g, from=ends(g2, e)[1], to=ends(g2, e)[2])$res) } )
E(g2)$weight = E(g2)$weight / 2

V(g2)$color[V(g2)$value <= .5 & V(g2)$value >= -.5] <- "yellow"

V(g2)$shape <- NA
V(g2)$shape <- ifelse(test = V(g2)$color == "yellow", yes = "square", no = "circle")



plot(g2, edge.arrow.size = .5,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
     vertex.size = 5, vertex.label.cex = .9)
############################################################################
# run code -> to upload CAMs
############################################################################

setwd("CAMs as JSON and R format BGE")
ids_CAMs <- unique(CAMfiles[[3]]$CAM.x); length(ids_CAMs)
for(i in 1:length(ids_CAMs)){
    save_graphic(filename = paste0(ids_CAMs[i]))
  CAM_igraph <- CAMdrawn[[c(1:length(CAMdrawn))[
    names(CAMdrawn) == paste0(unique(CAMfiles[[3]]$CAM.x)[i])]]]
  plot(CAM_igraph, edge.arrow.size = .7,
       layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
       vertex.size = 12, vertex.label.cex = .9)
  dev.off()
}

for(i in 1:length(raw_CAM)){
  if(!is_empty(raw_CAM[[i]]$nodes)){
    if(nrow(raw_CAM[[i]]$nodes) > 5){
      write(toJSON(raw_CAM[[i]], encoding = "UTF-8"),
            paste0(raw_CAM[[i]]$idCAM, ".json"))
    }
  }
}

setwd("..")
