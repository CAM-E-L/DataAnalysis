

tmp <- matrix(data = NA, nrow = 5, ncol = 5)

rownames(tmp) <- 1:5
colnames(tmp) <- 1:5


for(c in 1:ncol(x = tmp)){
  for(r in 1:nrow(x = tmp)){
    cat("c:", c, "r:", r, "\n")
    if(c != r && r > c){
      tmp[r,c] <- paste0(r, c, collapse = " ")
    }

  }
}

tmp
diag(x = tmp) <- 1

library(stringr)





tmp <- read.table(file = "summarizedWords(2).txt", header = TRUE, col.names = FALSE)
# tmp <- sort(unique(tmp[,1]))
tmp <- tmp$FALSE.

tmpOneWords <- str_split(string = tmp, pattern = " ", simplify = TRUE)
tmp <- tmp[rowSums(x = tmpOneWords != "") == 1]
tmp <- tolower(tmp)
tmp <- sort(unique(tmp))
tmp[str_detect(string = tmp, pattern = regex("\\W+"), negate = TRUE)]

tmp <- str_replace_all(string = tmp, pattern = regex("\\W+"), replacement = "")
tmp <- tmp[nchar(tmp) >= 3]
cat(tmp)


str_split(str_squish((str_replace_all(tmp, regex("\\W+"), " "))), " ")

cat(tmp$V1)
###########################
library(reticulate)
sys <- import("sys")
sys$path
import("rpytools")
use_condaenv("r-reticulate")
reticulate::conda_create("r-reticulate")
# py_install("spacy")

spacy <- import("spacy")
nlp <- spacy$load(name = "en_core_web_md")
# nlp = spacy.load("en_core_web_md")  # make sure to use larger package!
doc1 = spacy$nlp("I like salty fries and hamburgers.")
doc2 = spacy$nlp("Fast food tastes very good.")
doc1$similarity(doc2)
doc1 = nlp("baby")
doc2 = nlp("child")
doc1$similarity(doc2)





library(igraph)
g <- igraph::random.graph.game(n = 10, p.or.m = .9)
plot(g)
g <- igraph::random.graph.game(n = 10, p.or.m = .2)
plot(g)
