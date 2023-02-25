
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
