setwd("C:/DATEN/PHD/CAMtools_CAMapp/workingON")


library(reticulate)
# use_condaenv("r-reticulate")
# py_install("spacy")
library(shiny)
# plt <- import("matplotlib.pyplot")
# rpytools <- import("rpytools")
spacy <- import("spacy")
nlp <- spacy$load(name = "en_core_web_md")
# nlp = spacy.load("en_core_web_md")  # make sure to use larger package!
doc1 = nlp("I like salty fries and hamburgers.")
doc2 = nlp("Fast food tastes very good.")
doc1$similarity(doc2)
doc1 = nlp("baby")
doc2 = nlp("child")
doc1$similarity(doc2)


# Similarity of two documents
print(doc1, "<->", doc2, doc1$similarity(doc2))
# Similarity of tokens and spans
french_fries = doc1[2:4]
burgers = doc1[5]
print(french_fries, "<->", burgers, french_fries.similarity(burgers))
nlp = spacy.load("en_core_web_md")
tokens = nlp("dog cat banana afskfsd")

# for token in tokens:
#   print(token.text, token.has_vector, token.vector_norm, token.is_oov)





ui <- fluidPage(
  plotOutput(outputId = "plot01")
)

server <- function(input, output){

  # Import module
  plt <- import("matplotlib.pyplot")

  output$plot01 <- renderPlot({
    plt$plot(1:10)
    plt$show()
  })
}

# shinyApp(ui, server)
