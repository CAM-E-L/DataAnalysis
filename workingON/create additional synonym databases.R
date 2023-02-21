library(qdap)
singleWords <- c("skjdkfdsj", "the", "cat", "job", "environment", "read", "teach", "aback")
singleWords_found <- singleWords[singleWords %in% qdapDictionaries::key.syn$x]
singleWords_found
outPercent <- round(x = length(singleWords_found) / length(singleWords) * 100, digits = 2)
outPercent


qdap::syn(terms = w, return.list = FALSE)

### create synonyms
# syn::syns()
for(w in singleWords_found){
cat("\nfor word -->", w, "<-- following synonyms were found:\n")
 print( qdap::syn(terms = w, return.list = FALSE))
}

?qdapDictionaries::key.syn
qdapDictionaries::key.syn[1,]
