## see: https://rdrr.io/cran/qdapDictionaries/man/key.syn.html

library(qdap)
syn_dat <- qdapDictionaries::key.syn
head(syn_dat)




syn_dat_simplified <- data.frame(searched = syn_dat$x, synonyms=NA)

for(i in 1:nrow(syn_dat)){
  tmp_out <- syn_dat$y[stringr::str_detect(string = syn_dat$x,
                                           pattern = paste0("^", syn_dat$x[i], "$"))]
  tmp_out <- unlist(str_split(string = tmp_out, pattern = ","))
  tmp_out <- str_split(string = tmp_out, pattern = "@+", simplify = TRUE)
  tmp_out <- str_remove_all(string = tmp_out, pattern = "\\[[0-9]*\\]|\\\n|\\\"")
  tmp_out <- str_trim(string = tmp_out)
  tmp_out <- tmp_out[tmp_out != ""]
  tmp_out <- unique(tmp_out)
  tmp_out <- str_replace_all(string = tmp_out, pattern = "(?=\\()", replacement = " ")
  tmp_out <- str_squish(tmp_out)

  tmp_qdap <- qdap::syn(terms = syn_dat$x[i],
                        synonym.frame = qdapDictionaries::key.syn, return.list = FALSE)
  # tmp_qdap <- str_remove_all(string = tmp_qdap, pattern = " (?=\\()")

  if(all(sort(tmp_out) == sort(tmp_qdap))){
    syn_dat_simplified$synonyms[i] <- paste0(tmp_out, collapse = "++")
  }else{
    if(! i %in% c(3138, 9178)){
      print(i)
      break
    }
  }
}

sort(tmp_out)[sort(tmp_out) != sort(tmp_qdap)]
cbind(sort(tmp_out), sort(tmp_qdap))



xlsx::write.xlsx2(x = syn_dat_simplified, file = "syn_dat_English.xlsx", row.names = FALSE)
