# ==============================================================================
# R-Code - CAM
# date of creation: December, January 2021
# authors: Julius Fenn
# function: draw_CAM()
# ==============================================================================

############################################################################
# checkSpelling_words()
#
############################################################################
### args:
# words_split = nodes_split
# num = 9
# dictionary = dict_german
checkSpelling_words <- function(words_split, num, dictionary, string_dis = 2, verbose = FALSE, language = NULL){
  ## prepare data:
  tmp_text <- words_split[num,][words_split[num,] != ""]

  ## set stopwords: 
  if(language == "English"){
    tmp_stopwords <- stopwords::stopwords("en", source = "snowball")
  }else if(language == "German"){
        tmp_stopwords <- stopwords::stopwords("de", source = "snowball")

  }

  if(verbose){
    cat("\n", num, "word:", tmp_text, "\n")
  }

  ## if number of words == 1
  if(length(tmp_text) == 1){
    tmp_optimalmatching <- stringdist(tmp_text, dictionary, method = "osa")

    if(any(tmp_optimalmatching == 0)){
      tmp_match <- sort(dictionary[tmp_optimalmatching == 0])
      tmp_match
    }else{
      tmp_match <- sort(dictionary[tmp_optimalmatching <= string_dis])
      tmp_match
    }

    if(!identical(tmp_match, character(0)) && length(tmp_match) <= 5){
      if(verbose){
        cat("l==1: ", tmp_match)
      }
      return(as.list(tmp_match))
    }else{
      if(verbose){
        cat("no match for", tmp_text, "\n")
      }
       return(as.list(NA))
    }

  }else if(length(tmp_text) > 1){
    ## if number of words > 1 && <= 3
    tmp_out <- list()
    for(l in 1:length(tmp_text)){

      if(any(tmp_text[l] %in% tmp_stopwords)){
        if(verbose){
          cat("found stopwords: ", tmp_text[l], "\n")
        }
        tmp_out[[l]] <- -99
      }else{
        tmp_optimalmatching <- stringdist(tmp_text[l], dictionary, method = "osa")

        if(any(tmp_optimalmatching == 0)){
          tmp_match <- sort(dictionary[tmp_optimalmatching == 0])
        }else{
          tmp_match <- sort(dictionary[tmp_optimalmatching <= string_dis])
        }

        if(!identical(tmp_match, character(0)) && length(tmp_match) <= 5){
          if(verbose){
            cat("\n", "l>1: ", tmp_match, "\n")
          }
          tmp_out[[l]] <- tmp_match
        }else{
          if(verbose){
            cat("no match for", tmp_text[l], "\n")
          }
          tmp_out[[l]] <- NA
        }
      }
    }
    return(tmp_out)
  }else{
    ## if number of words >= 4
    if(verbose){
      cat("no match for", tmp_text[l], "-> ! longer than 3 words\n")
    }
    return(as.list(NA))
  }
}
