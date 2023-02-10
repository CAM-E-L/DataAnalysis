# ==============================================================================
# R-Code - CAM
# date of creation: December 2021
# authors: Julius Fenn
# function: create_CAMfiles()
# ==============================================================================

############################################################################
#
#
############################################################################
## add Rating column
addRatingColum <- function(filesRater = NULL){

  for(i in 1:length(filesRater)){
    filesRater[[i]] <- add_column(filesRater[[i]], Rating = NA, .after = "Superordinate")

    vector_words <- unique(x = filesRater[[i]]$Superordinate)
    # vector_dummy_words <- paste0("word_", 1:length(unique(x = filesRater[[i]]$Superordinate)))

    for(j in 1:length(vector_words)){
      tmp_numbers <- c(1:nrow(filesRater[[i]]))[filesRater[[i]]$Superordinate %in% vector_words[j]]
      tmp_numbers<- str_pad(tmp_numbers, 4, pad = "0")
      tmp_numbers<- paste0("r", tmp_numbers, collapse = "")
      filesRater[[i]]$Rating[filesRater[[i]]$Superordinate %in% vector_words[j]] <-
        tmp_numbers
    }
  }

  return(filesRater)
}

## create ratings in the form
# > r0001r0002... (= indicating which groups of words have been summarized)
computeCohensKappa <- function(files = NULL, numberRaters = NULL){

  ## add Rating column
  files <- addRatingColum(filesRater = files)

  ## compute Cohens Kappa pairwise
  #> set up matrix
  cohensKappas <- matrix(data = NA, nrow = numberRaters,
                         ncol = numberRaters)
  #> compute
  for(r in 1:nrow(cohensKappas)){
    for(c in 1:ncol(cohensKappas)){
      if(c != r){
        tmp_kappa <- kappa2(ratings = cbind(files[[r]]$Rating,files[[c]]$Rating))
        cohensKappas[r,c] <- tmp_kappa$value
      }else{
        tmp_kappa <- kappa2(ratings = cbind(files[[r]]$Rating,files[[c]]$Rating))
        cohensKappas[r,c] <- tmp_kappa$value
      }
    }
  }

  return(cohensKappas)
}


## compute Reliability while maximizing overlapping words
# > search for the largest overlap when giving words
computeCohensKappaMaximized <- function(files = NULL, numberRaters = NULL){
  ## add Rating column
  files <- addRatingColum(filesRater = files)

  ## compute Cohens Kappa pairwise while maximizing the overlap

  #> set up matrix
  cohensKappasMaximized <- matrix(data = NA, nrow = numberRaters,
                                  ncol = numberRaters)
  #> compute
  for(i in 1:length(files)){
    # files[[i]] <- add_column(files[[i]], Rating_Word = NA, .after = "Rating")


    for(r in 1:nrow(cohensKappasMaximized)){
      for(c in 1:ncol(cohensKappasMaximized)){
        if(c != r){
          tmp_row <- unique(str_split(string = files[[r]]$Rating, pattern = "r", simplify = TRUE))
          tmp_row <- tmp_row[, colSums(x = tmp_row == "") != nrow(tmp_row)]

          tmp_column <- unique(str_split(string = files[[c]]$Rating, pattern = "r", simplify = TRUE))
          tmp_column <- tmp_column[, colSums(x = tmp_column == "") != nrow(tmp_column)]

          tmp_row[tmp_row==""] <- NA
          tmp_column[tmp_column==""] <- NA

          ##
          if(nrow(tmp_row) > nrow(tmp_column)){
            tmp_longer <- tmp_row
            tmp_smaller <- tmp_column
          }else{
            tmp_longer <- tmp_column
            tmp_smaller <- tmp_row
          }
          tmp_longer <- tmp_longer[order(rowSums(x = !is.na(tmp_longer)),
                                         decreasing = TRUE),]
          tmp_smaller <- tmp_smaller[order(rowSums(x = !is.na(tmp_smaller)),
                                           decreasing = TRUE),]


          tmpRaterA <- str_pad(c(1:nrow(files[[1]])), 4, pad = "0")
          tmpRaterB <- str_pad(c(1:nrow(files[[1]])), 4, pad = "0")
          tmp_sum <- integer(length = nrow(tmp_smaller))


          for(t in 1:nrow(tmp_longer)){
            # cat("t:", t, "\n")

            for(u in 1:nrow(tmp_smaller)){
              tmp_sum[u] <- sum(tmp_longer[t,!is.na(tmp_longer[t,])] %in% tmp_smaller[u,])
              # print(sum(tmp_sum[u]))
            }
            tmpRaterA[tmpRaterA %in% tmp_longer[t,]] <- paste0("word", t)
            tmpRaterB[tmpRaterB %in% tmp_smaller[tmp_sum == max(tmp_sum),]] <- paste0("word", t)
          }


          # cat("r:", r, "c:", c,"\n")
          # print(table(tmpRaterA, tmpRaterB))
          tmp_kappa <- kappa2(ratings = cbind(tmpRaterA, tmpRaterB))
          cohensKappasMaximized[r,c] <- tmp_kappa$value
          ##

        }else{
          tmp_kappa <- kappa2(ratings = cbind(files[[i]]$Rating,files[[i]]$Rating))
          cohensKappasMaximized[r,c] <- tmp_kappa$value
        }
      }
    }
  }
  return(cohensKappasMaximized)
}


getOverallRaterList <- function(files = NULL,
                                orderAlphabetically = TRUE,
                                raterNames = NULL){

  if(length(raterNames) != length(files)){
    print("Not matching length of provided rater names, using default")
    raterNames <- 1:length(files)
  }


  ## add Rating column
  files <- addRatingColum(filesRater = files)


  ## create wide ratering wordlist
  files_out <- files[[1]]

  ## add Rating column
  for(i in 1:length(files)){
    files_out[[paste0("Superordinate_", raterNames[i])]] <- files[[i]]$Superordinate
    files_out[[paste0("CommentRater_", raterNames[i])]] <- files[[i]]$Comment
    files_out[[paste0("Rating_", raterNames[i])]] <- files[[i]]$Rating

  }

  ## remove empty comments
  files_out <- files_out[, colSums(x = files_out == "") != nrow(files_out)]
  files_out$Superordinate <- NULL
  files_out$Comment <- NULL

  if(orderAlphabetically){
    files_out <- files_out[ , order(names(files_out))]
  }

  return(files_out)
}
