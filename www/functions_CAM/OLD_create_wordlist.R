# ==============================================================================
# R-Code - CAM
# date of creation: December 2021
# authors: Julius Fenn
# function: create_CAMfiles()
# ==============================================================================

############################################################################
# create_CAMfiles()
#
############################################################################
# args: ! higher order function
# dat_nodes = CAMfiles[[1]]
# dat_merged = CAMfiles[[3]]
# order = "frequency"
# comments = TRUE
create_wordlist <- function(dat_nodes = CAMfiles[[1]],
                            dat_merged = CAMfiles[[3]],
                            order = NULL,
                            comments = TRUE){
  # check
  # if(!(typeof(drawn_CAM) == "list" & class(drawn_CAM[[1]]) == "igraph")){
  #   cat("Your specified drawn_CAM argument is of type:", typeof(drawn_CAM), "\n")
  #   cat("and / or the first list entry is of class:",  class(drawn_CAM[[1]]), "\n")
  #   stop("> specify a list with igraph classes")
  # }

  # check order
  if(!order %in% c("alphabetic", "frequency")){
    cat("Your specified order argument is:", order, "\n")
    stop("> Use \"alphabetic\" for alphabetic order of wordlist, or \"frequency\" for sorting according to frequency")
  }

  if("text_summarized" %in% colnames(dat_nodes)){
    dat_nodes$text <-   dat_nodes$text_summarized
  }

  ## tidy up blocks_dat
  dat_nodes$text <- str_replace_all(string=dat_nodes$text, pattern='_|-|:|,|!|;|\\"|\\*|&|\\?|>|<|=', repl="")
  # dat_nodes$text <- str_replace_all(string=dat_nodes$text, pattern=" ", repl="")
  dat_nodes$text <- str_replace_all(string=dat_nodes$text, pattern="[[:digit:]]", repl="")
  # dat_nodes$text <- tolower(x = dat_nodes$text)

  ## particularity qdap::wfdf(tmp_title)
  dat_nodes$text <- str_remove_all(string = dat_nodes$text, pattern = "[.]+")
  dat_nodes$text <- str_remove_all(string = dat_nodes$text, pattern = "[(]+")
  dat_nodes$text <- str_remove_all(string = dat_nodes$text, pattern = "[)]+")
  dat_nodes$text <- str_remove_all(string = dat_nodes$text, pattern = "[/]+")

  ## create WFT
  # freq_terms <- qdap::wfdf(dat_nodes$text)
  # head(freq_terms)
  # dim(freq_terms)

  freq_terms = dat_nodes %>% select(text) %>% count(text)
  colnames(freq_terms) <- c("Words", "all")
  freq_terms <- as.data.frame(freq_terms)

  if(order == "frequency"){
    freq_terms <- freq_terms[order(freq_terms$all, decreasing = TRUE),]
  } else if (order == "alphabetic"){
    freq_terms <- freq_terms[order(freq_terms$Words, decreasing = FALSE),]
  }

  freq_terms$percent <- round(x = freq_terms$all / length(unique(dat_nodes$CAM)) * 100, digits = 2)
  colnames(freq_terms) <- c("Words", "raw","percent")

  ## redraw CAMs
  drawn_CAM <- draw_CAM(dat_merged = dat_merged,
                        dat_nodes = dat_nodes,ids_CAMs = "all", plot_CAM = FALSE,
                        relvertexsize = 5,
                        reledgesize = 1)

  # rm(tmp_dat_out)
  ## degree total - directed
  for(j in 1:length(drawn_CAM)){
    # print(j)
    tmp_deg <- igraph::degree(graph = drawn_CAM[[j]], mode = "total")
    names(tmp_deg) <- V(drawn_CAM[[j]])$label

    if(j == 1){
      tmp_dat_out <-   cbind(names(tmp_deg), as.numeric(tmp_deg))
    } else{
      tmp_dat_out <- rbind(tmp_dat_out, cbind(names(tmp_deg), as.numeric(tmp_deg)))
    }
  }
  tmp_dat_out <- as.data.frame(tmp_dat_out)
  colnames(tmp_dat_out) <- c("label", "degree")
  tmp_dat_out$degree <- as.numeric(tmp_dat_out$degree)

  # valence
  freq_terms$mean_valence <- NA
  freq_terms$sd_valence <- NA

  # degree
  freq_terms$mean_degree <- NA
  freq_terms$sd_degree <- NA

  dat_nodes$value[dat_nodes$value == 10] <- 0



  ## if comments TRUE
  # ! limited to 100 comments
  if(comments){
    for(v in paste0("comment_", 1:100)){
      freq_terms[[v]] <- NA
    }
  }


  for(i in 1:length(freq_terms$Words)){
    tmp <- dat_nodes$value[dat_nodes$text == freq_terms$Words[i]]
    ## valence mean / sd
    freq_terms$mean_valence[freq_terms$Words == freq_terms$Words[i]] <- mean(tmp)
    freq_terms$sd_valence[freq_terms$Words == freq_terms$Words[i]] <- sd(tmp)

    ## degree mean / sd
    freq_terms$mean_degree[freq_terms$Words == freq_terms$Words[i]] <-
      mean(tmp_dat_out$degree[tmp_dat_out$label == freq_terms$Words[i]])
    freq_terms$sd_degree[freq_terms$Words == freq_terms$Words[i]] <-
      sd(tmp_dat_out$degree[tmp_dat_out$label == freq_terms$Words[i]])



    ## if comments TRUE
    if(comments){
    tmp_comments <- dat_nodes$comment[dat_nodes$text == freq_terms$Words[i]]
    if(any(tmp_comments != "") &&
       any(!is.na(tmp_comments)) &&
       !identical(tmp_comments, character(0))){
      tmp_comments <- tmp_comments[tmp_comments != ""]
      tmp_comments <- tmp_comments[!is.na(tmp_comments)]

      for(c in 1:length(tmp_comments)){
        freq_terms[i, paste0("comment_", c)] <-
          tmp_comments[c]
        if(c >= 100){
          cat("ERROR: so many comments are not supported by the current function")
        }
      }
    }
    }
  }


  freq_terms <- freq_terms[, colSums(is.na(freq_terms)) != nrow(freq_terms)]

  freq_terms[is.na(freq_terms)] <- ""
  freq_terms$mean_valence <- as.numeric(freq_terms$mean_valence)
  freq_terms$sd_valence <- as.numeric(freq_terms$sd_valence)
  freq_terms$mean_degree <- as.numeric(freq_terms$mean_degree)
  freq_terms$sd_degree <- as.numeric(freq_terms$sd_degree)


  return(freq_terms)
}
