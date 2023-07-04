    getlabels_appMatch <- function(typeLabels = NULL, getInput = NULL, counter = NULL, skipCond = NULL, 
    dataSummarized = NULL){

      counter <-counter + 1

      print("Counter labels bucketlist:")
      print(counter)

      ## avoid error
      if (counter <= max(getInput$num)) {
        labels_out <-
          getInput[getInput$num == counter, ]

        labels_list <- list()

        for (i in 1:nrow(labels_out)) {
          tmp_dat <-
            dataSummarized[[1]][dataSummarized[[1]]$text_summarized ==
                                             labels_out$word[i], ]

          if(typeLabels == "positive"){
          tmp_value <- tmp_dat$value[tmp_dat$value > 0 &
                            tmp_dat$value < 10]
          }else if(typeLabels == "negative"){
            tmp_value <- tmp_dat$value[tmp_dat$value < 0]
          }else if(typeLabels == "neutral"){
            tmp_value <- tmp_dat$value[tmp_dat$value == 0]
          }else if(typeLabels == "ambivalent"){
            tmp_value <- tmp_dat$value[tmp_dat$value == 10]
          }

          if (length(tmp_value) != 0) {
            ## compute N
            tmp_N <- length(tmp_value)
            ## compute mean
            tmp_mean <-
              round(x = mean(tmp_value), digits = 2)
            ## compute SD
            tmp_SD <-
              round(x = sd(tmp_value), digits = 2)

            ## remove X_positive, ...
            tmp_labelout <-
              str_remove_all(string = labels_out$word[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            if(tmp_N > 1) {
              labels_list[[labels_out$word[i]]] <-
                htmltools::tags$div(paste0(
                  tmp_labelout,
                  "   (N=",
                  tmp_N,
                  ", M=",
                  tmp_mean,
                  ", SD=",
                  tmp_SD,
                  ")"
                ))
            } else {
              labels_list[[labels_out$word[i]]] <-
                htmltools::tags$div(paste0(
                  tmp_labelout,
                  "   (N=",
                  tmp_N,
                  ", M=",
                  tmp_mean,
                  ")"
                ))
            }
          }
        }

        ## reset skip condition
        skipCond <- FALSE

        
        return(list(labels_list, counter, skipCond))
      } else {
        print("maximum reached")
        counter <- counter - 1
        return(list(NULL, counter, skipCond))
      }
    }

###################################
      getlabels_Search <- function(typeLabels = NULL, getInput = NULL, dataSummarized = NULL){

        ## remove suffix
        tmp_text_summarized <-
          str_remove(string = dataSummarized[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")


        labels_out <- getInput
        labels_list <- list()
        for (i in 1:length(labels_out)) {
          tmp_dat <-
            dataSummarized[[1]][tmp_text_summarized == labels_out[i], ]
          
          
          if(typeLabels == "positive"){
            tmp_value <- tmp_dat$value[tmp_dat$value > 0 &
                                         tmp_dat$value < 10]
          }else if(typeLabels == "negative"){
            tmp_value <- tmp_dat$value[tmp_dat$value < 0]
          }else if(typeLabels == "neutral"){
            tmp_value <- tmp_dat$value[tmp_dat$value == 0]
          }else if(typeLabels == "ambivalent"){
            tmp_value <- tmp_dat$value[tmp_dat$value == 10]
          }
          
          
          if (length(tmp_value) != 0) {
            ## compute N
            tmp_N <- length(tmp_value)
            ## compute mean
            tmp_mean <-
              round(x = mean(tmp_value), digits = 2)
            ## compute SD
            tmp_SD <-
              round(x = sd(tmp_value), digits = 2)
            
            ## remove X_positive, ...
            tmp_labelout <-
              str_remove_all(string = labels_out[i], pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
            labels_list[[labels_out[i]]] <-
              htmltools::tags$div(paste0(
                tmp_labelout,
                "   (N=",
                tmp_N,
                ", M=",
                tmp_mean,
                ", SD=",
                tmp_SD,
                ")"
              ))
          }
        }
          
        return(labels_list)
      }

###################################
getlabels_Synonyms <- function(typeLabels = NULL, getInput = NULL, counter = NULL, skipCond = NULL, 
                               dataSummarized = NULL){
  
  counter <-counter + 1
  
  print("Counter labels bucketlist:")
  print(counter)
  
  
  ## get text variable
  tmp_text <- str_remove_all(string = dataSummarized[[1]]$text_summarized, 
                             pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
  tmp_text <- tolower(x = tmp_text)
  
  
  ## avoid error
  if (counter <= length(getInput)) {
    labels_out <-
      getInput[[counter]]
    
    
    labels_list <- list()
    
    for (i in 1:length(labels_out)) {
      tmp_dat <-
        dataSummarized[[1]][tmp_text ==
                              labels_out[i], ]
      
      
      
      for(w in unique(tmp_dat$text_summarized)){
        tmp_dat_w <- tmp_dat[tmp_dat$text_summarized == w, ]
        
        if(typeLabels == "positive"){
          tmp_value <- tmp_dat_w$value[tmp_dat_w$value > 0 &
                                       tmp_dat_w$value < 10]
        }else if(typeLabels == "negative"){
          tmp_value <- tmp_dat_w$value[tmp_dat_w$value < 0]
        }else if(typeLabels == "neutral"){
          tmp_value <- tmp_dat_w$value[tmp_dat_w$value == 0]
        }else if(typeLabels == "ambivalent"){
          tmp_value <- tmp_dat_w$value[tmp_dat_w$value == 10]
        }
        
        
        
        ## ignore zero value (if words are too similar)
        if (length(tmp_value) != 0) {
          ## compute N
          tmp_N <- length(tmp_value)
          ## compute mean
          tmp_mean <-
            round(x = mean(tmp_value), digits = 2)
          ## compute SD
          tmp_SD <-
            round(x = sd(tmp_value), digits = 2)
          
          tmp_word <- str_remove_all(string = w, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
          labels_list[[tmp_word]] <-
            htmltools::tags$div(paste0(
              tmp_word,
              "   (N=",
              tmp_N,
              ", M=",
              tmp_mean,
              ", SD=",
              tmp_SD,
              ")"
            ))
        }
      }
    }
    
    ## reset skip condition
    skipCond <- FALSE
    
    return(list(labels_list, counter, skipCond))
  } else {
    print("maximum reached")
    counter <- counter - 1
    return(list(NULL, counter, skipCond))
  }
}