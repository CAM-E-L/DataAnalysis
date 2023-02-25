    getlabels <- function(typeLabels = NULL, getInput = NULL, counter = NULL, skipCond = NULL, 
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
