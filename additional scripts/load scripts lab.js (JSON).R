# dataset = dat1_pre
# listvars = ques_mixed_dat1_pre
# notNumeric = vec_notNumeric_dat1_pre
verbose=TRUE
questionnairetype <- function(dataset,
                              listvars = ques_mixed,
                              notNumeric = vec_notNumeric,
                              verbose=FALSE){

  datasetques <- data.frame(ID = unique(dataset$ID))

  for(c in 1:length(listvars)){
    if(verbose){
      print(c)
    }

    if(any(colnames(dataset) == listvars[c])){
      if(verbose){
        print(listvars[c])
      }

      ## tmp IDs
      tmpid <- dataset$ID[!is.na(dataset[, listvars[c]])]
      ## tmp value variable
      tmpvalue <- dataset[, listvars[c]][!is.na(dataset[, listvars[c]])]
      datasetques[listvars[c]]  <- NA

      if(listvars[c] %in% notNumeric){
        datasetques[datasetques$ID %in% tmpid, listvars[c]] <- tmpvalue
      }else if(is.list(tmpvalue)){
        tmpvalue_tmp <- unique(tmpvalue)
        tmpvalue <- c()
        counter = 1
        for(i in 1:length(tmpvalue_tmp)){
          if(!is.null(tmpvalue_tmp[[i]])){
            tmpvalue[counter] <- paste0(tmpvalue_tmp[[i]], collapse = " - ")
            counter = counter + 1
          }
        }
        datasetques[datasetques$ID %in% tmpid, listvars[c]] <- tmpvalue
      }else{
        datasetques[datasetques$ID %in% tmpid, listvars[c]] <- as.numeric(tmpvalue)
      }
    }
  }
  return(datasetques)
}


#####################


setwd("Covid19_perception")
getwd()
dir()

# > pre study
suppressMessages(read_file('preCAM.txt') %>%
                   # ... split it into lines ...
                   str_split('\n') %>% first() %>%
                   # ... filter empty rows ...
                   discard(function(x) x == '') %>%
                   # ... parse JSON into a data.frame
                   map_dfr(fromJSON, flatten=TRUE)) -> dat1_pre
# > post study
suppressMessages(read_file('postCAM.txt') %>%
                   # ... split it into lines ...
                   str_split('\n') %>% first() %>%
                   # ... filter empty rows ...
                   discard(function(x) x == '') %>%
                   # ... parse JSON into a data.frame
                   map_dfr(fromJSON, flatten=TRUE)) -> dat2_post


## create counter variable (lab.js specific)
dat1_pre$ID <- NA
tmp_IDcounter <- 0
for(i in 1:nrow(dat1_pre)){
  if(!is.na(dat1_pre$sender[i]) && dat1_pre$sender[i] == "Greetings"){
    # tmp <- dat1_pre$prolific_pid[i]
    tmp_IDcounter = tmp_IDcounter + 1
  }
  dat1_pre$ID[i] <- tmp_IDcounter
}

## create counter variable (lab.js specific)
dat2_post$ID <- NA
tmp_IDcounter <- 0
for(i in 1:nrow(dat2_post)){
  if(!is.na(dat2_post$sender[i]) && dat2_post$sender[i] == "CAMfeedbackGeneral"){
    # tmp <- dat2_post$prolific_pid[i]
    tmp_IDcounter = tmp_IDcounter + 1
  }
  dat2_post$ID[i] <- tmp_IDcounter
}



### PRE
colnames(dat1_pre)
ques_mixed_dat1_pre <- c("PROLIFIC_PID",
                         str_subset(string = colnames(dat1_pre), pattern = "^R"),
                         str_subset(string = colnames(dat1_pre), pattern = "^affImgAffect"))

vec_notNumeric_dat1_pre = c("PROLIFIC_PID",
                            str_subset(string = colnames(dat1_pre), pattern = "^R"))

questionnaire_pre <- questionnairetype(dataset = dat1_pre,
                                        listvars = ques_mixed_dat1_pre,
                                        notNumeric = vec_notNumeric_dat1_pre)

### POST
colnames(dat2_post)
ques_mixed_dat2_post <- c(str_subset(string = colnames(dat2_post), pattern = "^feedCAM"),
                         "ans1", "feedback_critic")

vec_notNumeric_dat2_post = c(str_subset(string = colnames(dat2_post), pattern = "^feedCAM"),
                            "ans1", "feedback_critic")

questionnaire_post <- questionnairetype(dataset = dat2_post,
                                       listvars = ques_mixed_dat2_post,
                                       notNumeric = vec_notNumeric_dat2_post)
questionnaire_post <- questionnaire_post[-1,]


dim(questionnaire_pre); colnames(questionnaire_pre)
dim(questionnaire_post); colnames(questionnaire_post)


########################
dir()
networkIndicators <- xlsx::read.xlsx2(file = "networkIndicators.xlsx", sheetIndex = 1)
dim(networkIndicators); colnames(networkIndicators)



dat <- cbind(questionnaire_pre, questionnaire_post, networkIndicators)



cbind(dat$CAM_ID, dat$mean_valence_macro, dat$ans1, dat$feedback_critic)
