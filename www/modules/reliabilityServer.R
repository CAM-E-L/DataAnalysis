
reliabilityServer <-
  function(id, dataCAM, drawnCAM, parent, globals) {
    moduleServer(id, function(input, output, session) {
      ns <- NS(id)
      
      ## reactive values
      outUI <- reactiveValues(elements = NULL)
      
      
      ################
      # default text + set output
      ################
      ## default text
      outUI$elements <- tagList(tags$div(
        h1("Reliability module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(
          tags$li(
            HTML(
              '<b>Train Reliability:</b> Here you can create a random sub-word list (e.g. 10% of all unique drawn concepts), which can be summarized by (independet) raters.'
            )
          ),
          tags$li(
            HTML(
              '<b>Get Reliability:</b> After the raters summarized all concepts to superordinate words, reliability coefficients can be computed in this reliability submodule. '
            )
          ),
          tags$li(
            HTML(
              '<b>Information:</b> Further information regarding this module.'
            )
          )
        )
      ))
      
      ## set output
      output$reliability_out <- renderUI({
        outUI$elements
      })
      
      


      ################################
      # single module options
      ################################


      ###############################
      ###############################
      ###############################

      ###### get wordlists for raters ######
      #> UI
      observeEvent(input$reliability_train, {
  ## change UI
        outUI$elements <- tagList(
          tags$h2("Create word lists of your concepts for your raters"),
          tags$br(),
          tags$div(
            HTML(
              "Here you can create word lists of your concepts, which can be summarized by (independet) raters. It is important, that
              you, in accordance with sending them the wordlist, inform your raters how to summarize the respective word list, possible text:"
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$div(
            HTML(
              "Please read the following text carefully before you start summarizing the concepts:
              <br>
              <b>Goal</b>: The purpose of the rating process is to train you as raters to summarize the given wordlist into superordinate
              or thematic terms as best as possible. This process is so important, because based on your reliable process of summarizing
              the written concepts, these concepts will be partially or fully summarized (aggregated) later on.
              <br>
              <br>
              <b>How to</b>: To summarize the concepts, please do the following: In the A (Words) you will see the concepts, which have the
              participants written in our study (raw data). Your goal is now to summarize these concepts into superordinate or
              thematic terms, which you have to write in column B (Superordinate). Please, based on your knowledge of the overall study, use as
              few superordinate concepts as possible. In general you should <u>not summarize the predefined concepts</u>.
              <br>
              <br>
              <b>Objectivity</b>: During the process it is of central importance that you are consistent and reliable in your ratings.
              If you facing any problems or you have open questions regarding single concepts, please write them down in the respective
              comment column C. Please do <u>not talk to other raters</u> during this process, because the independence of the raters
              is a crucial assumption. It is important that you remain objective and impartial.
              <br>
              <br>
              <b>Training</b>: After you have summarized the concepts you will get feedback and in group discussions, there will be
              opportunities for training and improvement where necessary. Conflicting concepts are discussed together."
            )
          , style="border: 2px solid black; padding: 10px;"),
          tags$br(),

          tags$h3("Create word lists of your concepts"),
          tags$h4("Settings:"),
          tags$div(
            HTML(
              "Please select how many percent of your overall wordlist your raters should summarize (the number of words, which have
              to be summarized are shown, when you moving the slider):"
            ),
            style = "font-size:14px"
          ),
                  sliderInput(ns("a_perctWordlist"), "Percentage of words to summarize (1-100):",
                  min = 1, max = 100,
                  value = 10, width = "50%"),

          tags$p(
              "Your raters need to summarize ",
              tags$b(textOutput(ns(
                "a_wordsToSummarize"
              ), inline = TRUE)),
              " unique concepts."
            ),
                      tags$br(),
         tags$div(
            HTML(
              "Please select the ordering of the word list, if words should be split by given valence and if comments should be visible 
              for your raters (recommended to keep default settings):"
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify order
                       tags$div(HTML("Please specify the order of your wordlist:"), style="font-size:14px"),
                   radioButtons(ns("a_Wordlist_Order"), label = NULL, c("alphabetic", "frequency"), selected = "alphabetic"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify split valence
		                     tags$div(HTML("Do you want to split your summarized words by the valence (X_positive, X_negative, ...)?"), style="font-size:14px"),
                   radioButtons(ns("a_Wordlist_Split"), label = NULL, c("yes", "no"), selected = "no"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # specify including comments
		                     tags$div(HTML("Do you want to include comments in your wordlist?"), style="font-size:14px"),
                   radioButtons(ns("a_Wordlist_Comments"), label = NULL, c("yes", "no"), selected = "yes"),
            ),
          ),
   tags$br(),
            actionButton(
              ns("a_createWordlistOut"),
              "create wordlist",
              style = "display: inline-block;"
            ),
                                tags$div(
            HTML(
              "<i>Remark: to download the wordlist for your raters please click on the global download button (top right) after 
              you have clicked on create wordlist.</i>"
            ),
            style = "font-size:14px"
          ),
                      tags$br(),
          tags$h4("Wordlist for raters to summarize:"),
          dataTableOutput(ns("a_wordlistTable")),
        )
      })

   #> Server
   ## change slider
   proportionWords <-
  eventReactive(c(
    input$a_perctWordlist,
        input$reliability_train ## avoid interference with summary functions
  ),{
          tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
     
          tmp_text <- unique(x = tmp_text)
          tmp_proportion <- ceiling(x = length(tmp_text) * input$a_perctWordlist / 100)

          tmp_proportion
    })

      output$a_wordsToSummarize <- renderText({
        req(drawnCAM())
      proportionWords()
      })

      
## create wordlist
wordlist <- eventReactive(input$a_createWordlistOut, {
          req(drawnCAM())
  ## define settings: 
  # > split by valence
  if(input$a_Wordlist_Split == "no"){
    tmp_splitByValence = FALSE
  }else{
    tmp_splitByValence = TRUE
  }
  # > include comments
  if(input$a_Wordlist_Comments == "no"){
    tmp_includeComments = FALSE
  }else{
    tmp_includeComments = TRUE
  }
  
  
  # > consider split by valence
  if(tmp_splitByValence){
    tmp_text <- globals$dataCAMsummarized[[1]]$text_summarized
  }else{
    tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, 
                               pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
  }
  
  tmp_text <- unique(x = tmp_text)
  wordsOut <- sample(x = tmp_text, size = proportionWords(), replace = FALSE)
  
  # print(wordsOut)
  
  
  CAMwordlist <- create_wordlist(
    dat_nodes = globals$dataCAMsummarized[[1]],
    dat_merged = globals$dataCAMsummarized[[3]],
    order = input$a_Wordlist_Order,
    splitByValence = tmp_splitByValence,
    comments = tmp_includeComments,
    raterSubsetWords = wordsOut,
    rater = TRUE
  )



  
  # print(head(CAMwordlist))
  # CAMwordlist <- CAMwordlist[CAMwordlist$Words %in% wordsOut, ]
  # print(head(CAMwordlist))
  
  #> change condition
  globals$condition <-
    c(globals$condition, "wordlistRatersCreated")
  #> save as global
  globals$wordlistRaters <- CAMwordlist
  
  CAMwordlist
})

output$a_wordlistTable <- renderDataTable({
  wordlist()
})





      ###############################
      ###############################
      ###############################

      ###### get inter-rater coefficients ######
      #> UI
      observeEvent(input$reliability_get, {
        ## change UI
        outUI$elements <- tagList(
          tags$h2("Compute reliability coefficients of your raters"),
          tags$br(),
          tags$div(
            HTML(
              "Based on multiple summarized wordlists (at least 2) you can compute two reliability coefficients."
            ),
            style = "font-size:14px"
          ),
          tags$br(),

          tags$h3("Compute reliability coefficients"),
          tags$h4("Upload:"),
          tags$div(
            HTML(
              "Please upload all your wordlists you have received by your raters (at least 2). It is important to note that every concepts
              need to be summarized - no missings in column B (Superordinate):"
            ),
            style = "font-size:14px"
          ),
      fileInput(
          ns("b_upload"),
          NULL,
          accept = c(".xlsx"),
          placeholder = "upload wordlists from raters",
          multiple = TRUE, width = "30%"
        ),
        tags$div(
          HTML(
            "Please wait a few seconds until the data is processed. You have uploaded the
                              following wordlist(s) - maximum four file names are shown:"
          )
        ),
        tableOutput(ns("b_file")),
          tags$br(),
          tags$h4("Output reliability coefficients:"),
                                        tags$div(
            HTML(
              "<i>Remark: to download a overall wordlist, which combines all the single wordlists of your 
              raters, please click on the global download button (top right) after computing the reliability coefficients.</i>"
            ),
            style = "font-size:14px"
          ),
          tags$br(),
          tags$h5("Cohens Kappa (Light's Kappa):"),
          tags$div(
            HTML(
              "Cohens Kappa is pairwise computed by assuming a perfect match of overlapping group of words, which have been 
              summarized (r0001r0002... is indicating which groups of words have been summarized):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_CohensKappaMatrix")),
                    tags$div(
            HTML(
              "Cohens Kappa is pairwise computed by maximizing overlapping words, which have been 
              summarized (search for the largest overlap when allocating words for every group of raters):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_CohensKappaMatrixMaximized")),
                              tags$div(
            HTML(
              "Summary statistics of Cohens Kappa by (a) assuming a perfect match of overlapping group of words and 
              (b) maximizing overlapping words:"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_CohensKappaSummary")),
          tags$br(),
          tags$h5("Fleiss Kappa:"),
                                        tags$div(
            HTML(
              "Fleiss Kappa for different groups of overlapping words (plus computing category-wise Kappa):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_FleissKappaOverlapping")),
                                                  tags$div(
            HTML(
              "> for different word combinations given superordinate words (sorted by highest frequency):"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_FleissKappaOverlapping_Descriptives")),
          tags$br(),
                                                  tags$div(
            HTML(
              "Fleiss Kappa for given superordinate words:"
            ),
            style = "font-size:14px"
          ),
          verbatimTextOutput(ns("b_FleissKappaSuperordinate")),
          tags$h5("Overall rater list:"),
          dataTableOutput(ns("b_wordlistTableOverall")),
        )
      })

   #> Server
### get the extensions and show file names
ext <- reactive({
  # wait for upload
  req(input$b_upload)
  
  # show uploaded file(s)
  output$b_file <- renderTable({
    if (nrow(input$b_upload) > 4) {
      rbind(input$b_upload[1:4,], c("...", "...", "...", "..."))
    } else {
      input$b_upload
    }
  })
  
  # extract file ending
  tools::file_ext(input$b_upload$name)
})


### create data sets of wordlists
data <- reactive({
  # wait for upload
  req(ext())
  message("uploaded file(s) - extensions:", ext())
  
  if (all(stringr::str_detect(string = ext(), pattern = "xlsx"))) {
    ## check: at least 2 datasets
    if (length(ext()) < 2) {
      showModal(
        modalDialog(
          title = "Wrong number of datasets",
          paste0(
            "Please upload at least two wordlists (.xlsx files)"
          ),
          easyClose = TRUE,
          footer = tagList(modalButton("Ok"))
        )
      )
      return(NULL)
    } else {
      ## go on !!!
      
      # Words	Superordinate Comment	raw	percent	mean_valence	sd_valence	mean_degree	sd_degree
      files <- list()
      for (i in 1:length(input$b_upload[, 1])) {
        files[[i]] <-
          xlsx::read.xlsx2(file = input$b_upload[[i, "datapath"]],
                           sheetIndex = 1)
      }
      
      return(files)
      
    }
  } else {
    showModal(
      modalDialog(
        title = "Wrong type of dataset(s)",
        paste0(
          "Please upload a only .xlsx files (Excel)"
        ),
        easyClose = TRUE,
        footer = tagList(modalButton("Ok"))
      )
    )
    return(NULL)
  }
  
})


observeEvent(input$b_upload, {
  req(ext())
  req(data())
  
  tmp_namesRater <- input$b_upload[, 1]
  tmp_namesRater <- str_remove(string = tmp_namesRater, pattern = ".xlsx$")
  
  ## Cohens Kappa pairwise
  cohensKappas <- computeCohensKappa(files = data(), numberRaters = length(data()))
  rownames(x = cohensKappas) <- tmp_namesRater
  colnames(x = cohensKappas) <- tmp_namesRater
  
  output$b_CohensKappaMatrix <- renderPrint({
    round(x = cohensKappas, digits = 2)
  })  
  ## Cohens Kappa pairwise maximized
  cohensKappasMaximized <- computeCohensKappaMaximized(files = data(), numberRaters = length(data()))
  rownames(x = cohensKappasMaximized) <- tmp_namesRater
  colnames(x = cohensKappasMaximized) <- tmp_namesRater
  
  output$b_CohensKappaMatrixMaximized <- renderPrint({
    round(x = cohensKappasMaximized, digits = 2)
  }) 
  
  output$b_CohensKappaSummary <- renderPrint({
    out_summaryCohensKappa <- list(
      mean(cohensKappas[lower.tri(x = cohensKappas, diag = FALSE)]),
      mean(cohensKappasMaximized[lower.tri(x = cohensKappasMaximized, diag = FALSE)])
    )
    names(out_summaryCohensKappa) <- c("a", "b")
    out_summaryCohensKappa
  }) 
  
  
  ovallRaterList <- getOverallRaterList(files = data(),
                                        orderAlphabetically = TRUE,
                                        raterNames = tmp_namesRater)
  
  #> change condition
  globals$condition <- c(globals$condition, "wordlistOverallRated")
  #> save as global
  globals$wordlistOverallRated <- ovallRaterList
  
  ## output overall table
  output$b_wordlistTableOverall <- renderDataTable({
    ovallRaterList
  })
  
  ## Fleiss Kappa for different groups of overlapping words
  output$b_FleissKappaOverlapping <- renderPrint({
    tmp <- ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                       pattern = "Rating_")]
    kappam.fleiss(ratings = tmp, detail=TRUE)
  })
  
  
  # > for different word combinations given superordinate words (sorted by highest frequency)
  output$b_FleissKappaOverlapping_Descriptives <- renderPrint({
    ## table of word counts sorted
    number_wordCom <- sort(table(unlist(ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                                                    pattern = "Rating_")])), decreasing = TRUE)
    
    ## wide to long data
    tmp_Ratings <- ovallRaterList[, c("Words", str_subset(string = colnames(ovallRaterList),
                                                          pattern = "Rating_"))]
    tmp_Ratings <- tmp_Ratings %>%
      gather(variable, value, -Words)
    
    tmp_Superordinate <- ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                                     pattern = "Superordinate_")]
    tmp_Superordinate <- tmp_Superordinate %>%
      gather(variable, value)
    
    tmp_Ratings$Superordinate <- tmp_Superordinate$value
    tmp_Ratings$variable <- str_remove(string = tmp_Ratings$variable, pattern = "Rating_")
    colnames(tmp_Ratings) <- c("Words", "Raters", "Rating", "Superordinate")
    
    ## print output
    for(w in 1:length(number_wordCom)){
      tmp_data <- data.frame(Raters = tmp_Ratings$Raters[tmp_Ratings$Rating %in% names(number_wordCom)[w]],
                             Superordinates = tmp_Ratings$Superordinate[tmp_Ratings$Rating %in% names(number_wordCom)[w]],
                             Words = tmp_Ratings$Words[tmp_Ratings$Rating %in% names(number_wordCom)[w]])
      
      cat("\nword combination: ", names(number_wordCom)[w],
          "\n      , # of Raters:", length(unique(tmp_data$Raters)),
          ", # of Superordinates:", length(unique(tmp_data$Superordinate)),
          ", # of words:", number_wordCom[w], "\n")
      print(tmp_data)
    }
  })
  
  
  
  
  ## Fleiss Kappa for given superordinate words
  output$b_FleissKappaSuperordinate <- renderPrint({
    tmp <- ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                       pattern = "Superordinate")] 
    kappam.fleiss(ratings = tmp, detail=TRUE)
  })
  
})




      ############
      # information reliability
      ############
      #> UI
      observeEvent(input$reliability_information, {
        ## change UI
        outUI$elements <-
          tagList(tags$h2("Module Specific Information"),
                  tags$div(
                    HTML('The options for this module are the following:'),
        tags$ul(
                   tags$li(
            HTML(
              '<b>Train Reliability:</b> A random sub-word list (e.g. 10% of all unique drawn concepts) can be generated. 
              This Excel file can be downloaded and sent to the independent raters together. 
              It is important, that you, in accordance with sending them the wordlist, inform your raters how to summarize the 
              respective word list, for a possible instruction text see the module.' 
            )
          ),
          tags$li(
            HTML(
              '<b>Get Reliability:</b> After the raters summarized all concepts to superordinate words, reliability coefficients can be computed 
              in this reliability submodule. Please note that the reliability coefficients only depend on the assumption that the same groups of 
              words are summarized under one term each by different raters, but not that identical terms are used as superordinate categories by 
              the raters. Three possible reliability coefficients can be computed: (a) Cohen\'s Kappa is pairwise computed between all raters by 
              assuming a perfect match of overlapping groups of words or (b) Cohen\'s Kappa is pairwise computed by maximizing overlapping words, 
              which have been summarized and finally (c) Fleiss’ Kappa and category-wise Kappa for different groups of overlapping words is 
              computed. Additionally to these reliability statistics, summary statistics of the group of summarized words are given. '
            )
          ),
        )
                  ))
      })


    })
}

