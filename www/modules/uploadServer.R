uploadServer <- function(id, parent, globals) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    ## reactive values
    outUI <- reactiveValues(elements = NULL)
    v <- reactiveValues(boolContinue = TRUE, dataUploaded = "no", protocol = FALSE, df = NULL, dfClean = NULL)

    ################
    # default text + set output
    ################
    ## default text
    outUI$elements <- tagList(
      h1("Thank you for using our Shiny CAM application"),
      tags$div(
        HTML(
          'The application is designed to analyse CAM data from our newly developed
                C.A.M.E.L. (Cognitive-Affective Map <i>extended logic</i>) software (link to software: <a href="https://camgalaxy.github.io/" target="_blank">C.A.M.E.L.</a>, '
        ),
        a(
          actionButton(
            inputId = "githubCAMEL",
            label = "Github",
            icon = icon("github", lib = "font-awesome"),
            style = 'padding:3px; font-size:70%; margin-right: -5px; margin-left: 3px'
          ),
          href = "https://github.com/CAMgalaxy/CAMgalaxy.github.io",
          target = "_blank"
        ),
        HTML(
          '). It is also possible to analyse data from another CAM software called Valence (link to software: <a href="https://cam1.psychologie.uni-freiburg.de/" target="_blank">Valence</a>, '
        ),
        a(
          actionButton(
            inputId = "osfValence",
            label = "OSF",
            icon = icon("glyphicon glyphicon-new-window", lib = "glyphicon"),
            style = 'padding:3px; font-size:70%; margin-right: -5px; margin-left: 3px'
          ),
          href = "https://osf.io/9tza2/",
          target = "_blank"
        ),
        HTML(').')
      ),
      tags$br(),
      tags$br(),
      tags$div(
        HTML(
          'To start the CAM application please click on the module option "Upload" on the sidebar panel. The options for
                     this module are the following:'
        ),
        tags$ul(
          tags$li(
            HTML(
              '<b>Upload:</b> Upload your raw data (C.A.M.E.L. or Valence) and - if you already have - your protocol.'
            )
          ),
          tags$li(
            HTML(
              '<b>Descriptive:</b> After uploading your data have a look at your raw data and some simple descriptive statistics.'
            )
          ),
          tags$li(
            HTML(
              '<b>Protocal stats:</b> Get some meta information out of your protocol in the form of a standardized report.'
            )
          ),
          tags$li(
            HTML(
              '<b>Information:</b> Further information regarding the CAM app.'
            )
          )
        )
      )
    )


    ## set output
    output$uploadOut <- renderUI({
      outUI$elements
    })


    ################
    # switch to
    ################
    ## switch to
    #> start preprocessing
    observeEvent(input$startPreprocessing, {
      #req(data())
      if (v$dataUploaded == "no") {
        showModal(
          modalDialog(
            title = "No dataset uploaded",
            paste0("Please upload a valid dataset to continue."),
            easyClose = TRUE,
            footer = tagList(modalButton("Ok"))
          )
        )
      } else{
        globals$clickedButton <- "startPreprocessing"

        shinyjs::disable(selector = '.navbar-nav a[data-value="upload data"')


        showTab(
          inputId = "tabs",
          target = "draw CAM",
          select = TRUE,
          session = parent
        )

        showTab(
          inputId = "tabs",
          target = "summarize terms",
          select = FALSE,
          session = parent
        )

        showTab(
          inputId = "tabs",
          target = "non-summarized terms",
          select = FALSE,
          session = parent
        )

        showTab(
          inputId = "tabs",
          target = "reliability",
          select = FALSE,
          session = parent
        )

        shinyjs::disable(selector = '.navbar-nav a[data-value="summarize terms"')
        shinyjs::disable(selector = '.navbar-nav a[data-value="non-summarized terms"')
        shinyjs::disable(selector = '.navbar-nav a[data-value="reliability"')

      }
    })

    #> start analysis
    observeEvent(input$startAnalysis, {
      if (v$dataUploaded == "no") {
        showModal(
          modalDialog(
            title = "No dataset uploaded",
            paste0("Please upload a valid dataset to continue."),
            easyClose = TRUE,
            footer = tagList(modalButton("Ok"))
          )
        )
      } else{
        globals$clickedButton <- "startAnalysis"

        shinyjs::disable(selector = '.navbar-nav a[data-value="upload data"')

        showTab(
          inputId = "tabs",
          target = "draw CAM",
          select = TRUE,
          session = parent
        )

        showTab(
          inputId = "tabs",
          target = "network indicators",
          select = FALSE,
          session = parent
        )

                showTab(
          inputId = "tabs",
          target = "word outputs",
          select = FALSE,
          session = parent
        )

       showTab(
          inputId = "tabs",
          target = "aggregate CAMs",
          select = FALSE,
          session = parent
        )

        showTab(
          inputId = "tabs",
          target = "clustering CAMs",
          select = FALSE,
          session = parent
        )

         showTab(
          inputId = "tabs",
          target = "slice CAMs",
          select = FALSE,
          session = parent
        )


                 showTab(
          inputId = "tabs",
          target = "get report",
          select = FALSE,
          session = parent
        )


        shinyjs::disable(selector = '.navbar-nav a[data-value="network indicators"')
        shinyjs::disable(selector = '.navbar-nav a[data-value="word outputs"')
        shinyjs::disable(selector = '.navbar-nav a[data-value="aggregate CAMs"')
        shinyjs::disable(selector = '.navbar-nav a[data-value="clustering CAMs"')
        shinyjs::disable(selector = '.navbar-nav a[data-value="slice CAMs"')
        shinyjs::disable(selector = '.navbar-nav a[data-value="get report"')


      }
    })

    ################
    # single module options
    ################
    ###### upload
    #> UI
    observeEvent(input$uploadData, {
      ## change UI
      outUI$elements <- tagList(
        tags$div(
          HTML(
            "Remark: if you have a protocol already please upload first your protocol and afterwards your raw data."
          ),
          style = "font-size:14px"
        ),
        tags$h2("Upload your protocol"),
        tags$div(
          HTML(
            "If you have a protocol - generated by the CAM App - upload here the protocol before you upload your raw CAM dataset."
          ),
          style = "font-size:14px"
        ),
        tags$br(),
        fileInput(
          ns("uploadProtocol"),
          "1) Upload your protocol:",
          accept = c(".txt", ".csv"),
          placeholder = "upload protocol",
          multiple = FALSE
        ),
        tags$br(),
        tags$h2("Upload your raw data"),
        tags$br(),
        fileInput(
          ns("upload"),
          "2) Upload a raw CAM dataset (.txt file for C.A.M.E.L. or .csv files for Valence):",
          accept = c(".txt", ".csv"),
          placeholder = "upload raw CAM data",
          multiple = TRUE
        ),
        tags$div(
          HTML(
            "Please wait a few seconds until the data is processed (a table will pop up). You have uploaded the
                              following file(s) - maximum four file names are shown:"
          )
        ),
        tableOutput(ns("file")),
        tags$br(),
        tags$div(
          HTML(
            'After you have uploaded your data (and protocol) you can continue with the
                              prepocessing or analysis part (see "Information" for more details):'
          )
        ),
        div(
          style = "margin: 0 auto; width: 50%; text-align:center;",
          actionButton(
            ns("startPreprocessing"),
            HTML('Preprocessing<br>Part'),
            style = "width: 200px;
                                 height: 150px; font-size: 22px;"
          ),
          actionButton(
            ns("startAnalysis"),
            HTML('Analysis<br>Part'),
            style = "width: 200px;
                                 height: 150px; margin-left:20px; font-size: 22px;"
          )
        )

      )
    })

    #> Server
    ### upload protocol dataset
    protocol <- reactive({
      ## to test validity of JSON file
      text <- readLines(input$uploadProtocol$datapath, warn = FALSE)
      text <- readLines(textConnection(text, encoding="UTF-8"), encoding="UTF-8")
      if (testIfJson(file = text)) {
      protocol <- rjson::fromJSON(file = input$uploadProtocol$datapath)
  
      return(protocol)
      } else{
         print("Invalid protocol uploaded")
         return(NULL)
      }
      

      # ## alternative
      # text <- readLines(input$uploadProtocol$datapath)
      # text <- readLines(textConnection(text, encoding="UTF-8"), encoding="UTF-8")
      # if (testIfJson(file = text)) {
      #   protocol <- jsonlite::fromJSON(txt = text)
      #   # jsonlite::read_json(path = input$uploadProtocol$datapath)
      #   # rjson::fromJSON(file = input$uploadProtocol$datapath)
      #   return(protocol)
      # } else{
      #   print("ERROR")
      #   return(NULL)
      # }
      # 
      # ## right encoding
      # for(i in 1:length(protocol$approximateMatching)){
      #   Encoding(x = protocol$approximateMatching[[i]]$wordsFound) <- "latin1"
      #   Encoding(x = protocol$approximateMatching[[i]]$supordinateWord) <- "latin1"
      # }
    })

    observeEvent(input$uploadProtocol, {
      if (!is.null(protocol())) {
        message("successfully uploaded protocol!")
        v$protocol = TRUE
      } else{
        v$protocol = FALSE
        showModal(
          modalDialog(
            title = "Invalid CAM protocol",
            paste0(
              "The file you have uploaded doesn't appear to be a valid protocol as generated by the CAM app. Please upload a valid protocol or continue with uploading your raw CAM dataset."
            ),
            easyClose = TRUE,
            footer = tagList(modalButton("Ok"))
          )
        )
      }
    })



    ### get the extensions and show file names
    ext <- reactive({
      # wait for upload
      req(input$upload)

      # show uploaded file(s)
      output$file <- renderTable({
        if (nrow(input$upload) > 4) {
          rbind(input$upload[1:4,], c("...", "...", "...", "..."))
        } else {
          input$upload
        }
      })

      # extract file ending
      tools::file_ext(input$upload$name)
    })


    ### create three data sets with nodes / connectors / merged
    data <- reactive({
      # wait for upload
      req(ext())

      ## if CAMEL data (checked by .txt ending)
      if (all(stringr::str_detect(string = ext(), pattern = "txt"))) {
        dat = as.list(
          vroom::vroom(
            file = input$upload$datapath,
            delim = "\t",
            show_col_types = FALSE,
            col_names = FALSE
          )$X1
        )


        raw_CAM <- list()
        for (i in 1:length(dat)) {
          if(testIfJson(dat[[i]])) {
            raw_CAM[[i]] <- jsonlite::fromJSON(txt = dat[[i]])
          } else {
            showModal(
            modalDialog(
              title = "Invalid raw data",
              paste0(
                "The file you have uploaded doesn't appear to be a valid C.A.M.E.L. dataset. Please check again."
              ),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )
          v$dataUploaded <- "no"
          return(NULL)
          }
        }
        CAMfiles <-
          create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE, verbose = FALSE)

       # check if parsing CAMs was successful
        if(is.null(CAMfiles)) {
            showModal(
            modalDialog(
              title = "Invalid raw data",
              paste0(
                "The file you have uploaded doesn't appear to be a valid C.A.M.E.L. dataset or it contains only empty CAMs. Please check again."
              ),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )
          v$dataUploaded <- "no"
          return(NULL)
        }

        ## trim whitespace
        CAMfiles[[1]]$text <-
          stringr::str_trim(string = CAMfiles[[1]]$text, side = "both")
        v$dataUploaded <- "yes"
        ## if Valence data (checked by .csv ending)
      } else if (all(stringr::str_detect(string = ext(), pattern = "csv"))) {
        ## check: at least 2 datasets
        if (length(ext()) < 2) {
          v$boolContinue <- FALSE
          showModal(
            modalDialog(
              title = "Wrong number of datasets",
              paste0(
                "Please upload at least two .csv files, which have the same number of blocks and links."
              ),
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )
          v$dataUploaded <- "no"
          return(NULL)
        }


        ## names for blocks and links files
        files_blocks <-
          sort(stringr::str_subset(string = input$upload[, "name"],
                                   pattern = "blocks"))
        files_links <-
          sort(stringr::str_subset(string = input$upload[, "name"],
                                   pattern = "links"))

        ## check: equal numbers of blocks and links dataset
        if (v$boolContinue) {
          if (length(files_blocks) != length(files_links)) {
            v$boolContinue <- FALSE
            showModal(
              modalDialog(
                title = "Wrong number of datasets",
                paste0(
                  "Please upload the same number of blocks and links csv files."
                ),
                easyClose = TRUE,
                footer = tagList(modalButton("Ok"))
              )
            )
            v$dataUploaded <- "no"
            return(NULL)
          }
        }

        ## check: equal IDs
        if (v$boolContinue) {
          if (!all(
            str_split(
              string = files_blocks,
              pattern = "_",
              simplify = TRUE
            )[, 1]
            == str_split(
              string = files_links,
              pattern = "_",
              simplify = TRUE
            )[, 1]
          )) {
            v$boolContinue <- FALSE
            showModal(
              modalDialog(
                title = "Wrong IDs",
                paste0(
                  'Please upload csv files with identical IDs in the format: "ID_CAMnumber_links/blocks".'
                ),
                easyClose = TRUE,
                footer = tagList(modalButton("Ok"))
              )
            )
            v$dataUploaded <- "no"
            return(NULL)
          }
        }


        if (v$boolContinue) {
          upload <- list()

          blocks <- list()
          links <- list()
          b <- 1
          l <- 1
          for (i in 1:length(input$upload[, 1])) {
            ## load blocks
            if (str_detect(string = input$upload[[i, "name"]], pattern = "blocks")) {
              if(str_detect(string = input$upload[[i, "name"]], pattern = "blocks_c")){  # adjustment Lars
              blocks[[b]] <-
                utils::read.csv(
                  file = input$upload[[i, "datapath"]],
                  sep = ";",
                  encoding = "UTF-8"
                )
              }else{
                blocks[[b]] <-
                utils::read.csv(
                  file = input$upload[[i, "datapath"]],
                  sep = ",",
                  encoding = "UTF-8"
                )
              }

              blocks[[b]]$participantCAM <-
                str_extract(string = input$upload[[i, "name"]], pattern = ".*(?=_blocks)|.*(?=_blocks_c)") # adjustment Lars
              b <- b + 1
            }
            ## load links
            if (str_detect(string = input$upload[[i, "name"]], pattern = "links")) {
              links[[l]] <-
                utils::read.csv(
                  file = input$upload[[i, "datapath"]],
                  sep = ",",
                  encoding = "UTF-8"
                )
              links[[l]]$participantCAM <-
                str_extract(string = input$upload[[i, "name"]], pattern = ".*(?=_links)")
              l <- l + 1
            }
          }

          CAMfiles <-
            create_ValenceFiles(datBlocks = blocks, datLinks = links)

          ## trim whitespace
          CAMfiles[[1]]$text <-
            stringr::str_trim(string = CAMfiles[[1]]$text, side = "both")

          v$dataUploaded <- "yes"
        } else{
          ## if error message
          v$dataUploaded <- "no"
          CAMfiles <- NULL
        }

      } else{
        ## if no CAMEL or Valence files have been upload
        showModal(modalDialog(
          title = "Wrong files",
          paste0(
            "Please upload a single .tsv file (for C.A.M.E.L.) OR at least two .csv files (for Valence)."
          ),
          easyClose = TRUE,
          footer = tagList(modalButton("Ok"))
        ))
        v$dataUploaded <- "no"
        CAMfiles <- NULL
      }

      # print(stringr::str_detect(string = ext(), pattern = "txt"))
      # print(head(CAMfiles[[1]]))
      # print(head(CAMfiles[[2]]))
      # print(head(CAMfiles[[3]]))

if(v$protocol){
   # print(globals$protocol)
    globals$protocol$software <- protocol()$software

    globals$protocol$cleanValence <- protocol()$cleanValence
    
    ## keep only CAMs which have not been deleted
    if(length(protocol()$currentCAMs) > 0){
      # if participants IDs have been used in the draw CAM step
if(all(unlist(protocol()$currentCAMs) %in% CAMfiles[[1]]$CAM)){
        CAMfiles[[1]] <- CAMfiles[[1]][CAMfiles[[1]]$CAM %in% unlist(protocol()$currentCAMs), ]
        CAMfiles[[2]] <- CAMfiles[[2]][CAMfiles[[2]]$CAM %in% unlist(protocol()$currentCAMs), ]
        CAMfiles[[3]] <- CAMfiles[[3]][CAMfiles[[3]]$CAM.x %in% unlist(protocol()$currentCAMs), ]
}else{
          CAMfiles[[1]] <- CAMfiles[[1]][CAMfiles[[1]]$participantCAM %in% unlist(protocol()$currentCAMs), ]
        CAMfiles[[2]] <- CAMfiles[[2]][CAMfiles[[2]]$participantCAM %in% unlist(protocol()$currentCAMs), ]
        CAMfiles[[3]] <- CAMfiles[[3]][CAMfiles[[3]]$participantCAM.x %in% unlist(protocol()$currentCAMs), ]
}


print("AAAAAAAAAAAAAA!!!")
print(dim(CAMfiles[[1]]))
print(dim(CAMfiles[[2]]))
print(dim(CAMfiles[[3]]))
        # overwrite protocol
        globals$protocol$deletedCAMs <- protocol()$deletedCAMs
        globals$protocol$currentCAMs <- protocol()$currentCAMs
    }

        if(length(protocol()$approximateMatching) > 0 
        || length(protocol()$searchTerms) > 0 
        || length(protocol()$findSynonyms) > 0
        || length(protocol()$modelwordVec) > 0){
        CAMfiles[[1]]$text_summarized <- CAMfiles[[1]]$text
        tmp_out <- overwriteTextNodes(protocolDat = protocol(), nodesDat = CAMfiles[[1]])     
        CAMfiles[[1]] <- tmp_out[[1]]

        # save globally already used words
        globals$usedWords <- tmp_out[[2]]
        # overwrite protocol
       globals$protocol$approximateMatching <- protocol()$approximateMatching
       globals$protocol$searchTerms <- protocol()$searchTerms
       globals$protocol$findSynonyms <- protocol()$findSynonyms
       globals$protocol$modelwordVec <- protocol()$modelwordVec
    }

        # print("globals$protocol:")
        # print(globals$protocol)
}
## save as global variable - else triggered to early

# print(globals$protocol$cleanValence)

if(!globals$protocol$cleanValence[[1]]){
     v$df <- CAMfiles
}else{
  v$df <- fix_ValenceData(dat_nodes =  CAMfiles[[1]],
                  dat_connectors =  CAMfiles[[2]],
                  dat_merged = CAMfiles[[3]],
                  verbose = FALSE)
   message("successfully cleaned Valence data!")

}

# nodes_raw$text_summarized <- nodes_raw$text

      return(v$df) ### ??? CAMfiles
    })


    ## SAVE to globals ##
    observeEvent(input$upload, {
      req(ext())
      req(data())
      message("uploaded file(s) - extensions:", ext())
      if (!is.null(v$df)) {
        message("successfully uploaded data!")
        #> change condition
        globals$condition <-
          c(globals$condition, "uploadedData")
        #> add protocol
        if (all(stringr::str_detect(string = ext(), pattern = "txt"))) {
          tmp_list = vector(mode = "list", length = 2)
          tmp_list[[1]] = "CAMEL"
          tmp_list[[2]] = as.character(as.POSIXct(Sys.time()))
          names(tmp_list) <- c("software", "time")
          globals$protocol$software[[length(globals$protocol$software) + 1]] <-
            tmp_list
        } else if (all(stringr::str_detect(string = ext(), pattern = "csv"))) {
          tmp_list = vector(mode = "list", length = 2)
          tmp_list[[1]] = "Valence"
          tmp_list[[2]] = as.character(as.POSIXct(Sys.time()))
          names(tmp_list) <- c("software", "time")
          globals$protocol$software[[length(globals$protocol$software) + 1]] <-
            tmp_list
        }
      } else{
        #> avoid error in download function
        globals$condition <-
          str_replace_all(
            string = globals$condition,
            pattern = "uploadedData",
            replacement = "wrongUploadedData"
          )
        #> add protocol
        globals$protocol$software[[length(globals$protocol$software) + 1]] <-
          c("unknown", as.character(as.POSIXct(Sys.time())))
      }
    })
    ####




    ###### Descriptive
    observeEvent(input$getDescriptive, {
      #> change UI
      outUI$elements <- tagList(
        tags$h2("Descriptive: check your raw data"),
        tags$br(),
        HTML('Your CAM dataset contains: '),
        tags$ul(
          tags$li(textOutput(ns("CAM_number"), inline = TRUE), " CAMs"),
          tags$li(
            textOutput(ns("CAM_nodes"), inline = TRUE),
            " nodes (in total), thereof are"
          ),
          tags$ul(
            tags$li(textOutput(ns(
              "CAM_nodesNegative"
            ), inline = TRUE), " negative nodes"),
            tags$li(textOutput(ns(
              "CAM_nodesNeutral"
            ), inline = TRUE), " neutral nodes"),
            tags$li(textOutput(
              ns("CAM_nodesAmbivalent"), inline = TRUE
            ), " ambivalent nodes"),
            tags$li(textOutput(ns(
              "CAM_nodesPositive"
            ), inline = TRUE), " positive nodes"),
          ),
          tags$li(
            textOutput(ns("CAM_connectors"), inline = TRUE),
            " connectors (in total), thereof are"
          ),
          tags$ul(
            tags$li(textOutput(
              ns("CAM_connectorsSolid"), inline = TRUE
            ), " agreeing (solid)"),
            tags$li(textOutput(
              ns("CAM_connectorsDashed"), inline = TRUE
            ), " disagreeing (dashed)"),
            tags$li("and.."),
            tags$li(textOutput(
              ns("CAM_connectorsBidirectional"), inline = TRUE
            ), " bidirectional"),
            tags$li(textOutput(
              ns("CAM_connectorsUnidirectional"), inline = TRUE
            ), " unidirectional")
          ),
        ),
        tags$br(),
        tags$div(
          HTML("You uploaded the following <b>nodes</b> (concepts) dataset (dynamic table):")
        ),
        dataTableOutput(ns("tableNodes")),
        tags$div(
          HTML("You uploaded the following <b>connectors</b> (connectors) dataset (dynamic table):")
        ),
        dataTableOutput(ns("tableConnectors"))
      )
    })

    #> Server
    ## show table for nodes
    output$tableNodes <- renderDataTable({
      if (!is.null(v$df)) {
        v$df[[1]]
      }
    },  options = list(pageLength = 5))
    ## show table for connectors
    output$tableConnectors <- renderDataTable({
      if (!is.null(v$df)) {
       v$df[[2]]
      }
    },  options = list(pageLength = 5))


    ## summary stats:
    output$CAM_number <-  renderText({
      length(unique(v$df[[1]]$CAM))
    })
    # > nodes
    output$CAM_nodes <- renderText({
      nrow(v$df[[1]])
    })

    output$CAM_nodesNegative <- renderText({
      sum(v$df[[1]]$value < 0)
    })

    output$CAM_nodesNeutral <- renderText({
      sum(v$df[[1]]$value == 0)
    })

    output$CAM_nodesAmbivalent <- renderText({
      sum(v$df[[1]]$value == 10)
    })

    output$CAM_nodesPositive <- renderText({
      sum(v$df[[1]]$value > 0 & v$df[[1]]$value < 10)
    })

    #> connectors
    output$CAM_connectors <-  renderText({
      nrow(v$df[[2]])
    })

    output$CAM_connectorsSolid <-  renderText({
      sum(v$df[[2]]$agreement == 1)
    })

    output$CAM_connectorsDashed <-  renderText({
      sum(v$df[[2]]$agreement == 0)
    })

    output$CAM_connectorsBidirectional <-  renderText({
      sum(v$df[[2]]$isBidirectional == 1)
    })

    output$CAM_connectorsUnidirectional <-  renderText({
      sum(v$df[[2]]$isBidirectional == 0)
    })



    ###### Protocol stats
    observeEvent(input$getProtocolStats, {
      ## change UI
      # check if a protocol has in fact been uploaded
      if(v$protocol) {
        outUI$elements <- tagList(tags$h2("Protocol statistics"),
               tags$div(
            HTML(
              "Here are your protocol statistics: "
            ),
            style = "font-size:14px"
          ),
          tags$br(),
         tags$div(
            HTML(
              "Software and time statistics:"
            ),
            style = "font-size:14px"
          ),
                      tags$ul(
            tags$li("You are using the ", tags$b(textOutput(ns("softwareProto"), inline = TRUE)), " software and you have used this software the last time at: ", 
            textOutput(ns("softwareLastTime"), inline = TRUE)),
            tags$li("First time you have applied the summarize terms functions: ", textOutput(ns("summarizeFirstTime"), inline = TRUE)),
            tags$li("Last time you have applied the summarize terms functions: ", textOutput(ns("summarizeLastTime"), inline = TRUE)),
          ),
          tags$div(
            HTML(
              "Number of steps using summarize terms functions:"
            ),
            style = "font-size:14px"
          ),
            tags$ul(
            tags$li("Number of times you have used approximate matching functions to summarize terms: ", textOutput(ns("approximateMatchingNumber"), inline = TRUE)),
            tags$li("Number of times you have used search functions to summarize terms: ", textOutput(ns("searchTermsNumber"), inline = TRUE)),
            tags$li("Number of times you have looped through the find synonyms function: ", textOutput(ns("findSynonymsNumber"), inline = TRUE)),
            tags$li("Number of times you have looped through the word2vec function: ", textOutput(ns("findword2vecNumber"), inline = TRUE)),


          )
          )
        # no protocol uploaded so far
      } else {
             outUI$elements <- tagList(tags$h2("Protocol statistics"),
                     tags$div(
         HTML("If you have created and uploaded a protocol, you see your protocol statistics here. You don't appear to have uploaded a protocol yet."
          ),
          style = "font-size:14px"
        ))
      }
    })


    #> Server
    ## summary stats:
    output$softwareProto <-  renderText({
        req(protocol())
        unlist(protocol()$software[[length(protocol()$software)]]$software)
    })

    output$softwareLastTime <-  renderText({
        req(protocol())
        unlist(protocol()$software[[length(protocol()$software)]]$time)
    })



    vev_time_Protocol <- reactive({
    ## check approximate and search term was used

    list_summarizeTerms <- c(protocol()$approximateMatching, protocol()$searchTerms, protocol()$findSynonyms, protocol()$modelwordVec)

        vec_time <- c()
        for (i in 1:length(list_summarizeTerms)) {
        vec_time[i] <- unlist(list_summarizeTerms[[i]]$time)
        }

        return(vec_time)
    })



    output$summarizeFirstTime <-  renderText({
        req(vev_time_Protocol())
min(vev_time_Protocol())
    })

    output$summarizeLastTime <-  renderText({
        req(vev_time_Protocol())
max(vev_time_Protocol())
    })


    output$approximateMatchingNumber <-  renderText({
        req(protocol())
        length(protocol()$approximateMatching)
    })
    
        output$searchTermsNumber <-  renderText({
        req(protocol())
        length(protocol()$searchTerms)
    })


        output$findSynonymsNumber <-  renderText({
        req(protocol())
        length(protocol()$findSynonyms)
    })

        output$findword2vecNumber <-  renderText({
        req(protocol())
        length(protocol()$modelwordVec)
    })



###### Clean Valence
    observeEvent(input$cleanValence, {
      ## change UI
      outUI$elements <- tagList(tags$h2("Clean Valence Data"),
             tags$div(
          HTML(
            "If you have already uploaded your Valence data you can click on the button <i>clean Valence</i> to preprocess your Valence data and automatically solve the following problems: "
          ),
          style = "font-size:14px"
        ),
        tags$ul(
            tags$li(
              HTML(
                '<b>Ghost nodes:</b> These are concepts (node) that are not connected to any other concepts in the CAM (i.e. do not form the smallest possible network consisting of two concepts).
                <br>
                <i class="fa fa-arrow-right"></i> these are deleted'
              )
            ),
            tags$li(
              HTML(
                '<b>Multiple connections:</b> In Valence it is technically possible to draw multiple connections between concepts. This leads to a complex network (multigraph) instead of a simple network.
                <br>
                <i class="fa fa-arrow-right"></i>  all multiple connections except for the last one drawn are deleted'
              )
            ),
            tags$li(
              HTML(
                '<b>Unconnected networks:</b> Valence does not force participants to connect all concepts into one large network and this results in multiple components (network parts).
                <br>
                <i class="fa fa-arrow-right"></i> all components except the largest are deleted, if there are two (or more) components of the same size, a component is selected at random'
              )
            )
            ),
            tags$div(HTML("Click on button to clean Valence data. Please click only once and wait few seconds."), style="font-size:14px"),
            actionButton(ns("clickCleanValence"), "clean Valence data"),
             tags$br(),
            tags$br(),
       tags$div(
          HTML(
            "Deleted elements:"
          ),
          style = "font-size:14px"
        ),
          tags$ul(
          tags$li("in", textOutput(ns("remGN"), inline = TRUE), "CAMs ghost nodes have been removed"),
         tags$li("in", textOutput(ns("remMC"), inline = TRUE), "CAMs multiple connections have been removed"),
        tags$li("in", textOutput(ns("remNC"), inline = TRUE), "CAMs unnconnected network components have been removed")
        ),
                    tags$br(),
                                tags$br(),
       tags$div(
          HTML(
            "Deleted elements CAM ID lists <i>(only shows up if anything has been found)</i>:"
          ),
          style = "font-size:14px"
        ),
        tags$h4("Ghost nodes:"),
          textOutput(ns("tableGN")),
                  tags$h4("Multiple connections:"),
          textOutput(ns("tableMC")),
                  tags$h4("Unconnected networks:"),
          textOutput(ns("tableNC")),
          
        )
    })

    #> Server
            ## draw CAMs
        ValenceClean <- eventReactive(input$clickCleanValence, {
          if(is.null(v$df)){
            showModal(modalDialog(
            title = "No dataset uploaded",
            paste0("Please upload a valid Valence dataset to subsequently clean your Valence data."),
              easyClose = TRUE,
              footer = tagList(
                modalButton("Ok")
              )
            ))

            clean_ValenceData <- NULL
          }else{
        clean_ValenceData <- fix_ValenceData(dat_nodes =  v$df[[1]],
                  dat_connectors =  v$df[[2]],
                  dat_merged = v$df[[3]],
                  verbose = FALSE)

## set protocol to true
          globals$protocol$cleanValence <- TRUE

          v$dfClean <- clean_ValenceData
          v$df <- clean_ValenceData


          #print(str(v$df[[1]]))
          #print(str(v$dfClean[[1]]))
          #print(str(v$df[[3]]))
          #print(str(v$dfClean[[3]]))
          }

        return(clean_ValenceData)
        })





            output$remGN <-  renderText({
            ValenceClean()[[5]] # trigger function
      if (!is.null(v$dfClean)) {
length(v$dfClean[[6]])
      }
    })
      output$tableGN <- renderText(v$dfClean[[6]])

    
            output$remMC <-  renderText({
      if (!is.null(v$dfClean)) {
length(v$dfClean[[5]])
      }
    })
  output$tableMC <- renderText(v$dfClean[[5]])
    
            output$remNC <-  renderText({
      if (!is.null(v$dfClean)) {
length(v$dfClean[[4]])
      }
    })
    output$tableNC <- renderText(v$dfClean[[4]])




    ###### information
    observeEvent(input$information, {
      ## change UI
      outUI$elements <- tagList(
        tags$h2("General Information"),
        tags$br(),
        HTML(
          'The software is a shiny-App programmed in the R environment. Every time you have finished the central step of
                     the module (here the Upload step) you can click "continue" in the bottom right to jump to the next
                     module. This is just a <i>recommended procedure</i>, whereby the first two steps - uploading your data and draw your CAMs - is mandatory. Feel free to skip every other module.
                     The central step within each module is depicted in <b>bold</b>.
                     <br>
                     The CAM app is divided in two central parts, whereby for the first part a protocol is generated, which should be uploaded together with the raw dataset(s).'
        ),
        tags$ol(tags$li(
          HTML('<b>Prep-rocessing Part:</b> using multiple modules it is possible to summarize the CAM data semi-automatically')
        ),
        tags$li(HTML(
          '<b>Analysis Part:</b> the (summarized) data can be subsequently analyzed and visualized using multiple implemented functions'
        ))),
        HTML('The protocol is internally a JavaScript Object Notation (JSON) file and, for example, it includes information if any CAMs were deleted or which summary functions have been used. 
        If you have applied any summarize terms module functions and you upload protocol and raw data, the text of the drawn concepts is internally summarized and you could continue summarizing 
        the CAM data.'),
        tags$br(),
        tags$br(),
        p(
          "If you have any technical problems, criticism or suggestions for improvement, feel free to contact me:",
          style = "display:inline"
        ),
        a(
          actionButton(
            inputId = "email",
            label = "Julius Fenn (Admin)",
            icon = icon("envelope", lib = "font-awesome"),
            style = 'padding:3px; font-size:80%; margin: 5px'
          ),
          href = "mailto:cam.contact@drawyourminds.de"
        ),
        tags$br(),
        tags$br(),
        tags$div(
          HTML(
            'Best regards and have fun
            <br> Julius Fenn, supported by Florian Gouret, Michael Gorki, Lisa Reuter, Wilhelm Gros, Paul Hüttner, and Andrea Kiesel (University of Freiburg)'
          )
        ),
        tags$br(),
        tags$br(),
        tags$h2("Module Specific Information"),
        tags$div(
          HTML('The options for this module are the following:'),
          tags$ul(
            tags$li(
              HTML(
                '<b>Upload - necessary step:</b> Upload your raw data (C.A.M.E.L. or Valence) and - if you already have - your protocol 
                before uploading your data.'
              )
            ),
            tags$li(
              HTML(
                '<b>Descriptive:</b> After uploading your data have a look at your raw data and some simple descriptive statistics.'
              )
            ),
            tags$li(
              HTML(
                '<b>Protocal stats:</b> After uploading your protocol, you will get some information about the steps you have taken in the form of a standardised report.'
              )
            ),
            tags$li(
              HTML(
                '<b>Information:</b> The module option you are currently on.'
              )
            )
          )
        )
      )
    })






      #return(list(df = reactive({v$df}), # return global df
      #      dfClean   = reactive({v$dfClean}))) # return global clean df

      return(reactive({v$df})) 
   # return(data)
  })
}
