summarizeCAMsServer <-
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
        h1("Aggregate CAMs Module"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>aggregateCAMs:</b> By creating a so called “canonical adjacency matrix” CAMs according to different criteria (all CAMs, CAMs of a certain group) are aggregated, whereby the size of the concept and the thickness of the connection is proportional to the frequency of the drawn concepts and the pairwise connections respectively.'))
            )
      ))

      ## set output
      output$uploadOutSummarizeCAMs <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
        ###### wordlist_overall
        #> UI
        observeEvent(input$aggregateCAMs, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("Aggregate CAMs"),
              tags$br(),
          tags$div(HTML("After choosing your settings (in most cases the default is recommended), please click on the aggregate CAMs button. 
          Please click only once and wait few seconds. It is recommended that you summarise the concepts before aggregating the CAM data:"), style="font-size:14px"),
            tags$h3("Your Settings:"),
                     tags$div(
            HTML(
              'Please select if you want to split your summarized concepts by the valence and if you want to (a) aggregate random 
              CAMs, (b) aggregate the most positive or negative CAMs or (c) choose CAMs you want to aggregate:'
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 45%; padding:10px;",
                       tags$div(HTML("Do you want to split your summarized concepts by the valence (X_positive, X_negative, ...)?"), style="font-size:14px"),
                   radioButtons(ns("aggregate_setting_split"), label = NULL, c("Yes", "No"), selected = "No"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 45%; padding:10px;",
		                     tags$div(HTML("Do you want to (a) aggregate random CAMs, (b) aggregate the most positive or negative CAMs 
                         or (c) choose CAMs you want to aggregate:"), style="font-size:14px"),
       selectInput(ns("aggregate_setting_type"), NULL, c("(a) aggregate random CAMs" = "a",
                                                       "(b) aggregate the most positive or negative CAMs" = "b",
                                                       "(c) choose CAMs you want to aggregate" = "c")),
            ),
            div(id=ns("a_setting"),
            tags$h3("specify setting for aggregating random CAMs:"),
            numericInput(ns("aggregate_setting_a"), label = "Number of random CAMs you want to aggregate (min=2)", 
            value = 3, min = 2, max = Inf, step = 1)
            ),
                 div(id=ns("b_setting"),
            tags$h3("specify setting the most positive or negative CAMs:"),

           div(
              style = "display: inline-block; vertical-align: top; width: 35%; padding:10px;",
                   radioButtons(ns("aggregate_setting_b_valence"), label = "Do you want to aggregate the X most positive or negative CAMs?", 
                   c("positive", "negative"), selected = "positive"),
            ),
            div(
              style = "display: inline-block; vertical-align: top; width: 35%; padding:10px;",
              numericInput(ns("aggregate_setting_b_number"), label = "Number of most positive / negative CAMs you want to aggregate (min=2)", 
            value = 3, min = 2, max = Inf, step = 1)
            ),
            ),
            div(id=ns("c_setting"),
            tags$h3("specify setting for aggregating choosen CAMs:"),
            uiOutput(ns("aggregate_setting_c")),

            )
          ),

                    tags$h3("Aggregate CAMs:"),
            actionButton(ns("clickAggregateCAMs"), "aggregate CAMs"),
            tags$br(),
            tags$br(),
            htmlOutput(ns("textRemovedIdentical")),
             tags$h4("Static aggregated CAM:"),
             plotOutput(ns("aggregatedCAM_static"), width = "90%"),
             tags$h4("Dynamic aggregated CAM:"),
             visNetworkOutput(ns("aggregatedCAM_dynamic"), height = "600px"),
             tags$div(HTML("You see a wordcloud of your aggregated CAM, which includes at maximum 200 words:"), 
             style="font-size:14px"),
        plotOutput(ns("plotWordcloudAgg"), width = "100%"),
          )
        })

        #> Server
        module_rv <- reactiveValues(numCAM = NULL)

        
        ## hide / show specific settings
        observeEvent(c(
          input$aggregate_setting_type), {
            message("The value of input$aggregate_setting_type is ",
                    input$aggregate_setting_type)

            if(input$aggregate_setting_type == "a"){
              shinyjs::show("a_setting")
              shinyjs::hide("b_setting")
              shinyjs::hide("c_setting")
            }else if(input$aggregate_setting_type == "b"){
              shinyjs::hide("a_setting")
              shinyjs::show("b_setting")
              shinyjs::hide("c_setting")
            } else if(input$aggregate_setting_type == "c"){
              shinyjs::hide("a_setting")
              shinyjs::hide("b_setting")
              shinyjs::show("c_setting")
            }
          })

        ## update list for (c)
        output$aggregate_setting_c <- renderUI({
            req(dataCAM())
            req(drawnCAM())
            selectInput(ns("aggregate_setting_c_choosen"),
                        "Which CAM(s) would you like to aggregate?",
                        choices = as.list(names(globals$drawnCAM())), width="50%", multiple = TRUE)
        })



        aggCAM <- eventReactive(c(
          input$clickAggregateCAMs), {
              message("The value of input$aggregate_setting_split is ",
                    input$aggregate_setting_split)
              message("The value of input$aggregate_setting_type is ",
                    input$aggregate_setting_type)    
                    
          ### get IDs of CAMs to aggregate
          if(input$aggregate_setting_type == "a"){
                       message("The value of input$aggregate_setting_a is ",
                    input$aggregate_setting_a)    
            ## random CAM
            selectedIDs <- sample(x = names(globals$drawnCAM()), size = input$aggregate_setting_a, replace = FALSE)
          } else if(input$aggregate_setting_type == "b"){
            ## most positive or negative CAMs
            if(is.null(globals$dataNetworkIndicators)){
              globals$dataNetworkIndicators <- compute_indicatorsCAM(drawn_CAM = globals$drawnCAM(),
                                  micro_degree = NULL,
                                  micro_valence = NULL,
                                  micro_centr_clo = NULL,
                                  largestClique = FALSE)
            }


            if(input$aggregate_setting_b_valence == "positive"){
              ids_ordered <- globals$dataNetworkIndicators$CAM_ID[order(globals$dataNetworkIndicators$mean_valence_macro,
                                                                   decreasing = TRUE)]
                }
                if(input$aggregate_setting_b_valence == "negative"){
              ids_ordered <- globals$dataNetworkIndicators$CAM_ID[order(globals$dataNetworkIndicators$mean_valence_macro,
                                                                   decreasing = FALSE)]
                }
                selectedIDs <- ids_ordered[1:input$aggregate_setting_b_number]
          } else if(input$aggregate_setting_type == "c"){
            ## choosen CAMs
            selectedIDs <- input$aggregate_setting_c_choosen
          }

          print("selectedIDs")
          print(selectedIDs)

module_rv$numCAM <- length(selectedIDs)
            ## remove X_negative, ...
            tmp_nodes <- globals$dataCAMsummarized[[1]]
            if (input$aggregate_setting_split == "No") {
              print("suffix removed")
              tmp_nodes$text_summarized <- str_remove(string = tmp_nodes$text_summarized, pattern = "_neutral|_positive|_negative|_ambivalent")
            }else{
              print("suffix kept")
            }

            ## rename identical terms
            for(i in 1:length(unique(tmp_nodes$CAM))){
              if(any(table(tmp_nodes$text_summarized[tmp_nodes$CAM == unique(tmp_nodes$CAM)[i]]) > 1)){
                tmp_nodes <- rename_identicalTerms(dat_nodes = tmp_nodes, drawn_CAM = globals$drawnCAM()) # changed input
                print("internally rename_identicalTerms() applied")
                  break
              }
            }
          ##
                
                
              tmp_aggCAM <- aggregate_CAMs(dat_merged = globals$dataCAMsummarized[[3]], dat_nodes = tmp_nodes,
                                             ids_CAMs = selectedIDs)


            ## rename identical terms - text
            for(i in 1:length(unique(tmp_aggCAM[[3]]$CAM))){
              if(any(table(tmp_aggCAM[[3]]$text_summarized[tmp_aggCAM[[3]]$CAM == unique(tmp_aggCAM[[3]]$CAM)[i]]) > 1)){
                ## show when single CAM contains identical terms
                output$textRemovedIdentical <- renderUI({
                  HTML('<b style="color:red;">single CAMs of the aggregated CAM contain identical terms</b> (<i>identical terms were renamed to X_1, X_2,...</i>)')
                  })
                  break
                  }
                  if(i == length(unique(tmp_aggCAM[[3]]$CAM))){
                    ## show when single CAM contains no identical terms
                    output$textRemovedIdentical <- renderUI({
                      HTML('<i style="font-size: 6px;">single CAMs of the aggregated CAM not contain identical terms</i>')
                      })
                    }
                }
            ##


            ## create wordlist
            if(all(selectedIDs %in% globals$dataCAMsummarized[[1]]$CAM)){
              sel_nodes <- globals$dataCAMsummarized[[1]][globals$dataCAMsummarized[[1]]$CAM %in% selectedIDs, ]
              sel_merged <- globals$dataCAMsummarized[[3]][globals$dataCAMsummarized[[3]]$CAM.x %in% selectedIDs, ]
            }else{
              sel_nodes <- globals$dataCAMsummarized[[1]][globals$dataCAMsummarized[[1]]$participantCAM %in% selectedIDs, ]
              sel_merged <- globals$dataCAMsummarized[[3]][globals$dataCAMsummarized[[3]]$participantCAM.x %in% selectedIDs, ]
            }


    CAMwordlist <- create_wordlist(
    dat_nodes = sel_nodes,
    dat_merged = sel_merged,
    order = "frequency",
    splitByValence = FALSE,
    comments = FALSE,
    raterSubsetWords = NULL,
    rater = FALSE
  )

        ## plot wordcloud
        output$plotWordcloudAgg<- renderPlot({
            colors_vec <- rep(NA, times = nrow(CAMwordlist))
            colors_vec <- ifelse(test = CAMwordlist$mean_valence > 2, yes = "darkgreen",
            no = ifelse(test = CAMwordlist$mean_valence > 1, yes = "green",
            no = ifelse(test = CAMwordlist$mean_valence > 0, yes = "lightgreen",
            no = ifelse(test = CAMwordlist$mean_valence < -2, yes = "darkred",
            no = ifelse(test = CAMwordlist$mean_valence < -1, yes = "red",
            no = ifelse(test = CAMwordlist$mean_valence < 0, yes = "indianred1",  no = "yellow"))))))
            # print(colors_vec)
            # print(table(colors_vec))
            wordcloud::wordcloud(words = CAMwordlist$Words, freq = CAMwordlist$raw, min.freq = 1,
                      max.words=200, random.order=FALSE, colors=as.character(colors_vec), ordered.colors=TRUE, scale=c(1,0.50))
            # , rot.per=0.35, colors=brewer.pal(8, "Dark2")
        })

            ##

                tmp_aggCAM[[2]] <- as.undirected(tmp_aggCAM[[2]])
                tmp_aggCAM
          })


        output$aggregatedCAM_static <- renderPlot({
            req(aggCAM())
            plot.igraph(aggCAM()[[2]], vertex.size=diag(aggCAM()[[1]]) / max(diag(aggCAM()[[1]]))*20,
                        edge.arrow.size=0.01, layout=layout_nicely,
                        vertex.frame.color="black", asp = .5,
                        margin = 0,
                        vertex.size = 12, vertex.label.cex = .7)
        })

        output$aggregatedCAM_dynamic <- renderVisNetwork({
            req(aggCAM())
               nodes <- data.frame(id = str_replace_all(string = V(aggCAM()[[2]])$name, pattern = " ", replacement = "_"),
                                label = V(aggCAM()[[2]])$name,
                                color  = V(aggCAM()[[2]])$color,
                                value = diag(aggCAM()[[1]]) / max(diag(aggCAM()[[1]])),
                                title = diag(aggCAM()[[1]]) / module_rv$numCAM)
            edges <- data.frame(from = str_replace_all(string = as_edgelist(aggCAM()[[2]])[,1], pattern = " ", replacement = "_"),
                                to = str_replace_all(string = as_edgelist(aggCAM()[[2]])[,2], pattern = " ", replacement = "_"))
            visNetwork(nodes, edges)
        })

        ###### information
        observeEvent(input$informationSummarizeCAMs, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
              tags$ul(
              tags$li(HTML('<b>aggregateCAMs:</b> By creating a so called “canonical adjacency matrix” CAMs according to different criteria (all CAMs, CAMs of a certain group) are aggregated, whereby the size of the concept and the thickness of the connection is proportional to the frequency of the drawn concepts and the pairwise connections respectively.'))
            )
          )
          )
        })

    })
    }