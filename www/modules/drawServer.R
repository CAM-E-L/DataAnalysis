drawServer <- function(id, dataCAM, parent, globals) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id)

        ## reactive values
        outUI <- reactiveValues(elements = NULL)

        # input validator for settings
        iv <- InputValidator$new()
        iv$add_rule("drawCAM_setting_relVertices", sv_between(1, 10))
        iv$add_rule("drawCAM_setting_relEdges", sv_between(.1, 1.5))

        ################
        # default text + set output
        ################
        ## default text
        outUI$elements <- tagList(
          tags$div(
            h1("Draw CAM module"),
            tags$br(),
            tags$br(),
            HTML('To start the module, please click on one of the module options on the sidebar panel. The options for
                     this module are the following:'),
            tags$ul(
              tags$li(HTML('<b>Draw R:</b> using the igraph package to draw the CAMs, which is a package in R (statistic software).')),
              tags$li(HTML('<b>Draw JS:</b> <i>to be implemented</i>.')),
              tags$li(HTML('<b>Information:</b> Further information regarding this module.'))
            )
          )
        )

        ## set output
        output$uploadOutDraw <- renderUI({
          outUI$elements
        })

        ################
        # single module options
        ################
        ###### drawCAMR
        #> UI
        observeEvent(input$drawCAMR, {

          ## change UI
          outUI$elements <- tagList(
            tags$h2("Draw CAMs using R (statistic software)"),
            tags$br(),
            tags$div(HTML("After choosing your settings (in most cases the default is recommended), please click on the draw CAMs button 
            to draw your CAMs. Please click only once and wait few seconds. If you have deleted a single or multiple CAMs please 
            click on this button again to update your drawn CAMs:"), style="font-size:14px"),
            tags$h3("Your Settings:"),
         tags$div(
            HTML(
              'Please select if you want to draw the concepts on the same positions as the participants, else a so called 
              <a href="https://en.wikipedia.org/wiki/Force-directed_graph_drawing" target="_blank">force-directed graph drawing algorithm</a> 
              is applied. Further you can change the relative size of the vertices and edges (only needed if you have large CAMs):'
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # position
                       tags$div(HTML("Please specify if positions should be considered:"), style="font-size:14px"),
                   radioButtons(ns("drawCAM_setting_position"), label = NULL, c("Yes", "No"), selected = "Yes"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # relative size of the vertices
		                     tags$div(HTML("You can change the relative size of the vertices (from 1 to 10):"), style="font-size:14px"),
                   numericInput(ns("drawCAM_setting_relVertices"), label = NULL, value = 4, min = 1, max = 10, step = 1, width = "80%"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 25%; padding:10px;",
			  # relative size of the edges
		                     tags$div(HTML("You can change the relative size of the edges (from .1 to 1.5):"), style="font-size:14px"),
                   numericInput(ns("drawCAM_setting_relEdges"), label = NULL, value = .5, min = .1, max = 1.5, step = .1, width = "80%"),
            ),
          ),

            tags$h3("Draw CAMs:"),
            actionButton(ns("clickDrawR"), "draw CAMs"),
			      tags$div(id = "drawnCAMsDiv", style = "visibility: hidden",
              tags$p(
                "You have drawn ",
                tags$b(textOutput(ns("numCAMsDrawnR"), inline = TRUE), " CAMs"), ". If you hover over the drawn CAM with your mouse
                you can increase the size."
              ),
              tags$br(),
              tags$br(),
              fluidRow(
                column(width = 7,
                       plotOutput(ns("plotR"), width = "95%"),
                       htmlOutput(ns("textR"))
                ),
                column(width = 4,
                       uiOutput(ns("selectCAMR")),
                       actionButton(ns("prevCAMR"), "Previous"),
                       actionButton(ns("nextCAMR"), "Next"),
                       tags$br(),
                       tags$span(HTML("order your drawn CAMs according to: "), style="font-size:14px;"),

                       selectInput(ns("orderCAMR"), NULL, c("mean valence",
                                                        "# of nodes",
                                                        "# of connectors",
                                                        "density",
                                                        "assortativity"), width = "200px"),
                       tableOutput(ns('tableR')),
                       tags$div(HTML("Click if you want to delete the displayed CAM (click again to keep it). After you have marked all CAMs you want to delete, draw the CAMs again:"), style="font-size:14px"),
                       actionButton(ns("deleteCAMR"), "un/delete CAM"),
                       tags$div(HTML("When you delete CAMs, you can save them as PDFs to upload them for example on OSF:"), style="font-size:14px"),
                       textInput(ns("renamefileR"), "Filename (PDF):", placeholder = "e.g. reason for the deletion"),
                       downloadButton(outputId = ns('PDFdownloadR'), label = "Download CAM as PDF")
                ),
                column(width = 11,
                tags$br(),
                        div(style="margin: 0 auto; text-align:left;",
                 tags$div(HTML('After you have drawn and / or deleted your CAMs you can continue with the
                                next part:')),
                      actionButton(ns("continueDrawnPreprocessingAnalysis"),  HTML('Continue'), style="width: 150px;
                                   height: 90px; font-size: 18px; padding: 10px")
                                   ))
              ) #tags.div()
			      )
          )

        # enable InputValidator for the drawing settings (drawCAM_setting_relVertices and Edges)
        iv$enable()
        })

        #> Server
        ## reactive values (to track deleted CAMs; counter)
        rv <- reactiveValues(deleted = NULL, diffdeleted = NULL, counter = 1L, networkIndicators = NULL, CAMsdrawn = FALSE)

        #rv <- reactiveValues(deleted = globals$protocol$deletedCAMs, 
        #diffdeleted = setdiff(globals$protocol$currentCAMs , globals$protocol$deletedCAMs), counter = 1L, networkIndicators = NULL, CAMsdrawn = FALSE)


        ## draw CAMs
        CAMs_drawnR <- eventReactive(input$clickDrawR, {
               req(dataCAM())
               req(iv$is_valid())

          # show div container that is initially hidden
          runjs("document.getElementById('drawnCAMsDiv').style.visibility = 'visible'")

          if (rv$counter == 1) {
            tmp_CAMs <- draw_CAM(
              dat_merged = dataCAM()[[3]],
              dat_nodes = dataCAM()[[1]], ids_CAMs = "all", plot_CAM = FALSE,
              relvertexsize = input$drawCAM_setting_relVertices,
              reledgesize = input$drawCAM_setting_relEdges
            )
          } else if (rv$counter >= 2) {
            if (!is.null(rv$diffdeleted)) {
              tmp_CAMs <- draw_CAM(
                dat_merged = dataCAM()[[3]],
                dat_nodes = dataCAM()[[1]], ids_CAMs = rv$diffdeleted,
                plot_CAM = FALSE,
              relvertexsize = input$drawCAM_setting_relVertices,
              reledgesize = input$drawCAM_setting_relEdges
              )
            } else {
              tmp_CAMs <- draw_CAM(
                dat_merged = dataCAM()[[3]],
                dat_nodes = dataCAM()[[1]], ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = input$drawCAM_setting_relVertices,
              reledgesize = input$drawCAM_setting_relEdges
              )
            }
          }
          rv$counter <- rv$counter + 1L

          ## globals ##
          if(length(tmp_CAMs) > 0){
            message("successfully drawn CAMs!")
            #> change condition
            globals$condition <- c(globals$condition, "drawnCAMs")
            #> possible to continue with preprocessing step
            rv$CAMsdrawn <- TRUE

            # keep list of all CAM IDs drawn
            globals$protocol$currentCAMs <- names(tmp_CAMs)
          }

          if(length(rv$deleted) > 0){
            tmp <- rv$deleted[!rv$deleted %in% globals$protocol$deletedCAMs] # add only CAMs which have not been yet added to the protocol
            globals$protocol$deletedCAMs <- c(globals$protocol$deletedCAMs, tmp)
            # globals$protocol$deletedCAMs[[length(globals$protocol$deletedCAMs)+ 1]] <- rv$deleted


            # only keep CAM in currentCAMs list which have not been deleted
            globals$protocol$currentCAMs <- globals$protocol$currentCAMs[!globals$protocol$currentCAMs %in% rv$deleted]
          }
          ####

          if(length(tmp_CAMs) > 0) {
            rv$networkIndicators <- compute_indicatorsCAM(drawn_CAM = tmp_CAMs,
                                  micro_degree = NULL,
                                  micro_valence = NULL,
                                  micro_centr_clo = NULL,
                                  largestClique = FALSE)
            rv$networkIndicators$assortativity_valence_macro[is.na(rv$networkIndicators$assortativity_valence_macro)] <- 0

            tmp_CAMs
          } else {
            NULL
          }
        })


        ## summary stats:
        output$numCAMsDrawnR <- renderText({
          req(CAMs_drawnR())

          length(CAMs_drawnR())
        })

        ## choices CAMs for selectInput
        CAM_choicesR <- reactive({
          req(CAMs_drawnR())

          ## set participantCAM as default ID if unique and all IDs are provided
          if(length(unique(rv$networkIndicators$participantCAM)) == nrow(rv$networkIndicators) & 
          !any(rv$networkIndicators$participantCAM == "NO ID PROVIDED")){
            print("== participantCAM")
            rv$networkIndicators$CAM_ID_use <- rv$networkIndicators$participantCAM
          }else{
            print("== CAM_ID")
            rv$networkIndicators$CAM_ID_use <- rv$networkIndicators$CAM_ID
          }

          print(input$orderCAMR)
          if(input$orderCAMR == "# of nodes"){
            print("# of nodes")
            rv$networkIndicators$CAM_ID_use[order(rv$networkIndicators$num_nodes_macro)]
          }else if(input$orderCAMR == "# of connectors"){
            print("# of connectors")
            rv$networkIndicators$CAM_ID_use[order(rv$networkIndicators$num_edges_macro)]
          }else if(input$orderCAMR == "density"){
            print("density")
            rv$networkIndicators$CAM_ID_use[order(rv$networkIndicators$density_macro)]
          }else if(input$orderCAMR == "assortativity"){
            print("assortativity")
            rv$networkIndicators$CAM_ID_use[order(rv$networkIndicators$assortativity_valence_macro)]
          }else{  # default
            print("mean valence")
            rv$networkIndicators$CAM_ID_use[order(rv$networkIndicators$mean_valence_macro)]
          }
        })

        output$selectCAMR <- renderUI({
          selectInput(ns("CAMs"),
                      "Which CAM would you like to draw?",
                      choices = as.list(CAM_choicesR()), width = "80%"
          )
        })
        observeEvent(input$prevCAMR, {
          current <- which(CAM_choicesR() == input$CAMs)
          if (current > 1) {
            updateSelectInput(session, "CAMs",
                              choices = as.list(CAM_choicesR()),
                              selected = CAM_choicesR()[current - 1]
            )
          }
        })
        observeEvent(input$nextCAMR, {
          current <- which(CAM_choicesR() == input$CAMs)
          if (current < length(CAM_choicesR())) {
            updateSelectInput(session, "CAMs",
                              choices = as.list(CAM_choicesR()),
                              selected = CAM_choicesR()[current + 1]
            )
          }
        })



        ## plot CAM
        output$plotR <- renderPlot({
          req(input$CAMs)
          req(CAMs_drawnR())

          if(input$drawCAM_setting_position == "Yes"){
          plot.igraph(CAMs_drawnR()[[input$CAMs]],
                      edge.arrow.size = .7,
                      layout=cbind(V(CAMs_drawnR()[[input$CAMs]])$xPos, V(CAMs_drawnR()[[input$CAMs]])$yPos),
                      vertex.frame.color = "black", asp = .5,
                      margin = 0, vertex.label.cex = .7
          )
          }else{
          plot.igraph(CAMs_drawnR()[[input$CAMs]],
                      edge.arrow.size = .7,
                      layout = layout_nicely, vertex.frame.color = "black", asp = .5,
                      margin = 0, vertex.label.cex = .7
          )
          }

        })



        ## delete CAM:
        observeEvent(input$deleteCAMR, {
          req(input$CAMs)
          if (input$CAMs %in% rv$deleted) {
            rv$deleted <- setdiff(rv$deleted, input$CAMs)
            rv$diffdeleted <- c(rv$diffdeleted, input$CAMs)
          } else {
            rv$deleted <- unique(c(rv$deleted, input$CAMs))
            rv$diffdeleted <- setdiff(CAM_choicesR(), rv$deleted)
          }

          #print(input$CAMs)
          #print(rv$deleted)
          #print(rv$diffdeleted)
        })



        ## show when CAM was deleted:
        output$textR <- renderUI({
          req(input$CAMs)
          if (input$CAMs %in% rv$deleted) {
            HTML('<b style="color:red;">deleted CAM</b> (<i>draw CAMs again to see result</i>)')
          } else {
            HTML('<i style="font-size: 6px;">not deleted</i>')
          }
        })



        ## table
        output$tableR <- renderTable({
          req(input$CAMs)
          req(CAMs_drawnR())
          tmp <- matrix(data = NA, nrow = 5, ncol = 2)
          tmp[1, 1] <- "mean valence"
          tmp[2, 1] <- "# of nodes"
          tmp[3, 1] <- "# of connectors"
          tmp[4, 1] <- "density"
          tmp[5, 1] <- "assortativity"

          tmp_value <- V(CAMs_drawnR()[[input$CAMs]])$value
          tmp_value[tmp_value == 10] <- 0
          tmp[1, 2] <- round(x = mean(tmp_value), digits = 2)
          tmp[2, 2] <- igraph::gorder(graph = CAMs_drawnR()[[input$CAMs]]) # nodes
          tmp[3, 2] <- igraph::gsize(graph = CAMs_drawnR()[[input$CAMs]])
          tmp[4, 2] <- round(x = igraph::graph.density(graph = CAMs_drawnR()[[input$CAMs]]), digits = 2)

          tmp_assortativity <- round(x = igraph::assortativity(graph = CAMs_drawnR()[[input$CAMs]], types1 =
                                                                 ifelse(test = igraph::V(CAMs_drawnR()[[input$CAMs]])$color == "green",
                                                                        yes = 3, no =
                                                                          ifelse(test = igraph::V(CAMs_drawnR()[[input$CAMs]])$color == "red", yes = 2, no = 1)), directed = FALSE)
                                     , digits = 2)
          if(is.numeric(tmp_assortativity)){
            tmp[5, 2] <- tmp_assortativity
          }else{
            tmp[5, 2] <- NA
          }


          tmp <- as.data.frame(tmp)
          colnames(tmp) <- c("indicator", "value")
          tmp
        })


        ## download CAMs as PDF
        observe({
          updateTextInput(session, "renamefileR",
                          value = paste0("CAM_", input$CAMs)
          )
        })


        output$PDFdownloadR <- downloadHandler(
          filename = function() {
            paste(input$renamefileR, ".pdf", sep = "")
          },
          content = function(file) {
            cairo_pdf(
              filename = file,
              width = 18, height = 10, pointsize = 12, family = "sans", bg = "transparent",
              antialias = "subpixel", fallback_resolution = 300
            )


          if(input$drawCAM_setting_position == "Yes"){
          plot.igraph(CAMs_drawnR()[[input$CAMs]],
                      edge.arrow.size = .7,
                      layout=cbind(V(CAMs_drawnR()[[input$CAMs]])$xPos, V(CAMs_drawnR()[[input$CAMs]])$yPos),
                      vertex.frame.color = "black", asp = .5,
                      margin = 0, vertex.label.cex = .7,
                           main = paste0("CAM with ID: ", input$CAMs)
          )
          }else{
          plot.igraph(CAMs_drawnR()[[input$CAMs]],
                      edge.arrow.size = .7,
                      layout = layout_nicely, vertex.frame.color = "black", asp = .5,
                      margin = 0, vertex.label.cex = .7,
                           main = paste0("CAM with ID: ", input$CAMs)
          )
          }
            dev.off()
          },
          contentType = "application/pdf"
        )



        ################
        # switch to
        ################
        ## switch to
        #> start preprocessing
        observeEvent(input$continueDrawnPreprocessingAnalysis, {
          #req(data())
          if(!rv$CAMsdrawn){
            showModal(modalDialog(
              title = "No CAMs drawn",
              paste0("Please drawn and / or delete CAMs to continue."),
              easyClose = TRUE,
              footer = tagList(
                modalButton("Ok")
              )
            ))
          }else{
            ## initalize dataCAMsummarized
             globals$dataCAMsummarized <- dataCAM()
            ## set text_summarized variable
            if(any(colnames(dataCAM()[[1]]) == "text_summarized")){
             globals$dataCAMsummarized[[1]]$text_summarized <- dataCAM()[[1]]$text_summarized
            }else{
              globals$dataCAMsummarized[[1]]$text_summarized <- dataCAM()[[1]]$text
            }
      # Encoding(x = globals$dataCAMsummarized[[1]]$text_summarized) <- "latin1"


            ## remove empty and deleted CAMs
          if(length(unique(rv$networkIndicators$participantCAM)) == nrow(rv$networkIndicators) & 
          !any(rv$networkIndicators$participantCAM == "NO ID PROVIDED")){
            print("== participantCAM delete")
            globals$dataCAMsummarized[[1]] <-
              globals$dataCAMsummarized[[1]][globals$dataCAMsummarized[[1]]$participantCAM %in% names(CAMs_drawnR()), ]
            globals$dataCAMsummarized[[2]] <-
              globals$dataCAMsummarized[[2]][globals$dataCAMsummarized[[2]]$participantCAM %in% names(CAMs_drawnR()), ]
            globals$dataCAMsummarized[[3]] <-
              globals$dataCAMsummarized[[3]][globals$dataCAMsummarized[[3]]$participantCAM.x %in% names(CAMs_drawnR()), ] 
          }else{
            print("== CAM_ID delete")
            globals$dataCAMsummarized[[1]] <-
              globals$dataCAMsummarized[[1]][globals$dataCAMsummarized[[1]]$CAM %in% names(CAMs_drawnR()), ]
            globals$dataCAMsummarized[[2]] <-
              globals$dataCAMsummarized[[2]][globals$dataCAMsummarized[[2]]$CAM %in% names(CAMs_drawnR()), ]
            globals$dataCAMsummarized[[3]] <-
              globals$dataCAMsummarized[[3]][globals$dataCAMsummarized[[3]]$CAM.x %in% names(CAMs_drawnR()), ] 
          }
 

            #print("empty CAMs:")
            #print(dataCAM()[[1]]$CAM[!dataCAM()[[1]]$CAM %in% names(CAMs_drawnR())])
            #print(length(unique(dataCAM()[[1]]$CAM[!dataCAM()[[1]]$CAM %in% names(CAMs_drawnR())])))
            #print("nrow draw CAMs:")
            #print(nrow(dataCAM()[[1]]))
            #print(nrow(globals$dataCAMsummarized[[1]]))
 
            if(globals$clickedButton == "startPreprocessing"){
            shinyjs::disable(selector = '.navbar-nav a[data-value="draw CAM"')

            shinyjs::enable(selector = '.navbar-nav a[data-value="summarize terms"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="non-summarized terms"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="reliability"')



            showTab(inputId = "tabs", target = "summarize terms", select = TRUE, session = parent)
            showTab(inputId = "tabs", target = "non-summarized terms", select = FALSE, session = parent)
            showTab(inputId = "tabs", target = "reliability", select = FALSE, session = parent)


            }else if(globals$clickedButton == "startAnalysis"){
            shinyjs::disable(selector = '.navbar-nav a[data-value="draw CAM"')
            
            shinyjs::enable(selector = '.navbar-nav a[data-value="network indicators"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="word outputs"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="aggregate CAMs"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="clustering CAMs"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="slice CAMs"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="get report"')


            showTab(inputId = "tabs", target = "network indicators", select = TRUE, session = parent)
            showTab(inputId = "tabs", target = "word outputs", select = FALSE, session = parent)
            showTab(inputId = "tabs", target = "aggregate CAMs", select = FALSE, session = parent)
            showTab(inputId = "tabs", target = "clustering CAMs", select = FALSE, session = parent)
            showTab(inputId = "tabs", target = "slice CAMs", select = FALSE, session = parent)
            showTab(inputId = "tabs", target = "get report", select = FALSE, session = parent)
            }
          }
        })


        ###### drawCAMJS
        observeEvent(input$drawCAMJS, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Draw CAMs using JavaScript (programming language)"),
            tags$br(),
            HTML('<i>to be implemented</i>')
          )
        })



        ###### information
        observeEvent(input$informationDraw, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
             tags$ul(
              tags$li(HTML('<b>Draw R:</b> using the igraph package the CAMs are visualized. It is possible to draw the concepts at the same positions as those of the participants 
              ("Yes" that positions should be considered), else a so called force-directed graph drawing algorithm is applied. Furthermore, aesthetics like the relative size of the 
              concepts (vertices) and of the edges (connectors) can be adjusted.')),
              tags$ul(
                tags$li(HTML('If you want to <b>delete single CAMs</b> this is possible by marking all the CAMs, which should be deleted, with the button "un/delete CAM". 
                It is recommended to save these CAMs as PDFs to upload them for example on OSF for transparency.')),
                tags$li(HTML('The drawn <b>CAMs could be ordered</b> according to the mean valence, number of concepts / connectors, density or assortativity. 
                These statistics are shown next to the individual plotted CAM.'))
              ),
              tags$li(HTML('<b>Draw JS:</b> <i>to be implemented</i>'))
            )
          )
          )
        })


    return(CAMs_drawnR)
    })
}
