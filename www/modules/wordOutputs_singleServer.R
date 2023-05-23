wordOutputs_singleServer <-
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
        h1("Word Outputs Module - single"),
        tags$br(),
        tags$br(),
        HTML(
          'To start the module please click on one of the the module option on the sidebar panel. The options for
                     this module are the following:'
        ),
              tags$ul(
              tags$li(HTML('<b>get table, pie chart:</b> Create a table (APA 7 format) and pie chart for every summarized superordinate concept in your data set seperately')),
              tags$li(HTML('<b>get summary statistics:</b> Get summary statistics (e.g. percantage of time word appears in CAMs) for every summarized superordinate concept in your data set seperately'))
            )
      ))

      ## set output
      output$uploadOutWordsSingle <- renderUI({
        outUI$elements
      })

      ################################
      # single module options
      ################################
        ###### ...
        #> UI
        observeEvent(input$concepts_single, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("get graphics and summary statistics for concept by concept"),
          tags$br(),
          tags$div(HTML("After choosen your concept you want to get summary statistics and your settings 
          (in most cases the default is recommended), please click on the get overview button. 
          Please click only once and wait few seconds:"), style="font-size:14px"),
            tags$h3("Your Settings:"),
          uiOutput(ns("selectSingleWord")),
                               tags$div(
            HTML(
              'Please select the minimum frequency a single concept has been drawn in the CAMs to be plotted in the pie chart and barplot / table 
              (at least for the pie chart it is recommended not to show too many different concepts):'
            ),
            style = "font-size:14px"
          ),
          div(
            style = "margin: 0 auto; width: 100%; text-align:left;",
            div(
              style = "display: inline-block; vertical-align: top; width: 35%; padding:10px;",
			  # minimum frequency pie charts
                       tags$div(HTML("Please specify the minimum frequency a single concept has been drawn in the CAMs to be plotted in the pie chart:"), style="font-size:14px"),
 numericInput(ns("minFreq_pieChart"), label = NULL, value = 1, min = 1, max = Inf, step = 1, width = "150px"),
            ),
      div(
              style = "display: inline-block; vertical-align: top; width: 35%; padding:10px;",
			  # minimum frequency table
		                     tags$div(HTML("Please specify the minimum frequency a single concept has been drawn in the CAMs to be listed in the barplot / table:"), style="font-size:14px"),
 numericInput(ns("minFreq_barplotTable"), label = NULL, value = 1, min = 1, max = Inf, step = 1, width = "150px"),
            ),
          ),
          tags$h3("get graphics and summary statistics:"),
          actionButton(ns("clickConceptSingle"), "get overview"),
          tags$div(HTML("On the left the pie chart and on the right the barplot for your choosen summarized concept 
          considering your settings is shown:"), style="font-size:14px"),
          fluidRow(
          column(5,plotOutput(ns("plotPie_concepts_single"), width="90%")),  
          column(5,plotOutput(ns("plotBarplot_concepts_single"), width="90%"))
        ),
                  tags$div(HTML("A table containing all drawn concepts, which were summarized to your choosen summarized concept:"), style="font-size:14px"),
                  dataTableOutput(ns("table_concepts_single")),
          )
        })


        #> Server
        ## choices CAMs for selectInput
        uniqueConcepts_wordOutputs <- reactive({
          req(dataCAM())

         tmp_text <- str_remove_all(string = globals$dataCAMsummarized[[1]]$text_summarized, 
         pattern = "_positive$|_negative$|_neutral$|_ambivalent$")

          sort(unique(tmp_text))
        })


        output$selectSingleWord <- renderUI({
          selectInput(ns("searchTerm_conceptsSingle"),
                      "For which summarized concept do you want to get an overview?",
                      choices = as.list(uniqueConcepts_wordOutputs()), 
                      width = "40%",   multiple = FALSE
          )
        })


      ## compute ...
    observeEvent(input$clickConceptSingle, {

################
# get APA table
################
searchTerm <- input$searchTerm_conceptsSingle

tmp_nodes <- globals$dataCAMsummarized[[1]]
tmp_nodes$text_summarized <- str_remove(string = tmp_nodes$text_summarized,
                                        pattern = "_neutral|_positive|_negative|_ambivalent")
print(sort(table(tmp_nodes$text_summarized)))
out_vec <- tmp_nodes$text[tmp_nodes$text_summarized == searchTerm]

## create APA table
outAPA <- table(out_vec)[sort(names(table(out_vec)))]
matAPA <- matrix(data = NA, nrow = length(outAPA), ncol = 3)

matAPA[1,1] <- searchTerm

for(c in 1:length(outAPA)){
  matAPA[c,2] <- outAPA[c]
  matAPA[c,3] <- names(outAPA)[c]
}

matAPA <- as.data.frame(matAPA)
colnames(matAPA) <- c("superordinate", "frequency", "concept")
matAPA$frequency <- as.numeric(matAPA$frequency)

matAPA[,1] <- searchTerm # matAPA[1,1] <- searchTerm



################
# get pie chart
################
matAPA_pie <- matAPA[matAPA$frequency >= input$minFreq_pieChart,]

        output$plotPie_concepts_single <- renderPlot({
          out_vec_pie <- out_vec[out_vec %in% matAPA_pie$concept]
          pie(x = table(out_vec_pie), col = rainbow(length(table(out_vec_pie))))
        })

################
# get barplot
################
matAPA_barplotTable <- matAPA[matAPA$frequency >= input$minFreq_barplotTable,]
tmp_occurences <- data.frame(CAM = unique(tmp_nodes$CAM), occur = 0)

for(i in 1:nrow(tmp_occurences)){
  tmp_dat <- tmp_nodes[tmp_nodes$CAM %in% tmp_occurences$CAM[i], ]
  tmp_occurences$occur[i] <- sum(tmp_dat$text %in% matAPA_barplotTable$concept)
}

        output$plotBarplot_concepts_single <- renderPlot({
fg <- tmp_occurences %>%
  count(occur)  %>%
  mutate(
    perc = round(proportions(n) * 100, 0),
    res = str_c(n, " (", perc, "%)"),
    group = as.factor(occur)
  )

ggplot(fg, aes(group, n, fill = group)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5) +
  ggplot_theme + # from helper functions
  theme(legend.position = "none") +
  labs(x ="Summarized concept occur x-times in CAMs", y = "N")
        })


################
# get table
################
 output$table_concepts_single <- renderDataTable({
    matAPA
  })

    })
       ###### ...
        #> UI
        observeEvent(input$concepts_overview, {

          ## change UI
          outUI$elements <- tagList(
          tags$h2("get summary statistics for all concepts"),
          tags$br(),
          tags$div(HTML("Please click on the get complete overview button. 
          Please click only once and wait few seconds:"), style="font-size:14px"),
          actionButton(ns("clickConceptOverall"), "get complete overview"),
                    tags$br(),
          tags$div(HTML("A table containing all unique summarized concepts and their respective frequencies (seperated by N=total, 
          Npositive=positive, and so on) seperated by CAMs:"), style="font-size:14px"),
          dataTableOutput(ns("table_concepts_overall")),


          )
        })


        #> Server
              ## compute ...
    observeEvent(input$clickConceptOverall, {

################
# get data set
################
## remove suffix
globals$dataCAMsummarized[[1]]$text_summarized_suffremoved <- str_remove(string = globals$dataCAMsummarized[[1]]$text_summarized, 
pattern = "_neutral|_positive|_negative|_ambivalent")

## ID vars
conceptOverall_dat <- data.frame(CAM = unique(globals$dataCAMsummarized[[1]]$CAM), 
participantCAM = unique(globals$dataCAMsummarized[[1]]$participantCAM))
## word vars
for(w in unique(globals$dataCAMsummarized[[1]]$text_summarized_suffremoved)){
  varName_w <- str_remove_all(string = str_to_title(string = w, locale = "en"), pattern = " |\\W+")

  conceptOverall_dat[[paste0("N_", varName_w)]] <- NA
  conceptOverall_dat[[paste0("Npositive_", varName_w)]] <- NA
  conceptOverall_dat[[paste0("Nnegative_", varName_w)]] <- NA
  conceptOverall_dat[[paste0("Nneutral_", varName_w)]] <- NA
  conceptOverall_dat[[paste0("Nambivalent_", varName_w)]] <- NA
}

#############
## get frequencies for different N
verbose = FALSE

for(c in unique(globals$dataCAMsummarized[[1]]$CAM)){
  if(verbose){
      cat("considered CAM: ", c, "\n")
  }
  tmp_CAM_nodes <- globals$dataCAMsummarized[[1]][globals$dataCAMsummarized[[1]]$CAM == c, ]

  for(w in unique(globals$dataCAMsummarized[[1]]$text_summarized_suffremoved)){
      if(verbose){
    cat("considered concept: ", w, "\n")
      }

    varName_w <- str_remove_all(string = str_to_title(string = w, locale = "en"), pattern = " |\\W+")
          if(verbose){
    cat("   > the frequencies for different N
      word without white spaces: ", varName_w, "\n")
          }

    if(sum(tmp_CAM_nodes$text_summarized_suffremoved == w) > 0){
      tmp_CAM_nodes_w <- tmp_CAM_nodes[tmp_CAM_nodes$text_summarized_suffremoved == w, ]

      ## add N
      conceptOverall_dat[conceptOverall_dat$CAM == c, paste0("N_", varName_w)] <- nrow(tmp_CAM_nodes_w)

      if(any(tmp_CAM_nodes_w$value > 0 && tmp_CAM_nodes_w$value < 10)){
        conceptOverall_dat[conceptOverall_dat$CAM == c, paste0("Npositive_", varName_w)] <- sum(tmp_CAM_nodes_w$value > 0 && tmp_CAM_nodes_w$value < 10)
      }

      if(any(tmp_CAM_nodes_w$value < 0)){
        conceptOverall_dat[conceptOverall_dat$CAM == c, paste0("Nnegative_", varName_w)] <- sum(tmp_CAM_nodes_w$value < 0)
      }
      
      if(any(tmp_CAM_nodes_w$value == 0)){
        conceptOverall_dat[conceptOverall_dat$CAM == c, paste0("Nneutral_", varName_w)] <- sum(tmp_CAM_nodes_w$value == 0)
      }
      
      if(any(tmp_CAM_nodes_w$value == 10)){
        conceptOverall_dat[conceptOverall_dat$CAM == c, paste0("Nambivalent_", varName_w)] <- sum(tmp_CAM_nodes_w$value == 10)
      }
    }
  }
        if(verbose){
  cat("\n")
        }
}
#############

################
# get table
################
 output$table_concepts_overall <- renderDataTable({
    conceptOverall_dat
  })
    })
         
        ###### information
        observeEvent(input$informationWordsSingle, {
          ## change UI
          outUI$elements <- tagList(
            tags$h2("Module Specific Information"),
            tags$div(
              HTML('The options for this module are the following:'),
              tags$ul(
                tags$li(HTML('<b>concept by concept:</b> Create a pie chart, barplot and table (APA 7 format) 
                for every summarized superordinate concept in your data set seperately')),
                tags$li(HTML('<b>overview of concepts:</b> Get summary statistics (e.g. percantage of time word appears in CAMs) for every summarized superordinate concept in your data.'))
            )
          )
          )
        })

    })
    }