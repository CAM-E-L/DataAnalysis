
c <- data.frame(a = c(TRUE, FALSE, FALSE), b = c(FALSE, TRUE, FALSE))
ifelse(test = c$a == TRUE | c$b == TRUE, yes = 1, no = 0)


#################################




library(shiny)
library(sortable)
library(jquerylib)

js <- '
  $(document).ready(function(){
$(".rank-list-container").eq(0).on("click", function () {
   console.log("global stuff");
});
});
  '



ui <- fluidPage(
  uiOutput("groups"),
  tags$head(tags$script(HTML(js))),
  tags$b("Result"),
  verbatimTextOutput("results_multi")

)

server <- function(input, output, session) {

  derp = (c(1:5))

  output$groups = renderUI({bucket_list(
    header = "",

    group_name = "rank_groups",
    orientation = "horizontal",

    add_rank_list(
      text = h5("Group 1"),
      input_id = "Group1",
      labels = derp,
      options = sortable_options(
        multiDrag = TRUE,
        onMove = htmlwidgets::JS('function (evt) { console.log("by moving"); }')
        )
      ),

    add_rank_list(
      text = h5("Delete"),
      input_id = "delete",
      options = sortable_options(
        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
      )
    ))})


  output$results_multi <- renderPrint({
    input$Group1[1] # This matches the input_id of the rank list
  })



}

shinyApp(ui, server)
