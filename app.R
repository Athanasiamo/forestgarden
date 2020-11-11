## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "Forestgarden dashboard"),
    dashboardSidebar(
        menuItem("Nitrogen calculator", tabName = "nitro-calc", icon = icon("dashboard"))
        ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            infoBox(title = "Tree area", htmlOutput("treeArea"), 
                    fill=TRUE, icon = icon("tree")),
            infoBox(title = "Nitrogen need", textOutput("nitro"), 
                    fill=TRUE, icon = icon("atom")),
            box(sliderInput("ntrees", "Number of trees", value = 5, 
                            min = 1, max = 50, round = TRUE)),
            box(uiOutput("sliders" ))
        )
    )
)

server <- function(input, output) { 
    output$sliders <- renderUI({
        ntrees <- as.integer(input$ntrees)
        lapply(1:ntrees, function(i) {
            box(title = paste("Tree", i), 
                solidHeader = TRUE, background = "green",
                numericInput(paste0("tree_num", i), 
                            label = "Radius",
                            value = 2, step = .2),
                selectInput(paste0("tree_weight", i),
                            label = "Nitro need",
                            choices = c("Low", 
                                        "Medium", 
                                        "High"), selected = "Medium")
            )
        })
    })
    
    
    output$treeArea <- renderText({
        ntrees <- as.integer(input$ntrees)
        
        area <- round(sum(
            unlist(lapply(1:ntrees, function(i) {
                pi*input[[paste0("tree_num", i)]]^2
            }))
        ), 2)
        
        paste(area, "m", tags$sup("2"))
    })
    
    output$nitro <- renderText({
        ntrees <- as.integer(input$ntrees)

        area <- unlist(lapply(1:ntrees, function(i) {
                (pi*input[[paste0("tree_num", i)]]^2)*ifelse(input[[paste0("tree_weight", i)]] == "Low", 4,
                                                             ifelse(input[[paste0("tree_weight", i)]] == "Medium", 8, 15))
            }))
        
        paste(round(sum(area),2), "g per year")
    })
}

shinyApp(ui, server)