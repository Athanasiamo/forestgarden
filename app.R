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
            column(12,
                   tags$h1("Nitrogen needs of a garden"),
                   tags$p("If you want a garden that more or less sustains itself, 
                   you will need to think about the necessary nutrients your plants need.
                   Trees and shrubs need nitrogen to grow, be healthy, and bear fruit.
                   ")),
            infoBox(title = "Tree area", htmlOutput("treeArea"), width = 6,
                    fill=TRUE, color = "green", icon = icon("tree")),
            infoBox(title = "Nitrogen need", textOutput("nitro"), width = 6,
                    fill=TRUE,color = "yellow", icon = icon("atom")),
            column(12,
                   tags$h2("Your garden"),
                   tags$p("Below you can set the number of trees and shrubs in
                          your garden. This will create a list for you to fill out
                          for each tree or shrub, indicating the radius of the 
                          trees fully grown crown, and its nitrogen need.
                          Once you have filled out the list, you can scroll back to the top,
                          where the tree crown area and nitrogen need for your entire garden
                          will have been calculated."),
                   tags$p("Since different types of trees require different amounts of nitrogen,
                          there are three tiers of nitrogen need you may indicate. You should
                          only here add trees that require nitrogen, not your trees and shrubs 
                          that are nitrogen fixators them selves."),
                   tags$p("These are rules-of-thumb and you might have trees or plants that require
                          more or less than we here allow you to set. The final calculation
                          should give you an idea of what your gardens need is as a whole."),
                   tags$ul(tags$b("Nitrogen tiers"),
                           tags$li("Low - Non-fruit bearing - 4g/m", tags$sup("2")),
                           tags$li("Medium - Fruit bearing - 8g/m", tags$sup("2")),
                           tags$li("High - Annual vegetables - 15g/m", tags$sup("2")))
                   ),
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
                                        "High"), 
                            selected = "Medium")
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