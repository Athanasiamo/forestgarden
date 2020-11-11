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
            box(title = "Tree area", textOutput("treeArea")),
            box(title = "Nitrogen need", textOutput("nitro")),
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
            sliderInput(inputId = paste0("tree", i), 
                        label = paste("Tree", i), step = .5,
                        min = 0, max = 10, value = 2)
        })
    })
    
    
    output$treeArea <- renderText({
        ntrees <- as.integer(input$ntrees)
        
        area <- round(sum(
            unlist(lapply(1:ntrees, function(i) {
                pi*input[[paste0("tree", i)]]^2
            }))
        ), 2)
        
        paste(area, "mm^2")
    })
    
    output$nitro <- renderText({
        ntrees <- as.integer(input$ntrees)
        
        area <- round(sum(
            unlist(lapply(1:ntrees, function(i) {
                pi*input[[paste0("tree", i)]]^2
            }))
        ), 2)
        
        paste(area*8, "g per yearecho "# forestgarden" >> README.md
              git init
              git add README.md
              git commit -m "first commit"
              git branch -M main
              git remote add origin https://github.com/Athanasiamo/forestgarden.git
              git push -u origin main")
    })
}

shinyApp(ui, server)