## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Forestgarden dashboard"),
                    dashboardSidebar(
                        menuItem("Nitrogen calculator", tabName = "nitro-calc", icon = icon("dashboard")),
                        HTML('<br><p style="padding: 10px; font-size: 12px; color: #aaa;"><i class="fab fa-creative-commons"></i>-<i class="fab fa-creative-commons-by"></i>-<i class="fab fa-creative-commons-sa"></i> 2020 Athanasia M. Mowinckel</p>')
                    ),
                    dashboardBody(
                        tabItems(
                            # nitro-calc
                            tabItem(tabName = "nitro-calc",
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
                          tree's fully grown crown, and its nitrogen need.
                          Once you have filled out the list, you can scroll back to the top,
                          where the tree crown area and nitrogen need for your entire garden
                          will have been calculated."),
                          tags$p("Since different types of trees require different amounts of nitrogen,
                          there are three tiers of nitrogen need you may indicate. You should
                          only here add trees that require nitrogen, not your trees and shrubs 
                          that are nitrogen fixators them selves."),
                          tags$p("These are rules-of-thumb and you might have trees or plants that require
                          more or less than we here allow you to set. The final calculation
                          should give you an idea of what your garden's need is as a whole."),
                          tags$ul(tags$b("Nitrogen tiers"),
                                  tags$li("Low - Non-fruit bearing - 4g/m", tags$sup("2")),
                                  tags$li("Medium - Fruit bearing - 8g/m", tags$sup("2")),
                                  tags$li("High - Annual vegetables - 15g/m", tags$sup("2")))
                   ),
                   column(12, box(sliderInput("ntrees", "Number of trees", value = 5, 
                                   min = 1, max = 50, round = TRUE), width = 12)),
                   column(12, box(uiOutput("sliders"), width = 12))
                                    )
                            )
                        )
                    )
)

server <- function(input, output) { 
    output$sliders <- renderUI({
        ntrees <- as.integer(input$ntrees)
        lapply(1:ntrees, function(i) {
            box(title = paste("Tree", i),  width = 3,
                solidHeader = TRUE, background = "green",
                numericInput(paste0("tree_num", i), 
                             label = "Radius",
                             value = 2, step = .2),
                selectInput(paste0("tree_weight", i),
                            label = "Nitro need",
                            choices = c("Low" = 4, 
                                        "Medium" = 8, 
                                        "High" = 15), 
                            selected = "Medium")
            )
        })
    })
    
    
    treeAreas <- reactive({
        sapply(1:input$ntrees, function(i) {
            pi*input[[paste0("tree_num", i)]]^2
        })
    })
    
    treeNitro <- reactive({
        
        nit <- sapply(1:input$ntrees, function(i) {
            as.numeric(input[[paste0("tree_weight", i)]])
        })
        
        treeAreas()*nit
        
    })
    
    output$nitro <- renderText({
        paste0(round(sum(treeNitro()),2), "g per year")
    })
    
    output$treeArea <- renderText({
        paste0(round(sum(treeAreas()),2), "m", tags$sup("2"))
    })
}

shinyApp(ui, server)