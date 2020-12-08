library(shiny)
library(ggfortify)

#import csv file
summary <- read.csv("dataforshiny/summary.csv", header=T)


# Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel("Summary Statistics"),
    # Sidebar panel
    sidebarLayout(
        sidebarPanel(
            #variable input selection using the column names of summary
            varSelectInput("sum_stat", "Summary Statistics:", summary[5:11])
        ),
        # Main panel
        mainPanel(
            plotOutput("summaryplot"),
        )
    ),
    dataTableOutput("summary")
)

# Define server logic
server <- function(input, output) {
    
    #render table of summary
    output$summary <- renderDataTable({
        datatable(summary, style = 'bootstrap', class = 'table-bordered',options = list(scrollX = TRUE))
    })    

    #render plot
    output$summaryplot <- renderPlot({
    ggplot(summary, aes_string(x=input$sum_stat)) + 
        geom_histogram(fill = "#EA5600", color="grey")+
        labs(title=paste0("Distribution of ", input$sum_stat),x=input$sum_stat, y = "Frequency")
    })
}


# Run the application 
shinyApp(ui = ui, server = server)