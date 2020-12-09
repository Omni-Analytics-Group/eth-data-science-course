library(shiny)
library(ggfortify)

#import csv file
summary <- read.csv("dataforshiny/summary.csv", header=T)
#change column names of summary statistcs of features
names(summary)[5:11] <- c("Mean", "Standard Deviation", "Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
#create category in summary table
summary$category<-ifelse(startsWith(summary$skim_variable, 'feature_intelligence') ,'Intelligence',ifelse(startsWith(summary$skim_variable, 'feature_charisma'),'Charisma', ifelse(startsWith(summary$skim_variable, 'feature_strength'), 'Strength', ifelse(startsWith(summary$skim_variable, 'feature_dexterity'), 'Dexterity', ifelse(startsWith(summary$skim_variable, 'feature_constitution'),'Constitution', 'Wisdom')))))


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Summary Statistics"),
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      #variable input selection using the column names of summary
      varSelectInput("sum_stat", "Summary Statistics:", summary[5:11]),
      checkboxInput("category", "Category:", FALSE)
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
    if (input$category){ggplot(summary, aes_string(x=input$sum_stat, fill = "category")) + 
        geom_histogram(color="grey60")+
        labs(title=paste0("Distribution of ", input$sum_stat),x=input$sum_stat, y = "Frequency") +
        facet_wrap(~category) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1))
      
    } else {
      ggplot(summary, aes_string(x=input$sum_stat)) + 
        geom_histogram(fill = "#EA5600", color="grey")+
        labs(title=paste0("Distribution of ", input$sum_stat),x=input$sum_stat, y = "Frequency")        
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)