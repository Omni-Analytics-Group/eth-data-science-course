library(shiny)
library(ggfortify)

#import csv file
training_data <- read.csv("dataforshiny/sample-numerai-training-data.csv", header=T)


# Define UI for application
ui <- fluidPage(
  tabsetPanel(
              tabPanel("Data", 
                  sidebarPanel(
                    HTML(" <p>The first three columns are as follow: </p>
              <ul>
              <li> 'id' - an unique identifier of each row </li>
              <li> 'era' - a time period corresponding to a trading day </li>
              <li> 'data_type' - indication of whether the row is 
              part of train/test/validation/live </li>
              </ul>
                                
              <p> These three columns are then followed by 310 features columns and 
              the last column for the 'Nomi' target. </p> ")
                  ),
                       
                  mainPanel(
                    dataTableOutput("training")
                       )
                       ),
              tabPanel("Distribution", 
                       sidebarPanel(
                         varSelectInput("var", "Variable:", training_data[4:314])
                       ),
                       
                       mainPanel(
                         plotOutput("distribution1"),
                         plotOutput("distribution2")
                       )
              )
  )
)

# Define server logic
server <- function(input, output) {
  
  #render table
  output$training <- renderDataTable({
    datatable(data=training_data, style = 'bootstrap', 
              class = 'table-bordered',options = list(scrollX = TRUE))
  })
  
  #render plot1
  output$distribution1 <- renderPlot({
    ggplot(training_data, aes_string(x=input$var))+
      geom_bar(fill = "#EA5600", color="grey")+ theme(axis.title.x = element_blank())+
      scale_x_continuous(breaks = seq(0, 1, by = 0.25)) 
  })

  #render plot2
  output$distribution2 <- renderPlot({
    training_data$target_factor <- as.factor(training_data$target)
    if(input$var != "target"){
      ggplot(training_data, aes_string(x=input$var, fill="target_factor"))+
        geom_bar()+ theme(axis.title.x = element_blank())+
        scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
        facet_wrap(~target_factor)
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)