library(shiny)
library(ggfortify)

#import csv file
target_mean_by_era <- read.csv("dataforshiny/target_mean_by_era.csv", header=T)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Target Across Era"),
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      #silder input from 1 to 120
      sliderInput("era1", "Era:", min = 1, max = 120, step=1, value = 1),
      textOutput("erainfo2")
    ),
    # Main panel
    mainPanel(
      plotOutput("targetmean")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  #render plot
  output$targetmean <- renderPlot({
    ggplot(data=target_mean_by_era, aes(x=era, y=Mean))+
      geom_line()+
      scale_x_continuous(breaks = seq(0, 120, by = 10))+
      labs(title = "Mean of target across eras", x="era")
  })
  
  #render text
  output$erainfo2 <- renderText({
    paste0("The mean of the target in era", input$era2, "is ", target_mean_by_era$Mean[target_mean_by_era$era==input$era2], ".")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)