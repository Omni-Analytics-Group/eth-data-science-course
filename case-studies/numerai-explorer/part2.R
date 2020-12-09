library(shiny)
library(ggfortify)

#import csv file
era_table <- read.csv("dataforshiny/era_table.csv", header=T)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Era"),
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      #silder input from 1 to 120
      sliderInput("era1", "Era:", min = 1, max = 120, step=1, value = 1),
      textOutput("erainfo1")
    ),
    # Main panel
    mainPanel(
      plotOutput("eraplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  #render plot
  output$eraplot <- renderPlot({
    ggplot(data = era_table, aes(x = era, y = n, group = 1)) +
      geom_line()+
      scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
      scale_x_continuous(breaks = seq(0, 120, by = 10))+
      labs(title="Number of rows corresponding to each era", y="Frequency")
  })
  
  #render text
  output$erainfo1 <- renderText({
    
    paste0("There are ", era_table$n[era_table$era==input$era1], " rows corresponding to era", input$era1, ".")
  }) 
}


# Run the application 
shinyApp(ui = ui, server = server)