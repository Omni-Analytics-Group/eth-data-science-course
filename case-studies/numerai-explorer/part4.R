library(shiny)
library(ggfortify)

#import csv file
correlations_by_era <- read.csv("dataforshiny/correlations.csv", header=T)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Correlations between Features and Target"),
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      #Variable input from columns 2 to 311
      varSelectInput("feature", "Feature:", correlations_by_era[2:311]),
      checkboxInput("smooth", "Smooth:", FALSE)
    ),
    # Main panel
    mainPanel(
      plotOutput("corrplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  #render plot
  output$corrplot <- renderPlot({
    
    if (input$smooth){
      ggplot(data=correlations_by_era, 
             aes_string(x="era", y=input$feature))+
        geom_line()+
        labs(title=paste0("Correlation of ", input$feature ," and the target across era"),y="Correlation")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
        scale_x_continuous(breaks = seq(0, 120, by = 5))+geom_smooth(se=FALSE)            
      
    } else {
      ggplot(data=correlations_by_era, aes_string(x="era", y=input$feature))+
        geom_line()+
        labs(title=paste0("Correlation of ", input$feature ," and the target across era"), y="Correlation")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=10))+
        scale_x_continuous(breaks = seq(0, 120, by = 5)) 
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)