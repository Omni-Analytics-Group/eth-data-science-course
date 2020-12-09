library(shiny)
library(shinythemes)
library(ggfortify)

#import csv file
summary <- read.csv("dataforshiny/summary.csv", header=T)
era_table <- read.csv("dataforshiny/era_table.csv", header=T)
target_mean_by_era <- read.csv("dataforshiny/target_mean_by_era.csv", header=T)
correlations_by_era <- read.csv("dataforshiny/correlations.csv", header=T)
training_data <- read.csv("dataforshiny/sample-numerai-training-data.csv", header=T)

#change column names of summary statistcs of features
names(summary)[5:11] <- c("Mean", "Standard Deviation", "Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
#create category in summary table
summary$category<-ifelse(startsWith(summary$skim_variable, 'feature_intelligence') ,'Intelligence',ifelse(startsWith(summary$skim_variable, 'feature_charisma'),'Charisma', ifelse(startsWith(summary$skim_variable, 'feature_strength'), 'Strength', ifelse(startsWith(summary$skim_variable, 'feature_dexterity'), 'Dexterity', ifelse(startsWith(summary$skim_variable, 'feature_constitution'),'Constitution', 'Wisdom')))))

#Define logo path
addResourcePath('logo.png', 'images/logo.png')

# Define UI for application
ui <-fluidPage(theme = shinytheme("cerulean"), 
               navbarPage(title=div(img(src="logo.png", height =30), 
                                    "Numerai Tournament Training Data Explorer"),
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
             tabPanel("Static Analysis", 
                      tabsetPanel(
                        tabPanel("Summary Statistics",
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
                      )),
             tabPanel("Analysis By Era", 
                      tabsetPanel(
                        tabPanel("Era",
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
                        ),
                        tabPanel("Target Across Era",
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
                        ),
                        tabPanel("Correlations between Features and Target",
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
                      ))
  )
)

# Define server logic
server <- function(input, output){

  #part 1
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

  #part 2
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
  
  #part 3
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
  
  #part 4
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
    
    #part 5
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