library(shiny)
library(tidyverse)
library(ggrepel)
library(shinythemes)
library(shinyjs)

ln <- read_csv("Investment_Bubble_Cycle_Line.csv") %>%
    mutate(Y = Y + 50 + 28)
cv <- read_csv("Investment_Bubble_Cycle.csv") %>%
    mutate(Y = Y + 50)

f2 <- function(x){
    
    # check if all are NA to skip loop
    if(!all(is.na(x))){
        
        # replace NA's until they are gone
        while(anyNA(x)){
            
            # replace from the left
            x[is.na(x)] <- c(NA,x[1:(length(x)-1)])[is.na(x)]
            
            # replace from the right
            x[is.na(x)] <- c(x[-1],NA)[is.na(x)]
        }
    }
    
    # return original or fixed x
    x
}

mapping = tibble(
    X = cv$X[c(20, 50, 70, 75, 105, 125, 138, 150, 175, 165, 185, 208, 225, 255, 280)],
    Label = c("Take off", "First Sell off", "Bear trap", "Media attention", "Enthusiasm",
              "Greed", "Delusion", "\"New Paradigm\"!!!", "Bull trap", "Denial",
              "Return to \"normal\"", "Fear", "Capitulation", "Despair", "Return to\nthe mean")
) %>%
    left_join(cv) %>%
    mutate(OrigX = X) %>%
    mutate(Y = ifelse(Label %in% c("Bear trap"), Y - 60, Y)) %>%
    mutate(Y = ifelse(Label %in% c("Media attention"), Y + 30, Y),
           X = ifelse(Label %in% c("Enthusiasm"), X - 60, X),
           X = ifelse(Label %in% c("Greed"), X - 30, X),
           X = ifelse(Label %in% c("Delusion"), X - 45, X),
           X = ifelse(Label %in% c("Denial"), X + 40, X),
           X = ifelse(Label %in% c("Return to \"normal\""), X + 90, X),
           Y = ifelse(Label %in% c("\"New Paradigm\"!!!"), Y + 10, Y),
           Y = ifelse(Label %in% c("Return to\nthe mean"), Y - 30, Y),
           X = ifelse(Label %in% c("Fear"), X + 25, X),
           X = ifelse(Label %in% c("Capitulation"), X + 45, X),
           Y = ifelse(Label %in% c("Despair"), Y + 40, Y),
           X = ifelse(Label %in% c("Despair"), X + 20, X),
           Y = ifelse(Label %in% c("Bull trap"), Y - 40, Y))

verticals <- tibble(
    X = cv$X[c(20, 70, 150)],
)

sub_labels <- tibble(
    X = cv$X[c(8, 45, 92, 250)],
    lbl = c("Stealth Phase", "Awareness Phase", "Mania Phase", "Blow off Phase"),
    lbl2 = c("Smart Money", "Institutional Investors", "Public", NA)
)

ui <- fluidPage(theme = shinytheme("cerulean"),
                
    useShinyjs(),

    titlePanel("Investment Curve"),

    sidebarLayout(
        sidebarPanel(
            helpText("Please enter your name, then click on the plot to identify where you think we are on the curve."),
            textInput("name", "Name"),
            
            hr(),
            textOutput("info"),
            hr(),
            
            shinyjs::disabled(actionButton("submit", "Submit")),
            
            textOutput("done")
        ),

        mainPanel(
           plotOutput("curve", click = "plot_click")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    values <- reactiveValues(closest = NULL, selected = NULL, phase = NULL, label = NULL, submitted = FALSE)
    
    observeEvent(input$submit, {
        mydat <- tibble(
            name = input$name, 
            location = values$selected, 
            label = values$label, 
            phase = values$phase
        )
        
        write_csv(mydat, paste0(input$name, ".csv"))
        values$submitted <- TRUE
        shinyjs::disable("submit")
    })
    
    output$curve <- renderPlot({
        p <- ggplot(data = ln, aes(x = X, y = Y)) +
            geom_line(linetype = "dashed") +
            geom_line(data = cv, colour = "red", size = 1.5) +
            geom_text(data = mapping, aes(label = Label), vjust = -1.5, fontface = "bold", size = 3.5) +
            geom_vline(data = verticals, aes(xintercept = X), linetype = "dotted", colour = "grey80") +
            geom_text(data = sub_labels, aes(label = lbl, y = 10), fontface = "bold") +
            geom_text(data = sub_labels, aes(label = lbl2, y = 375), fontface = "bold", colour = "grey80") +
            
            theme_bw() +
            theme(
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                panel.border = element_blank(), 
                axis.line = element_line(size = 1.5, colour = "grey55", arrow = grid::arrow(length = unit(0.3, "cm"))),
                axis.title = element_text(size = 16, face = "bold", hjust = 1),
                axis.title.y = element_text(angle = 0, margin = margin(l = 20, r = -80)),
                axis.title.x = element_text(hjust = 1.08, vjust = 4.5),
                plot.margin=unit(c(1.5,2.5,1.5,1.2),"cm"),
                panel.grid = element_blank()
            ) +
            labs(
                x = "Time",
                y = "Valuation"
            )
        
        if (!is.null(values$closest)) {
            p <- p + geom_vline(xintercept = cv$X[values$closest])
        }
        
        return(p)
    })
    
    observe({
        input$plot_click$x
        if ((nchar(input$name) > 0) && !values$submitted) {
            shinyjs::enable("submit")
        } else {
            shinyjs::disable("submit")
        }
    })
    
    output$done <- renderText({
        if (values$submitted) {
            return("Thanks for submitting!")
        }
    })
    
    output$info <- renderText({
        print(paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y))
        if (!is.null(input$plot_click$x)) {
            values$closest <- which.min(abs(cv$X - input$plot_click$x))
        }
        
        if (is.null(values$closest)) return(NULL)
        
        print(values$closest)
        
        joined <- cv %>%
            left_join(mapping) %>%
            mutate(Label = f2(Label))
        
        label_close <- which.min(abs(mapping$OrigX - cv$X[values$closest]))
        
        ## Get investment label
        inv_label <- "Stealth Phase"
        if (values$closest >= 20 && values$closest < 70) inv_label <- "Awareness Phase"
        if (values$closest >= 70 && values$closest < 150) inv_label <- "Mania Phase"
        if (values$closest >= 150) inv_label <- "Blow off Phase"
        
        values$selected <- round(cv$Y[values$closest])
        values$label <- mapping$Label[label_close]
        values$phase <- inv_label
        
        paste0("You've selected ", round(cv$Y[values$closest]), ": Closest label is ", mapping$Label[label_close], " in the ", inv_label)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
