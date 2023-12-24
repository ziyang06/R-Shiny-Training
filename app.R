# Install and load necessary packages
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(shiny)
library(tidyverse)

# Define the UI
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims Projection"),
# Design for side bar panel (left side)
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload .xlsx file", accept = c(".xlsx")),
      br(),
      numericInput("tail_factor", "Tail Factor", value = 0, step = 0.1),
      actionButton("projection_button", "Start Projection"),
      div(downloadButton("download_table", "Download Table")),
      div(downloadButton("download_graph", "Download Graph"))
    ),
#Design for main panel (in middle)
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Paid Claims Table", datatableOutput("table")),
        tabPanel("Cumulative Paid Claims Graph", plotOutput("plot"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  claims_data <- reactiveVal(NULL)
  
# Create reactive for loss years from reading the input file
  loss_year <- reactive({
    req(input$file)
    claims_data <- read_xlsx(input$file$datapath)
    unique_loss_year <- unique(claims_data$`Loss Year`)
    return(unique_loss_year)
  })
  
# Create eventReactive for projection when button is click
  projection_data <- eventReactive(input$projection_button, {
    req(input$file)
    claims_data <- read_xlsx(input$file$datapath)

  # Restructure table for cumulative paid claims    
    paid_claims <- matrix(NULL, 
                          nrow = length(loss_years()), 
                          ncol = length(loss_years()) + 1)
    
    cumulative_claims <- matrix(NULL, 
                                nrow = length(loss_years()), 
                                ncol = length(loss_years()) + 1)
    
    for (i in 1:length(loss_years())) {
      AoP <- 1
      for (j in 1:nrow(claims_table)) {
        if (claims_table$'Loss Year'[j] == loss_years()[i]) {
          paid_claims[i, AoP] <- claims_table$'Amount of Claims Paid ($)'[j]
          AoP <- AoP + 1
        } else {
          AoP <- 1
        }
      }

  # Transform paid claims to cumulative paid claims     
      for (j in 1:length(loss_years())) {
        if (paid_claims[i, j] != 0) {
          if (j == 1) {
            cumulative_claims[i, j] <- paid_claims[i, j]
          } else {
            cumulative_claims[i, j] <- sum(paid_claims[i, 1:j])
          }
        }
      }
    }
    
  # Calculate development factor and projected claims####
    dev_factor <- matrix(1, nrow = 1, ncol = length(loss_years()) + 1)
    for (i in 1:length(loss_years())) {
      df1 <- 0
      df2 <- 0
      if (i != 1) {
        df1 <- sum(cumulative_claims[1:(length(loss_years()) + 1 - i), i - 1])
        df2 <- sum(cumulative_claims[1:(length(loss_years()) + 1 - i), i])
        dev_factor[1, i] <- df2 / df1
      }
    }
    
    dev_factor[1, length(loss_years()) + 1] <- input$tail_factor
    
    projected_claims <- cumulative_claims
    for (i in 1:length(loss_years())) {
      for (j in 1:length(loss_years()) + 1) {
        if (projected_claims[i, j] == 0) {
          projected_claims[i, j] <- projected_claims[i, j - 1] * dev_factor[j]
        }
      }
    }
    round(projected_claims, 0)
  })
    
  
  
#################################################  
  # Load uploaded CSV file
  observeEvent(input$file, {
    req(input$file)
    claims_data(read.csv(input$file$datapath))
  })
  
  # Perform calculations and display data table
  output$dataTable <- renderDataTable({
    req(claims_data())
    claims_data()
  })
  
  # Calculate reserves and display results in a table
  observeEvent(input$calculateButton, {
    req(claims_data())
    
    # Perform your calculations here
    # For example, let's calculate reserves as the square of the "Amount" column
    reserves_data <- claims_data() %>%
      mutate(Reserves = Amount^2)
    
    # Display reserves table
    output$reservesTable <- renderTable({
      reserves_data
    })
    
    # Display reserves plot
    output$reservesPlot <- renderPlot({
      ggplot(reserves_data, aes(x = Amount, y = Reserves)) +
        geom_point() +
        labs(title = "Reserves Plot", x = "Amount", y = "Reserves")
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
