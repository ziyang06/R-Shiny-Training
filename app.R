library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(DT)
library(rsconnect)

# Define the UI
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims Projection"),
# Design for side bar panel (left side)
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload .xlsx file", accept = c(".xlsx")),
      br(),
      numericInput("tail_factor", "Tail Factor", value = 0, step = 0.1),
      actionButton("projection_button", "Start Projection")
      ),
    
#Design for main panel (in middle)
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Paid Claims Table", dataTableOutput("table")),
        tabPanel("Cumulative Paid Claims Graph", plotOutput("graph"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
# Create reactive for loss years from reading the input file
  loss_year <- reactive({
    req(input$file)
    claims_data <- read_xlsx(input$file$datapath) 
    unique_loss_year <- unique(claims_data$"Loss Year")
    return(unique_loss_year)
  })
  
# Create eventReactive for projection when button is click
  projection_data <- eventReactive(input$projection_button, {
    req(input$file)
    claims_data <- read_xlsx(input$file$datapath)

  # Restructure table for cumulative paid claims    
    paid_claims <- matrix(0, 
                          nrow = length(loss_year()), 
                          ncol = length(loss_year()) + 1)
    
    cumulative_claims <- matrix(0, 
                                nrow = length(loss_year()), 
                                ncol = length(loss_year()) + 1)
    
    for (i in 1:length(loss_year())) {
      DY <- 1
      for (j in 1:nrow(claims_data)) {
        if (claims_data$'Loss Year'[j] == loss_year()[i]) {
          paid_claims[i, DY] <- claims_data$'Amount of Claims Paid ($)'[j]
          DY <- DY + 1
        } else {
          DY <- 1
        }
      }

  # Transform paid claims to cumulative paid claims     
      for (j in 1:length(loss_year())) {
        if (paid_claims[i, j] != 0) {
          if (j == 1) {
            cumulative_claims[i, j] <- paid_claims[i, j]
          } else {
            cumulative_claims[i, j] <- sum(paid_claims[i, 1:j])
          }
        }
      }
    }
    
  # Calculate development factor for claims projection
    factor <- matrix(1, nrow = 1, ncol = length(loss_year()) + 1)
    for (i in 1:length(loss_year())) {
      df1 <- 0
      df2 <- 0
      if (i != 1) {
        df1 <- sum(cumulative_claims[1:(length(loss_year()) - i + 1), i - 1])
        df2 <- sum(cumulative_claims[1:(length(loss_year()) - i + 1), i])
        factor[1, i] <- df2 / df1
      }
    }
   
  # Assign the development factor for last development year to be Tail Factor input 
    factor[1, length(loss_year()) + 1] <- input$tail_factor

  # Perform projection    
    projected_claims <- cumulative_claims
    for (i in 1:length(loss_year())) {
      for (j in 1:length(loss_year()) + 1) {
        if (projected_claims[i, j] == 0) {
          projected_claims[i, j] <- projected_claims[i, j - 1] * factor[j]
        }
      }
    }
    round(projected_claims, 0)
  })
    
# Show output in table
  output$table <- renderDataTable({
    table <- as.data.frame(projection_data())
    DYlist <- c()
    for (i in 1:(length(loss_year()) + 1)){
      DYlist <- append(DYlist, 
                              paste("Develpoment Year ", i, sep = ""))
    }
    table <- cbind(loss_year(), table)
    colnames(table) <- c("Loss Year", DYlist)
    table
  })
  
# Show output in graph
  output$graph <- renderPlot({
    graph_data <- as.data.frame(projection_data())
    
    DYlist1 <- c()
    for (i in (1:(length(loss_year()) + 1))){
      DYlist1 <- append(DYlist1, i)
    }

# Show output in graph
    # Prepare data
    graph_data <- rbind(DYlist1, graph_data)
    transposed_graph_data <- as.data.frame(t(graph_data))
    colnames(transposed_graph_data) <- c("Development Year", loss_year())

    # Fill in lines
    graph <- ggplot() + labs(x = "Development Year", y = "Cumulative Claims ($)")
    for (i in 1:length(loss_year())){
      map1 <- aes_string(x = transposed_graph_data[,1],
                        y = transposed_graph_data[,(i+1)],
                        color = factor(loss_year()[i]))
      graph <- graph + geom_smooth(map1,
                           method = "loess",
                           se = FALSE,
                           linewidth = 0.5)
      graph <- graph + geom_text(transposed_graph_data,
                         mapping = map1,
                         label = paste(transposed_graph_data[,(i+1)]),
                         size = 3,
                         vjust = -1,
                         show.legend = FALSE)
    }
    graph
  })
}

# Run the Shiny app
shinyApp(ui, server)
