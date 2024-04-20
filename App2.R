#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(highcharter)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(shinycssloaders)
library(shinyBS)
library(shinyjs)
library(readxl)

# import unique excel sheet for each team
NBABOS <- read_excel("NBA2023.xlsx", sheet = "Celtics") %>% na.omit(df)
NBANYK <- read_excel("NBA2023.xlsx", sheet = "Knicks") %>% na.omit(df)
NBABKN <- read_excel("NBA2023.xlsx", sheet = "Nets") %>% na.omit(df)
NBATOR <- read_excel("NBA2023.xlsx", sheet = "Raptors") %>% na.omit(df)
NBAPHI <- read_excel("NBA2023.xlsx", sheet = "Sixers") %>% na.omit(df)
NBAteams <- read_excel("NBA2023.xlsx", sheet = "teamNameList")

df <- data.frame(NBABOS) %>% na.omit(df)
df <- rbind(df, data.frame(NBABKN) %>% na.omit(df))
df <- rbind(df, data.frame(NBANYK) %>% na.omit(df))
df <- rbind(df, data.frame(NBAPHI) %>% na.omit(df))
df <- rbind(df, data.frame(NBATOR) %>% na.omit(df))

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "NBA Insurance"),
  dashboardSidebar(),
  dashboardBody(
    h1(paste0("2023 NBA Season")) ,
    fluidRow(
      valueBoxOutput("CelticsBox") %>% withSpinner(type=4),
      valueBoxOutput("NetsBox") %>% withSpinner(type=4),
      valueBoxOutput("KnicksBox") %>% withSpinner(type=4),
      valueBoxOutput("SixersBox") %>% withSpinner(type=4),
      valueBoxOutput("RaptorsBox") %>% withSpinner(type=4)
    ),
    # Inputs ----------------------------------------------
    selectInput(inputId = "listOfNBAteams", 
                label = "Select team:", 
                choices = unique(NBAteams$NBAteams)),
    h2(paste0("Performance over time")),
    # Output ----------------------------------------------
    fluidRow( column( width = 12, highchartOutput('BosHc')))
  )
)



# Define server logic
server <- function(input, output) {
  
  output$CelticsBox <- renderValueBox({
    set_color <- ifelse(last(NBABOS$x100) > 99.999, "red", "green")
    valueBox(
      sprintf("$%.2f", last(NBABOS$x100)),
      "Boston Celtics", 
      icon = icon('import', lib = 'glyphicon'), # icon("sign-in"),
      color = set_color
    )
  })
  
  output$NetsBox <- renderValueBox({
    set_color <- ifelse(last(NBABKN$x100) > 99.999, "red", "green")
    valueBox(
      sprintf("$%.2f", last(NBABKN$x100)),
      "Brooklyn Nets", 
      icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
      color = set_color
    )
  })
  
  output$KnicksBox <- renderValueBox({
    set_color <- ifelse(last(NBANYK$x100) > 99.999, "red", "green")
    valueBox(
      sprintf("$%.2f", last(NBANYK$x100)),
      "New York Knicks", 
      icon = icon('import', lib = 'glyphicon'), # icon("sign-in"),
      color = set_color
    )
  })
  
  output$SixersBox <- renderValueBox({
    set_color <- ifelse(last(NBAPHI$x100) > 99.999, "red", "green")
    valueBox(
      sprintf("$%.2f", last(NBAPHI$x100)),
      "Philadelphia 76ers", 
      icon = icon('import', lib = 'glyphicon'), # icon("sign-in"),
      color = set_color
    )
  })
  
  output$RaptorsBox <- renderValueBox({
    set_color <- ifelse(last(NBATOR$x100) > 99.999, "red", "green")
    valueBox(
      sprintf("$%.2f", last(NBATOR$x100)), 
      "Toronto Raptors", 
      icon = icon('import', lib = 'glyphicon'), # icon("sign-in"),
      color = set_color
    )
  })
    
    # Highchart ----------------------------------------
    output$BosHc <-renderHighchart({
      #subset the data based on the selected team
      subset_data <- df[df$team == input$listOfNBAteams,]
      
      #extract the values for plotting
      x_values <- as.Date(subset_data$date)
      subset_data$x100 <- round(subset_data$x100, 2)
      y_values <- subset_data$x100
      
      #plot the data
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(categories = x_values) %>%
        hc_add_series(data = round(y_values,2), name = "") %>%
        hc_tooltip(valueDecimals = 2) %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
