
# 2nd day, google docs *** my project ***------------------------------------------------
options(shiny.maxRequestSize = 30*1024^2) # Increase file size limit to 30MB


library(shiny) # Package for shiny applications
library(shinythemes)
library(dplyr) # Package for data manipulations
library(magrittr) # Package for pipe operator
library(ggplot2) # Package for creating graphs

# course_data <- readRDS("data/europe.rds") %>% # Load the course data
#   mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius

# Define UI for application
ui <- fluidPage(
  
  # theme
  theme = shinytheme(theme = "darkly"),

  # Application title
  titlePanel("COURSE SHINY APP - BENJAMIN -V"),

  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      "This is the sidebar panel",
      
      # Input: A simple text input  ----
      textInput(inputId = "text_input", label = "Input text here:"),

      # Input: A simple slider ----
      sliderInput(inputId = "storm_year", label = "Year",
                  min = 2000,
                  max = 2019,
                  step = 1,
                  value = 2000,
                  sep = ''),

      # Input: A simple drop down list  ----
      selectInput(inputId = "storm_country", label = "Storm Country:",
                  # choices = c(unique(course_data$Country)),
                  choices = NULL,
                  selected = NULL),

      # Input: A simple drop down list  ----
      selectInput(inputId = "storm_city", label = "Storm City:",
                  # choices = c(unique(course_data$City)),
                  choices = NULL,
                  selected = NULL),

      # Input: A simple radio button input  ----
      radioButtons(inputId = "storm_status", label = "Status",
                   choices = list("Fahrenheit" = 1,
                                  "Celsius" = 2),
                   selected = 1),
      
      # Input: Action button that subsets storm data ----
      actionButton(inputId = "button", label = "GO!"),
      
      br(), br(),
      
      # Input: Upload a single RDS file
      fileInput(inputId = "file", 
                label = "Upload RDS file",
                multiple = FALSE,
                accept = c(".rds")),
      
      # Input: Download a file ----
      downloadButton(outputId = "download", label = "Download")
      

    ),

    # Main panel
    mainPanel(
      "This is the main panel",

      textOutput(outputId = "text_output"),

      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           "This is the course shiny app. It is created during the course
                           exercises using the europe.rds data:
                           Average daily temperatures (in Fahrenheit) from cities around
                           Europe from 2000 to 2019",

                           verbatimTextOutput("data_summary")
                  ),
                  tabPanel(title = "Data",

                           dataTableOutput("data_table")
                  ),
                  tabPanel(title = "Plots",
                           
                           fluidRow(
                             column(width=12, plotOutput("lineplot"))
                           ),
                           fluidRow(
                             column(width=6, plotOutput("boxplot")),
                             column(width=6, plotOutput("lineplotF"))
                           )
                  )
      )
    )
  )
)

# Define server side logic
server <- function(input, output, session) {
  
  # downloadable csv of selected dataset ----
  output$download <- downloadHandler(
    filename = "storm_data.csv",
    content = function(file) {
      write.csv(city_df(), file, row.names = FALSE)
    }
  )
  
  # upload file and read in data ----
  course_data <- eventReactive(input$file, {
    readRDS(input$file$datapath) %>%  # comes from fileInput()
    mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius
  })
  
  # Reactive Expression 1 ----
  city_df <- reactive({
    country_df() %>%
      filter(City == input$storm_city) %>% # Subset the rows for specific City
      filter(Year == input$storm_year) # Subset the rows for specific Year
  })

  # Reactive Expression 2
  country_df <- eventReactive(input$button, {
    course_data() %>%
      filter(Year >= input$storm_year) %>% # Subset the rows to keep data more than or equal to a year
      filter(Country == input$storm_country) # Subset the rows to keep a specific country
  })
  
  # Reactive Expression 3
  year_df <- reactive({
    country_df() %>% 
      filter(City == input$storm_city) %>% # Subset the rows for specific City
      filter(Year == input$storm_year) %>%  # Subset the rows for specific Year
      group_by(Country, City, Year, Month) %>% 
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>%
      ungroup()
    
  })
  
  
  # Output: Render a text output  ----
  output$text_output <- renderText({
    paste("Your inputs are:", input$slider_1, input$drop_down_1, input$text_input, input$radio_1)
  })

  # Output: Render a print output  ----
  output$data_summary <- renderPrint({
    summary(course_data())
  })

  # Output: Render a (dynamic) table output  ----
  output$data_table <- renderDataTable({
    city_df()
  })

  # Output: Render a plot output  ----
  output$lineplot <- renderPlot({
    ggplot(data = city_df()) +
      geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })

  # Output: Render a plot output  ----
  output$boxplot <- renderPlot({
    ggplot(data = country_df()) +
      geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))
  })

  # Output: Render a plot output  ----
  output$lineplotF <- renderPlot({
    ggplot(data = year_df()) +
      geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
      geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
      geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
      scale_x_discrete(name = "", limits = month.abb) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })

  # Output: Render a plot output  ----
  output$lineplotC <- renderPlot({
    ggplot(data = year_df()) +
      geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
      geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
      geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
      scale_x_discrete(name = "", limits = month.abb) +
      ylab("Average daily temperatures (in Celsius)")
  })
  
  # Observer 1 ----
  observe({
    
    new_country_choices <- course_data() %>%
      pull(Country) %>%
      unique()
    
    updateSelectInput(session, inputId = "storm_country", choices = new_country_choices)
    # updateSelectInput(session, inputId = "storm_country", choices = new_country_choices$Country) # as dataframe
    
  })
  
  # Observer 2 ----
  observe({
    
    new_choices <- unique(course_data()$City[course_data()$Country == input$storm_country])

    updateSelectInput(session, inputId = "storm_city", choices = new_choices)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


