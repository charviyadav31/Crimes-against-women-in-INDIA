library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(caret)

# Load the cleaned dataset
data <- CrimesOnWomenDatanew

# Convert 'State' to a factor
data$State <- as.factor(data$State)

# Preprocess the data
data <- data %>%
  na.omit()  # Remove rows with missing values

# Train-test split
set.seed(123)
trainIndex <- createDataPartition(data$Rape, p = 0.8, list = FALSE)
traindata <- data[trainIndex, ]
testdata <- data[-trainIndex, ]

# Train a predictive model
model <- train(
  Rape ~ Year + K.A + DD + AoW + AoM + DV, 
  data = traindata, 
  method = "rf",
  trControl = trainControl(method = "cv", number = 5)
)

ui <- fluidPage(
  useShinyjs(),  # Enable JavaScript for dynamic behaviors
  
  # Include custom CSS for styling
  tags$head(
    tags$style(HTML("
      body { 
        font-family: Arial, sans-serif; 
        background-color: #f9f9f9; 
        margin: 0; padding: 0; 
      }
      .login-panel { 
        text-align: center; 
        margin-top: 100px; 
      }
      .login-btn { 
        background-color: #4285F4; 
        color: white; 
        font-size: 18px; 
        padding: 12px 25px; 
        border: none; 
        border-radius: 5px; 
        cursor: pointer; 
        transition: background-color 0.3s ease; 
      }
      .login-btn:hover { 
        background-color: #357AE8; 
      }
      .main-content { 
        display: none; 
        padding: 20px; 
      }
      .main-header { 
        background-color: #4CAF50; 
        color: white; 
        padding: 20px; 
        text-align: center; 
        font-size: 28px; 
        margin-bottom: 20px; 
      }
      .sidebar-panel { 
        background-color: #ffffff; 
        padding: 20px; 
        border-radius: 8px; 
        box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); 
      }
      .main-panel { 
        background-color: #ffffff; 
        padding: 20px; 
        border-radius: 8px; 
        box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); 
      }
      .plot-title { 
        font-size: 20px; 
        font-weight: bold; 
        margin-bottom: 15px; 
      }
      .graph-container { 
        display: flex; 
        justify-content: center; 
        align-items: center; 
        height: 100%; 
      }
      .centered-content { 
        display: flex; 
        flex-direction: column; 
        align-items: center; 
      }
    "))
  ),
  
  # Login Page
  div(
    id = "login-page",
    class = "login-panel",
    h2("Crimes Against Women Data Explorer"),
    h3("Please log in to continue"),
    actionButton("login_btn", "Login with Google", class = "login-btn"),
    textOutput("login_status"),
    img(src = "https://accountabilityindia.in/wp-content/uploads/2020/01/Source_UN-Women.png", 
        height = "475px", width = "80%", style = "margin-top: 20px;")
  ),
  
  # Main App Content (hidden initially)
  hidden(
    div(
      id = "main-content",
      div(class = "main-header", "Crimes Against Women Data Explorer"),
      
      # Tabs for different functionalities
      tabsetPanel(
        # Dashboard Tab
        tabPanel("Dashboard",
                 fluidPage(
                   fluidRow(
                     column(
                       width = 12,
                       div(
                         class = "main-panel",
                         tags$h4("Overview of Crimes Against Women"),
                         tags$p(
                           "Crimes against women are a significant societal issue, with a range of offenses including 
                   rape, domestic violence, and human trafficking. This dashboard aims to provide a data-driven 
                   understanding of trends, patterns, and insights for proactive measures."
                         )
                       )
                     )
                   ),
                   fluidRow(
                     column(
                       width = 4,
                       div(
                         class = "main-panel",
                         tags$h4("Total Reported Cases"),
                         tags$p(style = "font-size: 22px; color: #E64A19; font-weight: bold;", "3,50,000+"),
                         tags$p("Annual cases of crimes against women reported nationwide.")
                       )
                     ),
                     column(
                       width = 4,
                       div(
                         class = "main-panel",
                         tags$h4("Top Crime Type"),
                         tags$p(style = "font-size: 22px; color: #4CAF50; font-weight: bold;", "Domestic Violence"),
                         tags$p("Accounts for 30% of all reported cases.")
                       )
                     ),
                     column(
                       width = 4,
                       div(
                         class = "main-panel",
                         tags$h4("Most Affected State"),
                         tags$p(style = "font-size: 22px; color: #4285F4; font-weight: bold;", "Uttar Pradesh"),
                         tags$p("Reports the highest number of cases annually.")
                       )
                     )
                   ),
                   fluidRow(
                     column(
                       width = 6,
                       div(
                         class = "main-panel",
                         tags$h4("Key Statistics"),
                         tags$ul(
                           tags$li("Rape cases reported annually exceed 30,000."),
                           tags$li("Domestic violence is one of the most underreported crimes."),
                           tags$li("Kidnapping and abduction cases often involve trafficking and forced marriages."),
                           tags$li("Dowry deaths remain a challenge despite strict laws.")
                         )
                       )
                     ),
                     column(
                       width = 6,
                       div(
                         class = "main-panel",
                         tags$h4("Trends and Insights"),
                         tags$ul(
                           tags$li("Urban areas report higher cases, but rural areas face significant underreporting."),
                           tags$li("Machine learning models can predict future crime trends and hotspots."),
                           tags$li("Community awareness is critical in preventing crimes against women.")
                         )
                       )
                     )
                   )
                 )
        ),
        
        # Data Explorer Tab
        tabPanel("Data Explorer",
                 sidebarLayout(
                   sidebarPanel(
                     class = "sidebar-panel",
                     tags$h4("Filter Options"),
                     selectInput("state", "Select State:", choices = unique(data$State)),
                     tags$label("Select Year Range:"),
                     sliderInput("year", "", 
                                 min = min(data$Year), max = max(data$Year),
                                 value = c(min(data$Year), max(data$Year))),
                     selectInput("crime_type", "Select Crime Type:", 
                                 choices = c("Rape", "DD", "AoW", "AoM", "DV"))
                   ),
                   mainPanel(
                     class = "main-panel",
                     tags$h4("Crime Data Visualization"),
                     plotOutput("crimePlot")
                   )
                 )
        ),
        
        # Visualizations Tab
        tabPanel("Visualizations",
                 sidebarLayout(
                   sidebarPanel(
                     class = "sidebar-panel",
                     tags$h4("Visualization Options"),
                     selectInput("vis_state", "Select State:", choices = c("All States", unique(data$State))),
                     selectInput("vis_crime_type", "Select Crime Type:", 
                                 choices = c("Rape", "DD", "AoW", "AoM", "DV")),
                     selectInput("chart_type", "Select Chart Type:", 
                                 choices = c("Bar Chart", "Line Chart", "Heatmap", "Scatter Plot")),
                     sliderInput("vis_year", "Select Year Range:",
                                 min = min(data$Year), max = max(data$Year),
                                 value = c(min(data$Year), max(data$Year)))
                   ),
                   mainPanel(
                     class = "main-panel",
                     tags$h4("Crime Data Visualization"),
                     plotOutput("visualizationPlot")
                   )
                 )
        ),
        
        tabPanel("Predictive Analysis",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("predict_state", "Select State:", 
                                 choices = unique(data$State)),
                     sliderInput("predict_year", "Select Year for Prediction:", 
                                 min = min(data$Year), max = max(data$Year) + 10, 
                                 value = max(data$Year)),
                     selectInput("predict_crime", "Select Crime Type:", 
                                 choices = c("Rape", "K.A", "DD", "AoW", "AoM", "DV"))
                   ),
                   mainPanel(
                     textOutput("prediction_result"),
                     plotOutput("trend_plot")
                   )
                 )
        ),
        
        # Logout Tab
        tabPanel(
          div("Logout", class = "logout-tab"),
          fluidPage(
            fluidRow(
              column(
                width = 12,
                div(
                  style = "text-align: center; margin-top: 50px; padding: 20px; background-color: #ffffff; border-radius: 8px; box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.1);",
                  h3("Are you sure you want to log out?", style = "color: #1a5276;"),
                  actionButton("confirm_logout", "Logout", 
                               class = "btn btn-logout", 
                               style = "margin-top: 20px; background-color: #FF5722; color: white; font-size: 18px; padding: 10px 20px; border: none; border-radius: 5px; cursor: pointer; transition: background-color 0.3s ease;")
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  user_logged_in <- reactiveVal(FALSE)  # Track login status
  
  observeEvent(input$login_btn, {
    req(input$login_btn)
    
    # Initiate OAuth 2.0 authentication
    oauth_endpoints("google")
    myapp <- oauth_app("google", key = google_client_id, secret = google_client_secret)
    goog_auth <- oauth2.0_token(oauth_endpoints("google"), myapp, scope = scopes, cache = FALSE)
    
    # Get user information
    user_info <- GET("https://www.googleapis.com/oauth2/v2/userinfo", config(token = goog_auth))
    user_info_data <- content(user_info)
    
    if (!is.null(user_info_data$email)) {
      user_logged_in(TRUE)
      output$login_status <- renderText(paste("Hello,", user_info_data$name))
      
      # Show the main content and hide the login page
      hide("login-page")
      show("main-content")
    } else {
      output$login_status <- renderText("Login failed. Please try again.")
    }
  })
  
  observeEvent(input$confirm_logout, {
    user_logged_in(FALSE)
    shinyjs::alert("You have been logged out successfully.")
    user_status(FALSE)
  })
  
  # Data Explorer visualization
  output$crimePlot <- renderPlot({
    filtered_data <- data %>%
      filter(State == input$state, Year >= input$year[1], Year <= input$year[2])
    
    ggplot(filtered_data, aes_string(x = "Year", y = input$crime_type)) +
      geom_line(color = "#4CAF50") +
      labs(title = paste(input$crime_type, "Cases in", input$state),
           x = "Year", y = "Number of Cases") +
      theme_minimal()
  })
  
  # Visualization tab plots
  output$visualizationPlot <- renderPlot({
    filtered_data <- if (input$vis_state == "All States") {
      data %>%
        filter(Year >= input$vis_year[1], Year <= input$vis_year[2])
    } else {
      data %>%
        filter(State == input$vis_state, Year >= input$vis_year[1], Year <= input$vis_year[2])
    }
    
    if (input$chart_type == "Bar Chart") {
      ggplot(filtered_data, aes_string(x = "Year", y = input$vis_crime_type)) +
        geom_bar(stat = "identity", fill = "#4285F4") +
        labs(title = paste(input$vis_crime_type, "in", input$vis_state),
             x = "Year", y = "Number of Cases") +
        theme_minimal()
    } else if (input$chart_type == "Line Chart") {
      ggplot(filtered_data, aes_string(x = "Year", y = input$vis_crime_type)) +
        geom_line(color = "#4CAF50") +
        labs(title = paste(input$vis_crime_type, "in", input$vis_state),
             x = "Year", y = "Number of Cases") +
        theme_minimal()
    } else if (input$chart_type == "Heatmap") {
      ggplot(filtered_data, aes_string(x = "Year", y = "State", fill = input$vis_crime_type)) +
        geom_tile(color = "white") +
        labs(title = "Heatmap of Crime Data",
             x = "Year", y = "State", fill = "Cases") +
        theme_minimal()
    } else if (input$chart_type == "Scatter Plot") {
      ggplot(filtered_data, aes_string(x = "Year", y = input$vis_crime_type)) +
        geom_point(color = "#FF9800") +
        labs(title = "Scatter Plot of Crime Data",
             x = "Year", y = "Number of Cases") +
        theme_minimal()
    }
  })
  
  # Predictive analysis
  prediction <- reactive({
    state_data <- data %>%
      filter(State == input$predict_state)
    
    new_data <- data.frame(
      Year = input$predict_year,
      K.A = mean(state_data$K.A, na.rm = TRUE),
      DD = mean(state_data$DD, na.rm = TRUE),
      AoW = mean(state_data$AoW, na.rm = TRUE),
      AoM = mean(state_data$AoM, na.rm = TRUE),
      DV = mean(state_data$DV, na.rm = TRUE)
    )
    
    if (input$predict_crime == "Rape") {
      predict(model, newdata = new_data)
    } else {
      mean(state_data[[input$predict_crime]], na.rm = TRUE) * 
        (1 + (input$predict_year - max(data$Year)) * 0.02)
    }
  })
  
  output$prediction_result <- renderText({
    paste("Predicted", input$predict_crime, "cases for", input$predict_state, 
          "in", input$predict_year, ":", round(prediction()))
  })
  
  output$trend_plot <- renderPlot({
    state_data <- data %>%
      filter(State == input$predict_state)
    
    ggplot(state_data, aes(x = Year, y = .data[[input$predict_crime]])) +
      geom_line(color = "blue", size = 1) +
      geom_point(aes(x = input$predict_year, y = prediction()), color = "red", size = 4) +
      labs(title = paste(input$predict_crime, "Trend in", input$predict_state),
           x = "Year", y = "Cases") +
      theme_minimal() 
  })
}

shinyApp(ui, server)
