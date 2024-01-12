library(dplyr)
library(ggplot2)
library(shiny)
#library(plotly)
library(shinythemes)
library(knitr)
covid=read.csv("5city.csv")

city = covid[-320,]
normalized_data = city[,c(10,11,17,19:28)]
normalized_data$risk_category = cut(city$cases_per_1000, breaks=c(0,15.5,23 , 100), right = FALSE, labels = c(1,2,3))

set.seed(2123)
n <- nrow(normalized_data)
sample_size <- floor(0.70 * n)
training_indices <- sample(1:n, size = sample_size, replace = FALSE)
training_data <- normalized_data[training_indices, ]

library(nnet)

training_data$risk_category = as.factor(training_data$risk_category)
model <- multinom(risk_category ~average_household_size + pct_more_than_one_occupant_per_room
                  + median_age + pct_below_poverty_level + household_median_income + 
                    pct_services +  pct_over_age_65 + pct_white + pct_minority + 
                    pct_black + pct_hispanic, data = training_data)

shinyApp(
  ui = fluidPage(
    theme = shinytheme("sandstone"),
    navbarPage("COVID-19 Risk Factors",
      tabPanel("Predict your Zip Code's COVID-19 Risk Category",
        fluidRow(
          sidebarPanel(
            textInput("num1", "Average Household Size:", value = "3"),
            textInput("num2", "% >1 Occupant per Room:", value = "4.2"),
            textInput("num3", "Median Age:", value = "36"),
            textInput("num4", "% Below Poverty Level:", value = "7.313917"),
            textInput("num5", "Household Median Income:", value = "54972"),
            textInput("num6", "% Service Workers:", value = "0.2636216"),
            textInput("num7", "% >65 years old:", value = "0.13142337"),
            textInput("num8", "% White:", value = "0.528"),
            textInput("num9", "% Minority:", value = "0.472"),
            textInput("num10", "% Black:", value = "0.406"),
            textInput("num11", "% Hispanic:", value = "0.0681"),
            verbatimTextOutput("risk_category")
          )
        )
      ),
      tabPanel("Scatterplot of Variable by Cases per 1000",
        fluidRow(
            sidebarPanel(
              selectInput("x","Select X-Axis Variable:",choices = c(
                  "average_household_size", "pct_more_than_one_occupant_per_room",
                  "median_age", "pct_below_poverty_level", "household_median_income",
                  "household_mean_income", "pct_services", "pct_over_age_65", "pct_white",
                  "pct_minority", "pct_black", "pct_hispanic", "zip_code"
                )
              ),
              selectInput("y","Select Y-Axis Variable:",choices = c(
                "average_household_size", "pct_more_than_one_occupant_per_room",
                "median_age", "pct_below_poverty_level", "household_median_income",
                "household_mean_income", "pct_services", "pct_over_age_65", "pct_white",
                "pct_minority", "pct_black", "pct_hispanic", "zip_code","cases_per_1000"
                ), selected = "cases_per_1000"
              ),
              checkboxGroupInput("city","Select Cities:",choices = c("nyc", "balt", "chi", "phi", "det"),selected = "nyc")
            ),
          mainPanel(plotOutput("scatter",brush = "plot_brush"),
                                     verbatimTextOutput("info")))
        ),
      tabPanel("EDA & Modeling",
        fluidPage(uiOutput('markdown'))
      ),
    )
  ),
  server = function(input, output) {
    output$risk_category=renderPrint({
      data <- data.frame(
        average_household_size = as.numeric(input$num1),
        pct_more_than_one_occupant_per_room = as.numeric(input$num2),
        median_age = as.numeric(input$num3),
        pct_below_poverty_level = as.numeric(input$num4),
        household_median_income = as.numeric(input$num5),
        pct_services = as.numeric(input$num6),
        pct_over_age_65 = as.numeric(input$num7),
        pct_white = as.numeric(input$num8),
        pct_minority = as.numeric(input$num9),
        pct_black = as.numeric(input$num10),
        pct_hispanic = as.numeric(input$num11)
      )
      prediction = predict(model, newdata = data, type = "class")
      if (prediction == 1){
        print("Low Infection Rate Predicted")
      }
      if (prediction == 2){
        print("Medium Infection Rate Predicted")
      }
      if (prediction ==3){
        print("High Infection Rate Predicted")
      }
    })
    
    dat = reactive({
      covid %>%
        filter(city %in% input$city)
    })
    
    output$scatter = renderPlot({
      ggplot(dat(), aes_string(x = input$x, y = input$y, fill = "city")) +
        geom_point(shape = 21)
    })
    output$info=renderPrint({
      brushed_data <- brushedPoints(covid, input$plot_brush, xvar = input$x, yvar = input$y)
      
      # Extract only the selected variables
      selected_data <- brushed_data[, c("zip_code", "total_population", input$x, input$y)]
      
      print(selected_data)
    })
    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit('week 2.1.rmd', quiet = TRUE)))
    })
  }
)
