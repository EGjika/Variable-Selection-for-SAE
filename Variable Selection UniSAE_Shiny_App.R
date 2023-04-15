
library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(shiny)

# Load data generator script
source("Meza_Lahiri_Rcode.R")


# Define UI
ui <- fluidPage(
  titlePanel("Variable Selection UniSAE"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_runs", "Number of runs:", min = 1, max = 500, value = 100),
      sliderInput("sigma_e_sq", "Error variance:", min = 1,max=5,value=1),
      sliderInput("sigma_v_sq", "Group variance:", min = 1, max = 5, value = 5),
      sliderInput("n_groups", "Number of groups:", min = 5, max = 5, value = 5),
      sliderInput("n_obs", "Number of observations per group:", min = 10, max = 10, value = 10),
      numericInput("beta0", "Beta 0:", value = 1),
      numericInput("beta1", "Beta 1:", value = 2),
      numericInput("beta2", "Beta 2:", value = 0),
      numericInput("beta3", "Beta 3:", value = 5),
      numericInput("beta4", "Beta 4:", value = 2),
      actionButton("run_button", "Run")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Generator:",  verbatimTextOutput("generated_data")),
        tabPanel("AIC Table", tableOutput("results_table_1")),
        tabPanel("BIC Table", tableOutput("results_table_2")),
        # tabPanel("BIC&AIC Table",
        #          tableOutput("results_table_1"),
        #          br(),
        #          tableOutput("results_table_2")
        # ),
        tabPanel("AIC&BIC Plot", plotOutput("aic_plot")),
        tabPanel("Scatterplot", plotOutput("scatterplot_plot")),
        tabPanel("About", 
                 h3("Variable selection under unit-level small area models"),
                 h5("This study investigates the effectiveness of a combined variable selection approach for nested-error regression (NER) models in small area estimation (SAE), by building upon the ideas of two main articles. Meza et al.(2005) proposed a variable selection procedure utilizing Cp statistics, while Li et al. (2019) suggested a transformation for the response variable such that the NER model provides more flexibility of model fitting. The aim of this research project is to apply the transformation proposed by Li et al. to meet the assumption of NER models, then apply Meza et al.'s procedure for variable selection using Akaike information criterion (AIC) and Bayesian information criterion (BIC). The goal is to determine whether the combined approach is effective in selecting the most relevant predictors for the response variable."),
                 h5("The proposed approach presented in this research project aims to address violations of the Nested Error Regression (NER) model assumptions by implementing the Li et al. transformation and selecting the optimal variables through the Meza and Lahiri variable selection procedure. When the assumptions of the NER model are violated, the estimates of the model parameters may be biased, leading to incorrect inferences. To overcome this issue, the Li et al. transformation is applied to the data to make the error terms more normally distributed. Subsequently, the Meza and Lahiri variable selection procedure is used to select the most appropriate variables for inclusion in the NER model."),
                 h5("To better understand the performance of the combined approach of Li et al. transformation and Meza and Lahiri variable selection procedure in addressing NER model violations, a simulation study was conducted. The simulation study involved generating datasets with known parameter values and varying levels of NER model violations. The datasets were then analyzed using the proposed approach, and the resulting parameter estimates and predictive accuracy were compared to those obtained using a naive approach such as Meza and Lahiri."),
                 h5("In conclusion, this study demonstrates the effectiveness of a combined approach for variable selection in NER models for SAE, utilizing the transformation proposed by Li et al. and variable selection procedure proposed by Meza et al. using AIC and BIC. The approach can be applied to real-world data sets and has the potential to improve the performance of NER models in SAE. By investigating two main articles related to NER models and their variable selection procedures, this study contributes to the development of more effective and accurate methods for SAE."),
                 h4("16 April 2023"),
                 h4(p(span("Author", style = "color:red"))),
                 HTML('
                 <div style="clear: left;">
                 <img src="data:image/jpeg;base64,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" alt="" style="height: 220px; width: 200px; "> </div>
                 <p>
                 <a href="https://al.linkedin.com/in/eralda-dhamo-gjika-71879128" target="_blank"> Eralda Gjika (Dhamo)</a><br>
                 Master Student (2021-2023), School of Mathematics and Statistics<br>
                 Carleton University, Ottawa, Canada<br>
                 Lead of Data and Psychometrics, Vretta Inc, Ottawa, Canada<br>
                 <a href="https://al.linkedin.com/in/eralda-dhamo-gjika-71879128" target="_blank">Linkedin</a> <br/>
        </p>')
        )
        #tabPanel("Scatterplot_0", plotOutput("scatterplot"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive function to run var selection and return results
  var_selection_results_3 <- reactive({
    run_var_selection_3(n_runs = input$n_runs, 
                        sigma_e_sq = input$sigma_e_sq, 
                        sigma_v_sq = input$sigma_v_sq, 
                        n_groups = input$n_groups, 
                        n_obs = input$n_obs,
                        beta = c(input$beta0, input$beta1, input$beta2, input$beta3, input$beta4))
    
  })
  
  var_selection_results_2 <- reactive({
    run_var_selection_2(n_runs = input$n_runs, 
                        sigma_e_sq = input$sigma_e_sq, 
                        sigma_v_sq = input$sigma_v_sq, 
                        n_groups = input$n_groups, 
                        n_obs = input$n_obs,
                        beta = c(input$beta0, input$beta1, input$beta2, input$beta3, input$beta4))
    
  })
    
  var_selection_results_1_1 <- reactive({
    run_var_selection_1_1(n_runs = input$n_runs, 
                        sigma_e_sq = input$sigma_e_sq, 
                        sigma_v_sq = input$sigma_v_sq, 
                        n_groups = input$n_groups, 
                        n_obs = input$n_obs,
                        beta = c(input$beta0, input$beta1, input$beta2, input$beta3, input$beta4))
 
     })
  
  var_selection_results_1_2 <- reactive({
    run_var_selection_1_2(n_runs = input$n_runs, 
                        sigma_e_sq = input$sigma_e_sq, 
                        sigma_v_sq = input$sigma_v_sq, 
                        n_groups = input$n_groups, 
                        n_obs = input$n_obs,
                        beta = c(input$beta0, input$beta1, input$beta2, input$beta3, input$beta4))
    
  })
  
  
  # Define the function to generate data based on user input
  generate_data <- function() {
    # Set the seed for reproducibility
    set.seed(123)
    
    # Get the user input values
    sigma_e_sq <- input$sigma_e_sq
    sigma_v_sq <- input$sigma_v_sq
    n_groups <- input$n_groups
    n_obs <- input$n_obs
    beta <- c(input$beta0, input$beta1, input$beta2, input$beta3, input$beta4)
    
    # Generate data using the generate_data_1 function
    data <- generate_data_1(sigma_e_sq, sigma_v_sq, n_groups, n_obs, beta)
    
    # Return the data as a data frame
    return(data)
  }
  
  # Define the reactive value for the Shiny app
  data <- reactiveValues(
    generated_data = NULL
  )
  
  # Define the event for the Run button
  observeEvent(input$run_button, {
    # Call the generate_data function to get the generated data
    generated_data <- generate_data()
    
    # Assign the generated data to the reactive value
    data$generated_data <- generated_data
  })
  
  # Define the output for the Shiny app
  output$generated_data <- renderPrint({
    # Print the generated data from the reactive value
    print(data$generated_data)
  })
  
  
  # Render results table
  output$results_table_1 <- renderTable({
    var_selection_results_1_1()
  })
  
  # Render results table
  output$results_table_2 <- renderTable({
    var_selection_results_1_2()
  })
  
  
  
  # Render AIC plot
  output$aic_plot <- renderPlot({
    var_selection_results_2()
  })
  
  # Render Scatterplot plot
  output$scatterplot_plot <- renderPlot({
    var_selection_results_3()
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
