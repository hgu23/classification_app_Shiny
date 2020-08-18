shinyUI(fluidPage(
  
  # Application title
  titlePanel("Finding the best Classification model for a Dataset"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Available methods",
             h3("Classification methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("naomit","dummy")),
                                 bsTooltip(id = "GlmnetPreprocess",
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1,
                                 actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
                tabPanel("SVM Radial Model",
                         verbatimTextOutput(outputId = "SVMModelSummary0"),
                         fluidRow(
                           column(width = 4,
                                  selectizeInput(inputId = "SVMRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                  bsTooltip(id = "SVMRadialPreprocess",
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                           ),
                           column(width = 1,
                                  actionButton(inputId = "SVMRadialGo", label = "Train", icon = icon("play")),
                                  bsTooltip(id = "SVMRadialGo", title = "This will train or retrain your model")
                           )
                         ),
                         hr(),
                         h3("Resampled performance:"),
                         tableOutput(outputId = "SVMRadialMetrics"),
                         hr(),
                         plotOutput(outputId = "SVMRadialModelPlots"),
                         verbatimTextOutput(outputId = "SVMRadialRecipe"),
                         verbatimTextOutput(outputId = "SVMRadialModelSummary2")
                ),
               tabPanel("GBM",
                        verbatimTextOutput(outputId = "gbmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gbmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "gbmPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gbmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gbmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gbmMetrics"),
                        hr(),
                        plotOutput(outputId = "gbmModelPlots"),
                        verbatimTextOutput(outputId = "gbmRecipe"),
                        verbatimTextOutput(outputId = "gbmModelSummary2")
               )
               
######################################################### maintenance point ####################################################
               
             )
             ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Fitting the test set",
            fluidRow(
                      column(width=1,
                      actionButton(inputId = "TestDataGo", label = "Predict for Test data?", icon = icon("play")),
                      bsTooltip(id = "TestDataGo", title = "This will fit the Test data with this model")
               )
            
    )
  )
)))
