shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "train.csv")
  })
  getData2 = reactive({
      read.csv(file= "test.csv")
  })
        
  mydf = reactive({
      df = getData()
      df$Survived = as.factor(df$Survived)
      df$Pclass = as.factor(df$Pclass)
      df$SibSp = as.factor(df$SibSp)
      df$Parch = factor(df$Parch,levels = c("0","1","2","3","4","5","6","9"))
      df$Embarked = factor(df$Embarked, levels = c ("C","Q","S"))
      df$Name = NULL
      df$Ticket = NULL
      df$Cabin = NULL
      df
  })
  
  mydf2 = reactive({
    df = getData2()
    df$Pclass = as.factor(df$Pclass)
    df$SibSp = as.factor(df$SibSp)
    df$Parch = factor(df$Parch,levels = c("0","1","2","3","4","5","6","9"))
    df$Embarked = factor(df$Embarked, levels = c ("C","Q","S"))
    df$Name = NULL
    df$Ticket = NULL
    df$Cabin = NULL
    for (i in (1:length(df$Fare))){
      if (is.na(df$Fare[i])==TRUE){df$Fare[i] <- median(df$Fare, na.rm = TRUE)}
    }
    df
  })
  
  
  
  
                      
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Survived"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- mydf()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- mydf()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- mydf()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(mydf())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- mydf()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Classification != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })


  
  ############################################################################## 
  getTrainData <- reactive({
    mydf()
  })
  
  ############################################################################## 
  getTestData <- reactive({
    mydf2()
  })


  # ############################################################################## 
  # 
  # output$SplitSummary <- renderPrint({
  #   cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  # })
  


# ############################################################ GLMNET ########################################################
#   
#   
#   
#   
  ##############################################################################
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Survived ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })

  ##############################################################################
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "Accuracy", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ##############################################################################
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.max(models$glmnet$results[, "Accuracy"]), ]
  })

  ##############################################################################
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  ##############################################################################
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })

  ##############################################################################
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })




######################################################### maintenance point ####################################################

############################################# SVM Radial #########################################
  getSVMRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Survived ~ ., data = getTrainData()), input$SVMRadialPreprocess)
  })

  ##############################################################################
  observeEvent(
    input$SVMRadialGo,
    {
      library(kernlab)
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getSVMRadialRecipe(), data = getTrainData(), method = method, metric = "Accuracy", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ##############################################################################
  output$SVMRadialModelSummary0 <- renderText({
    description("svmRadial")
  })

  ##############################################################################
  output$SVMRadialMetrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.max(models$svmRadial$results[, "Accuracy"]), ]
  })

  ##############################################################################
  output$SVMRadialModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })

  ##############################################################################
  output$SVMRadialRecipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })

  ##############################################################################
  output$SVMRadialModelSummary2 <- renderPrint({
    req(models$svmRadial)
    summary(models$svmRadial$finalModel)
  })



######################### Gradient boosting machines ############################################
  getgbmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Survived ~ ., data = getTrainData()), input$gbmPreprocess)
  })

  ##############################################################################
  observeEvent(
    input$gbmGo,
    {
      library(gbm)
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgbmRecipe(), data = getTrainData(), method = method, metric = "Accuracy", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ##############################################################################
  output$gbmModelSummary0 <- renderText({
    description("gbm")
  })

  ##############################################################################
  output$gbmMetrics <- renderTable({
    req(models$gbm)
    models$gbm$results[ which.max(models$gbm$results[, "Accuracy"]), ]
  })

  ##############################################################################
  output$gbmModelPlots <- renderPlot({
    req(models$gbm)
    plot(models$gbm)
  })

  ##############################################################################
  output$gbmRecipe <- renderPrint({
    req(models$gbm)
    models$gbm$recipe
  })

  ##############################################################################
  output$gbmModelSummary2 <- renderPrint({
    req(models$gbm)
    summary(models$gbm$finalModel)
  })



############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "Null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("Accuracy")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
    predicting = reactive({
      test = getTestData()
      print(nrow(test))
      req(input$Choice)
      mod <- models[[input$Choice]]
      predictions <- predict(mod, newdata = test)
      d <- data.frame(test$PassengerId, predictions)
      colnames(d) <- c("PassengerId", "Survived")
      ### Saving the predictions dataset as a csv after applying the chosen model on the test data ###
      write.csv(d,"C:\\Desktop\\Lecture Notes MADS\\classification_app\\glmnet.csv")
      d
    })
    vals <- reactiveValues()
    
    observeEvent(
       input$TestDataGo,
      {vals$predictions = predicting()
      showModal(modalDialog("Predictions saved!"))})
    
})
