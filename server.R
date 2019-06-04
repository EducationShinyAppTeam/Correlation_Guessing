library(shiny)
library(ggplot2)
library(shinyBS)
library(shinyjs)
library(V8)
library(shinydashboard)
library(graphics)
library(shinyWidgets)

# this funcation, especially the easy level is from xxx: the function is about correlation plot
generateData = function(difficulty,numPoints){
  value = rnorm(1,0,10)
  valueB = rnorm(1,4,1)
  outlier = as.numeric( sample(2:6, 1)) #create a random number
  if (difficulty ==2){ # with outlier
    choice = sample(2,1)
    if (choice ==2){
      X1 = rnorm(numPoints,value,valueB)
      Y1 = rnorm(numPoints,rnorm(1)*X1,rgamma(1,1)*valueB)
      mux = mean(X1)
      sdx = sd(X1)
      outx = mux + (outlier*sdx) # function for outlier
      X = c(X1,outx)
      muy = mean(Y1)
      sdy = sd(Y1)
      outy = muy + (outlier*sdy) 
      Y = c(Y1, outy)
      return(data.frame(X,Y))
      
    }
    else{
      X1 = rnorm(numPoints,value,valueB)
      Y1 = rnorm(numPoints,rnorm(1)*X1,rgamma(1,1)*valueB)
      mux = mean(X1)
      sdx = sd(X1)
      outx = mux - (outlier*sdx)
      X = c(X1,outx)
      muy = mean(Y1)
      sdy = sd(Y1)
      outy = muy - (outlier*sdy)
      Y = c(Y1, outy)
      return(data.frame(X,Y))
      
      
    }
  }
  else if (difficulty == 1){
    X = rnorm(numPoints,value,valueB)
    Y = rnorm(numPoints,rnorm(1)*X,rgamma(1,1)*valueB)
    return(data.frame(X,Y))
  }
  
}
###########################################Feedback#####################################################################################
generateResponse = function(response){
  if (response==1){
    (sample(list("Correct!","Spot on!","Got it!"),1)[[1]])
  }
  else if (response ==2){
    print(sample(list("Close to the correct answer! ",
                      "Getting close ",
                      "You are almost right","Just a bit off.."),1)[[1]])
  }
  else if (response == 3){
    print(sample(list("When correlation = 1.00/ -1.00, all points are in a line."),1)[[1]])
  }
  else if (response ==4){
    print(sample(list( "Far away...", "Try again"),1)[[1]])
  }
  else if (response ==5){
    print("Check the sign" )
  }
}

################################################################################################################################

shinyServer(
  
  func=function(input, output, clientData, session) {
    observeEvent(input$info,{
      sendSweetAlert(
        session = session,
        title = "Instructions:",
        text = "Generate a new plot, use the slider to guess the correlation, repeat and track your performance in the bottom plot.",
        type = "info"
      )
    })
    score <<- 0 
    hhh <<- 5
    # in order to save the order data we need reactivevalues
    scoresave <- reactiveValues(score = NULL)
    heartsave <- reactiveValues(hhh = NULL)
    answersave <- reactiveValues(answer = NULL)
    corsave <- reactiveValues(correlation = NULL)
    hardsave <- reactiveValues(hard = NULL)
    easysave <- reactiveValues(easy = NULL)
    anhard <- reactiveValues(answerhard = NULL)
    aneasy <- reactiveValues(answereasy = NULL)
    
    observeEvent(input$restart,{
      score <<- 0 
      hhh <<- 5
    })
    output$sub <- renderUI({
      bsButton("submit",
               label = "Submit",
               icon("hand-o-up"),
               size = "medium",
               style = "warning",
               disabled = TRUE)
    })
    
    # force student submit answer before generate a new plot and only submit answer once
    
    observeEvent(input$newplot,{
      
      updateButton(session,"submit",disabled = FALSE)
      updateButton(session,"newplot",disabled = TRUE)
    })
    
    
    observeEvent(input$submit,{
      
      updateButton(session,"submit",disabled = TRUE)
      updateButton(session,"newplot",disabled =FALSE)
    })
    observeEvent(input$check,{
      
      updateButton(session,"check",disabled = TRUE)
    })
    
    # observeEvent(input$finish,{
    #   
    #   updateButton(session,"submit",disabled = TRUE)
    #   updateButton(session,"newplot",disabled =TRUE)
    #   output$lead <- renderUI({
    #     bsButton("leader1", label = "Score Board", icon("table"), size = "large", style = "warning")
    #   })
    # })
    
    
    
    observeEvent(input$submit, {
      answersave$answer <- c(answersave$answer, input$slider)
      score1 = c()
      scoresave$score <- c(scoresave$score, score1)
      
    })
    #observeEvent(input$leader1, {
      
    #  updateTabItems(session, "tabs", "leader")
    #})
    observeEvent(input$start, {
      
      updateTabItems(session, "tabs", "game")
    })
    
    # starting message
    output$click <- reactive({
      if(input$newplot == 0){
        output$click <- renderPlot({"plot1"})
      }
      #else{
      #  output$click <- renderText({
      #    "Click 'Generate New Plot' to Start Game"
      #  })}
    })
    
    
    
    # define difficulty
    observeEvent(input$newplot,{
      withProgress(session, min = 1, max = 15, {
        setProgress(message = 'Generating Plot',
                    detail = '')
        for (i in 1:10) {
          setProgress(value = i)
          Sys.sleep(0.05)
        }
      })
      if(input$difficulty == "Without Outlier"){
        difficulty <- 1
        numPoints <- 50
      }
      else if (input$difficulty == "With Outlier"){
        difficulty <- 2
        numPoints <- sample(5:25, 1)
      }
      else if (input$difficulty == "Random"){
        select = sample(c(1,2), 1) # sample function can make the plot generate random
        select
        if(select == "1"){
          difficulty <-1
          numPoints <- 50
        }
        else if(select == "2"){
          difficulty <-2
          numPoints <- sample(5:25,1)
        }
      }
      data = generateData(difficulty, numPoints)
      correlation = round(cor(data[,1],data[,2]),2)
      ## correct correlation message will be shown after the answer submitted
      output$status1 <- renderText({""}) 
      output$status2 <- renderText({""})
      output$status3 <- renderText({""})
      corsave$correlation <- c(corsave$correlation, round(cor(data[,1],data[,2]),2))
      ## show regression line or not    
      isolate({
        observe({
          
          options = is.na(pmatch(c( "Show Regression Line"), input$options))
          
          output$plot1 <- renderPlot({
            plot(data,
                 col = "#FFA500",
                 cex = 2,
                 pch = 16,
                 main = "Current Scatterplot")
            if (!options[1]){
              fit1 <- lm(data$Y ~ data$X, data)
              plot(data,
                   col = "#FFA500",
                   cex = 2,
                   pch = 16)
              abline(fit1, col = "#BB8FCE", lwd = 2.5)
              
            }
            
          })
        })
      })
    })  
    
    ############################ Track your performance and change the color of points for different level#####################################################       
    output$plot3 <- renderPlot({
      # Adding a progress bar
      withProgress(session, min = 1, max = 15, {
        setProgress(message = 'Generating Plot',
                    detail = '')
        for (i in 1:10) {
          setProgress(value = i)
          Sys.sleep(0.05)
        }
      })
      
      #Start the plot
      
      if(length(answersave$answer) == 0){
        plot(-5, xlim = c(-1,1),
             ylim = c(-1,1),
             xlab = "True Correlation",
             ylab = "Your Answer",
             cex = 2, pch = 16)
        lines(x = seq(-2,2),
              y = seq(-2,2),
              col = "black",
              lwd = "2")
      }
      
      # different dataset to show different color of points
      else{
        plot(y = aneasy$answereasy,
             x = easysave$easy,
             xlim = c(-1,1), 
             ylim = c(-1,1),
             xlab = "True Correlation",
             ylab = "Your Answer",
             main = "Track your Performance",
             cex = 2,
             pch = 16,
             col = "#FF0000")
        points(x= anhard$answerhard,
               y=hardsave$hard, 
               cex = 2,
               pch = 16,
               col ="#690000")
        lines(x = seq(-2,2),
              y = seq(-2,2),
              col = "black",
              lwd = "2")
      }
    })
    ################################################################################################################################
    observeEvent(input$submit,{
      if(input$difficulty == "Without Outlier"){
        difficulty <- 1
        numPoints <- 50   # points will be shown on the plot
      }
      else if (input$difficulty == "With Outlier"){
        difficulty <- 2
        numPoints <- sample(5:25, 1)  # random points
      }
      else if (input$difficulty == "Random"){
        select = sample(1:2, 1)
        select
        if(select=="1"){
          difficulty <- 1
          numPoints <- 50
        }
        else if(select == "2"){
          difficulty <- 2
          numPoints <- sample(5:25,1)
        }
      }
      
      ######## Grading scale: make the shape similar to diamond 
      ###corsave$correlation[length(corsave$correlation)] would take only the last value of a vector
      #otherwise it would check only the first element of the vector
      if((corsave$correlation[length(corsave$correlation)] == 1.00)||(corsave$correlation[length(corsave$correlation)]==-1.00)){
        if(abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.2){
          output$status1 <- renderText({paste(generateResponse(1),generateResponse(3))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+1 # value for hearts
          
          if(difficulty == "1"){
            score <<- score+5}
          else if (difficulty == "2"){
            score <<- score+5}
          
          
        }
        else if (abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.3){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+0
          if(difficulty=="1"){
            score <<- score+0}
          else if (difficulty =="2"){
            score <<- score+0}
        }
        
        else if(((input$slider >0)&& (corsave$correlation[length(corsave$correlation)]<0))||((input$slider <0)&& (corsave$correlation[length(corsave$correlation)]>0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
        }
        
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
          
        }
      }
      
      
      else if(((corsave$correlation[length(corsave$correlation)]<1.00) && (corsave$correlation[length(corsave$correlation)] >=0.70))||((corsave$correlation[length(corsave$correlation)]>-1.00) && (corsave$correlation[length(corsave$correlation)] <=-0.0))){
        if(((input$slider==1.00)&&(abs(input$slider-corsave$correlation)<0.20))||((input$slider==-1.00) &&(abs(input$slider-corsave$correlation)<0.20))){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2),"but",generateResponse(3)
          )})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+0
          if(difficulty=="1"){
            score <<- score+0}
          else if (difficulty =="2"){
            score <<- score+0}
          
        }
        else if(abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.20){
          output$status1 <- renderText({paste(generateResponse(1))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          #hhh <<- hhh+1
          if(difficulty=="1"){
            score <<- score+5}
          else if (difficulty =="2"){
            score <<- score+5}
          
        }
        else if (abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.25){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+0
          if(difficulty=="1"){
            score <<- score+0}
          else if (difficulty =="2"){
            score <<- score+0}
          
        }
        else if(((input$slider >0)&& (corsave$correlation[length(corsave$correlation)]<0))||((input$slider <0)&& (corsave$correlation[length(corsave$correlation)]>0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
        }
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
          
        }
      }
      else if(((corsave$correlation[length(corsave$correlation)]<0.70) && (corsave$correlation[length(corsave$correlation)]>= 0.45))||((corsave$correlation[length(corsave$correlation)]>-0.70) && (corsave$correlation[length(corsave$correlation)]<= -0.45))){
        if(abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.25){
          output$status1 <- renderText({paste(generateResponse(1))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
         # hhh <<- hhh+1
          if(difficulty=="1"){
            score <<- score+5}
          else if (difficulty =="2"){
            score <<- score+5}
          
        }
        else if (abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.35){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+0
          if(difficulty=="1"){
            score <<- score+0}
          else if (difficulty =="2"){
            score <<- score+0}
          
        }
        else if(((input$slider >0)&& (corsave$correlation[length(corsave$correlation)]<0))||((input$slider <0)&& (corsave$correlation[length(corsave$correlation)]>0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
        }
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
          
        }
      }
      else if(((corsave$correlation[length(corsave$correlation)]<0.45))||((corsave$correlation[length(corsave$correlation)]>-0.45))){
        if(abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.25){
          output$status1 <- renderText({paste(generateResponse(1))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
         # hhh <<- hhh+1
          if(difficulty=="1"){
            score <<- score+5}
          else if (difficulty =="2"){
            score <<- score+5}
          
        }
        else if (abs(input$slider-corsave$correlation[length(corsave$correlation)])<0.35){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+0
          if(difficulty=="1"){
            score <<- score+0}
          else if (difficulty =="2"){
            score <<- score+0}
          
        }
        else if(((input$slider >0)&& (corsave$correlation[length(corsave$correlation)]<0))||((input$slider <0)&& (corsave$correlation[length(corsave$correlation)]>0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
        }
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh-1
          if(difficulty=="1"){
            score <<- score-5}
          else if (difficulty =="2"){
            score <<- score-5}
          
        }
        
      }
      ##### seperate values based on the difficulty in order to use different datasets in "Track your Performance" plot
      if(difficulty=="2"){
        hardsave$hard <- c(hardsave$hard,corsave$correlation[length(corsave$correlation)])
        anhard$answerhard <- c(anhard$answerhard, input$slider)
      }
      else if(difficulty=="1"){
        easysave$easy <- c(easysave$easy,corsave$correlation[length(corsave$correlation)])
        aneasy$answereasy <- c(aneasy$answereasy, input$slider)
      }
      ##### The max heart is five 
      if(hhh>5){
        hhh<<- 5
      }
      
      ### using hhh to define html plot
      if(hhh == 5) {
        output$heart1 <- renderUI({
          img(src = "5hearts.png", width = 230)
        })
        #output$heart2 <- renderUI({
         # img(src = "pixelHeart.png", width = 50)
        #})
        #output$heart3 <- renderUI({
        #  img(src = "pixelHeart.png", width = 50)
        #})
        #output$heart4 <- renderUI({
        #  img(src = "pixelHeart.png", width = 50)
        #})
        #output$heart5 <- renderUI({
        #  img(src = "pixelHeart.png", width = 50)
        #})
        
      }
      else if(hhh==4){
        output$heart1 <- renderUI({
          img(src = "4hearts.png", width = 230)
        })
       # output$heart2 <- renderUI({
      #    img(src = "pixelHeart.png", width = 50)
       # })
        #output$heart3 <- renderUI({
        #  img(src = "pixelHeart.png", width = 50)
        #})
        #output$heart4 <- renderUI({
        #  img(src = "pixelHeart.png", width = 50)
        #})
        #output$heart5 <- renderUI({
        #  NULL
        #})
        
      }
      else if(hhh==3){
        output$heart1 <- renderUI({
          img(src = "3hearts.png", width = 230)
        })
        #output$heart2 <- renderUI({
         # img(src = "pixelHeart.png", width = 50)
       # })
        #output$heart3 <- renderUI({
        #  img(src = "pixelHeart.png", width = 50)
        #})
       # output$heart4 <- renderUI({
        #  NULL
       # })
       # output$heart5 <- renderUI({
       #   NULL
       # })
      }
      else if(hhh==2){
        output$heart1 <- renderUI({
          img(src = "2hearts.png", width = 230)
        })
       # output$heart2 <- renderUI({
      #    img(src = "pixelHeart.png", width = 50)
        #})
        #output$heart3 <- renderUI({
        #  NULL
        #})
        #output$heart4 <- renderUI({
        #  NULL
        #})
        #output$heart5 <- renderUI({
        #  NULL
        #})
      }
      else if(hhh==1){
        output$heart1 <- renderUI({
          img(src = "1heart.png", width = 230)
        })
       # output$heart2 <- renderUI({
      #    NULL
       # })
        #output$heart3 <- renderUI({
        #  NULL
        #})
        #output$heart4 <- renderUI({
        #  NULL
        #})
        #output$heart5 <- renderUI({
        #  NULL
        #})
      }
      else if(hhh==0){
        output$heart1 <- renderUI({
          img(src = "gameisover.gif", width = 200)
        })
        output$heart2 <- renderUI({
          NULL
        })
        output$heart3 <- renderUI({
          NULL
        })
        output$heart4 <- renderUI({
          NULL
        })
        output$heart5 <- renderUI({
          NULL
        })
        output$sub <- renderUI({
          NULL
        })
        #output$lead <- renderUI({
        #  bsButton("leader1", label = "Score Board", size = "large", style = "warning")
        #})
        
      }
      #### show score!
      output$score <- renderText({
        paste("Score:", score)
      })
      
    })
    #####Leader Board ########
    score <<- score
    hhh <<- hhh
    output$score1 <- renderText({
      paste("Your Score:", score)
    })
    
    outputDir = "scores"
    
    # options(warn = -1)
    values = reactiveValues()
    
    update = reactive({
      value = data.frame("Name" = as.character(input$name),
                         
                         "TotalScore" = as.numeric(score),
                         "HeartRemaining" = as.numeric(hhh))
      
      
    })
    values$df = data.frame()
    
    
    saveQuestions <- function(data) {
      # data <- t(data)
      # Create a unique file name
      fileName <- sprintf("%s_%s.csv", as.integer(Sys.Date()), digest::digest(data))
      # Write the file to the local system
      write.csv(
        x = data,
        file = file.path(outputDir, fileName), 
        row.names = FALSE, quote = TRUE
      )
    }
    
    loadData <- function() {
      # Read all the files into a list
      files <- list.files(outputDir, full.names = TRUE)
      data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
      # Concatenate all data together into one data.frame
      data <- do.call(rbind, data)
      data
    }
    # First pattern try
    # paste0("^", Sys.Date(), sep = '')
    x = as.character(as.numeric(Sys.Date()))
    y = as.character(as.numeric(Sys.Date() - 7))
    if(substring(x, 5, 5) >= 7){
      pattern = paste0(substring(x, 1, 4), "[", substring(y, 5, 5), "-", substring(x, 5, 5), "]", sep = '')
    }
    else{
      pattern = paste0(substring(y, 1,4), "[",substring(y, 5, 5), "-9]|", substring(x,1,4), "[0-", substring(x, 5, 5), "]")
    }
    
    
    loadDataWeek <- function() {
      files = list.files(outputDir, pattern = pattern, full.names = TRUE)
      data = lapply(files, read.csv, stringsAsFactors = FALSE)
      data = do.call(rbind, data)
      data
    }
    
    data = reactive({
      data = loadData()
      data = data[order(-data[,"TotalScore"], data[,"HeartRemaining"]),]
      # data
    })
    
    data2 = reactive({
      data = loadDataWeek()
      data = data[order(-data[,"TotalScore"], data[,"HeartRemaining"]),]
    })
    
    
    observeEvent(input$check, {
      scores = update()
      values$df = rbind(values$df, scores)
      saveQuestions(values$df)
    })
    observeEvent(input$weekhigh, {
      output$highscore = renderDataTable({
        # head(data(), 5)
        if(is.null(loadDataWeek()) == TRUE){
          "No Highscores This week"
        }
        else{
          data2()  
        } 
        
      })
    })
    
    observeEvent(input$totalhigh, {
      output$highscore = renderDataTable({
        if(is.null(loadData()) == TRUE){
          "No Highscores"
        }
        else{
          data()
        }
      })
    })
  })

