library(boastUtils)
# this function, especially the easy level is from xxx: 
# the function is about correlation plot
generateData = function(difficulty,numPoints){
  value = rnorm(1, 0, 10)
  valueB = rnorm(1, 4, 1)
  outlier = as.numeric( sample(2:6, 1)) #create a random number
  if (difficulty == 2){ # with outlier
    choice = sample(2, 1)
    if (choice == 2){
      X1 = rnorm(numPoints, value, valueB)
      Y1 = rnorm(numPoints, rnorm(1) * X1, rgamma(1, 1)*valueB)
      mux = mean(X1)
      sdx = sd(X1)
      outx = mux + (outlier * sdx) # function for outlier
      X = c(X1, outx)
      muy = mean(Y1)
      sdy = sd(Y1)
      outy = muy + (outlier * sdy) 
      Y = c(Y1, outy)
      return(data.frame(X, Y))
    }
    else{
      X1 = rnorm(numPoints, value, valueB)
      Y1 = rnorm(numPoints, rnorm(1) * X1, rgamma(1, 1) * valueB)
      mux = mean(X1)
      sdx = sd(X1)
      outx = mux - (outlier * sdx)
      X = c(X1, outx)
      muy = mean(Y1)
      sdy = sd(Y1)
      outy = muy - (outlier * sdy)
      Y = c(Y1, outy)
      return(data.frame(X, Y))
    }
  }
  else if (difficulty == 1){
    X = rnorm(numPoints, value, valueB)
    Y = rnorm(numPoints, rnorm(1) * X, rgamma(1, 1) * valueB)
    return(data.frame(X, Y))
  }
}
## Feedback
generateResponse = function(response){
  if (response == 1){
    sample(list("Correct!","Spot on!","Got it!"), 1)[[1]]
  }
  else if (response == 2){
    sample(list("Close to the correct answer! ", "Getting close ", 
                "You are almost right","Just a bit off.."), 1)[[1]]
  }
  else if (response == 3){
    sample(list("When correlation = 1.00/ -1.00,  
                all points are in a line."), 1)[[1]]
  }
  else if (response == 4){
    sample(list( "Far away...", "Try again"), 1)[[1]]
  }
  else if (response == 5){
    "Check the sign"
  }
}
## Server Starts ---
shinyServer(
  func=function(input, output, clientData, session) {
    #Initialized learning  locker connection
    connection <- rlocker::connect(session, list(
      base_url = "https://learning-locker.stat.vmhost.psu.edu/",
      # This weird thing should be in the single-line. paste() function does not work
      auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",  
      agent = rlocker::createAgent()
    ))
    # Setup demo app and user.
    currentUser <- 
      connection$agent
    if(connection$status != 200){
      warning(paste(connection$status, "\nTry checking your auth token.")) 
    }
    # Info message
    observeEvent(input$info,{
      sendSweetAlert(
        session = session,
        title = "Instructions:", 
        text = "Generate a new plot, use the slider to guess the correlation, 
        repeat and track your performance in the bottom plot.", 
        type = "info"
      )
    })
    # in order to save the order data we need reactivevalues
    scoresave <- reactiveValues(score = NULL)
    heartsave <- reactiveValues(hhh = NULL)
    answersave <- reactiveValues(answer = NULL)
    corsave <- reactiveValues(correlation = NULL)
    hardsave <- reactiveValues(hard = NULL)
    easysave <- reactiveValues(easy = NULL)
    anhard <- reactiveValues(answerhard = NULL)
    aneasy <- reactiveValues(answereasy = NULL)
    # set score and heart value
    score <<- 0 
    hhh <<- 5
    # navigate to the game tab
    observeEvent(input$start, {
      updateTabItems(session, "tabs", "game")
    })
    ## Start the challenge
    # show score!
    output$score <- renderText({
      paste("Score:", score)
    })
    output$heart <- renderUI({
      if(hhh == 5){
        img(src = "5hearts.png",
            alt = "Show how many life user has - five", 
            width = '100%')
      }
    })
    # Generate Plot actions
    observeEvent(input$newplot,{
      # progress bar
      withProgress(session, min = 1, max = 15, {
        setProgress(message = 'Generating Plot', 
                    detail = '')
        for (i in 1:10) {
          setProgress(value = i)
          Sys.sleep(0.05)
        }
      })
      updateButton(session, "submit", disabled = FALSE)
      updateButton(session, "newplot", disabled = TRUE)
    })
    # Submit Actions
    observeEvent(input$submit,{
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "newplot", disabled = FALSE)
      answersave$answer <- c(answersave$answer, input$slider)
      score1 = c()
      scoresave$score <- c(scoresave$score, score1)
      if(input$difficulty == "Without Outlier"){
        difficulty <- 1
        numPoints <- 50   # points will be shown on the plot
      }
      else if (input$difficulty == "With Outlier"){
        difficulty <- 2
        numPoints <- sample(5:25, 1)  # random points
      }
      else if (input$difficulty == "Both"){
        select = sample(1:2, 1)
        select
        if(select == "1"){
          difficulty <- 1
          numPoints <- 50
        }
        else if(select == "2"){
          difficulty <- 2
          numPoints <- sample(5:25, 1)
        }
      }
      ## Grading scale: make the shape similar to diamond 
      ## corsave$correlation[length(corsave$correlation)]
      ## would take only the last value of a vector
      ## otherwise it would check only the first element of the vector
      if((corsave$correlation[length(corsave$correlation)] == 1.00)||
         (corsave$correlation[length(corsave$correlation)] == -1.00)){
        if(abs(input$slider - corsave$correlation[length(corsave$correlation)])
           < 0.2){
          output$status1 <- renderText({paste(generateResponse(1), 
                                              generateResponse(3))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+1 # value for hearts
          if(difficulty == "1"){
            score <<- score+5}
          else if (difficulty == "2"){
            score <<- score+5}
        }
        else if (abs(input$slider - corsave$correlation[
            length(corsave$correlation)]) < 0.3){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh+0
          if(difficulty == "1"){
            score <<- score + 0}
          else if (difficulty == "2"){
            score <<- score + 0}
        }
        else if(((input$slider > 0) && (corsave$correlation[
                    length(corsave$correlation)] < 0))|| ((input$slider < 0) 
                   && (corsave$correlation[length(corsave$correlation)] > 0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",
                             corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",
                             corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
      }
      else if(((corsave$correlation[length(corsave$correlation)] < 1.00) 
               && (corsave$correlation[length(corsave$correlation)] >= 0.70))||
              ((corsave$correlation[length(corsave$correlation)] > -1.00)
               && (corsave$correlation[length(corsave$correlation)] <= -0.0))){
        if(((input$slider == 1.00) && (abs(input$slider - corsave$correlation)
                                       < 0.20))||
           ((input$slider == -1.00) && (abs(input$slider - corsave$correlation)
                                        < 0.20))){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2), "but",
                                              generateResponse(3)
          )})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh + 0
          if(difficulty == "1"){
            score <<- score + 0}
          else if (difficulty == "2"){
            score <<- score + 0}
        }
        else if(abs(input$slider - corsave$correlation[
                      length(corsave$correlation)]) < 0.20){
          output$status1 <- renderText({paste(generateResponse(1))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          if(difficulty == "1"){
            score <<- score + 5}
          else if (difficulty == "2"){
            score <<- score + 5}
        }
        else if (abs(input$slider - corsave$correlation[
                                  length(corsave$correlation)]) < 0.25){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",
                             corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh + 0
          if(difficulty == "1"){
            score <<- score + 0}
          else if (difficulty == "2"){
            score <<- score + 0}
        }
        else if(((input$slider > 0) && (corsave$correlation[
                length(corsave$correlation)] < 0))||((input$slider < 0) 
                  && (corsave$correlation[length(corsave$correlation)] > 0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
      }
      else if(((corsave$correlation[length(corsave$correlation)] < 0.70)
               && (corsave$correlation[length(corsave$correlation)] >= 0.45))||
              ((corsave$correlation[length(corsave$correlation)] > -0.70)
               && (corsave$correlation[length(corsave$correlation)]<= -0.45))){
        if(abs(input$slider - corsave$correlation[length(corsave$correlation)])
                                                                      < 0.25){
          output$status1 <- renderText({paste(generateResponse(1))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          if(difficulty == "1"){
            score <<- score + 5}
          else if (difficulty == "2"){
            score <<- score + 5}
        }
        else if (abs(input$slider - corsave$correlation[
                              length(corsave$correlation)]) < 0.35){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",
                             corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh + 0
          if(difficulty == "1"){
            score <<- score + 0}
          else if (difficulty == "2"){
            score <<- score + 0}
        }
        else if(((input$slider > 0) && (corsave$correlation[
              length(corsave$correlation)] < 0))||((input$slider < 0)
                   && (corsave$correlation[length(corsave$correlation)] > 0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
      }
      else if(((corsave$correlation[length(corsave$correlation)] < 0.45))||
              ((corsave$correlation[length(corsave$correlation)] >- 0.45))){
        if(abs(input$slider - corsave$correlation[length(corsave$correlation)])
                                                                  < 0.25){
          output$status1 <- renderText({paste(generateResponse(1))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          if(difficulty == "1"){
            score <<- score + 5}
          else if (difficulty == "2"){
            score <<- score + 5}
        }
        else if (abs(input$slider - corsave$correlation[
                                length(corsave$correlation)]) < 0.35){
          output$status1 <- renderText({""})
          output$status2 <- renderText({paste(generateResponse(2))})
          output$status3 <- renderText({paste("True correlation: ",
                           corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh + 0
          if(difficulty == "1"){
            score <<- score + 0}
          else if (difficulty == "2"){
            score <<- score + 0}
        }
        else if(((input$slider > 0) && (corsave$correlation[
                                      length(corsave$correlation)] < 0))||
                ((input$slider <0) && (corsave$correlation[
                                      length(corsave$correlation)] > 0))){
          output$status1 <- renderText({paste(generateResponse(5))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation:",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
        else {
          output$status1 <- renderText({paste(generateResponse(4))})
          output$status2 <- renderText({""})
          output$status3 <- renderText({paste("True correlation: ",
                            corsave$correlation[length(corsave$correlation)])})
          hhh <<- hhh - 1
          if(difficulty == "1"){
            score <<- score - 5}
          else if (difficulty == "2"){
            score <<- score - 5}
        }
      }
      ##### seperate values based on the difficulty in order to use different datasets in "Track your Performance" plot
      if(difficulty == "2"){
        hardsave$hard <- c(hardsave$hard,corsave$correlation[
                                                  length(corsave$correlation)])
        anhard$answerhard <- c(anhard$answerhard, input$slider)
      }
      else if(difficulty == "1"){
        easysave$easy <- c(easysave$easy,corsave$correlation[
                                                  length(corsave$correlation)])
        aneasy$answereasy <- c(aneasy$answereasy, input$slider)
      }
      ##### The max heart is five 
      if(hhh > 5){
        hhh <<- 5
      }
      ### remaining hearts
      if(hhh == 5) {
        output$heart <- renderUI({
          img(src = "5hearts.png",
              alt = "Show how many life user has - five",
              width = '100%')
        })
      }
      else if(hhh == 4){
        output$heart <- renderUI({
          img(src = "4hearts.png",
              alt = "Show how many life user has - four",
              width = '100%')
        })
      }
      else if(hhh == 3){
        output$heart <- renderUI({
          img(src = "3hearts.png",
              alt = "Show how many life user has - three",
              width = '100%')
        })
      }
      else if(hhh == 2){
        output$heart <- renderUI({
          img(src = "2hearts.png",
              alt = "Show how many life user has - two",
              width = '100%')
        })
      }
      else if(hhh == 1){
        output$heart <- renderUI({
          img(src = "1heart.png",
              alt = "Show how many life user has - one",
              width = '100%')
        })
      }
      else if(hhh == 0){
        output$heart <- renderUI({
          img(src = "gameisover.gif",
              alt = "Show how many life user has - no life(game over)",
              width = '100%')
        })
        updateButton(session, "submit", disabled = TRUE)
        updateButton(session, "newplot", disabled = TRUE)
        updateButton(session, "reset", disabled = FALSE)
      }
      #### show score!
      output$score <- renderText({
        paste("Score:", score)
      })
      statement <- rlocker::createStatement(
        list(
          verb = list(
            display = "submitted"
          ),
          object = list(
            id = paste0(getCurrentAddress(session), "#", 1),
            name = paste('Type'),
            description = "NA"
          ),
          result = list(
            success = if(score > 
                         0) TRUE else FALSE,
            response = "SOMETHING"
          )
        )
      )
      # Store statement in locker and return status
      status <- rlocker::store(session, statement)
    })
    observeEvent(input$reset,{
      score <<- 0 
      hhh <<- 5
      # reset the points in the track performance plot
      aneasy$answereasy <- NULL
      easysave$easy <- NULL
      anhard$answerhard <- NULL
      hardsave$hard <- NULL
      if(hhh == 5) {
        output$heart <- renderUI({
          img(src = "5hearts.png",
              alt = "Show how many life user has - five", 
              width = '100%')
        })
        output$score <- renderText({
          paste("Score:", score)
        })
      }
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "newplot", disabled = FALSE)
      updateButton(session, "reset", disabled = TRUE)
    })
    # define difficulty - when clicking - Generate Plot, GO!
    observeEvent(input$newplot || input$start,{
      if(input$difficulty == "Without Outlier"){
        difficulty <- 1
        numPoints <- 50
      }
      else if (input$difficulty == "With Outlier"){
        difficulty <- 2
        numPoints <- sample(5:25, 1)
      }
      else if (input$difficulty == "Both"){
        select = sample(c(1, 2), 1) # sample function can make the plot generate random
        select
        if(select == "1"){
          difficulty <-1
          numPoints <- 50
        }
        else if(select == "2"){
          difficulty <-2
          numPoints <- sample(5:25, 1)
        }
      }
      data = generateData(difficulty, numPoints)
      correlation = round(cor(data[, 1],data[, 2]), 2)
      ## correct correlation message will be shown after the answer submitted
      output$status1 <- renderText({""}) 
      output$status2 <- renderText({""})
      output$status3 <- renderText({""})
      corsave$correlation <- c(corsave$correlation, round(cor(data[, 1],
                                                              data[, 2]), 2))
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
                   pch = 16, 
                   main = "Current Scatterplot")
              abline(fit1, col = "#BB8FCE", lwd = 2.5)
            }
          })
        })
      })
      # update Buttons
      updateButton(session, "submit", disabled = FALSE)
      updateButton(session, "newplot", disabled = TRUE)
      updateButton(session, "reset", disabled = TRUE)
    }) 
    # define difficulty - when clicking - Game tab
    observeEvent(input$tabs,{
      if(input$tabs == 'game') {
        if (input$difficulty == "Without Outlier") {
          difficulty <- 1
          numPoints <- 50
        }
        else if (input$difficulty == "With Outlier") {
          difficulty <- 2
          numPoints <- sample(5:25, 1)
        }
        else if (input$difficulty == "Both") {
          select = sample(c(1, 2), 1) # sample function can make the plot generate random
          select
          if (select == "1") {
            difficulty <- 1
            numPoints <- 50
          }
          else if (select == "2") {
            difficulty <- 2
            numPoints <- sample(5:25, 1)
          }
        }
        data = generateData(difficulty, numPoints)
        correlation = round(cor(data[, 1], data[, 2]), 2)
        ## correct correlation message will be shown after the answer submitted
        output$status1 <- renderText({
          ""
        })
        output$status2 <- renderText({
          ""
        })
        output$status3 <- renderText({
          ""
        })
        corsave$correlation <-
          c(corsave$correlation, round(cor(data[, 1],
                                           data[, 2]), 2))
        ## show regression line or not
        isolate({
          observe({
            options = is.na(pmatch(c("Show Regression Line"), input$options))
            output$plot1 <- renderPlot({
              plot(
                data,
                col = "#FFA500", 
                cex = 2, 
                pch = 16, 
                main = "Current Scatterplot"
              )
              if (!options[1]) {
                fit1 <- lm(data$Y ~ data$X, data)
                plot(
                  data, 
                  col = "#FFA500", 
                  cex = 2, 
                  pch = 16, 
                  main = "Current Scatterplot"
                )
                abline(fit1, col = "#BB8FCE", lwd = 2.5)
              }
            })
          })
        })
        # update Buttons
        updateButton(session, "submit", disabled = FALSE)
        updateButton(session, "newplot", disabled = TRUE)
        updateButton(session, "reset", disabled = TRUE)
      }
    })
    ## Track your performance and change the color of points for different level
    output$plot2 <- renderPlot({
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
      plot(-5, xlim = c(-1, 1),
           ylim = c(-1, 1),
           xlab = "True Correlation", 
           ylab = "Your Answer", 
           main = "Track your Performance", 
           cex = 2, pch = 16)
      lines(x = seq(-2, 2), 
            y = seq(-2, 2), 
            col = "black", 
            lwd = "2")
      # different dataset to show different color of points
      if(length(answersave$answer) != 0){
        plot(y = aneasy$answereasy, #without outlier
             x = easysave$easy, 
             xlim = c(-1, 1), 
             ylim = c(-1, 1), 
             xlab = "True Correlation", 
             ylab = "Your Answer", 
             main = "Track your Performance", 
             cex = 2, 
             pch = 16, 
             col = "#ff0000")
        points(x = anhard$answerhard, #with outlier
               y = hardsave$hard,  
               cex = 2, 
               pch = 16, 
               col = "#2300ff")
        lines(x = seq(-2, 2), 
              y = seq(-2, 2), 
              col = "black", 
              lwd = "2")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      }
    })
    # Gets current page address from the current session
    getCurrentAddress <- function(session){
      return(paste0(
        session$clientData$url_protocol, "//",
        session$clientData$url_hostname,
        session$clientData$url_pathname, ":",
        session$clientData$url_port,
        session$clientData$url_search
      ))
    }
})

