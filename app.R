# Load Libraries ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(MASS)

# Define global constants and functions ----
generateData <- function(difficulty){
  ## Set number of points
  numPoints <- sample(
    x = seq.int(from = 5, to = 50, by = 5),
    size = 1
  )
  
  ## Randomly sample correlation
  targetCorr <- sample(
    x = seq(from = -1, to = 1, by = 0.01),
    size = 1
  )
  
  ## Generate covariance matrix
  corrMat <- matrix(
    data = c(1, targetCorr, targetCorr, 1),
    nrow = 2,
    ncol = 2,
    byrow = TRUE
  )
  
  stdDevX <- rnorm(n = 1, mean = 4, sd = 1)
  stdDevY <- rgamma(n = 1, shape = 1) * stdDevX
  
  stdDevs <- matrix(
    data = c(stdDevX, stdDevY), 
    nrow = 1,
    ncol = 2,
    byrow = TRUE
  )
  
  covMat <- sweep(x = sweep(corrMat, 1, stdDevs, "*"), 2, stdDevs, "*")
  
  ## Generate sample data
  outData <- as.data.frame(
    MASS::mvrnorm(
      n = numPoints,
      mu = rnorm(2, 0, 10),
      Sigma = covMat
    )
  )
  
  colnames(outData) <- c("X", "Y")
  
  ## Adjust for outlier
  if (difficulty == 2) {
    outlierX <- mean(outData$X) + (-1)^(sample(1:2, 1)) * sample(2:6, 1) * sd(outData$X)
    outlierY <- mean(outData$Y) + (-1)^(sample(1:2, 1)) * sample(2:6, 1) * sd(outData$Y)
    outData[numPoints,] <- c(outlierX, outlierY)
  }
  
  return(outData)
}

gradeEstimate <- function(user, corr) {
  diff <- abs(user - corr)
  output <- list(
    heartChange = 0,
    scoreChange = 0,
    icon = "default",
    message = "feedback"
  )
  if (0.9 <= abs(corr) && abs(corr) <= 1) {
    if (user * corr < 0) {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Check your sign."
    } else if (diff < 0.1) {
      output$heartChange <- 1
      output$scoreChange <- 5
      output$icon <- "correct"
      output$message <- "Fantastic Job!"
    } else if (diff < 0.2) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You're close but not quite there."
    } else {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Your guess is too far away."
    }
  } else if (0.7 <= abs(corr) && abs(corr) < 0.9) {
    if (user * corr < 0) {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Check your sign."
    } else if (abs(user) == 1 && diff < 0.2) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You're just a bit off."
    } else if (diff < 0.1) {
      output$heartChange <- 0
      output$scoreChange <- 5
      output$icon <- "correct"
      output$message <- "Great guess!"
    } else if (diff < 0.25) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You're close but not quite there."
    } else {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Your guess is too far away."
    }
  } else if (0.45 <= abs(corr) && abs(corr) < 0.7) {
    if (user * corr < 0) {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Check your sign."
    } else if (diff < 0.2) {
      output$heartChange <- 0
      output$scoreChange <- 5
      output$icon <- "correct"
      output$message <- "Correct!"
    } else if (diff < 0.3) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You're almost there."
    } else {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Your guess is too far away."
    }
  } else if (0.1 < abs(corr) && abs(corr) < 0.45 ) {
    if (user * corr < 0) {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Check your sign."
    } else if (diff < 0.2) {
      output$heartChange <- 0
      output$scoreChange <- 5
      output$icon <- "correct"
      output$message <- "Well done!"
    } else if (diff < 0.3) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You're close but not quite there."
    } else {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Your guess is too far away."
    }
  } else {
    if (user * corr < 0) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You are close!"
    } else if (diff < 0.1) {
      output$heartChange <- 0
      output$scoreChange <- 5
      output$icon <- "correct"
      output$message <- "Well done!"
    } else if (diff < 0.3) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You're close but not quite there."
    } else {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "Your guess is too far away."
    }
  }
  return(output)
}

## Base performance plot ----
basePerformPlot <- ggplot(
  data = data.frame(x = seq(from = -1, to = 1), y = seq(from = -1, to = 1)),
  mapping = aes(x = x, y = y)
) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  xlab("True Correlation") +
  ylab("Your Estimate") +
  labs(title = "Your Performance") +
  theme(
    text = element_text(size = 18)
  ) +
  scale_x_continuous(limits = c(-1, 1), expand = expansion(mult = 0.01, add = 0)) +
  scale_y_continuous(limits = c(-1, 1), expand = expansion(mult = 0.01, add = 0))

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "yellow",
    # Dashboard Header ----
    dashboardHeader(
      titleWidth = 250,
      title = "Correlation Guessing",
      tags$li(
        class = "dropdown",
        actionLink("info", icon("info"))
      ),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Correlation_Guessing")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://shinyapps.science.psu.edu/",
          icon("home")
        )
      )
    ),
    # Dashboard Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub")
        )
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    # Dashboard Body
    dashboardBody(
      tabItems(
        ## First tab - Overview ----
        tabItem(
          tabName = "overview",
          h1("Correlation Guessing"),
          p("This app is designed to help you better understand the
            numerical value of correlations for scatterplots with
            or without outliers."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Select the difficulty mode regarding outliers: Without,
                    With, or Surprise (some graphs will have outliers, some won't)."),
            tags$li("(Optional) - Show the regression line."),
            tags$li("Estimate the correlation using the slider and hit submit."),
            tags$li("Review your feedback."),
            tags$li("Hit 'New Plot' to move to the next scatterplot."),
            tags$li("You have 5 hearts and once you lose all of your hearts,
                     the game is over. You can click the 'RESET' button
                     to restart the game.")
          ),
          p("You'll gain 5 points for each estimate deemed correct; you'll lose 5
            points for each estimate that is too far from the actual value. You
            can earn 0 points if your estimate is somewhat close to the actual
            value."),
          p("Make use of the Performance plot to track how you are doing over
            many tries."),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "start",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          ),
          # Acknowledgements
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Sitong Liu.
            The app was futher updated by Zhiliang Zhang and Jiajun Gao
            in June 2018, Oluwafunke Alliyu in June 2019, Daehoon Gwak in July 2020,
            and Qiaojuan Tu in July 2021.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 10/05/2021 by NJH.")
          )
        ),

        ## Second tab - Prerequisites ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get most out of this app, please review the following:"),
          tags$ul(
            tags$li("Correlation is a measure of the direction and strength of 
                    the linear relationship between two variables. In a sample,
                    we use the symbol \\(r\\), while for the population, we use
                    the Greek letter \\(\\rho\\)."),
            tags$li("Correlation can be no smaller than -1 and no larger than +1,
                    \\(-1\\leq r\\leq1\\)."),
            tags$li("When correlation is positive (\\(r > 0\\)), we say that
                    there is a positive linear association between the variables.
                    When the correlation is negative (\\(r < 0\\)), we say that
                    there is a negative linear association between the variables."),
            tags$li("When there is a correlation of 0 (\\(r = 0\\)), then there
                    is no linear relationship between the variables; the best
                    straight line model is horizontal."),
            tags$li("The closer \\(r\\) is to 0, the weaker the linear relatinship;
                    the closser \\(r\\) is to +1 or -1, the stronger the linear
                    relationship."),
            tags$li("The sign of \\(r\\) (positive or negative) indicates only
                    the direction of the relationship.")
          ),
          br(),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "ready",
              label = "GO!",
              size = "large",
              icon = icon("gamepad")
            )
          )
        ),
        ## Third tab - Game ----
        tabItem(
          tabName = "game",
          h2("Find the Appropriate Correlation"),
          fluidRow(
            column(
              # choose mode
              width = 3,
              wellPanel(
                div(
                  align = "center",
                  selectInput(
                    inputId = "difficulty",
                    label = "Choose difficulty mode",
                    choices = list(
                      "Without Outlier" = 1,
                      "With Outlier" = 2,
                      "Surprise" = 3
                    )
                  ),
                  # checkbox for regression line
                  checkboxInput(
                    inputId = "showRegLine",
                    label = "Show regression line"
                  ),
                  br(),
                  align = "center",
                  sliderInput(
                    inputId = "slider",
                    label = "Select the correlation",
                    min = -1,
                    max = 1,
                    step = 0.01,
                    value = 0
                  ),
                  br(),
                  bsButton(
                    inputId = "submit",
                    label = "Submit",
                    size = "large"
                  ),
                  br(),
                  bsButton(
                    inputId = "newplot",
                    label = "New Plot",
                    size = "large"
                  )
                ),
                br(),
                uiOutput("gradingIcon"),
                uiOutput("feedback"),
                uiOutput("corrVal", class = "bluetext")
              )
            ),
            column(
              width = 6,
              align = "center",
              # Scatter plot
              plotOutput(outputId = "plot1", width = "100%"),
              bsPopover(
                id = "plot1",
                title = "Scatterplot",
                content = paste(
                  "Move the slide bar on the left",
                  "to guess the correlation"
                ),
                placement = "top",
                trigger = "hover"
              ),
              # Alt text
              tags$script(
                HTML(
                  "$(document).ready(function()
                       { document.getElementById('plot1').
                       setAttribute('aria-label',
                       `The scatterplot of points you're to guess the correlation
                       value of`)
                       })"
                )
              ),
              # Track performance plot
              plotOutput(outputId = "plot2", width = "100%"),
              bsPopover(
                id = "plot2",
                title = "Performance Plot",
                content = paste(
                  "Your guess is on the vertical axis while the true value is ",
                  "on the horizontal axis.",
                  "Red dots represents the difficulty level of Without Outliers,",
                  "Blue triangles represent the difficulty of With Outliers,",
                  "and Purple squares represent the Surprise Difficulty level",
                  "where you might have Outliers."
                ),
                placement = "top",
                trigger = "hover"
              ),
              tags$script(
                HTML(
                  "$(document).ready(function()
                       { document.getElementById('plot2').
                       setAttribute('aria-label',
                       `The plot shows the user performance by adding points with
                       a solid black line runing diagonally from the lower left to
                       the upper right indicating perfect guessing`)
                       })"
                )
              )
            ),
            # score UI
            column(
              width = 3,
              wellPanel(
                textOutput("score"),
                uiOutput("heart"),
                bsButton(
                  inputId = "reset",
                  label = "Reset",
                  style = "danger",
                  size = "large"
                )
              )
            )
          ),
          div(
            style = "text-align: right;",
            bsButton(
              inputId = "resetPerf",
              label = "Reset Performance Log",
              style = "danger",
              size = "default"
            )
          )
        ),
        ### References ----
        tabItem(
          tabName = "References",
          h2("References"),
          p( 
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
             R package. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p( 
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
             Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p( # reference for ideas
            class = "hangingindent",
            "Chance, B., and Chance, F. (2014), Guess the Correlation Applet.
            Available from
            http://www.rossmanchance.com/applets/GuessCorrelation.html"
          ),
          p( 
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p( 
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p( 
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny, R package.
            Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
          p(
            class = "hangingindent",
            "Venables, W. N., and Ripley, B. D. (2002) Modern Applied Statistics
             with S. Fourth Edition. Springer, New York. [MASS R Package]."
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the Server ----
server <- function(input, output, session) {
  ## Reactive Values ----
  hearts <- reactiveVal(5)
  score <- reactiveVal(0)
  correlation <- reactiveVal(0)
  numPoints <- reactiveVal(0)
  cooksD <- reactiveVal(0)
  hatVal <- reactiveVal(0)
  tracking <- reactiveValues()
  tracking$DT <- data.frame(
    userGuess = numeric(),
    actualValue = numeric(),
    difficulty = character()
  )

  currentPlot <- reactiveVal()

  ## Render a new plot ----
  newPlot <- function() {

    ### Reset output ----
    output$gradingIcon <- boastUtils::renderIcon()
    output$feedback <- renderUI(NULL)
    output$corrVal <- renderUI(NULL)

    ### Create data ----
    data <- generateData(input$difficulty)
    correlation(round(cor(data$X, data$Y), 2))
    numPoints(nrow(data))
    cooksD(round( cooks.distance(glm(data$Y ~ data$X))[numPoints()], 4))
    hatVal(round(hatvalues(glm(data$Y ~ data$X))[numPoints()], 4))

    ### Make Plots ----
    currentPlot(
      ggplot(
        data = data,
        mapping = aes(x = X, y = Y)
      ) +
        geom_point(color = psuPalette[4], size = 4) +
        theme_bw() +
        labs(title = "Current Scatterplot") +
        xlab("X") +
        ylab("Y") +
        theme(
          text = element_text(size = 18)
        )
    )

    ### Render new plot ----
    output$plot1 <- renderPlot({
      currentPlot()
    })

    ### Update Buttons ----
    updateButton(
      session = session,
      inputId = "submit",
      disabled = FALSE
    )
    updateButton(
      session = session,
      inputId = "newplot",
      disabled = TRUE
    )
    updateCheckboxInput(
      session = session,
      inputId = "showRegLine",
      value = FALSE
    )
  }

  # Info message ----
  observeEvent(
    eventExpr = input$info, 
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "Generate a new plot, use the slider to guess the correlation,
        repeat and track your performance in the bottom plot.",
        type = "info"
      )
    })

  ## Start button ----
  observeEvent(
    eventExpr = input$start, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    })

  ## Ready Button ----
  observeEvent(
    eventExpr = input$ready, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game"
      )
    }
  )

  ## Submit Button ----
  observeEvent(
    eventExpr = input$submit, 
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "newplot",
        disabled = FALSE
      )
      
      ### Store new values ----
      currentPoints <- data.frame(
        userGuess = input$slider,
        actualValue = correlation(),
        difficulty = input$difficulty
      )
      tracking$DT <- rbind(tracking$DT, currentPoints)
      
      results <- gradeEstimate(user = input$slider, corr = correlation())
      
      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-game",
        description = "Find the appropriate correlation",
        interactionType = "numeric",
        response = input$slider,
        success = ifelse(results$scoreChange > 0, TRUE, FALSE),
        extensions = list(
          ref = "https://educationshinyappteam.github.io/BOAST/xapi/result/extensions/context",
          value = jsonlite::toJSON(list(
            target = correlation(),
            numPoints = numPoints(),
            cooksD = cooksD(),
            hatVal = hatVal(),
            delta = (input$slider - correlation()),
            difficulty = input$difficulty,
            feedback = results$message
          ), auto_unbox = TRUE)
        )
      )
      
      boastUtils::storeStatement(session, stmt)
      
      hearts(hearts() + results$heartChange)
      score(score() + results$scoreChange)
      output$gradingIcon <- boastUtils::renderIcon(results$icon)
      output$feedback <- renderUI(results$message)
      output$corrVal <- renderUI({
        paste("True correlation:", correlation())
      })
      
      ##### The max heart is five
      if (hearts() > 5) {
        hearts(5)
      }
    })
  
  ## Reset button ----
  observeEvent(
    eventExpr = input$reset,
    handlerExpr = {
      score(0)
      hearts(5)
      
      newPlot()
    })
  
  ## Making plots? ----
  observeEvent(input$newplot || input$start, 
    handlerExpr = {
      newPlot()
    })
  
  observeEvent(
    eventExpr = input$difficulty, 
    handlerExpr = {
      newPlot()
    })
  
  ## Add linear regression line ----
  observeEvent(
    eventExpr = input$showRegLine, 
    handlerExpr = {
      if (input$showRegLine) {
        output$plot1 <- renderPlot({
          currentPlot() +
            geom_smooth(
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              na.rm = TRUE,
              color = "blue",
              size = 2
            )
        })
      } else {
        output$plot1 <- renderPlot({
          currentPlot()
        })
      }
    })
  
  ## Performance Plot ----
  ## Track your performance and change the color of points for different level
  output$plot2 <- renderPlot({
    if (is.null(tracking$DT) || nrow(tracking$DT) == 0) {
      basePerformPlot
    } else {
      basePerformPlot +
        geom_point(
          data = tracking$DT,
          mapping = aes(
            x = actualValue,
            y = userGuess,
            color = difficulty,
            shape = difficulty
          ),
          size = 4
        ) +
        scale_color_manual(
          name = "Difficulty",
          values = c(
            "1" = boastUtils::psuPalette[2],
            "2" = boastUtils::psuPalette[1],
            "3" = boastUtils::psuPalette[6]
          ),
          labels = c(
            "Without Outlier",
            "With Outlier",
            "Surprise"
          )
        ) +
        scale_shape_manual(
          name = "Difficulty",
          values = c(
            "1" = 16,
            "2" = 17,
            "3" = 15
          ),
          labels = c(
            "Without Outlier",
            "With Outlier",
            "Surprise"
          )
        ) +
        theme(
          legend.position = "bottom"
        )
    }
  })
  
  ## Game Heart Display ----
  ### Score ----
  output$score <- renderText({
    paste("Score:", score())
  })
  
  ### Remaining hearts ----
  output$heart <- renderUI({
    if (hearts() == 5) {
      img(
        src = "5hearts.png",
        alt = "You have five hearts remaining",
        width = "100%"
      )
    } else if (hearts() == 4) {
      img(
        src = "4hearts.png",
        alt = "You have four hearts remaining",
        width = "100%"
      )
    } else if (hearts() == 3) {
      img(
        src = "3hearts.png",
        alt = "You have three hearts remaining",
        width = "100%"
      )
    } else if (hearts() == 2) {
      img(
        src = "2hearts.png",
        alt = "You have two hearts remaining",
        width = "100%"
      )
    } else if (hearts() == 1) {
      img(
        src = "1heart.png",
        alt = "You have one heart remaining",
        width = "100%"
      )
    } else {
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "newplot", disabled = TRUE)
      msg <- "You have no hearts left; game over"
      
      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "failed",
        object = "shiny-tab-game",
        description = "Find the appropriate correlation",
        response = msg,
        success = FALSE
      )
      
      boastUtils::storeStatement(session, stmt)
      img(
        src = "gameisover.gif",
        alt = msg,
        width = "100%"
      )
    }
  })
  
  ## Reset Performance Button ----
  observeEvent(
    eventExpr = input$resetPerf,
    handlerExpr = {
      tracking$DT <- data.frame(
        userGuess = numeric(),
        actualValue = numeric(),
        difficulty = character()
      )
    }
  )
}


# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

