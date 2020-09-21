# Load Libraries ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Correlation Guessing"
APP_DESCP <<- paste(
  "This app helps a student better understand the numerical value of correlations",
  "for scatterplots with or without outliers."
)
## End App Meta Data------------------------------------------------------------

# Define global constants and functions ----
generateData <- function(difficulty) {
  if (as.numeric(difficulty) == 3) {
    difficulty <- sample(1:2, 1)
    numPoints <- ifelse(as.numeric(difficulty) == 1, 50, sample(5:25, 1))
  } else {
    numPoints <- ifelse(as.numeric(difficulty) == 1, 50, sample(5:25, 1))
  }
  value <- rnorm(1, 0, 10)
  valueB <- rnorm(1, 4, 1)
  outlier <- as.numeric(sample(2:6, 1)) # create a random number
  if (difficulty == 2) { # with outlier
    choice <- sample(2, 1)
    if (choice == 2) {
      X1 <- rnorm(numPoints, value, valueB)
      Y1 <- rnorm(numPoints, rnorm(1) * X1, rgamma(1, 1) * valueB)
      mux <- mean(X1)
      sdx <- sd(X1)
      outx <- mux + (outlier * sdx) # function for outlier
      X <- c(X1, outx)
      muy <- mean(Y1)
      sdy <- sd(Y1)
      outy <- muy + (outlier * sdy)
      Y <- c(Y1, outy)
      return(data.frame(X, Y))
    }
    else {
      X1 <- rnorm(numPoints, value, valueB)
      Y1 <- rnorm(numPoints, rnorm(1) * X1, rgamma(1, 1) * valueB)
      mux <- mean(X1)
      sdx <- sd(X1)
      outx <- mux - (outlier * sdx)
      X <- c(X1, outx)
      muy <- mean(Y1)
      sdy <- sd(Y1)
      outy <- muy - (outlier * sdy)
      Y <- c(Y1, outy)
      return(data.frame(X, Y))
    }
  }
  else if (difficulty == 1) {
    X <- rnorm(numPoints, value, valueB)
    Y <- rnorm(numPoints, rnorm(1) * X, rgamma(1, 1) * valueB)
    return(data.frame(X, Y))
  }
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
    } else if (diff < 0.05) {
      output$heartChange <- 1
      output$scoreChange <- 5
      output$icon <- "correct"
      output$message <- "Fantastic Job!"
    } else if (diff < 0.15) {
      output$heartChange <- 0
      output$scoreChange <- 0
      output$icon <- "partial"
      output$message <- "You're close but not quite there."
    } else {
      output$heartChange <- -1
      output$scoreChange <- -5
      output$icon <- "incorrect"
      output$message <- "You're guess is too far away."
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
      output$message <- "You're guess is too far away."
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
      output$message <- "You're guess is too far away."
    }
  } else { # -0.45 < corr < 0.45
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
      output$message <- "You're guess is too far away."
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
        actionLink("info", icon("info", class = "myClass"))
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://shinyapps.science.psu.edu/",
          icon("home", lib = "font-awesome")
        )
      )
    ),
    # Dashboard Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview",
          tabName = "overview",
          icon = icon("tachometer-alt")
        ),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References",
          tabName = "References",
          icon = icon("leanpub")
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
          # Title
          p("This App is designed to help you better understand the
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
                     to restart the game."),
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
            in June 2018 and Oluwafunke Alliyu in June 2019
            and Daehoon Gwak in July 2020.
            Special thanks to Caihui Xiao and Yuxin Zhang
            for help on some programming issues.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/15/2020 by NJH.")
          )
        ),
        ## Second tab - Game ----
        tabItem(
          tabName = "game",
          h2("Find the appropriate correlation"),
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
                    inputId = "options",
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
                place = "top",
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
                  "and Purple Squares represent the Surprise Difficulty level",
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
          p( # shinyBS
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
             R package. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p( # Boast Utilities
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
          p( # shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p( # shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p( # shinyWidgets
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny, R package.
            Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
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
server <- function(input, output, clientData, session) {
  ## Reactive Values ----
  hearts <- reactiveVal(5)
  score <- reactiveVal(0)
  correlation <- reactiveVal(0)
  tracking <- reactiveValues()
  tracking$DT <- data.frame(
    userGuess = numeric(),
    actualValue = numeric(),
    difficulty = character()
  )
  currentPlot <- reactiveVal()

  # Info message ----
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "Generate a new plot, use the slider to guess the correlation,
        repeat and track your performance in the bottom plot.",
      type = "info"
    )
  })

  ## Start button ----
  observeEvent(input$start, {
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "game"
    )
  })

  ## New plot button ----
  observeEvent(input$newplot, {
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

    output$gradingIcon <- boastUtils::renderIcon()
    output$feedback <- renderUI(NULL)
    output$corrVal <- renderUI(NULL)

    # Generation of data?
  })

  ## Submit Button ----
  observeEvent(input$submit, {
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
      response = jsonlite::toJSON(list(
        answered = input$slider,
        target = correlation(),
        delta = (input$slider - correlation()),
        difficulty = input$difficulty,
        feedback = results$message
      ), auto_unbox = TRUE),
      success = ifelse(results$scoreChange > 0, TRUE, FALSE)
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
  observeEvent(input$reset, {
    score(0)
    hearts(5)

    output$gradingIcon <- boastUtils::renderIcon()
    output$feedback <- renderUI(NULL)
    output$corrVal <- renderUI(NULL)

    ### Create data ----
    data <- generateData(input$difficulty)
    correlation(round(cor(data[, 1], data[, 2]), 2))

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
      inputId = "options",
      value = FALSE
    )
  })

  ## Making plots?----
  # define input$difficulty - when clicking - Generate Plot, GO!
  observeEvent(input$newplot || input$start, {

    ### Create data ----
    data <- generateData(input$difficulty)
    correlation(round(cor(data[, 1], data[, 2]), 2))

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
      inputId = "options",
      value = FALSE
    )
  })

  ## Add linear regression line ----
  observeEvent(input$options, {
    if (input$options) {
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
  observeEvent(input$resetPerf, {
    tracking$DT <- data.frame(
      userGuess = numeric(),
      actualValue = numeric(),
      difficulty = character()
    )
  })
}


# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
