#library(rlocker)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
# UI starts --- 
shinyUI(
  dashboardPage(
    skin = "yellow", 
    #Dashboard Header
    dashboardHeader(
      titleWidth = 250, 
      title = "Correlation Guessing", 
      tags$li(class = "dropdown", 
              actionLink("info",icon("info",class = "myClass"))), 
      tags$li(
        class = "dropdown", 
        tags$a(href = "https://shinyapps.science.psu.edu/", 
               icon("home", lib = "font-awesome"))
      )
    ), 
    #Dashboard Sidebar
    dashboardSidebar(
      width = 250, 
      sidebarMenu(id = "tabs", 
                  menuItem("Overview", tabName = "overview", 
                           icon = icon("dashboard")), 
                  menuItem("Game", tabName = "game", icon = icon("gamepad")), 
                  menuItem("Reference", tabName = "References", 
                           icon = icon("leanpub"))
      ),
      tags$div(class = "sidebar-logo", 
               boastUtils::psu_eberly_logo("reversed"))
    ), 
    #Dashboard Body
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", 
                  href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"), 
        #Change the style of progress bar
        tags$style(
          HTML(
            ".shiny-notification {
                                 height: 100px;
                                 width: 800px;
                                 position:fixed;
                                 top: calc(50% - 50px);;
                                 left: calc(50% - 400px);;
                                 }
                                 "
          )
        )
      ), 
      tabItems(
        ## First tab - Overview
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
            tags$li("Select the mode"), 
            tags$li("(Optional) - Show the regression line"), 
            tags$li("Estimate the correlation using the slider and hit submit.
                    Next, hit 'Generate Plot' to proceed the game"), 
            tags$li("You have 5 lives and once you lose all hearts, game is over
                    and you can clikc 'RESET' button to restart"), 
            tags$li("Gain 5 points per correct answer and 
                    lose 5 points per wrong answer"), 
            tags$li("You can also track your performance from 
                    the performance plot")
          ), 
          div(
            style = "text-align: center" , 
            bsButton(
              inputId = "start", 
              label = "GO!", 
              size = "large", 
              icon = icon("bolt")
            )
          ), 
          #Acknowledgement
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
            div(class = "updated", "Last Update: 7/24/2020 by DG.")
          )
        ), 
        ## Second tab - Game
        tabItem(tabName = "game", 
                h2('Find the appropriate correlation'), 
                fluidRow(
                  column(
                    #choose mode
                    width = 3, 
                    wellPanel(
                      div(
                        align = "center", 
                        selectInput(
                          inputId = "difficulty", 
                          label = strong("Choose Mode"), 
                          choices = list("Without Outlier", "With Outlier", 
                                         "Both"), 
                          width = '100%'
                        ), 
                        #checkbox for regression line
                        checkboxGroupInput(
                          inputId = "options", 
                          label = "", 
                          choices = list("Show Regression Line"), 
                          width = '100%'
                        )
                      ), 
                      br(), 
                      div(
                        align = 'center', 
                        sliderInput(
                          inputId = "slider", 
                          label = "Select the Correlation", 
                          min = -1, 
                          max = 1, 
                          step = 0.01, 
                          value = 0, 
                          width = '100%'
                        )
                      ), 
                      br(),
                      div(align = 'center',
                          bsButton(
                            inputId = "submit", 
                            label = "Submit", 
                            icon = icon("hand-o-up"), 
                            size = "large", 
                            width = '100%'
                          )
                      ), 
                      br(), 
                      div(
                        align = 'center', 
                        bsButton(
                          inputId = "newplot", 
                          label = "Generate Plot", 
                          icon = icon("arrow-circle-right"), 
                          size = "large", 
                          width = '100%')
                      ), 
                      br(), 
                      #Pass or Fail notification button
                      div(textOutput("status1"), align = 'center', 
                          class = "redtext"), 
                      div(textOutput("status2"), align = 'center'), 
                      br(), 
                      #True correlation
                      div(textOutput("status3"), align = 'center', 
                          class = "bluetext"), 
                    )
                  ), 
                  column(
                    width = 6, 
                    align = "center", 
                    # Scatter plot
                    plotOutput(outputId = "plot1", width = '100%'), 
                    bsPopover(
                      id = "plot1", 
                      title =  "Scatterplot", 
                      content = paste("Move the slide bar on the left", 
                                      "to guess the correlation"), 
                      place = "top", 
                      trigger = "hover"
                    ),  
                    # Alt text
                    tags$script(
                      HTML(
                        "$(document).ready(function()
                       { document.getElementById('plot1').
                       setAttribute('aria-label',
                       `This is a Scatter plot`)
                       })"
                      )
                    ), 
                    # Track performance plot
                    plotOutput(outputId = "plot2", width = '100%'), 
                    bsPopover(
                      id = "plot2", 
                      title = "Your Guess vs. Answer", 
                      content = paste("Red dot represents Without outlier", 
                                      "and Blue dot represents With outlier."), 
                      placement = "top", 
                      trigger = "hover"
                    ), 
                    tags$script(
                      HTML(
                        "$(document).ready(function()
                       { document.getElementById('plot2').
                       setAttribute('aria-label',
                       `The plot shows the user performance by adding points`)
                       })"
                      )
                    )
                  ), 
                  br(), 
                  # score UI
                  column(
                    width = 3, 
                    wellPanel(
                      textOutput("score"), 
                      uiOutput('heart'), 
                      actionButton(inputId = "reset", label =  "RESET", 
                                   width = '100%')
                    )
                  )
                )
        ), 
        tabItem(
          tabName = "References", 
          h2("References"), 
          p(     #shinyBS
            class = "hangingindent", 
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, 
             R package. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ), 
          p(     #Boast Utilities
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
             Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ), 
          p(     #reference for ideas
            class = "hangingindent", 
            "Chance, B., and Chance, F. (2014), Guess the Correlation Applet. 
            Available from 
            http://www.rossmanchance.com/applets/GuessCorrelation.html"
          ), 
          p(     #shinydashboard
            class = "hangingindent", 
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ), 
          p(     #shiny
            class = "hangingindent", 
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ), 
          p(     #shinyWidgets
            class = "hangingindent", 
            "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny, R package.
            Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          )
        )
      )
    )
  )
)
