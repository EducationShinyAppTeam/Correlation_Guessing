library(shiny)
library(ggplot2)
library(shinyBS)
library(shinyjs)
library(V8)
library(shinydashboard)
library(shinyWidgets)

rm(list = ls())
shinyUI(dashboardPage(skin = "yellow",
                      
                      dashboardHeader(title = "Correlation Guessing Game",
                                      tags$li(class = "dropdown",
                                              tags$a(href = "https://shinyapps.science.psu.edu/",
                                                     icon("home", lib = "font-awesome"))), 
                                      tags$li(class = "dropdown",
                                              actionLink("info", icon("info"), class = "myClass"))),
                      dashboardSidebar(
                        sidebarMenu(
                          id = "tabs",
                          menuItem("Overview", tabName = "intro",icon = icon("dashboard")),
                          menuItem("Game", tabName = "game",icon = icon("gamepad"))
                          #menuItem("Score Board", tabName = "leader",icon=icon("dashboard"))
                        )
                      ),
                      dashboardBody(
                        tags$head( 
                          tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
                          tags$style(HTML(
                            '.popover-title{
                            color: black;
                            background-color: orange }'
                          )),
                          
                          #Change the style of progress bar
                          tags$style(
                            HTML(".shiny-notification {
                                 height: 100px;
                                 width: 800px;
                                 position:fixed;
                                 top: calc(50% - 50px);;
                                 left: calc(50% - 400px);;
                                 }
                                 "
                            )
                            )),
                        tabItems(
                          # First tab content
                          tabItem(tabName = "intro",
                                  
                                  #' fluidRow(
                                  #'    the color for slider bar
                                  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange}")),
                                  
                                    tags$a(href='http://stat.psu.edu/', tags$img(src ='logo.png', align = "left", width = 180)),
                                    br(),
                                    br(),
                                    br(),
                                    h3(strong("About:")),
                                    
                                    h4("This game is designed to help you better understand the numerical value of correlations for scatterplots with or without outliers."),
                                    
                                    h3(strong("Instructions:")),
                                    tags$ol(
                                    h4(tags$li("Select the mode")),
                                    h4(tags$li("(Optional) - Show the regression line")),
                                    h4(tags$li("Estimate the correlation using the slider")),
                                    h4(tags$li("Hit submit"))),
                                    h3(strong("Scoring")),
                                    h4(tags$li("Start the game with 5 lives (hearts)")),
                                    h4(tags$li("Gain 5 points per correct answer")),
                                    h4(tags$li("Lose 5 points per wrong answer")),
                                    br(),
                                    div(style = "text-align: center" ,
                                        bsButton("start", "GO!", icon("bolt"),
                                                 size = "large", style = "warning", class = "circle grow")),

                                    h3(strong("Acknowledgements:")),
                                    h4("This app was developed and coded by Sitong Liu and futher updated by Zhiliang Zhang and Jiajun Gao.",
                                       "This app is based on extending the idea in the Rossman/Chance applet at http://www.rossmanchance.com/applets/GuessCorrelation.html.",
                                       " Special thanks to Caihui Xiao and Yuxin Zhang for help on some programming issues."),
                                    br()
                                    
                                  ),
                          tabItem(tabName = "game",
                                  
                                  fluidRow(
                                    column(3,
                                           wellPanel(
                                             style = "background-color: #EAF2F8",
                                             #div(style="display: inline-block;vertical-align:top;",
                                             #   tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                            # ),
                                             #div(style="display: inline-block;vertical-align:top;",
                                             #    circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
                                             #),
                                            div(style = "text-align:center",
                                             selectInput("difficulty",
                                                         label = h4(strong("Choose Mode")),
                                                         choices = list("Without Outlier","With Outlier", "Random")),
                                             checkboxGroupInput("options",
                                                                label = h3(""),
                                                                choices = list("Show Regression Line"))),
                                            br(),
                                            div(style = "text-align:center",
                                            # h3(""),
                                             sliderInput("slider",
                                                         label = "Select the Correlation:",
                                                         min = -1,
                                                         max = 1,
                                                         step = 0.01,
                                                         value = 0)),
                                             br(),
                                             div(style = "text-align:center",
                                                 bsButton("newplot",
                                                          label = "Generate New Plot",
                                                          icon("arrow-circle-right"),
                                                          size = "medium",
                                                          style = "warning")),
                                             br(),
                                             div(style = "text-align:center",
                                                 uiOutput("sub"),
                                                 br(),
                                                 uiOutput("restart")), 
                                             br(),
                                             
                                             div(style = "text-align:center",
                                                 p(textOutput("status1"),
                                                   tags$head(tags$style("#status1{color: #FA8072;
                                                                        font-size: 20px;
                                                                        font-style: bold;
                                                                        }"
                         )
                                                   )
                         
                                                   )),
                         div(style = "text-align:center",
                             h5(textOutput("status2"),
                                tags$head(tags$style("#status2{color: #AF7AC5;
                                                     font-size: 20px;
                                                     font-style: bold;
                                                     }"
                         )
                                )
                         
                                )),
                         
                         h5(textOutput("status3"),
                            tags$head(tags$style("#status3{color: #1E407C;
                                                 font-size: 23px;
                                                 font-style: bold;
                                                 }"
                         
                            )
                            
                            ))
                         
                         )
                         
                            ),

                         column(6, align = "center",
                                mainPanel(
                                  plotOutput("plot1", height = 350, width = 475),
                                  bsPopover(id = "plot1",
                                            title =  "Scatterplot", 
                                            content = "Move the slide bar on the left to guess the correlation", 
                                            place = "bottom",
                                            trigger = "hover"))),
                         br(),
                         
                         column(3,
                                #absolutePanel(bottom = 0,
                                  wellPanel(
                                    style = "background-color: #EAF2F8",
                                    tags$h2(textOutput("score")),
                                    fluidRow(
                                      column(2, 
                                             htmlOutput('heart1')),
                                      
                                      column(2, 
                                             htmlOutput('heart2')),
                                      
                                      column(2, 
                                             htmlOutput('heart3')),
                                      
                                      column(2, 
                                             htmlOutput('heart4')),
                                      
                                      column(2,
                                             htmlOutput('heart5'))
                                      

                                    )),
                                    
                                    # conditionalPanel("input.submit !=0",
                                    #                 bsButton("finish", "Finish", icon("stop-circle"),size = "large", style = "danger")),
                                    # uiOutput("fin"),
                                    
                                    uiOutput("lead")
                                    
                                  ),#)


                                  column(6, align = "center",
                                         mainPanel(#"input.newplot == 0",
                                                          plotOutput("plot3", height = 350, width = 475),
                                                          bsPopover("plot3", "Your Guess vs. Answer",
                                                                    "This plot will show how your estimates compare to the correct answers.",
                                                                    #"Each purple dot represents one guess without outliers, orange dots are guesses with outliers.", 
                                                                    place = "top",
                                                                    trigger = "hover"))
                                  ))
                                
                                

                                
                         
                        
                         
                                                 )
                         # tabItem(tabName = "leader",
                         #         conditionalPanel("input.leader1 > 0", 
                         #                          fluidPage(
                         #                            fluidRow(
                         #                              div(style = "text-align:center",h1(textOutput("score1"))),
                         #                              br(),
                         #                              fluidRow(
                         #                                wellPanel(
                         #                                  wellPanel(textInput("name",h4("Please type in your name to submit the score:"),placeholder = "Name",width = 600)),
                         #                                  wellPanel(bsButton("check","Submit",style = "warning",size = "large"))
                         #                                ),
                         #                                
                         #                                
                         #                                
                         #                                conditionalPanel("input.check != 0", dataTableOutput("highscore")),
                         #                                actionButton("weekhigh", "Show Weekly High Scores"),
                         #                                actionButton("totalhigh", "Show All-Time High Scores")
                         #                                
                         #                              )
                         #                            ))))
                         ))))
