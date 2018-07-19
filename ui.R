

navbarPage("KickStater",
           tabPanel("World Map",
                    fluidPage(
                      leafletOutput("worldmap_plotID")
                    )
           ),
           tabPanel("United States",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons(inputId="state_ID", label="State of the project",
                                    choices= c("failed"="failed", "success"="successful",
                                       "cancelled"="canceled","live"="live",
                                       "undefined"="undefined","suspened"="suspended")
                        ),
                        sliderInput(inputId = "goal_range_ID",
                                       label = "Choose a goal range",
                                       min=0,max=200000,value=10000),
                        sliderInput(inputId = "binwidth_ID",
                                    label = "Choose a binwidth",
                                    min=0,max=10000,value=500)
                        ),
                      mainPanel(
                        plotlyOutput("US_plotID"),
                        verbatimTextOutput("event")
                      )
                    )
           ),
           tabPanel("Rest of the world",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"="p", "Line"="l")
                        )
                      ),
                      mainPanel(
                        plotOutput("rest_of_the_world_plotID")
                      )
                    )
           ),
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About",
                               fluidRow(
                                 column(6,
                                        includeMarkdown("about.md")
                                 ),
                                 column(3,
                                        img(class="img-polaroid",
                                            src=paste0("http://upload.wikimedia.org/",
                                                       "wikipedia/commons/9/92/",
                                                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Club's July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                               )
                      )
           )
)