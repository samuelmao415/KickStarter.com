
        #name of the project
navbarPage("KickStarter",
            #####name of the tab
           tabPanel("World Map", 
                    fluidPage(
                      leafletOutput("worldmap_plotID")
                    )
           ),
           ####Create a main tab of United Staes 
           navbarMenu("United States",
                        #sub panel under United States
              tabPanel("Fund Amount",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons(inputId="state_ID", label="State of the project",
                                    choices= c("All"="All","failed"="failed", "success"="successful",
                                       "cancelled"="canceled","live"="live",
                                       "undefined"="undefined","suspened"="suspended"
                                       ), selected = "successful"
                        ),
                        sliderInput(inputId = "goal_range_ID",
                                       label = "Choose a goal range",
                                       min=0,max=300000,value=10000),
                        sliderInput(inputId = "binwidth_ID",
                                    label = "Choose a binwidth",
                                    min=0,max=10000,value=500),
                        verbatimTextOutput("summary_ID")
                        ),
                      mainPanel(
                        plotlyOutput("US_goal_ID"),
                        fluidPage(
                        DT::dataTableOutput("US_tableID"))
                      )
                    )
           ),
          ###category distribution tab that returns a bar plot
              tabPanel("Category Distribution",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons(inputId="category_state_ID", label="State of the project",
                                     choices= c("All"="All","failed"="failed", "success"="successful",
                                                "cancelled"="canceled","live"="live",
                                                "undefined"="undefined","suspened"="suspended"
                                     ), selected = "successful"
                        ),
                        sliderInput(inputId = "category_observation_ID",
                                    label = "Choose a category number threshold",
                                    min=0,max=15000,value=3000)
                        
                      ),
                      mainPanel(
                        plotlyOutput("US_category_ID")
                        #DT::dataTableOutput("US_category_tableID")
                        )),
                    DT::dataTableOutput("US_category_tableID")
                    ),
          ####backers analysis########
          tabPanel("Backers Distribution",
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons(inputId="backers_state_ID", label="State of the project",
                                    choices= c("All"="All","failed"="failed", "success"="successful",
                                               "cancelled"="canceled","live"="live",
                                               "undefined"="undefined","suspened"="suspended"
                                    ), selected = "successful"
                       )),
                     mainPanel(
                       fluidPage(
                         plotlyOutput("US_backers_ID")
                         #DT::dataTableOutput("US_category_tableID")
                       )
                     )
           )),
          ####unsuccessful project: raised divided by goal#########################################
          tabPanel("Almost 'Made IT' Project",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput(inputId="prop_max_ID", label="Choose the max of failure",
                                   min=0, max=1, value=0.9),
                       sliderInput(inputId="prop_min_ID", label="Choose the base of failure",
                                   min=0, max=1, value=0.5)
                     ),
                     mainPanel(
                       fluidPage(
                         DT::dataTableOutput("US_almost_made_it_ID")
                       )
                     )
                   )
            
          ),
          
          ##extremely successful project###########################
          tabPanel("Extremely Succesful Project",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput(inputId="prop_success_max_ID", label="Choose the minimum of success",
                                   min=1, max=2000, value=1.2)
                 
                     ),
                     mainPanel(
                       fluidPage(
                         DT::dataTableOutput("US_successful_ID")
                       )
                     )
                   )
                   
          ),
          tabPanel("Word Cloud",
                   sidebarLayout(
                     # Sidebar with a slider and selection inputs
                     sidebarPanel(
                       selectInput("wordcloud_category_ID", "Choose a category:",
                                   choices = unique(ks18$main_category)),
                       sliderInput("freq_ID",
                                   "Minimum Frequency:",
                                   min = 1,  max = 100, value = 20),
                       sliderInput("max_ID",
                                   "Maximum Number of Words:",
                                   min = 1,  max = 300,  value = 100)
                     ),
                     
                     # Show Word Cloud
                     mainPanel(
                       plotOutput("word_cloud_plot_ID")
                     )
                   ))),
          
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
           tabPanel("Data",
                    DT::dataTableOutput("tableID")
           ),
           navbarMenu("More",
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
           ))
          
