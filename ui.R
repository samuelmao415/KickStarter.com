
        #name of the project
fluidPage(navbarPage("KickStarter",
                    tabPanel("Report",
                             fluidRow(
                               column(6,
                              htmlOutput("inc")),
                              column(3,
                                     img(class="img-polaroid",
                                       src = "kickstarter2.png")))
                     ), 
            #####name of the tab
           tabPanel("World Map", 
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(plotlyOutput("pie_plotID"),width=3,height=2),
                      mainPanel(
                 leafletOutput("worldmap_plotID"))
                 
                    )
           )),
           ####Create a main tab of United Staes 
           navbarMenu("United States",
                        #sub panel under United States
                    ####successful rate analysis########
              tabPanel("Successful Rate",
                               fluidPage(
                                 plotlyOutput("US_scatterplot_ID"),
                                 plotlyOutput("US_success_rate_ID")
                                 
                               )),
                      
              tabPanel("Fund Amount",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons(inputId="state_ID", label="Outcome of the listed project",
                                    choices= c("All"="All","Failed"="failed", "Successful"="successful",
                                       "Canceled"="canceled","Live"="live",
                                       "Undefined"="undefined","Suspended"="suspended"
                                       ), selected = "successful"
                        ),
                        selectInput(inputId="category_under_goal_ID", label="Choose a category:",
                                    choices = c("All"="All",unique((ks18)%>%filter(region=="United States")%>%select(main_category))), 
                                    selected ="All"),
                        sliderInput(inputId = "goal_range_ID",
                                       label = "Upper limit of desired amount of funding",
                                       min=0,max=300000,value=10000),
                        sliderInput(inputId = "min_goal_range_ID",
                                    label = "Lower limit of desired amount of funding",
                                    min=0,max=300000,value=0),
                        sliderInput(inputId = "binwidth_ID",
                                    label = "Binwidth for better visualization",
                                    min=0,max=10000,value=500),
                        verbatimTextOutput("summary_ID")
                        ),
                      mainPanel(
                        plotlyOutput("US_goal_ID"),
                        fluidPage(
                        DT::dataTableOutput("US_tableID"))
                      )))
                    
           ,
  
          ###category(include subcategory) distribution tab that returns a bar plot
              tabPanel("Category Distribution",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons(inputId="category_state_ID", label="Outcome of the listed project",
                                     choices= c("All"="All","Failed"="failed", "Success"="successful",
                                                "Cancelled"="canceled","Live"="live",
                                                "Undefined"="undefined","Suspended"="suspended"
                                     ), selected = "successful"
                        ),width=2
                        
                      ),
                      mainPanel(
                        plotlyOutput("US_category_ID"),width= 10
                        )),
                    DT::dataTableOutput("US_category_tableID")
                    ), 
          
          
         
          ####backers analysis########
          tabPanel("Backers Distribution",
                   sidebarLayout(
                         plotlyOutput("US_backers_ID"),
                         plotlyOutput("US_backers_by_category_ID")
                       )),
                   
          ####almost made it project: raised divided by goal#########################################
          tabPanel("Almost 'Made IT' Project",
                   titlePanel('These projects were so close to be funded...'),
                   sidebarLayout(
                     
                     sidebarPanel(
                       sliderInput(inputId="prop_max_ID", label="Choose upper bound of percentage funded",
                                   min=0, max=1, value=0.9),
                       sliderInput(inputId="prop_min_ID", label="Choose the lower bound of percentage funded",
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
                   titlePanel('These projects rasied more than they asked for...'),
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput(inputId="prop_success_max_ID", label="Choose lower bound of percentage overfunded",
                                   min=1, max=2000, value=1.2),
                       #choice to filter goal less than $10
                       selectInput("sucessful_showone_ID", "Display options",
                                   choices = c("Hide goal less than $100","Show all")
                 
                     )),
                     mainPanel(
                       fluidPage(
                         DT::dataTableOutput("US_successful_ID")
                       )
                     )
                   )
                   
          ),
          tabPanel("Word Cloud",
                   sidebarLayout(
                     # Sidebar with a slider and selection inputs wordcloud_state_ID
                     sidebarPanel(
                       selectInput("wordcloud_category_ID", "Choose a category:",
                                   choices = unique(ks18$main_category)),
                       sliderInput("freq_ID",
                                   "Minimum Frequency:",
                                   min = 1,  max = 100, value = 20),
                       sliderInput("max_ID",
                                   "Maximum Number of Words:",
                                   min = 1,  max = 300,  value = 100),
                       radioButtons(inputId="wordcloud_state_ID", label="Outcome of the listed project",
                                    choices= c("All"="All","Failed"="failed", "Success"="successful",
                                               "Cancelled"="canceled","Live"="live",
                                               "Undefined"="undefined","Suspended"="suspended"
                                    ), selected = "All"
                       )
                     ),
                     
                     # Show Word Cloud
                     mainPanel(
                       imageOutput("word_cloud_plot_ID")
                     )
                   ))),
          
          ########Rest of the world##################################################
          ########Rest of the world##################################################
          ########Rest of the world##################################################
          navbarMenu("Rest of the world",
                     tabPanel("Successful Rate",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(inputId="Rest_success_rate_ID", 
                                                label=h3("Select country for bar plot"), 
                                                choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                                selected ="All"),
                                  selectInput(inputId="Rest_scatter_ID", 
                                              label=h3("Select country for scatter plot"), 
                                              choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                              selected ="All"),width = 2),
                                  mainPanel(
                                    fluidPage(
                                plotlyOutput("Rest_US_success_rate_ID"),
                                plotlyOutput("Rest_US_scatterplot_ID"))
                                
                              ))),
                     tabPanel("Fund Amount",
                              sidebarLayout(
                                sidebarPanel(
                                  radioButtons(inputId="Rest_state_ID", label="Outcome of the project",
                                               choices= c("All"="All","Failed"="failed", "Success"="successful",
                                                          "Cancelled"="canceled","Live"="live",
                                                          "Undefined"="undefined","Suspended"="suspended"
                                               ), selected = "successful"
                                  ),
                                  sliderInput(inputId = "Rest_goal_range_ID",
                                              label = "Upper limit of desired amount of funding",
                                              min=0,max=300000,value=10000),
                                  sliderInput(inputId = "Rest_min_goal_range_ID",
                                              label = "Lower limit of desired amount of funding",
                                              min=0,max=300000,value=0),
                                  sliderInput(inputId = "Rest_binwidth_ID",
                                              label = "Binwidth for better visualization",
                                              min=0,max=10000,value=500),
                                  selectInput(inputId="Rest_region_ID", 
                                              label=h3("Select country"), 
                                              choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                              selected ="All"),
                                  selectInput(inputId="Rest_category_under_goal_ID", label="Choose a category:",
                                              choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(main_category))), 
                                              selected ="All"),
                                  verbatimTextOutput("Rest_summary_ID")
                                ),
                                mainPanel(
                                  plotlyOutput("Rest_US_goal_ID"),
                                  fluidPage(
                                    DT::dataTableOutput("Rest_US_tableID"))
                                )
                              )
                     ),
                     ###main cateogry distribution tab that returns bar plot
                     tabPanel("Category Distribution",
                              sidebarLayout(
                                sidebarPanel(
                                  radioButtons(inputId="Rest_main_category_state_ID", label="State of the project",
                                               choices= c("All"="All","Failed"="failed", "Success"="successful",
                                                          "Cancelled"="canceled","Live"="live",
                                                          "Undefined"="undefined","Suspended"="suspended"
                                               ), selected = "successful"
                                  ),
                                  selectInput(inputId="Rest_main_category_region_ID", 
                                              label="Select country", 
                                              choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                              selected ="All"),
                                  
                                  width=2 ),
                                mainPanel(
                                  plotlyOutput("Rest_main_US_category_ID"), width= 10
                                )),
                              DT::dataTableOutput("Rest_main_US_category_tableID")
                     ),
                     ####backers analysis################################
                     tabPanel("Backers Distribution",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("Rest_backers_region_ID", "Top: Choose a country:",
                                              choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                              selected ="All"),
                                  selectInput("Rest_backers_category_region_ID", "Bottom: Choose a country for category:",
                                              choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                              selected ="All"),width=2 ),
                                mainPanel(
                                  fluidPage(
                                    plotlyOutput("Rest_US_backers_ID"),
                                    plotlyOutput("Rest_backers_by_category_ID")
                                    
                                  ),width=10
                                )
                              )),
                     ####unsuccessful project: raised divided by goal##########################
                     tabPanel("Almost 'Made IT' Project",
                              titlePanel("These projects were so close to be funded..."),
                              sidebarLayout(
                                sidebarPanel(
                                  sliderInput(inputId="Rest_prop_max_ID", label="Choose upper bound of percentage founded",
                                              min=0, max=1, value=0.9),
                                  sliderInput(inputId="Rest_prop_min_ID", label="Choose lower bound of percentage founded",
                                              min=0, max=1, value=0.5)
                                ),
                                mainPanel(
                                  fluidPage(
                                    DT::dataTableOutput("Rest_US_almost_made_it_ID")
                                  )
                                )
                              )
                              
                     ),
                     
                     ##extremely successful project########################
                     
                     tabPanel("Extremely Succesful Project",
                              titlePanel('These projects rasied more than they asked for...'),
                              sidebarLayout(
                                sidebarPanel(
                                  sliderInput(inputId="Rest_prop_success_max_ID", label="Choose lower bound of percentage overfunded",
                                              min=1, max=2000, value=1.2),
                                  #choice to filter goal less than $10
                                  selectInput("Rest_sucessful_showone_ID", "Display options",
                                              choices = c("Hide goal less than $100","Show all")
                                              
                                  )),
                                mainPanel(
                                  fluidPage(
                                    DT::dataTableOutput("Rest_US_successful_ID")
                                  )
                                )
                              )
                              
                     ),
###############word cloud#####################################################    
                     
                     
                     tabPanel("Word Cloud",
                              sidebarLayout(
                                # Sidebar with a slider and selection inputs
                                sidebarPanel(
                                  selectInput("Rest_wordcloud_category_ID", "Choose a category:",
                                              choices = unique(ks18%>%filter(region!="United States")%>%select(main_category))),
                                  radioButtons(inputId="Rest_wordcloud_state_ID", label="Outcome of the listed project",
                                               choices= c("All"="All","Failed"="failed", "Success"="successful",
                                                          "Cancelled"="canceled","Live"="live",
                                                          "Undefined"="undefined","Suspended"="suspended"
                                               ), selected = "All"),
                                  selectInput("Rest_word_cloud_region_ID", "Choose a country:",
                                              choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                              selected ="All"),
                                  sliderInput("Rest_freq_ID",
                                              "Minimum Frequency:",
                                              min = 1,  max = 50, value = 10),
                                  sliderInput("Rest_max_ID",
                                              "Maximum Number of Words:",
                                              min = 1,  max = 200,  value = 100)
                                  
                                ),
                                
                                # Show Word Cloud
                                mainPanel(
                                  plotOutput("Rest_word_cloud_plot_ID")
                                )
                              )
          )),
          
          ####################################################
           tabPanel("Data",
                    DT::dataTableOutput("tableID")
           )
          
          ##############################################
        
        ))
          
