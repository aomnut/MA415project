library(shiny)
library(ggplot2)



ui <- shinyUI(fluidPage(
  titlePanel("Welcome to Manchester"),
  navbarPage(title = "CONTENT",
             tabPanel("Intoduction",
                      sidebarPanel(
                      h2("Table of Content"),
                      h4("Methodology"),
                      h4("ManUtd"),
                      h4("Mancity"),
                      h4("Head-to-Head")

                      ),
                      mainPanel(
                      h1(strong("It's a Manchester's Time !!!")),
                      br(),
                      br(),
                      h4("Since 1894, the world has been shaken, and the soccer field has been on fire"),
              
                      h4(em(" 'Who is the best soccer player in the world'  ") , "is out ---  the more important question is whether..."),
                      
                      br(),
                      
                      h3("Manchester is Blue or Red ?"),
                      hr(),
                      img(src="manchester.png"),
                      br(),
                      br(),
                      br(),
                      h3(em("You will decide"))
                      )
                      ),
             tabPanel("Methodology",
                      sidebarPanel(
                        h2("Analysis"),
                        br(),
                        h4("   ","On the right-hand-side, please select tab to see the analysis description of each section"),
                        h5(em("Player Tab"), "navigate to the individual player analysis"),
                        h5(em("Team Tab"), "navigate to the team  analysis"),
                        
                        hr()
                        ),
                      mainPanel(
                        tabsetPanel(type="tabs",
                                    tabPanel("Player",
                                             h1("Player Analysis"),
                                             hr(strong()),
                                             br(),
                                             h4("In this section, we want to explore if Twtter Comminuty think of each team's members"),
                                             br(),
                                             h4(strong("Manchester United squads"), " -- pogba, zlatan, lukaku, martial, matic, DeGea, rashford, beckham, rooney,lingard,herrera"),
                                             h4(strong("Manchester City squads"),"-- aguero,debruyne,gabriel,sane,sterling,silva,toure, fernandinho, otamendi, kompany"),
                                             br(),
                                             h4("By collecting data from Twitter Users and collect poitive and negative words based on the standard Lexington Dictionary, we are able to see which players got the most hated from the fans or which players are the most favorite of the fans"),
                                             h4("Assign 1 score for every positive word and -1 to every negative word")
                                             ),
                                    
                                    tabPanel("Team",
                                             h1("Team Analysis"),
                                             hr(),
                                             br(),
                                             h4("Now we move from individual to see the opinions of Twtter fans"),
                                             h4("We then collect the most #hashtag used the most by Twitter when the fans refer to these two teams"),
                                             h4("Only use the top 5 most hastag to each team"),
                                             br(),
                                             h4(strong("Manchester United keyword","#mufc , #manchesterunited, #redevil, #manu", "ggmanu"),
                                             h4(strong("Manchester City keyword"), "#mcfc, #mancity, #manchestercity, #manchesterisblue, #thecitizens"),
                                             br(),
                                             h4("Similar to individual, based on Lexington dictionary, positive and negative score will be indexed (1 and -1 )"),
                                             br(),
                                             br(),
                                             h4("Finally, we will implement statistical test on these following"),
                                             h4("1) mean difference of the positive comments for both team received"),
                                             h4("2) mean difference of the negative comments for both team received"),
                                             h4("3) mean difference of the relative percentage of the positive comments for both team received")
                                             
                                             
                                             
                                             ),
                                    tabPanel("Head-to-Head")
                        )
                        )
                      )
                      ),
                      
                      
                      
             tabPanel("ManUtd",
                      sidebarPanel(
                
                        selectInput("manups","Choice of analysis:",choices = c("positive", "negative", "total","overall")),
                        hr(),
                        helpText(strong("Positive"),"-- total frequency of being positively tweeted"),
                        helpText(strong("Negative"),"-- total frequency of being positively tweeted"),
                        helpText(strong("Total "),"-- total number of being tweeted"),
                        helpText(strong("Overall")," -- Positive Percentage"),
                        
                        helpText("***overall calculated by (positive)/(Positive+Negative)")
                        
                        
                        
                      ),
                      mainPanel(
                      tabsetPanel(type="tabs",
                                    tabPanel("Player", 
                                             splitLayout(plotOutput("manupp"),tableOutput("manuptable")
                                                                   ), 
                                             plotOutput("manupg")),
                                    tabPanel("Team", 
                                            splitLayout(plotOutput("manutg"), tableOutput("manutable"))
                                            , plotOutput("manutgb"))
                        )
                        
                        
                      )
                      ),# end of pic
             
             
             
             tabPanel("ManCity",
                      sidebarPanel(
                        
                        selectInput("mancps","Choice of analysis:",choices = c("positive", "negative", "total","overall")),
                        hr(),
                        helpText(strong("Positive"),"-- total frequency of being positively tweeted"),
                        helpText(strong("Negative"),"-- total frequency of being positively tweeted"),
                        helpText(strong("Total "),"-- total number of being tweeted"),                        
                        helpText(strong("Overall")," -- Positive Percentage"),
                        
                        helpText("***overall calculated by (positive)/(Positive+Negative)")
                        
                        
                      ),
                      mainPanel(
                        tabsetPanel(type="tabs",
                                    tabPanel("Player", 
                                             splitLayout(plotOutput("mancpp"),tableOutput("mancptable")
                                                          ),
                                             plotOutput("mancpg")),
                                    
                                    tabPanel("Team",
                                             splitLayout(plotOutput("manctg"), tableOutput("manctable")
                                                         ),
                                            plotOutput("manctgb"))
                                    
                                    )
                       
                        
                      )
                      
                      ),# end of pic
             
             tabPanel("Head-to-Head Analysis",
                      sidebarLayout(
                        sidebarPanel(
                        
                            selectInput("choice","Choice of analysis:",choices = c("positive", "negative","overall")),
                            hr(),
                            helpText(strong("Positive"),"-- total frequency of being positively tweeted"),
                            helpText(strong("Negative"),"-- total frequency of being positively tweeted"),
                            helpText(strong("Overall")," -- Positive Percentage"),
                            
                            helpText("***overall calculated by (positive)/(Positive+Negative)")
                       
                        ),
                        
                        mainPanel(
                          splitLayout(plotOutput("head"),tableOutput("headsum") ),
                          plotOutput("headb")

                      ))
                      )
             

                   )
                 # end of navbar
            )# end fluid page
            )# end shiny UI





 

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  
###man u
  
  ##player
  
  output$manupp <- renderPlot({
    pie(sscore_manup[,input$manups],
        labels = manup1
    )
  })
  
  output$manupg <- renderPlot({
    par(mar=c(5,10,10,10))
    barplot(sscore_manup[,input$manups],
            main= input$manups,
            ylab = "Frequency",
            xlab="player",
            names.arg = manup1
            
            )
  })
  
  # team
  output$manutg <- renderPlot({
      pie(sscore_manu[,input$manups],
          labels = manu1
      )
  })
  
  output$manutgb <- renderPlot({
    par(mar=c(5,20,5,20))
    barplot(sscore_manu[,input$manups],
            main= input$manups,
            ylab = "Frequency",
            xlab="Hashtag",
            names.arg = manu1
            
    )
  })
    
 
  
  
 
  ###city
  ## player
  
  output$mancpp <- renderPlot({
    pie(sscore_mancp[,input$mancps],
        labels = mancp1
    )
  })
  
  output$mancpg <- renderPlot({
    par(mar=c(5,10,5,10))
    barplot(sscore_mancp[,input$mancps],
            main= input$manups,
            ylab = "Frequency",
            xlab="player",
            names.arg = mancp1 
            
    )
  })
  ## team
  output$manctg <- renderPlot({
    pie(sscore_manc[,input$mancps],
    labels = manc1
    )
  })
  
  
  output$manctgb <- renderPlot({
    par(mar=c(5,20,5,20))
    barplot(sscore_manc[,input$mancps],
            main= input$manups,
            ylab = "Frequency",
            xlab="Hashtag",
            names.arg = manc1
    )
  })
  
  
  #####head
  
  
  output$head <- renderPlot({
    pie(sumderby[,input$choice],
        labels = c("ManUtd","ManCity")
    )
  })
    

  
  output$headb <- renderPlot({
    par(mar=c(3,20,0.05,20))
    boxplot(derby[,input$choice] ~class, ylab="Frequency"
    )
                  
      
    
  })
  
  output$headsum <- renderTable({
    test <- data.frame(
      ttesttable$Type,
      ttesttable[,input$choice]
    )
    colnames(test) <- c("Type of analysis", "Statistics")
    test
  })
  
    
    
    
  
  
  
  ####################################
  # add data set
  ##################################

  # Table of selected dataset ----
  output$manuptable <- renderTable({
    test <- data.frame(
      sscore_manup$team,
      sscore_manup[,input$manups]
      
    )
    colnames(test) <- c("Player", "Frequency/Percentage")
    test
    
    
    

  })
  
  output$mancptable <- renderTable({
    test <- data.frame(
      sscore_mancp$team,
      sscore_mancp[,input$mancps]
      
    )
    colnames(test) <- c("Player", "Frequency/Percentage")
    test
  
  

  })
  
  output$manutable <- renderTable({
    test <- data.frame(
      sscore_manu$team,
      sscore_manu[,input$manups]
      
    )
    colnames(test) <- c("Keyword", "Frequency/Percentage")
    test
  })
  
    output$manctable <- renderTable({
      test <- data.frame(
        sscore_manc$team,
        sscore_manc[,input$mancps]
        
      )
      colnames(test) <- c("Keyword", "Frequency/Percentage")
      test    
    })
  
  })
  

# Run the application 

shinyApp(ui = ui, server = server)

