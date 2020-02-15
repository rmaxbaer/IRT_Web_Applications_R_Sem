

##includeMarkdown("~/TestAnalysisShiny/ShinyMarkdown.Rmd")
##includeMarkdown("~/Documents/TestAnalysisShiny/Test.Rmd")
#includeMarkdown("~/TestAnalysisShiny/BeliefApply.Rmd")

library(ggplot2)
library(shiny)
library(DT)
#library(rsconnect)
#rsconnect::deployApp("C:/Users/robertmaxbaer/Documents/TestAnalysisShiny")

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Question'),
      th(rowspan = 2, 'Doctrine'),
      th(class = 'dt-center', colspan = 4, '         I _______ this is true.'),
      th(class = 'dt-center', colspan = 4, 'How Much do you care about this doctrine?')
    ),
    tr(
      
      lapply(c("Don't know if", "Sort of Believe", "Believe", "Know", "Not Important", "Somewhat Important", "Important", "Very Important"), th)
    )
  )
))
print(sketch)



# Define UI for application that draws a histogram
ui <- navbarPage( 
      title = "Student Assessment Analysis",
      
# PAGE 1 ------------------------------------------------------------------
      
      tabPanel("Completion", 
               
          tabsetPanel(
#####  PAGE 1 PLOT
            
             tabPanel(title = "Plot", h4("Number of Assessments Each Year (by Category)"), 
                      
                      sidebarPanel(
                        selectInput(inputId = "select_Area", label = "Area:", 
                                    c("All", unique(as.character(ourdata2$AREA)))),
                                    helpText("Assessments Completed by Area and Category."),
                        
                        
                        radioButtons("radio_Year", label = ("School Years:"),
                                     c("All", unique(as.character(ourdata2$SCHOOL_YEAR))), 
                                     selected = "All")
                        
                        ),
                      
                      mainPanel(
                          plotOutput("areagraph", height = "800px"))),
             
#####  PAGE 1 TABLE

             tabPanel(title = "Table", h4("Assessments Completed each Year"), 
                      sidebarPanel(
                        selectInput(inputId = "select_Pg1Area", label = "Area:", 
                                    c("All", unique(as.character(ourdata2$AREA)))),
                        
                        selectInput(inputId = "select_Pg1Type", label = "Assessment Category:",
                                    c("All",
                                      unique(as.character(ourdata2$TYPE)))),
                        #helpText("Assessments Completed by Area and Category."),
                        
                        radioButtons("select_Pg1Year", label = ("School Years:"),
                                     c("All", unique(as.character(ourdata2$SCHOOL_YEAR))), 
                                     selected = "All"), 
                        helpText("Click here for the full table."),
                        actionButton("action_Pg1Tab", label = "SHOW MORE / SHOW LESS", icon = icon("arrows-alt-h", lib = "font-awesome")  )
                        
                        
                        #DT::dataTableOutput("totals_key")
                         
                      ),
                      
                      mainPanel(
                          # Create a new row for the table.
                          DT::dataTableOutput("table_Pg1"),
                          h4("Total Assessments for Current Selection"), 
                          DT::dataTableOutput("totals_key")
                      )
                ), 

#####    COMPARE
          tabPanel(title = "Compare", h4("Compare Assessment Completion Across Areas"), 
                   
                   sidebarPanel(
                     selectInput(inputId = "select_AreaComp", label = "Compare Data From:",
                                 c(unique(as.character(ourdata2$AREA)))), 
                     selectInput(inputId = "select_AreaComp2", label = "To Data From", 
                                 c("All Areas", unique(as.character(ourdata2$AREA))))
                   ),
                   
                   
                   mainPanel(
                     span(textOutput("Comp_Title"), style = "font-size: 20px; text-align:center"),
                     plotOutput(outputId = "Comp1"), 
                     span(textOutput("Comp_Title2"), style = "font-size: 20px; text-align:center; padding-top:120 px"),
                     plotOutput(outputId = "Comp2")
                   )
                   
                   
                   
                   
                   )

              
              )
          ),
      

# PAGE 2 ------------------------------------------------------------------

      tabPanel("Results", 
               
          tabsetPanel(
            tabPanel(title = "Question Analysis", h4("Question Scoring and Distractor Analysis"), 
               
                # This is the table that shows data
                fluidRow(
                      column(4,
                             selectInput(inputId = "type", 
                                         label = "Assessment Category:",
                                         c("All",
                                           unique(as.character(df_Combined$TYPE))))
                      ),
                      column(4,
                             selectInput("id_Num",
                                         "Test ID:",
                                         c("All",
                                           sort(unique(as.numeric(df_Combined$TEST_ID))))
                                         )

                      ),
                      column(4, style='padding-bottom:6px;',
                             span(htmlOutput("title_Pg2ActBttn"))
                      ),
                      column(4,
                             actionButton("action_Pg2QA", label = "CLICK HERE", icon = icon("arrows-alt-h", lib = "font-awesome")  )
                             
                      )
                    ), 
                    # Create a new row for the table.
                    DT::dataTableOutput("table"),
                    textOutput("text_Pg2QA")
            ), 

            
            ##### SCORES

            
            tabPanel(title = "Scores", h4("Average Scores by Area"), 
                     
                     # This is the table that shows data
                     fluidRow(
                       column(4,
                              selectInput(inputId = "score_Area", 
                                          label = "Area:",
                                          c("All",
                                            unique(as.character(ourdata2$AREA))))
                       ),
                       column(4,
                              selectInput("score_Cat",
                                          "Category:",
                                          c("All",
                                            unique(as.character(ourdata2$TYPE))))
                       ),
                       column(4,
                              selectInput("score_Year",
                                          "School Years:",
                                          c("All", 
                                            unique(as.character(ourdata2$SCHOOL_YEAR))))
                       )
                     ), 
                     # Create a new row for the table.
                     DT::dataTableOutput("scores_Table"), 
                     h4("Total Scores for Current Selection"), 
                     DT::dataTableOutput("scores_TotalsTab")
            )
            
        )
      ),

# PAGE 3 ------------------------------------------------------------------
       tabPanel("Belief and Apply", 
               
        tabsetPanel(
        
#####           BELIEF AND APPLY TABLE

            tabPanel(title = "Table", 
                     sidebarPanel(
                       selectInput(inputId = "select_BaArea", 
                                   label = "Area:",
                                   choices = c(setNames(df_Area$ID, df_Area$names))

                       ),
                       uiOutput("select_BANa"),
                       
                       helpText("Belief and Apply Responses for each Test.")
                       
                     ),
                     mainPanel(
                       h4("Belief Survey:"),
                       DT::dataTableOutput("Bel_Table" , width = '150%'), 
                       h4("Apply Survey:"),
                       DT::dataTableOutput("App_Table" , width = '150%')
                     )
              
            ),
            
#####     COMPARE TAB            
            tabPanel(title = "*Practice*", h4("Number of Assessments Each Year (by Category)"),
                     column(4, 
                            fluidRow(
                              selectInput(inputId = "select_BacompArea1", 
                                          label = "Area:",
                                          choices = c("All", setNames(df_Area$ID, df_Area$names))
                                          
                              ),
                              uiOutput("select_BAcompNa1"),
                              
                              h4("Belief Survey:"),
                              DT::dataTableOutput("tab_BAcompBel1"), #, width = '50%')
                              h4("Apply Survey:"),
                              DT::dataTableOutput("tab_BAcompApp1") # , width = '150%')
                              
                            )
                     ), 
                     column(4, offset = 2,    
                            fluidRow(
                              selectInput(inputId = "select_BacompArea2", 
                                          label = "Area:",
                                          choices = c("All", setNames(df_Area$ID, df_Area$names))
                                          
                              ),
                              selectInput(inputId = "select_BAcompNa2", label = "Assessment:", 
                                          choices = c(as.character(unique(df_BALong$Name)))
                                          
                              ),
                              
                              h4("Belief Survey:"),
                              DT::dataTableOutput("tab_BAcompBel2"), #, width = '50%')
                              h4("Apply Survey:"),
                              DT::dataTableOutput("tab_BAcompApp2") # , width = '150%')
                              
                            )
                            
                            
                            
                            
                            
                     )
            ) 
             
             
        )
         
) #This Tab
) #Whole Nav Bar

































































# OUTPUT ------------------------------------------------------------------

# Define server logic 
server <- function(input, output) {


# OUTPUT PAGE 1 -----------------------------------------------------------
  
  #  Bar Plot
    output$areagraph <- renderPlot({
      if(input$radio_Year != "All"){
        ourdata2 <- ourdata2[ourdata2$SCHOOL_YEAR == input$radio_Year,]
      }
      if(input$select_Area != "All"){
        ourdata2 <- ourdata2[ourdata2$AREA == input$select_Area, ]
      }
      area_Dat <- ourdata2
      area_Dat <- aggregate(area_Dat$DELIVERY_TYPE ~ area_Dat$TYPE +area_Dat$SCHOOL_YEAR, data = area_Dat, FUN = NROW)
      colnames(area_Dat) <- c("TYPE", "SCHOOL_YEAR", "NUMBER" )
      
      # Render a barplot
      ggplot(data = area_Dat, aes(x = area_Dat$SCHOOL_YEAR, y = area_Dat$NUMBER, fill = area_Dat$SCHOOL_YEAR)) + geom_bar(stat = "identity" ) + facet_wrap(~area_Dat$TYPE) + scale_fill_discrete(name = "School Year") + theme(legend.key.size = unit(1, "cm"), legend.position = "left", legend.box.spacing = unit(1, "cm"), legend.title = element_text(size = 20), legend.text = element_text(size = 16), strip.text = element_text(face = "plain", size=20,lineheight=5.0, color = "gray38"), axis.text.y = element_text(size = 16), axis.text.x = element_blank(), axis.title = element_blank() ) + scale_y_continuous(name="Assessments Completed", labels = scales::comma, limits = c(0, (max(area_Dat$NUMBER) *1.1  ))) + geom_text(size = 6, aes(label= paste(round((signif(area_Dat$NUMBER, 3)/1000), 2), " K", sep = "") ),stat='identity', position=position_dodge(0.9), vjust=-0.5)                                                                                                                                                 ###
      
      
    }) 
    
  # Table 1
    output$table_Pg1 <- DT::renderDataTable({
      data_Pg1Tab <- ourdata2
      
      if(input$select_Pg1Area != "All"){
        data_Pg1Tab <- data_Pg1Tab[data_Pg1Tab$AREA == input$select_Pg1Area,]
      }
      if(input$select_Pg1Type != "All"){
        data_Pg1Tab <- data_Pg1Tab[data_Pg1Tab$TYPE == input$select_Pg1Type, ]
      }
      
      if(input$select_Pg1Year != "All"){
        data_Pg1Tab <- data_Pg1Tab[data_Pg1Tab$SCHOOL_YEAR == input$select_Pg1Year, ]
      }
      
      if(input$action_Pg1Tab %% 2 == 1){
        pg1_Data <- aggregate(data_Pg1Tab$PERSON_ID ~  data_Pg1Tab$TYPE + data_Pg1Tab$SCHOOL_YEAR + data_Pg1Tab$TEST_ID + data_Pg1Tab$DELIVERY_TYPE, data = data_Pg1Tab, FUN = NROW)
        colnames(pg1_Data) <- c("Category", "School-Year", "Test ID", "Delivery Type", "Assessments Completed")
      } else{ 
        
        
        
        pg1_Data <- aggregate(data_Pg1Tab$PERSON_ID ~  data_Pg1Tab$SCHOOL_YEAR + data_Pg1Tab$TYPE, data = data_Pg1Tab, FUN = NROW)
        pg1_Data <- pg1_Data[, c(2, 1, 3)]
        colnames(pg1_Data) <- c("Category", "School-Year", "Assessments Completed")
      }
      
      
      
      
       DT::datatable(pg1_Data, options = list(orderClasses = TRUE))
    })   
    
    output$title_Pg1ActBttn <- renderText({
      paste("<b>Show/Hide Test IDs:</b> <br/>")
    })
    
    
    # COMPARE
    
output$Comp_Title <- renderText({
  paste("Assessments Completed in", input$select_AreaComp)
})

output$Comp_Title2 <- renderText({
  paste("Assessments Completed in", input$select_AreaComp2)
})

output$Comp1 <- renderPlot({

  if(input$select_AreaComp != "All"){
    ourdata2 <- ourdata2[ourdata2$AREA == input$select_AreaComp, ]
  }
  area_Dat <- ourdata2
  area_Dat <- aggregate(area_Dat$DELIVERY_TYPE ~ area_Dat$TYPE +area_Dat$SCHOOL_YEAR, data = area_Dat, FUN = NROW)
  colnames(area_Dat) <- c("TYPE", "SCHOOL_YEAR", "NUMBER" )
  
  # Render a barplot
  ggplot(data = area_Dat, aes(x = area_Dat$SCHOOL_YEAR, y = area_Dat$NUMBER, fill = area_Dat$SCHOOL_YEAR)) + geom_bar(stat = "identity" ) + facet_wrap(~area_Dat$TYPE) + scale_fill_discrete(name = "School Year") + theme(legend.key.size = unit(1, "cm"), legend.position = "left", legend.box.spacing = unit(1, "cm"), legend.title = element_text(size = 20), legend.text = element_text(size = 16), strip.text = element_text(face = "plain", size=20,lineheight=5.0, color = "gray38"), axis.text.y = element_text(size = 16), axis.text.x = element_blank(), axis.title = element_blank() ) + scale_y_continuous(name="Assessments Completed", labels = scales::comma, limits = c(0, (max(area_Dat$NUMBER) *1.1  ))) + geom_text(size = 6, aes(label= paste(round((signif(area_Dat$NUMBER, 3)/1000), 2), " K", sep = "") ),stat='identity', position=position_dodge(0.9), vjust=-0.5)                                                                                                                                                 ###
  
  
}) 
    


output$Comp2 <- renderPlot({

  if(input$select_AreaComp2 != "All Areas"){
    ourdata2 <- ourdata2[ourdata2$AREA == input$select_AreaComp2, ]
  }
  area_Dat <- ourdata2
  area_Dat <- aggregate(area_Dat$DELIVERY_TYPE ~ area_Dat$TYPE +area_Dat$SCHOOL_YEAR, data = area_Dat, FUN = NROW)
  colnames(area_Dat) <- c("TYPE", "SCHOOL_YEAR", "NUMBER" )
  
  # Render a barplot
  ggplot(data = area_Dat, aes(x = area_Dat$SCHOOL_YEAR, y = area_Dat$NUMBER, fill = area_Dat$SCHOOL_YEAR)) + geom_bar(stat = "identity" ) + facet_wrap(~area_Dat$TYPE) + scale_fill_discrete(name = "School Year") + theme(legend.key.size = unit(1, "cm"), legend.position = "left", legend.box.spacing = unit(1, "cm"), legend.title = element_text(size = 20), legend.text = element_text(size = 16), strip.text = element_text(face = "plain", size=20,lineheight=5.0, color = "gray38"), axis.text.y = element_text(size = 16), axis.text.x = element_blank(), axis.title = element_blank() ) + scale_y_continuous(name="Assessments Completed", labels = scales::comma, limits = c(0, (max(area_Dat$NUMBER) *1.1  ))) + geom_text(size = 6, aes(label= paste(round((signif(area_Dat$NUMBER, 3)/1000), 2), " K", sep = "") ),stat='identity', position=position_dodge(0.9), vjust=-0.5)                                                                                                                                                 ###
  
  
}) 
    
    
    
    
    
    
#####         THE TOTALS KEY TABLE
    output$totals_key <- DT::renderDataTable({
      
      data_Pg1Tab <- ourdata2
      
      if(input$select_Pg1Area != "All"){
        data_Pg1Tab <- data_Pg1Tab[data_Pg1Tab$AREA == input$select_Pg1Area,]
      }
      if(input$select_Pg1Type != "All"){
        data_Pg1Tab <- data_Pg1Tab[data_Pg1Tab$TYPE == input$select_Pg1Type, ]
      }
      
      if(input$select_Pg1Year != "All"){
        data_Pg1Tab <- data_Pg1Tab[data_Pg1Tab$SCHOOL_YEAR == input$select_Pg1Year, ]
      }
      
      if(input$action_Pg1Tab %% 2 == 1){
        data_Key <- aggregate(data_Pg1Tab$PERSON_ID ~ data_Pg1Tab$AREA + data_Pg1Tab$TYPE + data_Pg1Tab$SCHOOL_YEAR + data_Pg1Tab$TEST_ID, data = data_Pg1Tab, FUN = NROW)
        colnames(data_Key) <- c("Area", "Category", "School-Year", "Test ID", "Assessments Completed")
      } else{ 
        data_Key <- aggregate(data_Pg1Tab$PERSON_ID ~ data_Pg1Tab$AREA + data_Pg1Tab$TYPE + data_Pg1Tab$SCHOOL_YEAR, data = data_Pg1Tab, FUN = NROW)
        colnames(data_Key) <- c("Area", "Category", "School-Year", "Assessments Completed")
      }
      
      data_Key <- aggregate(data_Key$`Assessments Completed` ~ data_Key$`School-Year`, data = data_Key, FUN = sum)
      colnames(data_Key) <- c("School-Year", "Total Assessments")
      total <- c("TOTAL", sum(data_Key$`Total Assessments`))
      total <- format(total, big.mark = ",", scientific = FALSE)
      data_Key <- rbind(data_Key, total)
      
      
      DT::datatable(data_Key, options = list(orderClasses = TRUE, searching = FALSE, paging = FALSE))
    })
    

# OUPUT PAGE 2 ------------------------------------------------------------
                #####   TABLE   #####
    output$table <- DT::renderDataTable({
      my_Data <- df_Combined
      if(input$type != "All"){
        my_Data <- my_Data[my_Data$TYPE == input$type, ] 
      }
      
      if(input$id_Num != "All"){
        my_Data <- my_Data[my_Data$TEST_ID == input$id_Num, ]
      }
      
      colnames(my_Data) <- c("Test ID", "Q #", "Percent Correct", "Question Text", "Correct Option", "% Correct", "Option 2 (Distractor 1)", "%2", "Option 3 (Distractor 2)", "%3", "D1 Percent", "D2 Percent", "Responses", "Category")

      
      if(input$action_Pg2QA %% 2 == 1){
        my_Data <- my_Data[, c(14, 1:2, 4:5, 3, 7, 11, 9, 12)]
      } else{ 
        my_Data <- my_Data[, c(1, 4:5, 3, 7, 11, 9, 12)]
        }
      
      
      
      
      DT::datatable(my_Data, options = list(orderClasses = TRUE))
    })   
    
    output$title_Pg2ActBttn <- renderText({
      paste("<b>Show More / Show Less:</b> <br/>")
    })
    
    output$text_Pg2QA <- renderText({
      "The columns named ' # ' represent the number of students who chose each answer."
    })
    
#####  SCORES   
    
    output$scores_Table <- DT::renderDataTable({
      if(input$score_Area != "All"){
        ourdata2 <- ourdata2[ourdata2$AREA == input$score_Area, ]
      }
      
      if(input$score_Cat != "All"){
        ourdata2 <- ourdata2[ourdata2$TYPE == input$score_Cat, ]
      }
      
      if(input$score_Year != "All"){
        ourdata2 <- ourdata2[ourdata2$SCHOOL_YEAR == input$score_Year, ]
      }
      
      
      
      
      ourdata2$SCORE <- as.numeric(ourdata2$SCORE)
      ourdata2 <- ourdata2[ourdata2$TYPE != "Belief And Apply", ]
      
      score_Dat <- aggregate(ourdata2$SCORE ~ ourdata2$AREA + ourdata2$TYPE + ourdata2$SCHOOL_YEAR, data = ourdata2, FUN = mean)
      colnames(score_Dat) <- c("Area", "Category", "School-Year", "Average Score")
      score_Dat$`Average Score` <- round(score_Dat$`Average Score`, digits= 2)
      
      DT::datatable(score_Dat, options = list(orderClasses = TRUE, searching = FALSE))
    })
  
    output$scores_TotalsTab <- DT::renderDataTable({
      if(input$score_Area != "All"){
        ourdata2 <- ourdata2[ourdata2$AREA == input$score_Area, ]
      }
      
      if(input$score_Cat != "All"){
        ourdata2 <- ourdata2[ourdata2$TYPE == input$score_Cat, ]
      }
      
      if(input$score_Year != "All"){
        ourdata2 <- ourdata2[ourdata2$SCHOOL_YEAR == input$score_Year, ]
      }
      
      ourdata2$SCORE <- as.numeric(ourdata2$SCORE)
      
      total_ScoreDat <- aggregate(ourdata2$SCORE ~ ourdata2$SCHOOL_YEAR, data = ourdata2, FUN = mean)
      colnames(total_ScoreDat) <- c("School-Year", "Average Score")
      total_ScoreDat$`Average Score` <- round(total_ScoreDat$`Average Score`, digits= 2)
      
      DT::datatable(total_ScoreDat, options = list(orderClasses = TRUE, paging = FALSE, searching = FALSE))
    })
    


# OUTPUT PAGE 3 -----------------------------------------------------------

    
    
output$select_BAcompNa1 <- renderUI({
  
  
  Area_ID <- as.numeric(input$select_BacompArea1)
  area <- area_List[[Area_ID]]
  area <- area[area$Num_care != 0 & area$Num_BelApp !=0, ]
  area$Name <- as.character(area$Name)
  
  
  
  
  selectInput("Assessment", "Date:", choices = c("All", unique(area$Name)))
})    
    
# COMPARISON
    #AREA
    
    
output$tab_BAcompBel1 <- DT::renderDataTable({
  if(input$select_BacompArea1 != "All"){
    i <- as.numeric(input$select_BacompArea1)
    Area_BAData <- area_List[[i]]
    my_BelData <- Area_BAData[as.character(Area_BAData$Type) == "Belief", c(15, 14, 2, 4:11) ]
    colnames(my_BelData)[8:11] <- c("Know", "Believe", "Sort of believe", "Don't know if")
    
  }
  if(input$Assessment != "All"){
    my_BelData <- my_BelData[my_BelData$Name == input$Assessment, ]
  }
  my_BelData <- my_BelData[, c(2:3, 11:8, 7:4)]
  
  DT::datatable(my_BelData, options = list(searching = FALSE, paging = FALSE), container = sketch, rownames = FALSE)  
  
})    
output$tab_BAcompApp1 <- DT::renderDataTable({
  
  if(input$select_BacompArea1 != "All"){
    i <- as.numeric(input$select_BacompArea1)
    Area_BAData <- area_List[[i]]
    
    my_AppData <- Area_BAData[as.character(Area_BAData$Type) == "Apply", c(15, 14, 2, 4:11) ]
    colnames(my_AppData)[8:11] <- c("Always", "Almost Always", "Sometimes", "Rarely or Never")
    
  }
  
  my_AppData <- my_AppData[my_AppData$Name == input$select_BAcompNa1, ]
  my_AppData <- my_AppData[, c(2:3, 11:8, 7:4)]
  
  
  
  datatable(my_AppData, options = list(searching = FALSE, paging = FALSE), container = sketch, rownames = FALSE)
  
})
    
    
    #TOTAL
    
output$tab_BAcompBel2 <- DT::renderDataTable({
  if(input$select_BacompArea2 != "All"){
    i <- as.numeric(input$select_BacompArea2)
    Area_BAData <- area_List[[i]]
    my_BelData <- Area_BAData[as.character(Area_BAData$Type) == "Belief", c(15, 14, 2, 4:11) ]
    colnames(my_BelData)[8:11] <- c("Know", "Believe", "Sort of believe", "Don't know if")
    
  }
  
  my_BelData <- my_BelData[my_BelData$Name == input$select_BAcompNa2, ]
  my_BelData <- my_BelData[, c(2:3, 11:8, 7:4)]
  
  DT::datatable(my_BelData, options = list(searching = FALSE, paging = FALSE), container = sketch, rownames = FALSE)  
  
})   

output$tab_BAcompApp2 <- DT::renderDataTable({
  
  if(input$select_BacompArea2 != "All"){
    i <- as.numeric(input$select_BacompArea2)
    Area_BAData <- area_List[[i]]
    
    my_AppData <- Area_BAData[as.character(Area_BAData$Type) == "Apply", c(15, 14, 2, 4:11) ]
    colnames(my_AppData)[8:11] <- c("Always", "Almost Always", "Sometimes", "Rarely or Never")
    
  }
  
  my_AppData <- my_AppData[my_AppData$Name == input$select_BAcompNa2, ]
  my_AppData <- my_AppData[, c(2:3, 11:8, 7:4)]
  
  
  
  datatable(my_AppData, options = list(searching = FALSE, paging = FALSE), container = sketch, rownames = FALSE)
  
})


    

# Table
output$select_BANa <- renderUI({
  
  
  Area_ID <- as.numeric(input$select_BaArea)
  area <- area_List[[Area_ID]]
  area <- area[area$Num_care != 0 & area$Num_BelApp !=0, ]
  area$Name <- as.character(area$Name)
  
  
  
  
  selectInput("select_TabID", "Assessment:", choices = c(unique(area$Name)))
})  








output$Bel_Table <- DT::renderDataTable({
  
  if(input$select_BaArea != "All"){
    i <- as.numeric(input$select_BaArea)
    Area_BAData <- area_List[[i]]
    my_BelData <- Area_BAData[as.character(Area_BAData$Type) == "Belief", c(15, 14, 2, 4:11) ]
    colnames(my_BelData)[8:11] <- c("Know", "Believe", "Sort of believe", "Don't know if")
    
  }
  
  my_BelData <- my_BelData[my_BelData$Name == input$select_TabID, ]
  my_BelData <- my_BelData[, c(2:3, 11:8, 7:4)]
  
  DT::datatable(my_BelData, options = list(searching = FALSE, paging = FALSE), container = sketch, rownames = FALSE)
  
  
  
})

output$App_Table <- DT::renderDataTable({
  
  if(input$select_BaArea != "All"){
    i <- as.numeric(input$select_BaArea)
    Area_BAData <- area_List[[i]]
    
    my_AppData <- Area_BAData[as.character(Area_BAData$Type) == "Apply", c(15, 14, 2, 4:11) ]
    colnames(my_AppData)[8:11] <- c("Always", "Almost Always", "Sometimes", "Rarely or Never")
    
    }
  
  my_AppData <- my_AppData[my_AppData$Name == input$select_TabID, ]
  my_AppData <- my_AppData[, c(2:3, 11:8, 7:4)]
  

  
  datatable(my_AppData, options = list(searching = FALSE, paging = FALSE), container = sketch, rownames = FALSE)
  
})

#####    PLOT











    
    
}

# Run the application 
shinyApp(ui = ui, server = server)






