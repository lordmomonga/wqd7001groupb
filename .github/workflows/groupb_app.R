
#-----------------library---------------------------------------
library(shiny)
library(plotly)
library(DT)
library(tidyr)
library(htmltools)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(caret)
library(e1071)
library(randomForest)

#--------------------------import data, modify data----------------------------
df_country<-read.csv('df_country.csv')

#use entity, year and death_percent
df<-subset.data.frame(df_country,select=c(Entity,Year,Deaths_Percent),stringsAsFactors=F)

df1<-data.frame(df,stringsAsFactors = FALSE)

#df for dataset 2
df_1<-spread(df,Entity,Deaths_Percent)
df2<-data.frame(df_1,stringsAsFactors = FALSE)




# Import the Cleveland dataset-----------------------------------------------------------------------------
df_clev <-read.csv('df_clev_clean.csv')


#-----------------------------------------helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}
#-------------------------prediction dataset setup----------------
split <- 0.8
set.seed(998)
train_index <- createDataPartition(df_clev$num, p=split, list = F)
data_train <- df_clev[train_index, ]
data_test <- df_clev[-train_index, ]
# 10 fold cross validation
train_control <- trainControl(method="cv", number=10)
# 1. knn
# Set the same random seed for each algorithm to ensure the data split for 10-fold cross validation


#---------------------UI----------------------------------------------------------------------------
ui <- fluidPage ( 
  
  navbarPage(
    theme = shinytheme("united"),
    #theme = "superhero",
    inverse = FALSE,
    title = ("Heart Disease Analysis"),
    
# Homepage----------------------------------------------------------------------------------
    tabPanel("Overview",
             fluidPage(
               fluidRow(column(width = 12,h2(strong("Group B Team Members")))),
               fluidRow(column(width = 12, HTML("<p>Team Leader: ZHONGLIANG SHI (17221268)"))),         
               fluidRow(column(width = 12, HTML("<p>Team Member: SUNNY CHAN ZI YANG (S2022037)"))),         
               fluidRow(column(width = 12,HTML("<p>Team Member: HO RU XIN ((S2023921)"))),      
               fluidRow(column(width = 12,HTML("<p>Team Member: FANYUE (17220701)"))),
               
               fluidRow(column(width = 2,h2(strong("Introduction")))),
               
               fluidRow(column(width =12,
                               p("According to World Health Organization, cardiovascular disease is the number 1 cause of death globally.
Individuals at risk of CVD may demonstrate raised blood pressure, glucose, and lipids as well as overweight and obesity. These can all be easily measured in primary care facilities. Identifying those at highest risk of CVDs and ensuring they receive appropriate treatment can prevent premature deaths.
The risk factors for CVD include behavioural factors, such as tobacco use, an unhealthy diet, harmful use of alcohol and inadequate physical activity,
     and physiological factors, including high blood pressure (hypertension), high blood cholesterol and high blood sugar 
     or glucose which are linked to underlying social determinants and drivers, such as ageing, income and urbanization."))),
               br(),
               fluidRow(column(width =5,h2(strong("Interesting questions")))),
               fluidRow(column(width =12,h4("-What type of data and data source could we obtain for the project?"))),
               fluidRow(column(width =12,h4("-What analytics and analysis can we conduct based on heart diseases datasets?"))),
               fluidRow(column(width =12,h4("-How can we visualize our data outcome in our application from health care perspective?"))),
               fluidRow(column(width =12,h4("-How can we predict the heart disease?"))),
               br(),
               fluidRow(column(width =12,h2(strong("Some facts you would want to know")))),
               fluidRow(column(width = 12, HTML("<p> Heart disease remains the number 1 killer; diabetes and dementia enter the top 10
                      Heart disease has remained the leading cause of death at the global level for the last 20 years. However, 
                      it is now killing more people than ever before. 
                      The number of deaths from heart disease increased by more than 2 million since 2000,
                      to nearly 9 million in 2019. Heart disease now represents 16% of total deaths from all causes.")))
              

)),
#--------------------Data Preparation dataset 1--------------------------------   
navbarMenu("Data Preparation",
           tabPanel("Dataset 1", 
                    
                    fluidRow(
                      tabPanel("Description of data", HTML("<p>Data source 1: 
                                                               <a href='https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data'>
                                                              UCI ML database: heart disease dataset(Cleveland)</a></p>")),
                      tabBox(
                        
                        height = "250px",width = "500px",
                        tabPanel(
                          "Description of data",h3("There are 14 attributes for dataset 1:"), 
                          HTML("<p>1.age: age in years</p>"),
                          
                          
                          
                          
                          p("2.sex: sex (1 = male; 0 = female)"),
                          p("3.cp: chest pain type "),
                          
                          
                          
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 1: typical angina"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 2: atypical angina"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 3: non-anginal pain"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 4: asymptomatic"),
                          
                          HTML("<p>4.trestbps: resting blood pressure (in mm Hg on admission to the hospital)</p>"), 
                          HTML("<p>5.chol: serum cholestoral in mg/dl</p>"),
                          HTML("<p>6.fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)</p>"),
                          HTML("<p>7.restecg: resting electrocardiographic results</p>"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 0: normal"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria"),
                          HTML("<p>8.thalach: maximum heart rate achieved</p>"),
                          HTML("<p>9.exang: exercise induced angina (1 = yes; 0 = no)</p>"),
                          HTML("<p>10.oldpeak = ST depression induced by exercise relative to rest</p>"),
                          HTML("<p>11.slope: the slope of the peak exercise ST segment</p>"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 1: upsloping"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 2: flat"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 3: downsloping"),
                          HTML("<p>12.ca: number of major vessels (0-3) colored by flourosopy</p>"),
                          HTML("<p>13.thal: 3 = normal; 6 = fixed defect; 7 = reversable defect</p>"),
                          HTML("<p>14.num: diagnosis of heart disease (angiographic disease status)</p>"),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 0: 0 major vessels with greater than 50% diameter narrowing. No presence of heart disease."),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 1: 1 major vessels with greater than 50% diameter narrowing."),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 2: 2 major vessels with greater than 50% diameter narrowing."),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 3: 3 major vessels with greater than 50% diameter narrowing."),
                          HTML("<p>&nbsp;&nbsp;&nbsp;Value 4: 4 major vessels with greater than 50% diameter narrowing."),
                          br(),
                          h3("Have a look at the summary information"),
                          column(width = 12, verbatimTextOutput("summary_df1") ),
                          br(),
                          h3("Have a look at the structure"),
                          column(width = 12, verbatimTextOutput("struc_df1") )
                        ),
                        
                        tabPanel("Overview of dataset",h1('UCI heart disease dataset (Cleveland) '),tabPanel("df2",DT::dataTableOutput("table_df1")))
                        
                        
                      ))
                    
                    
           ),
           
#---------------------data preparation dataset 2-------------------------------              
           tabPanel("Dataset 2",
                    fluidRow(
                      tabPanel("Description of data",
                               HTML("<p>Data source 2: 
                              <a href=' https://ourworldindata.org/grapher/share-deaths-heart-disease?tab=chart&country=~MYS'>
                              Dataset of Share of deaths from heart disease, 1990 to 2017 in different country</a></p>")),
                      
                      tabBox(
                        # The id lets us use input$tabset1 on the server to find the current tab
                        height = "250px",width = "500px",
                        
                        tabPanel(
                          "Description of data",h3("There are 4 attributes for dataset 2:"), 
                          
                          p("1.Entity: Country name in the world"),  
                          p("2.Code: Country code"),
                          p("3.Year: Year from 1990 to 2017"),
                          p(" 4.Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (Percent)"),
                          br(),
                          h3("Have a look at the summary information"),
                          column(width = 12, verbatimTextOutput("summary_df2") ),
                          br(),
                          h3("Have a look at the structure"),
                          column(width = 12, verbatimTextOutput("struc_df2") )
                        ),
                        
                        tabPanel("Overview of dataset",h1('Share of deaths from heart disease, 1990 to 2017 '),tabPanel("df2",DT::dataTableOutput("table_df2")))
                        
                        
                      )))
           
           ),
#------------------------------------------EDA UI----------------------------------------------
tabPanel("EDA", 
         fluidPage(
           
           fluidRow(
             
             tabBox(
               
               height = "250px",width = "500px",
#---------------------------------------Dataset 1---------------------------------------------------------                            
               tabPanel("Dataset 1", 
                        
                        fluidRow(
                          # title
                          h1("Select Options"),
                          
                          #input
                          sidebarPanel
                          (
                            
                            
                            uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
                            uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
                            selectInput("plot.type","Plot Type:", 
                                        list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
                            ),
                            checkboxInput("show.points", "show points",F)
                          ),	
                          
                          # output				
                          mainPanel(
                            h3(textOutput("caption")),
                            #h3(htmlOutput("caption")),
                            uiOutput("plot") # depends on input 
                          )
                        )
                        
               ),
#----------------------------------------------Dataset 2----------------------------------------------------------------------                            
               tabPanel("Dataset 2",
                        h3('Share of deaths from heart disease, 1990 to 2017 '),
                        sidebarLayout(
                          sidebarPanel(selectInput(inputId = "df2_variable",
                                                   label = "Select country ", 
                                                   choices = unique(df$Entity),
                                                   selected = "Malaysia",
                                                   multiple = TRUE)),
                          mainPanel (plotlyOutput('df2_line_plot'))))
               
               
             )),
         )),

#--------------------------------Prediction UI------------------------------------------------------------------
tabPanel("Prediction",
         
         fluidRow(
           tabPanel("Modelling",h1(strong("Modelling")),
                    
                    h4("In this part, we will compare several models using 10-fold cross validation and select the best one from df_clev. 
                Finally we will test its performance on the testing dataset to see whether it is overfitting."),
                    br(),        
                    tabBox(
                      # The id lets us use input$tabset1 on the server to find the current tab
                      height = "250px",width = "500px",
                      
                      tabPanel(
                        "K-nearest Neighbours", 
                        
                        column(width = 12, verbatimTextOutput("pred_knearest") )
                      ),
                      tabPanel(
                        "Logistics Regression", 
                        
                        column(width = 12, verbatimTextOutput("logis_r") )
                      ),
                      tabPanel(
                        "Random Forest", 
                        
                        column(width = 12, verbatimTextOutput("random_f") )
                      ),
                      tabPanel(
                        "Confusion Metrics", 
                        
                        column(width = 12, verbatimTextOutput("conf") )
                      )
                    ))
))


)
)  
    
    
    

             
             
               
                
                 
                 

                             

            

    








#-------------Server----------------------------------
server <- function(input, output) {
  dataDF<-df2
  # dataDf <- reactive({
   #     temp <- get(input$dataset)
        
  #  })
  
  
  
  
#--------------df2_line plot graph-----------------------------    
    output$df2_line_plot <- renderPlotly({
        
        
        # if input country more than 1 
        if (length(input$df2_variable) > 1){
            plot_ly(df2, x = ~Year, y =~get(input$df2_variable[1]), 
                    type = 'scatter', mode = 'lines', name = input$df2_variable[1]) %>%
                add_trace(df2, x = ~Year, y = ~get(input$df2_variable[2]), 
                          
                          type = 'scatter', mode = 'lines',name = input$df2_variable[2]) %>%
                layout(xaxis = list(title = "Year"),yaxis = list(title = "Death Percentage"))
        }
        #if input country less than 1
        else {

             plot_ly(df2, x = ~Year, y =~get(input$df2_variable[1]), name = input$df2_variable[1], type = 'scatter', mode = 'lines') %>%
                layout(xaxis = list(title = "Year"), yaxis = list(title = "Death Percentage"))
        }
        
    })
    
    #----------------df1 data summary--------------------
    output$summary_df1 <- renderPrint({summary(df_clev)}) 
    output$struc_df1 <- renderPrint({str(df_clev)}) 
    output$table_df1 <- DT::renderDataTable({df_clev })
       
#----------------df2 data summary--------------------
    output$summary_df2 <- renderPrint({summary(df_country)}) 
    output$struc_df2 <- renderPrint({str(df_country)}) 
       
#-----------------df2 dataset table visualization------------------------    
    output$table_df2 <- DT::renderDataTable({df_country })



 
#--------------------------------------------------------EDA df1------------------    
    #update variable and group based on dataset
    output$variable <- renderUI({ 
      obj<-df_clev 	 
      var.opts<-namel(colnames(obj))
      selectInput("variable","Variable:", var.opts) # uddate UI 				 
    }) 
    
    output$group <- renderUI({ 
      obj<-df_clev 
      var.opts<-namel(colnames(obj))
      selectInput("group","Groups:", var.opts) # uddate UI 				 
    }) 
    
    output$caption<-renderText({
      switch(input$plot.type,
             "boxplot" 	= 	"Boxplot",
             "histogram" =	"Histogram",
             "density" 	=	"Density plot",
             "bar" 		=	"Bar graph")
    })
    
    
    output$plot <- renderUI({
      plotOutput("p")
    })
    
    #plotting function using ggplot2
    output$p <- renderPlot({
      
      plot.obj<<-list() # not sure why input$X can not be used directly?
      plot.obj$data<<-df_clev
      plot.obj$variable<<-with(plot.obj$data,get(input$variable)) 
      plot.obj$group<<-with(plot.obj$data,get(input$group)) 
      
      #dynamic plotting options
      plot.type<-switch(input$plot.type,
                        "boxplot" 	= 	geom_boxplot(),
                        "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                        "density" 	=	geom_density(alpha=.75),
                        "bar" 		=	geom_bar(position="dodge")
      )
      
      require(ggplot2)
      #plotting theme
      .theme<- theme(
        axis.line = element_line(colour = 'gray', size = .75), 
        panel.background = element_blank(),  
        plot.background = element_blank()
      )	 
      if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
        p<-ggplot(plot.obj$data, 
                  aes(
                    x 		= plot.obj$group, 
                    y 		= plot.obj$variable,
                    fill 	= as.factor(plot.obj$group)
                  )
        ) + plot.type
        
        if(input$show.points==TRUE)
        { 
          p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
        }
        
      } else {
        
        p<-ggplot(plot.obj$data, 
                  aes(
                    x 		= plot.obj$variable,
                    fill 	= as.factor(plot.obj$group),
                    group 	= as.factor(plot.obj$group),
                    #color 	= as.factor(plot.obj$group)
                  )
        ) + plot.type
      }
      
      p<-p+labs(
        fill 	= input$group,
        x 		= "",
        y 		= input$variable
      )  +
        .theme
      print(p)
    })    
    
    
#------------------------------Prediction--------------------------------------------------------
    output$pred_knearest <- renderPrint({
      # Split the dataset into 80% and 20% as training and testing dataset respectively
      # Set the random seed to ensure we get the same traning dataset and testing dataset everytime we run the code.
      
    #  set.seed(2)
      model_knn <- train(num~., data=data_train, trControl=train_control, method="knn", metric="Accuracy")
      print(model_knn)})


    
    output$logis_r<- renderPrint({
    #  set.seed(2)
      model_glm <- train(num~., data=data_train, trControl=train_control, method="glm", family = 'binomial', metric="Accuracy")
      print(model_glm)
      }) 

  output$random_f <- renderPrint({

   # set.seed(2)
    model_rf <- train(num~., data=data_train, trControl=train_control, method="rf", metric="Accuracy")
    print(model_rf)
 
  }) 

  output$conf <- renderPrint({
    
  #  set.seed(2)
    model_glm <- train(num~., data=data_train, trControl=train_control, method="glm", family = 'binomial', metric="Accuracy")
    
    # Make predictions
    x_test <- data_test[, 1:length(data_train) - 1]
    y_test <- data_test[, length(data_train)]
    prediction <- predict(model_glm, data_test)
    # Have a look at the confusion metrics
    confusionMatrix(factor(prediction), factor(y_test))
  })
    
   
       
}

shinyApp(ui,server)
