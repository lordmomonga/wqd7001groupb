
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
library(data.table)
library(ggplot2)
library(shinyWidgets)

setBackgroundImage(src = NULL, shinydashboard = FALSE)

b64 <- base64enc::dataURI(file="bp.png", mime="image/png")

#--------------------------import data, modify data for df_country dataset----------------------------
df_country<-read.csv('df_country.csv')

#use entity, year and death_percent
df<-subset.data.frame(df_country,select=c(Entity,Year,Deaths_Percent),stringsAsFactors=F)

df1<-data.frame(df,stringsAsFactors = FALSE)

#df for dataset 2
df_1<-spread(df,Entity,Deaths_Percent)
df2<-data.frame(df_1,stringsAsFactors = FALSE)




# Import the Cleveland dataset--------------------------------------------------------------------------------------------------------------------------------------
df_clev <-read.csv('df_clev_clean.csv')


#-----------------------------------------helper function (convert vector to named list)----------------------------------------------------------------------------
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

#-------------------------------------------EDA setup for dataset 1------------------------------------------------------------------------------------------------

names_vars <- names(df_clev)

# Get the categorical and numerical variables
class_vars <- sapply(df_clev, class)
cat_vars <- names(class_vars[class_vars == "character"])
num_vars <- names(class_vars[class_vars != "character"])

# Define a plot function
plot_func <- function(var_1, var_2) {
  if (var_1 == var_2) {
    if (var_1 %in% cat_vars) {
      ggplot() + aes(df_clev[[var_1]]) + geom_bar(fill = "#56B4E9") + labs(x = var_1, y = "count", title = paste("Barplot of", var_1)) + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5))
    } else {
      ggplot() + aes(df_clev[[var_1]]) + geom_histogram(fill = "turquoise") + labs(x = var_1, y = "count", title = paste("Barplot of", var_1)) + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5))
    }
  } 
  else {
    if (var_1 %in% cat_vars & var_2 %in% cat_vars) {
      ggplot() + aes(df_clev[[var_1]], fill = df_clev[[var_2]]) + geom_bar(position = "dodge") + labs(x = var_1, y = "count", title = paste("Barplot of", var_1, "group by", var_2))  + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title=var_2))
    } else if (var_1 %in% cat_vars & var_2 %in% num_vars) {
      ggplot() + aes(df_clev[[var_2]], fill = df_clev[[var_1]]) + geom_density(alpha = 0.2) + labs(x = var_2, y = "density", title = paste("Density curve of", var_2, "group by", var_1))  + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title=var_1))
    } else if (var_1 %in% num_vars & var_2 %in% cat_vars) {
      ggplot() + aes(df_clev[[var_1]], fill = df_clev[[var_2]]) + geom_density(alpha = 0.2) + labs(x = var_1, y = "density", title = paste("Density curve of", var_1, "group by", var_2))  + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title=var_2))
    } else {
      ggplot() + aes(df_clev[[var_1]], df_clev[[var_2]]) + geom_point(col = "maroon3") + geom_smooth() + labs(x = var_1, y = var_2, title = paste("Scatter plot between", var_1,"and", var_2)) + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5))
    }
  }
}

#-------------------------prediction dataset setup-------------------------------------------------------------------------------------------------------------------
model<-readRDS('model86.rds')


#------------------------------------------------------------------------------------UI----------------------------------------------------------------------------
ui <- fluidPage ( 
  
 setBackgroundImage(
  #img(src=b64)
   src = b64
 ),
  
  navbarPage(
    theme = shinytheme("united"),     # 
    #theme = "superhero",
    inverse = FALSE,
    title = (strong("Heart Disease Analysis")),
    
#-------------------------------------------------------------------user guide tab---------------------------------------------------------------------------------
tabPanel("User Guide",
         fluidPage(
           fluidRow(column(width = 12,h1(strong("App User Guide:")))),
           fluidRow(column(width = 12,h2("Overview tab: Shows the information of the group members and the application"))),
           fluidRow(column(width = 12,h2("Data Preparation tab: Shows the overview and description of the dataset used"))),
           fluidRow(column(width = 12, h2("EDA tab: Shows the interactive graphing function in both dataset 1 (consist of boxplot, bar chart, histogram, density graph and scatter plot) and dataset 2(consist of multi-line graph)."))),
           fluidRow(column(width = 12,h2("Prediction tab: Shows the interactive heart diseases prediction model using svm method")))
         )), 
#-------------------------------------------------------------------OVerview tab------------------------------------------------------------------------------------
 tabPanel("Overview",
             fluidPage(
               fluidRow(column(width = 12,h2(strong("Group B Team Members")))),
               fluidRow(column(width = 12, HTML("<p>Team Leader: ZHONGLIANG SHI (17221268)"))),         
               fluidRow(column(width = 12, HTML("<p>Team Member: SUNNY CHAN ZI YANG (S2022037)"))),         
               fluidRow(column(width = 12,HTML("<p>Team Member: HO RU XIN ((S2023921)"))),      
               fluidRow(column(width = 12,HTML("<p>Team Member: FANYUE (17220701)"))),
 #style="color:white",              
               fluidRow(column(width = 2,h2(strong("Introduction")))),
               
               fluidRow(column(width =12,
                               h3("According to World Health Organization, cardiovascular disease is the number 1 cause of death globally.
Individuals at risk of CVD may demonstrate raised blood pressure, glucose, and lipids as well as overweight and obesity. These can all be easily measured in primary care facilities. Identifying those at highest risk of CVDs and ensuring they receive appropriate treatment can prevent premature deaths.
The risk factors for CVD include behavioural factors, such as tobacco use, an unhealthy diet, harmful use of alcohol and inadequate physical activity,
     and physiological factors, including high blood pressure (hypertension), high blood cholesterol and high blood sugar 
     or glucose which are linked to underlying social determinants and drivers, such as ageing, income and urbanization."))),
               br(),
               fluidRow(column(width =5,h2(strong("Interesting questions")))),
               fluidRow(column(width =12,h3("-What type of data and data source could we obtain for the project?"))),
               fluidRow(column(width =12,h3("-What analytics and analysis can we conduct based on heart diseases datasets?"))),
               fluidRow(column(width =12,h3("-How can we visualize our data outcome in our application from health care perspective?"))),
               fluidRow(column(width =12,h3("-How can we predict the heart disease?"))),
               br(),
               fluidRow(column(width =12,h2( style="color:navy",strong("Some facts you would want to know")))),
               fluidRow(column(width = 12, h3("Heart disease remains the number 1 killer; diabetes and dementia enter the top 10
                      Heart disease has remained the leading cause of death at the global level for the last 20 years. However, 
                      it is now killing more people than ever before. 
                      The number of deaths from heart disease increased by more than 2 million since 2000,
                      to nearly 9 million in 2019. Heart disease now represents 16% of total deaths from all causes.")))
              

)),
#--------------------------------------------------------------------------------------Data Preparation Dataset 1-------------------------------------------   
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
                        
                        tabPanel("Overview of dataset",h1('UCI heart disease dataset (Cleveland) '),tabPanel("df1",DT::dataTableOutput("table_df1")))
                        
                        
                      ))
                    
                    
           ),
           
#----------------------------------------------------------------------------------------------------------data preparation Dataset 2-------------------------------              
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
#----------------------------------------------------------------------------------------------------------EDA UI tab----------------------------------------------
tabPanel("EDA", 
         fluidPage(
           
           fluidRow(
             
             tabBox(
               
               height = "250px",width = "500px",
#-------------------------------------------------------------------------------------------------EDA for Dataset 1---------------------------------------------------------                            
               tabPanel("Dataset 1", 
                        
                        fluidRow(
                          # title
                          h1("Select Options"), 
                          
                  
                          #input
                          sidebarPanel(
                            selectInput('var_1', 'Select the first variable', names_vars),
                            selectInput('var_2', 'Select the second variable', names_vars)
                          ),	
                          
                          # plot output				
                          mainPanel(
                            plotOutput("distPlot")
                          )
                        )
                        
               ),
#----------------------------------------------------------------------------EDA for Dataset 2----------------------------------------------------------------------                            
               tabPanel("Dataset 2",
                        h3('Share of deaths from heart disease, 1990 to 2017 '),
                        sidebarLayout(
                          sidebarPanel(selectizeInput(inputId = "df2_variable",
                                                   label = "Please Select 2 country for comparison ", 
                                                   choices = unique(df$Entity),
                                                   selected = "Malaysia",
                                                   multiple = TRUE,
                                                   options=list(maxItems=2L))),
                          mainPanel (plotlyOutput('df2_line_plot'))))
               
               
             )),
         )),

#------------------------------------------------------------------------------------Prediction UI tab------------------------------------------------------------------
tabPanel("Prediction",
       
         headerPanel('Heart disease predictor'),
         # Input values
         sidebarPanel(
           #HTML("<h3>Input parameters</h3>"),
           h4('Disclaimer: The prediction function of this application is meant for research & educational purposes only, 
                          and not intended to be a tool for clinical decision-making.
                          Please consult a qualified physician if symptoms are present or heart disease is suspected.'),
           tags$label(h4('Please key in your information to get your prediction.')),
           numericInput("age", label = "Age", value = 30, min= 1, max=100,step=1),
           
           selectInput("sex", label = " Gender ", c("Male"=1,"Female"=0)),
           
           selectInput("cp", label = "Chest pain", c("Asymptomatic"=0,"Typical Angina"=1,"Atypical Angina"=2,"Non-Anginal Pain"=3)),
           
           numericInput("trestbps", label = " Current Resting Blood Pressure", value = 120, min= 1, max=500,step=1),
           
           numericInput('chol', label = 'Current Serum Cholesterol(mg/dl)', value = 170,min= 1, max=500,step=1),
           
           selectInput('fbs', label = 'Current Fasting Blood Sugar (if Sugar > 120 mg/dl please select true.)', c("True"=0,"False"=1)),
           
           selectInput('restecg', label = "Resting Electrocardiographic Results ",c("normal"=0,"having ST-T wave abnormality"=1,"showing probable or definite left ventricular hypertrophy by Estes' criteria"=2)),
           
           numericInput('thalach', label = 'Maximum heart rate achieved', value = 120,min= 1, max=500,step=1),
           
           selectInput('exang', label = 'Excercise induced angina ', c("Yes"=0,"No"=1)),
           
           numericInput('oldpeak', label = 'ST depression induced by exercise relative to rest', value = 1.5,min= 1, max=500,step=1),
          
           selectInput('slope', label = 'Slope of of the peak exercise ST segment', c("Downsloping"=0,"Upsloping"=1,"Flat"=2)),
           
           selectInput('ca', label = 'Number of major vessels (0-3) colored by flourosopy', c(0,1,2,3,4)),
           
           selectInput('thal', label = 'Present of thalassemia ', c("Normal"=0,"Fixed Defect"=1,"Reversable Defect"=2, "severe"=3)),
           
           actionButton("submitbutton", "Confirm", class = "btn btn-primary")
         ),
         
         
         mainPanel(
           tags$label(h3('Status/Output')),
           tableOutput('tabledata'), 
           verbatimTextOutput('contents')
         )  
       )


)
)  
    
                          

#--------------------------------------------------------------------------------Server----------------------------------------------------------------------------
server <- function(input, output) {
  dataDF<-df2

#-----------------------------------------------------Dataset1 data summary and table visualization---------------------------------------------------------------------------------------------------------------------------------
  #output$summary_df1 <- renderPrint({summary(df_clev)}) 
  output$struc_df1 <- renderPrint({str(df_clev)}) 
  output$table_df1 <- DT::renderDataTable({df_clev })
  
#-------------------------------------------------------Dataset2 data summary and table visualization---------------------------------------------------------------
  output$summary_df2 <- renderPrint({summary(df_country)}) 
  output$struc_df2 <- renderPrint({str(df_country)}) 
  output$table_df2 <- DT::renderDataTable({df_country })
  
  
#--------------------------------------------------------EDA Dataset 1 plot graph------------------------------------------------------------------------------------    
  output$distPlot <- renderPlot({
    # generate a plot based on input$var_1 and input$var_2
    plot_func(input$var_1, input$var_2)
  }) 
  
    
#--------------------------------------------------------------------EDA Dataset2 line plot graph-------------------------------------------------------------------------    
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

    
#----------------------------------------------------------------Predictor model -----------------------------------------------------------------------------------
   # Input Data
    datasetInput <- reactive({  
    #define variable  
      df <- data.frame(
        Name = c("age",
                 "sex",
                 "cp",
                 "trestbps",
                 "chol",
                 "fbs",
                 "restecg",
                 "thalach",
                 "exang",
                 "oldpeak",
                 "slope",
                 "ca",
                 "thal"),
    #input variable setuu     
        Value = as.character(c(input$age,
                               input$sex,
                               input$cp,
                               input$trestbps,
                               input$chol,
                               input$fbs,
                               input$restecg,
                               input$thalach,
                               input$exang,
                               input$oldpeak,
                               input$slope,
                               input$ca,
                               input$thal)),
        stringsAsFactors = FALSE)
      
      target<- 0
      df <- rbind(df, target)
      input <- transpose(df)
      write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      
      Output <- predict(model,test)#,round(predict(model,test,type="prob"), 3))
      if(Output=='No'){
        print("You are not diagnosed with heart disease, please go for a check up if you feel discomfort.")
      } else {
        print('Please consult a doctor')
      }
      #print(Output)
      tab<-data.frame(Heart_Disease=predict(model,test),round(predict(model,test,type="prob"), 3))
    })
    
    # Show Status/Output Text Box
    output$contents <- renderPrint({
      if (input$submitbutton>0) { 
        isolate(datasetInput()) 
      } else {
        return("Ready for prediction.")
      }
    })
    
    # Show Prediction results table
    output$tabledata <- renderTable({
      if (input$submitbutton>0) { 
        isolate(datasetInput()) 
      } 
    })


    





    
   
       
}

shinyApp(ui,server)