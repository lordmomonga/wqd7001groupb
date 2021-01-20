library(shiny)
library(shinythemes)
library(data.table)

model<-readRDS('model86.rds')

# create user interface

ui <- fluidPage(theme = shinytheme('cyborg'),
            
                headerPanel('Heart disease predictor'),
                # Input values
                sidebarPanel(
                    #HTML("<h3>Input parameters</h3>"),
                    tags$label(h4('Input parameters')),
                    numericInput("age", label = "Age", value = 40),
                    sliderInput("sex", label = "Sex (Male = 1, Female = 0)", value = 1,min=0,max=1,step=1),
                    sliderInput("cp", label = "Chest pain (0 = Asymptomatic, 1 = Typical Angina, 2 = Atypical Angina, 3 = Non-Anginal Pain)", value = 1,min=0,max=3,step=1),
                    numericInput("trestbps", label = "resting Bp", value = 120),
                    numericInput('chol', label = 'Cholesterol(mg/dl)', value = 170),
                    sliderInput('fbs', label = 'Fasting Sugar (Sugar > 120 mg/dl, 1 = true; 0 = false)', value = 1,min=0,max=1,step=1),
                    sliderInput('restecg', label = "ECG at rest (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)", value=1,min=0,max=2,step=1),
                    numericInput('thalach', label = 'Max heart rate', value = 120),
                    sliderInput('exang', label = 'Excercise induced angina (1 = yes; 0 = no)', value = 0,min=0,max=1,step=1),
                    numericInput('oldpeak', label = 'ST segments', value = 1.5),
                    sliderInput('slope', label = 'slope of ST segment (0 = Downsloping, 1 = Upsloping, 2 = Flat)', value = 1,min=0,max=2,step=1),
                    sliderInput('ca', label = 'number of coronary artery', value = 3,min=0,max=4,step=1),
                    sliderInput('thal', label = 'thalassemia (0 = Normal, 1 = Fixed Defect, 2 = Reversable Defect, 3 = severe)', value = 2,min=0,max=3,step=1),
                    
                    actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                
                mainPanel(
                    tags$label(h3('Status/Output')),
                    tableOutput('tabledata'), 
                    verbatimTextOutput('contents')
                )
)

# Server                           #
####################################

server<- function(input, output) {
    
    # Input Data
    datasetInput <- reactive({  
        
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
            print("You don't have heart disease but if you feel discomfort it's better to go for a check up")
        } else {
            print('Immediately go to doctor')
        }
        #print(Output)
        tab<-data.frame(Heart_Disease=predict(model,test),round(predict(model,test,type="prob"), 3))
    })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
        if (input$submitbutton>0) { 
            isolate(datasetInput()) 
        } else {
            return("Server is ready for calculation.")
        }
    })
    
    # Prediction results table
    output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
            isolate(datasetInput()) 
        } 
    })
    
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)