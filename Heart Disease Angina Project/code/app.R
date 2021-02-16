# Importing required packages and libraries:
library(shiny)
library(shinydashboard)
library(data.table)


# Read in the RDS files which is Random Forest model based 
pred.model <- readRDS("model.rds")


#-------------------------------------------------------------------------------------------
#Fuzzify the prediction object to interpret the output
fuzzify <- function(prediction){
  
  "
	  This method implements fuzzification which helps to classify 
	  the output probability into fuzzy logic. It selects ranges of 
	  probability values and returns the interpretable classes according to
	  the probability value based on its range.
	"
  
  #Storing the prediction value to be classified further
  y_prob = prediction[1]
  
  #Checking for the range of the prediction value
  #Ranges and corresponding classes defined are: 
  # less than 0.2: "Very Low"                        
  # less than 0.4 and greater than 0.2: "Low"
  # less than 0.6 and greater than 0.4: "Medium"
  # less than 0.8 and greater than 0.6: "High"
  # greater than 0.8: "Extreme"
  
  if(y_prob < 0.2){
    rating = "Very Low"
  }
  if(y_prob < 0.4 & y_prob >= 0.2){
    rating = "Low"
  }
  if(y_prob < 0.6 & y_prob >= 0.4){
    rating = "Medium"
  }
  if(y_prob < 0.8 & y_prob >= 0.6){
    rating = "High"
  }
  if(y_prob > 0.8){
    rating = "Extreme"
  }
  return(rating)
}


#---------------------------------------------------------------------------------------------
#-------------------------------- User interface----------------------------------------------
#---------------------------------------------------------------------------------------------

"
	  The following method creates the webpage like user interface to communicate end-user; 
	  It is made up of all input variables which are taken from the feature selection method, 
	  Checkboxes for categorical variables and numerical input boxes are created for numerical 
	  data as below. In these boxes, users can input their choice of values or tick boxes as 
	  yes and no otherwise. Submit button below would call the model from sever and compute 
	  the probability values and interpret classes accordingly.
	"

ui <- pageWithSidebar(
  
  # Header of the web page of user interface 
  headerPanel('Heart Disease Angina Prediction'),
  
  # Input parameters format in the side-bar panel
  sidebarPanel(
    HTML("<h2>Input parameters</h2>"),
    
    #Creating check boxes for categorical features where tick for yes and no otherwise
    #Check box for Typical chest pain 
    checkboxInput("Typical.Chest.Pain", 
                  label = "Typical Chest Pain", 
                  FALSE),
    div(),
    
    #Check box for Atypical chest pain 
    checkboxInput("Atypical", 
                  label = "Atypical Chest Pain", 
                  FALSE),
    div(),
    
    #Check box for Nonanginal Chest Pain  
    checkboxInput("Nonanginal", 
                  label = "Nonanginal Chest Pain", 
                  FALSE),
    div(),
    
    #Check box for Hypertension
    checkboxInput("HTN", 
                  label = "Hypertension", 
                  FALSE),
    div(),
    
    #Check box for Severe Valvular Heart Disease
    checkboxInput("VHD_sev", 
                  label = "Severe Valvular Heart Disease", 
                  FALSE),
    div(),
    
    #Check box for T-Inversion
    checkboxInput("Tinversion", 
                  label = "T-Inversion", 
                  FALSE),
    div(),
    
    #Input box for numerical variables accepting numerical inputs
    #Input box for Age
    numericInput("Age", 
                 label = "Age", 
                 value = NULL),
    div(),
    
    #Input box for Erythrocyte Sedimentation Rate (ESR) (mm/h)
    numericInput("ESR", 
                 label = "Erythrocyte Sedimentation Rate (ESR) (mm/h)", 
                 value = NULL),
    div(),
    
    #Input box for Ejection Fraction from TTE (%)
    numericInput("EF.TTE", 
                 label = "Ejection Fraction from TTE (%)", 
                 value = NULL),
    div(),
    
    #Input box for BMI (kg/m^2)
    numericInput("BMI", 
                 label = "BMI (kg/m^2)", 
                 value = NULL),
    div(),
    
    #Input box for Blood Pressure (mmHg)
    numericInput("BP", 
                 label = "Blood Pressure (mmHg)", 
                 value = NULL),
    div(),
    
    #Input box for Triglyceride (mg/dl)
    numericInput("TG", 
                 label = "Triglyceride (mg/dl)",
                 value = NULL),
    div(),
    
    #Submit button to make a call to server for model prediction
    actionButton("submitbutton", "Submit", 
                 class = " btn-primary")
  ),
  
  #Outputting table in the main panel which contains the result carried out using server
  mainPanel(
    tags$label(h3('Status/Output')), # Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table including interpretation of fussy
    
  )
)


#---------------------------------------------------------------------------------------------
#-------------------------------- Server -----------------------------------------------------
#---------------------------------------------------------------------------------------------


server<- function(input, output, session) {
  
  "
	  This method works as communication between end-user and server. When user enters the data, 
	  it will be taken as input to server and server performs the model which is read from rds 
	  file earlier. It is creating the data frame as the output box which has the probability of 
	  heart disease as prediction along with interpreted classes given the fact that all the input 
	  entries are valid. If not, it will do error handling where invalid entries to numerical input
	  boxes will display an error message as shown below. Same way, it can handle error messages 
	  for null or empty values and range of values too.
  	"
  
  # Input parameters format in the side-bar panel
  datasetInput <- reactive({  
    
    "
	    Check for NA values
	    Check input types
	    Creating type.err.list and if there is null value of any parameter, error value will 
	    be changed to 1 and the comment with parameter name will be added to this list
	  "
    type.err.list = c()
    type.error = 0
    
    #Checking if the value entered in input box of Age is Null
    #If yes, then store into type.err.list data
    if(is.na(input$Age)){
      type.error = 1
      type.err.list = c(type.err.list, "Age")
    }
    
    #Checking if the value entered in input box of ESR is Null
    #If yes, then store into type.err.list data
    if(is.na(input$ESR)){
      type.error = 1
      type.err.list = c(type.err.list, "ESR")
    }
    
    #Checking if the value entered in input box of EF.TTE is Null
    #If yes, then store into type.err.list data
    if(is.na(input$EF.TTE)){
      type.error = 1
      type.err.list = c(type.err.list, "Ejection Fraction")
    }
    
    #Checking if the value entered in input box of BMI is Null
    #If yes, then store into type.err.list data
    if(is.na(input$BMI)){
      type.error = 1
      type.err.list = c(type.err.list, "BMI")
    }
    
    #Checking if the value entered in input box of Blood Pressure is Null
    #If yes, then store into type.err.list data
    if(is.na(input$BP)){
      type.error = 1
      type.err.list = c(type.err.list, "Blood Pressure")
    }
    
    #Checking if the value entered in input box of Triglyceride is Null
    #If yes, then store into type.err.list data
    if(is.na(input$TG)){
      type.error = 1
      type.err.list = c(type.err.list, "TG")
    }
    
    #Returning the list for all inputs carrying null values
    if(type.error == 1){
      return(c("Input error in field:", type.err.list))
    }
    
    "
	    Check Ranges
	    Creating range.err.list and if there is any value is out of range for specific 
	    parameter, error value will be changed to 1 and the comment with parameter name 
	    will be added to this list
	  "
    
    range.err.list = c()
    range.error = 0
    
    #Age
    #Defining the range of Age between 1 and 120 [inclusive] 
    #Checking if the value entered in input box of Age is out of range
    #If yes, then store into type.err.list data
    
    if(input$Age > 120 | input$Age <= 0){
      range.error = 1
      range.err.list = c(range.err.list, "Age")
    }
    
    #Erythrocyte Sedimentation Rate [ESR] 
    #Defining the range of ESR between 2 and 90 [inclusive] 
    #Checking if the value entered in input box of ESR is out of range
    #If yes, then store into type.err.list data
    if(input$ESR > 90 | input$ESR <= 1){
      range.error = 1
      range.err.list = c(range.err.list, "ESR")
    }
    
    #Ejection Fraction
    #Defining the range of Ejection Fraction between 2 and 100 [inclusive] 
    #Checking if the value entered in input box of Ejection Fraction is out of range
    #If yes, then store into type.err.list data
    if(input$EF.TTE > 100 | input$EF.TTE <= 1){
      range.error = 1
      range.err.list = c(range.err.list, "EF.TTE")
    }
    
    #TG
    #Defining the range of Triglyceride between 2 and 1200 [inclusive] 
    #Checking if the value entered in input box of Triglyceride is out of range
    #If yes, then store into type.err.list data
    if(input$TG > 1200 | input$TG <= 1){
      range.error = 1
      range.err.list = c(range.err.list, "TG")
    }
    
    #Blood Pressure
    #Defining the range of Blood Pressure between 71 and 220 [inclusive] 
    #Checking if the value entered in input box of Blood Pressure is out of range
    #If yes, then store into type.err.list data
    if(input$BP > 220 | input$BP <= 70){
      range.error = 1
      range.err.list = c(range.err.list, "Blood Pressure")
    }
    
    #BMI
    #Defining the range of BMI between 16 and 50 [inclusive] 
    #Checking if the value entered in input box of BMI is out of range
    #If yes, then store into type.err.list data
    if(input$BMI > 50 | input$BMI <= 15){
      range.error = 1
      range.err.list = c(range.err.list, "BMI")
    }
    
    
    "
	    Data frame to convert the input data inserted into compatible form as model input Converting all 
	    numerical variables into character form to read it into model And Converting categorical variables 
	    'Typical.Chest.Pain, HTN, VHD_sev, Tinversion' into numerical as they are given True/False 
	    input by ticking or non-ticking; However, Atypical and Nonanginal features to be converted into 
	    'Y' and 'N' form as they are into similar form in the model input file.
	  "
    
    df <- data.frame(
      Name = c("Typical.Chest.Pain", "Age", "Atypical","ESR", "Nonanginal", "EF.TTE", "BMI", "HTN", "VHD_sev", "Tinversion","BP", "TG"),
      Value = c(as.numeric(input$Typical.Chest.Pain),
                as.character(input$Age),
                as.numeric(input$Atypical),
                as.character(input$ESR),
                as.numeric(input$Nonanginal),
                as.character(input$EF.TTE),
                as.character(input$BMI),
                as.numeric(input$HTN),
                as.numeric(input$VHD_sev),
                as.numeric(input$Tinversion),
                as.character(input$BP),
                as.character(input$TG)),
      
      stringsAsFactors = FALSE)
    
    #Storing the data frame as they keep being entered and writing into "input.csv" file
    Disease <- 0
    df <- rbind(df, Disease)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    #Reaing the file to store the data into the model compatible form
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    #Building the dataframe containing the prediction using predict() and fuzzy logic having table of interpretable classes
    Output <- data.frame(CAD.Probability = predict(pred.model,test,type="prob")[,1], Risk.Rating = fuzzify(predict(pred.model,test,type="prob")))
    
    #Printing the data frame
    print(Output)
    
    #In case error appears, error list will be displayed and stored into data frame
    if(range.error > 0){Output = c(Output, data.frame(Check.Range = range.err.list))}
    
    #Returning the dataframe 
    return(Output)
    
  })
  
  # Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation completed.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results dataframe containing tables of prediction values and fussy interpretation 
  #And error messages table in case of unexpected inputs
  #When submit button is pressed, it will call server function to print the returned output data frame
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
}


# Create the shiny app----------------------------------
#Calling the application using user interface page and server 
shinyApp(ui = ui, server = server)
