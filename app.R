#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Install necessary packages

install.packages("anytime")
install.packages("shiny")
install.packages("eeptools")
install.packages("shinythemes")
install.packages("shinyWidgets")
library("anytime")
library("shiny")
library(dplyr)
library(stringr)
library(eeptools)
library(shinythemes)
library(shinyWidgets)


# Ingesting Raw Data & Combining Dataframes

# Ingesting raw data 
patients <- read.csv("patients.csv")
encounters <- read.csv("Encounter.csv") 
payers <- read.csv("payers.csv")

# Combining "patients" and "encounters"
combined <- right_join(patients, encounters, by=c("Id" = "PATIENT"), copy = FALSE, keep = FALSE, na_matches = "na")

# Combining "payers" to the intersection of "payers" and "encounters"
combined_payers <- right_join(combined, payers, by=c("PAYER" = "Id"), copy = FALSE, keep = FALSE, na_matches = "na")
#```

# Cleaning combined dataframe 

# Removing unnecessary columns
combined_payers <- subset(combined_payers, select = -c(ADDRESS.y:MEMBER_MONTHS, SSN:PASSPORT, CITY.x, STATE))
combined_payers <- subset(combined_payers, select = -c(LAT, LON, LAST:MARITAL, BIRTHPLACE, ADDRESS.x, ZIP.x:PROVIDER, TOTAL_CLAIM_COST, ETHNICITY))

#Changes the name of the column from name to PAYER_NAME
colnames(combined_payers)[which(names(combined_payers) == 'NAME')] <- 'PAYER_NAME'

# Change reason code column to be the new cost after insurance column
colnames(combined_payers)[which(names(combined_payers) == 'REASONCODE')] <- 'AFTER_INSURANCE'

# Making no-insurance designation consistent along with fixing titles on data set itself
for(i in 1:length(combined_payers$BIRTHDATE)){
  if (combined_payers$PAYER_NAME[i] == "NO_INSURANCE"){
    combined_payers$PAYER_NAME[i] = "No Insurance"
  }
  if (combined_payers$ENCOUNTERCLASS[i] == "urgentcare"){
    combined_payers$ENCOUNTERCLASS[i] = "Urgent Care"
  } 
  if (combined_payers$GENDER[i] == "M"){
    combined_payers$GENDER[i] = "Male"
  }
  if (combined_payers$GENDER[i] == "F"){
    combined_payers$GENDER[i] = "Female"
  }
}

for(i in 1:length(combined_payers$BIRTHDATE)){ 
  # Calculate a column for the estimated cost after insurance
  combined_payers$AFTER_INSURANCE[i] = combined_payers$BASE_ENCOUNTER_COST[i] - combined_payers$PAYER_COVERAGE[i] 
  if (combined_payers$AFTER_INSURANCE[i] < 0){
    combined_payers$AFTER_INSURANCE[i] = 0
  }
  
  # Convert strings to title case for better visual appeal
  combined_payers$ENCOUNTERCLASS[i] = str_to_title(combined_payers$ENCOUNTERCLASS[i]) #Capitalize encounter classes to make UI look good
  combined_payers$RACE[i] = str_to_title(combined_payers$RACE[i])
  combined_payers$DESCRIPTION[i] = str_to_title(combined_payers$DESCRIPTION[i])
  combined_payers$PAYER_NAME[i] = str_to_title(combined_payers$PAYER_NAME[i])
}


# AGE CALCULATIONS
#```{r}
# Change prefix column to be the new age column
colnames(combined_payers)[which(names(combined_payers) == 'PREFIX')] <- 'AGE'

# Set today's date
Todays_Date = as.Date(anydate("2024-01-01"))

for(i in 1:length(combined_payers$BIRTHDATE)){
  Pat_Birth_Date = anydate(combined_payers$BIRTHDATE[i])
  
  # Patients without death date (alive)
  if (combined_payers$DEATHDATE[i] == ""){
    Age = floor(age_calc(Pat_Birth_Date, enddate = Todays_Date, units = "years"))
    combined_payers$AGE[i] = Age
    
    # Patients with death date (dead)
  } else { 
    Death_Date = anydate(combined_payers$DEATHDATE[i])
    Age = floor(age_calc(Pat_Birth_Date, enddate = Death_Date, units = "years"))
    combined_payers$AGE[i] = Age
  }
}
#```

# Define Age Classes
#r}
# Redefining the "FIRST" column to be the new "AGECLASS" column
colnames(combined_payers)[which(names(combined_payers) == 'FIRST')] <- 'AGECLASS'

# Assign the correct age class for each observation of the age column
for(i in 1:length(combined_payers$AGE)){
  Pat_Age = combined_payers$AGE[i]
  
  if (Pat_Age < 20) {
    combined_payers$AGECLASS[i] = "0-20"
  }
  else if (Pat_Age >= 20 && Pat_Age < 30){
    combined_payers$AGECLASS[i] = "20-29"
  }
  
  else if (Pat_Age >= 30 && Pat_Age < 40){
    combined_payers$AGECLASS[i] = "30-39"
  }
  
  else if (Pat_Age >= 40 && Pat_Age < 50){
    combined_payers$AGECLASS[i] = "40-49"
  }
  
  else if (Pat_Age >= 50 && Pat_Age < 60){
    combined_payers$AGECLASS[i] = "50-59"
  }
  else if (Pat_Age >= 60 && Pat_Age < 70){
    combined_payers$AGECLASS[i] = "60-69"
  }
  
  else if (Pat_Age >= 70 && Pat_Age < 80){
    combined_payers$AGECLASS[i] = "70-79"
  }
  
  else if (Pat_Age >= 80){
    combined_payers$AGECLASS[i] = "80+"
  }
}

#```

# Defining drop-down filters & options
#```{r}
# Create lists that store the drop down filter information for each attribute
Locations = unique(combined_payers$COUNTY)
Genders = unique(combined_payers$GENDER)
Races = str_to_title(unique(combined_payers$RACE))
Encounter_Types = unique(combined_payers$ENCOUNTERCLASS)
Insurance_Types = str_to_title(unique(combined_payers$PAYER_NAME))
Ages = str_sort(unique(combined_payers$AGECLASS))
Compare_By = c("", "Location", "Gender", "Age", "Race", "Encounter Class", "Visit Type", "Insurance")
Visit_Types = str_to_title(unique(combined_payers$DESCRIPTION))

# Visit Types will be filtered further based on user inputs (server) 
#```

# Gather Visit Types by Encounter Class
#```{r}

# Group each visit type by its encounter class
for (i in 1:length(Encounter_Types)){
  
  current = unique(subset(combined_payers, ENCOUNTERCLASS==Encounter_Types[i],select=c("ENCOUNTERCLASS","DESCRIPTION")))
  
  if (Encounter_Types[i]== "Ambulatory"){
    Ambulatory_VT = current$DESCRIPTION
  }
  
  else if (Encounter_Types[i]== "Wellness"){
    Wellness_VT = current$DESCRIPTION
  }
  
  else if (Encounter_Types[i]== "Inpatient"){
    Inpatient_VT = current$DESCRIPTION
  }
  
  else if (Encounter_Types[i]== "Urgent Care"){
    UrgentCare_VT = current$DESCRIPTION
  }
  
  else if (Encounter_Types[i]== "Outpatient"){
    Outpatient_VT = current$DESCRIPTION
  }
  
  else if (Encounter_Types[i]== "Emergency"){
    Emergency_VT = current$DESCRIPTION
  }
  
}

#```

# UI Builder
#```{r}
ui <- fluidPage(theme = shinytheme("flatly"), 
                titlePanel(h1("Massachusetts Estimated Medical Cost Finder", align = 'center', style = "font-size:32px;")),
                
                sidebarLayout(position='left',
                              
                              sidebarPanel(
                                
                                p("Instructions: Please select your desired input for each category using the drop-down menus below. Please select inputs in order.  The 'All' option gives an average result for all of the options in that category combined.", style = "font-size:16px;"),
                                
                                p(h1("", style = "font-size:32px;")),
                                
                                selectInput(inputId = 'Location',
                                            label = 'Location:',
                                            choices = c("", "All", Locations),
                                            selected = NULL),
                                
                                selectInput(inputId = 'Gender',
                                            label = 'Gender:',
                                            choices = c("", "All", Genders),
                                            selected = NULL),
                                
                                selectInput(inputId = 'Age',
                                            label = 'Age Range (in years):',
                                            choices = c("", "All", Ages),
                                            selected = NULL),
                                
                                selectInput(inputId = 'Race',
                                            label = 'Race:',
                                            choices = c("", "All", Races),
                                            selected = NULL),
                                
                                selectInput(inputId = 'EncounterClass',
                                            label = 'Encounter Class:',
                                            choices = c("", "All", Encounter_Types),
                                            selected = NULL),
                                
                                selectInput(inputId = 'VisitType',
                                            label = 'Visit Reason:',
                                            choices = c("", "All", Emergency_VT),
                                            selected = NULL),
                                
                                selectInput(inputId = 'Insurance',
                                            label = 'Insurance Company:',
                                            choices = c("", "All", Insurance_Types),
                                            selected = NULL),
                                
                                selectInput(inputId = 'CompareBy',
                                            label = 'Compare Results By:',
                                            choices = Compare_By,
                                            selected = NULL),
                                width = 3
                              ),
                              
                              mainPanel(
                                
                                br(),
                                plotOutput("FilterPlot"),
                                br(),
                                plotOutput("InsPlot"),
                                
                                h2("Based on the inputs you provided, here are your estimated costs:", align = 'left',  style = "font-size:25px;"),
                                
                                textOutput("Base_Cost"), tags$head(tags$style("#Base_Cost{color: black; font-size: 20px;font-style: bold;}")),
                                textOutput("After_Ins_Cost"), tags$head(tags$style("#After_Ins_Cost{color: black; font-size: 20px;font-style: bold;}")),
                                br(),
                                br(),
                                
                              )
                )
)
# --------------------- SERVER FUNCTION ----------------------------------------
server = function(input, output, session){
  
  # Make Visit Type drop-down options dependent on Encounter Class user-input-----
  observeEvent(input$EncounterClass, {
    if (input$EncounterClass == "Ambulatory"){
      updateSelectInput(session, "VisitType", choices = c("All", Ambulatory_VT))
    }
    else if (input$EncounterClass == "Wellness"){
      updateSelectInput(session, "VisitType", choices = c("All", Wellness_VT))
    }
    else if (input$EncounterClass == "Inpatient"){
      updateSelectInput(session, "VisitType", choices = c("All", Inpatient_VT))
    }
    else if (input$EncounterClass == "Urgent Care"){
      updateSelectInput(session, "VisitType", choices = c("All", UrgentCare_VT))
    }
    else if (input$EncounterClass == "Outpatient"){
      updateSelectInput(session, "VisitType", choices = c("All", Outpatient_VT))
    }
    else if (input$EncounterClass == "Emergency"){
      updateSelectInput(session, "VisitType", choices = c("All", Emergency_VT))
    }
    else if (input$EncounterClass == "All"){
      updateSelectInput(session, "VisitType", choices = "All")
    }
  })
  
  
  # Filtering data frame by filters to calculate two output costs------------------
  
  Base_Cost <- reactiveVal()
  After_Ins_Cost <- reactiveVal()
  
  filters <- reactive({list(input$Location,input$Gender, input$Age, input$Race,input$EncounterClass, input$VisitType, input$Insurance)})
  
  observeEvent(filters(), {
    
    if (input$Location != "All"){
      costs_filtered_df <- subset(combined_payers, COUNTY == input$Location)
      c_df1 <- costs_filtered_df #update usable df for next if statement
    } else (c_df1 <- combined_payers) #if no filtering, keep same df
    
    if (input$Gender != "All"){
      costs_filtered_df <- subset(c_df1, GENDER == input$Gender)
      c_df2 <- costs_filtered_df #update usable df for next if statement
    } else {c_df2 <- c_df1} #if no filtering, keep same df
    
    if (input$Age != "All"){
      costs_filtered_df <- subset(c_df2, AGECLASS == input$Age)
      c_df3 <- costs_filtered_df #update usable df for next if statement
    } else {c_df3 <- c_df2} #if no filtering, keep same df
    
    if (input$Race != "All"){
      costs_filtered_df <- subset(c_df3, RACE == input$Race)
      c_df4 <- costs_filtered_df #update usable df for next if statement
    } else {c_df4 <- c_df3} #if no filtering, keep same df
    
    if (input$EncounterClass != "All"){
      costs_filtered_df <- subset(c_df4, ENCOUNTERCLASS == input$EncounterClass)
      c_df5 <- costs_filtered_df #update usable df for next if statement
    } else {c_df5 <- c_df4} #if no filtering, keep same df
    
    if (input$Insurance != "All"){
      costs_filtered_df <- subset(c_df5, PAYER_NAME == input$Insurance)
      c_df6 <- costs_filtered_df #update usable df for next if statement
    } else {c_df6 <- c_df5} #if no filtering, keep same df
    
    if (input$VisitType != "All"){
      costs_filtered_df <- subset(c_df6, DESCRIPTION == input$VisitType)
      c_df7 <- costs_filtered_df #update usable df for next if statement
    } else {c_df7 <- c_df6} #if no filtering, keep same df
    
    c_df7
    
    Base_Cost = ceiling(colMeans(c_df7['BASE_ENCOUNTER_COST']))
    After_Ins_Cost = ceiling(colMeans(c_df7['AFTER_INSURANCE']))
    
    output$Base_Cost <- renderText({ 
      
      if (Base_Cost == 'NaN'){
        paste("Estimated Base Cost: Please update the selected inputs. There is no data found.")
      }
      
      else
        paste0("Estimated Base Cost: $", Base_Cost)
      
    })
    
    
    output$After_Ins_Cost <- renderText({ 
      if (After_Ins_Cost == 'NaN'){
        paste("Estimated Cost after Insurance: Please update the selected inputs. There is no data found.")
      }
      else
        paste0("Estimated Cost After Insurance: $", After_Ins_Cost)
      
    })
  })
  
  
  # Handling "Compare By" and "All" for PLOTTING DATAFRAME ONLY-------------------
  filter_plot <- reactive({
    req(input$CompareBy) 
    
    if (input$CompareBy != "Location" & input$Location != "All"){
      filtered_df <- subset(combined_payers, COUNTY == input$Location)
      df1 <- filtered_df #update usable df for next if statement
    } else (df1 <- combined_payers) #if no filtering, keep same df
    
    if (input$CompareBy != "Gender" & input$Gender != "All"){
      filtered_df <- subset(df1, GENDER == input$Gender)
      df2 <- filtered_df #update usable df for next if
    } else {df2 <- df1} #if no filtering, keep same df
    
    if (input$CompareBy != "Age" & input$Age != "All"){
      filtered_df <- subset(df2, AGECLASS == input$Age)
      df3 <- filtered_df #update usable df for next if
    } else {df3 <- df2} #if no filtering, keep same df
    
    if (input$CompareBy != "Race" & input$Race != "All"){
      filtered_df <- subset(df3, RACE == input$Race)
      df4 <- filtered_df #update usable df for next if
    } else {df4 <- df3} #if no filtering, keep same df
    
    if (input$CompareBy != "Encounter Class" & input$EncounterClass != "All"){
      filtered_df <- subset(df4, ENCOUNTERCLASS == input$EncounterClass)
      df5 <- filtered_df #update usable df for next if
    } else {df5 <- df4} #if no filtering, keep same df
    
    if (input$CompareBy != "Visit Type" & input$Insurance != "All"){
      filtered_df <- subset(df5, PAYER_NAME == input$Insurance)
      df6 <- filtered_df #update usable df for next if
    } else {df6 <- df5} #if no filtering, keep same df
    
    if (input$CompareBy != "Insurance" & input$VisitType != "All"){
      filtered_df <- subset(df6, DESCRIPTION == input$VisitType)
      df7 <- filtered_df #update usable df for next if
    } else {df7 <- df6} #if no filtering, keep same df
    
    df7
  })
  
  
  # Reassigning Compare By to an actual series for x-series of bar plot-----------
  filterBy <- eventReactive(input$CompareBy, {
    CompareByLocation = unique(filter_plot()$COUNTY)
    CompareByGender = unique(filter_plot()$GENDER)
    CompareByAge = unique(filter_plot()$AGECLASS)
    CompareByRace = unique(filter_plot()$RACE)
    CompareByEncClass = unique(filter_plot()$ENCOUNTERCLASS)
    CompareByVisType = unique(filter_plot()$DESCRIPTION)
    CompareByInsurance = unique(filter_plot()$PAYER_NAME)
    
    if(input$CompareBy =='Location'){filterBy <- CompareByLocation}
    else if (input$CompareBy =='Gender'){filterBy <- CompareByGender}
    else if (input$CompareBy =='Age'){filterBy <- CompareByAge}
    else if (input$CompareBy =='Race'){filterBy <- CompareByRace}
    else if (input$CompareBy =='Encounter Class'){filterBy <- CompareByEncClass}
    else if (input$CompareBy =='Visit Type'){filterBy <- CompareByVisType}
    else if (input$CompareBy =='Insurance'){filterBy <- CompareByInsurance}
  })
  
  # Calculate mean base cost for each x-series element--------------------------
  cost_vals <- reactive({
    req(input$CompareBy)
    if(input$CompareBy =='Location'){
      y1 <- filter_plot() %>% group_by(COUNTY) %>% summarise(Cost = mean(BASE_ENCOUNTER_COST))}
    else if (input$CompareBy =='Gender'){
      y1 <- filter_plot() %>% group_by(GENDER) %>% summarise(Cost = mean(BASE_ENCOUNTER_COST))}
    else if (input$CompareBy =='Age'){
      y1 <- filter_plot() %>% group_by(AGECLASS) %>% summarise(Cost = mean(BASE_ENCOUNTER_COST))}
    else if (input$CompareBy =='Race'){
      y1 <- filter_plot() %>% group_by(RACE) %>% summarise(Cost = mean(BASE_ENCOUNTER_COST))}
    else if (input$CompareBy =='Encounter Class'){
      y1 <- filter_plot() %>% group_by(ENCOUNTERCLASS) %>% summarise(Cost = mean(BASE_ENCOUNTER_COST))}
    else if (input$CompareBy =='Visit Type'){
      y1 <- filter_plot() %>% group_by(DESCRIPTION) %>% summarise(Cost = mean(BASE_ENCOUNTER_COST))}
    else if (input$CompareBy =='Insurance'){
      y1 <- filter_plot() %>% group_by(PAYER_NAME) %>% summarise(Cost = mean(BASE_ENCOUNTER_COST))}
    
    if(input$CompareBy =='Location'){
      y2 <- filter_plot() %>% group_by(COUNTY) %>% summarise(Cost = mean(AFTER_INSURANCE))}
    else if (input$CompareBy =='Gender'){
      y2 <- filter_plot() %>% group_by(GENDER) %>% summarise(Cost = mean(AFTER_INSURANCE))}
    else if (input$CompareBy =='Age'){
      y2 <- filter_plot() %>% group_by(AGECLASS) %>% summarise(Cost = mean(AFTER_INSURANCE))}
    else if (input$CompareBy =='Race'){
      y2 <- filter_plot() %>% group_by(RACE) %>% summarise(Cost = mean(AFTER_INSURANCE))}
    else if (input$CompareBy =='Encounter Class'){
      y2 <- filter_plot() %>% group_by(ENCOUNTERCLASS) %>% summarise(Cost = mean(AFTER_INSURANCE))}
    else if (input$CompareBy =='Visit Type'){
      y2 <- filter_plot() %>% group_by(DESCRIPTION) %>% summarise(Cost = mean(AFTER_INSURANCE))}
    else if (input$CompareBy =='Insurance'){
      y2 <- filter_plot() %>% group_by(PAYER_NAME) %>% summarise(Cost = mean(AFTER_INSURANCE))}
    
    df1 = data.frame(y1,Cost_Type=rep(c("Base Cost"),each=nrow(y1)))
    colnames(df1)[1] = "x"
    
    df2 = data.frame(y2,Cost_Type=rep(c("Cost After Insurance"),each=nrow(y2)))
    colnames(df2)[1] = "x"
    
    df = rbind(df1, df2)
    df = data.frame(df)
    
    df
  })
  
  # Update comparison plot with any new input------------------------------------------------
  output$FilterPlot <- renderPlot({
    
    req(cost_vals())
    data = cost_vals()
    
    ggplot(data, aes(x = x, y= Cost, fill=Cost_Type)) + 
      geom_col(position=position_dodge()) + #Makes a dodged plot, or a group bar plot
      labs(title="Filtered Cost Estimations", x="Selected Comparison Attribute", y="Cost Estimation ($)", fill = "Type of Cost:") +
      #Adds titles
      theme(plot.title = element_text(hjust = 0.5,size=22)) +
      theme(text = element_text(size = 15), axis.text.x = element_text(angle = -35, vjust = 1, hjust = 0), legend.position = "bottom") + #Changing text size and position
      geom_text(aes(label= round(Cost, 1)), position=position_dodge(0.9), vjust = -0.5) #This adds the cost values above each bar
    
  })
  
  #Create the Compare by Insurance Plot ------------------------------
  
  ins_filter_df <- reactive({
    if (input$Location != "All"){
      ins_filtered_df <- subset(combined_payers, COUNTY == input$Location)
      i_df1 <- ins_filtered_df #update usable df for next if statement
    } else (i_df1 <- combined_payers) #if no filtering, keep same df
    
    if (input$Gender != "All"){
      ins_filtered_df <- subset(i_df1, GENDER == input$Gender)
      i_df2 <- ins_filtered_df #update usable df for next if
    } else {i_df2 <- i_df1} #if no filtering, keep same df
    
    if (input$Age != "All"){
      ins_filtered_df <- subset(i_df2, AGECLASS == input$Age)
      i_df3 <- ins_filtered_df #update usable df for next if
    } else {i_df3 <- i_df2} #if no filtering, keep same df
    
    if (input$Race != "All"){
      ins_filtered_df <- subset(i_df3, RACE == input$Race)
      i_df4 <- ins_filtered_df #update usable df for next if
    } else {i_df4 <- i_df3} #if no filtering, keep same df
    
    if (input$EncounterClass != "All"){
      ins_filtered_df <- subset(i_df4, ENCOUNTERCLASS == input$EncounterClass)
      i_df5 <- ins_filtered_df #update usable df for next if
    } else {i_df5 <- i_df4} #if no filtering, keep same df
    
    
    if (input$VisitType != "All"){
      ins_filtered_df <- subset(i_df5, DESCRIPTION == input$VisitType)
      i_df6 <- ins_filtered_df #update usable df for next if
    } else {i_df6 <- i_df5} #if no filtering, keep same df
    
    i_df6
    
  })
  
  #Create data frame with the information for the insurance plot
  ins_vals <- reactive({
    req(input$CompareBy)
    y3 <- ins_filter_df() %>% group_by(PAYER_NAME) %>% summarise(Aft_Ins_Cost = mean(AFTER_INSURANCE))
    y3
    
  })
  
  # Create x series
  ins_v <- reactive({
    CompareByInsurance_x = unique(ins_filter_df()$PAYER_NAME)
    CompareByInsurance_x
  })
  
  # Create the insurance plot
  output$InsPlot <- renderPlot({
    
    req(ins_vals())
    
    ggplot(ins_vals(), aes(x=ins_v(), y=Aft_Ins_Cost)) + 
      geom_bar(stat="identity", fill = "skyblue1") + 
      labs(title="Cost Estimations by Insurance Type", x="Insurance Company", y="Cost After Insurance ($)") + 
      theme(plot.title = element_text(hjust = 0.5, size=22)) +
      theme(text = element_text(size = 15), axis.text.x = element_text(angle = -35, vjust = 1, hjust = 0)) + 
      geom_text(aes(label= round(Aft_Ins_Cost, 1)), vjust = -0.5)
    
  })
  
}

shinyApp(ui = ui, server = server)
