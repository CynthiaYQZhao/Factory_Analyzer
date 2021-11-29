#install.packages("shiny")
# library(shiny)
# library(rsconnect)

source("init.R")

# Define UI for app
ui <- shinyUI(
  navbarPage(
    # Fluid pages scale their components in realtime to fill all available browser width
    # titlePanel: create a header panel containing an application title
    titlePanel(title = "Faculty Analyzer"),
    
    wellPanel(
      p(
        "Please upload your resume file as a plain text file, and course description file as a csv file."
      ),
      p(
        "When finished, you can download scoring result table as excel file."
      )
    ),
    
    # SidebarLayout():create a layout with a sidebar and main area
    sidebarLayout(
      sidebarPanel(
        #Inupt
        fileInput('upload_resume', 'Upload Resume File (.txt)', multiple = FALSE),
        
        fileInput(
          'upload_syllabus',
          'Choose Course Description File (.csv)',
          multiple = FALSE,
          accept = c('text/csv', 'text/comma-separated-values,text/plain')
        ),
        actionButton("go", "Compute"),
        helpText("Default max. file size is 5MB"),
        
        uiOutput("formatBtn"), 
        uiOutput("downloadBtn")
      ),
      
      
      mainPanel(
        uiOutput("reportTitle"),
        dataTableOutput("case_resume_result"),
        plotlyOutput("rankBarChart")
      )
    )
  )
)