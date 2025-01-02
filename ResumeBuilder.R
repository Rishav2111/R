library(shiny)
library(rmarkdown)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Resume Builder"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Personal Information", tabName = "personal_info", icon = icon("user")),
      menuItem("Experience", tabName = "experience", icon = icon("briefcase")),
      menuItem("Education", tabName = "education", icon = icon("university")),
      menuItem("Skills", tabName = "skills", icon = icon("cogs")),
      menuItem("Export Resume", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "personal_info",
              fluidRow(
                box(title = "Personal Information", status = "primary", solidHeader = TRUE, width = 12,
                    textInput("name", "Full Name:"),
                    textInput("email", "Email:"),
                    textInput("phone", "Phone:"),
                    textInput("address", "Address:")
                )
              )
      ),
      
      tabItem(tabName = "experience",
              fluidRow(
                box(title = "Work Experience", status = "primary", solidHeader = TRUE, width = 12,
                    textInput("company_name", "Company Name:"),
                    textInput("job_title", "Job Title:"),
                    textInput("job_duration", "Duration:"),
                    textAreaInput("job_description", "Job Description:", rows = 4)
                )
              )
      ),
      
      tabItem(tabName = "education",
              fluidRow(
                box(title = "Education", status = "primary", solidHeader = TRUE, width = 12,
                    textInput("school_name", "School/College Name:"),
                    textInput("degree", "Degree:"),
                    textInput("grad_year", "Year of Graduation:")
                )
              )
      ),
      
      tabItem(tabName = "skills",
              fluidRow(
                box(title = "Skills", status = "primary", solidHeader = TRUE, width = 12,
                    textAreaInput("skills", "Skills (comma-separated):", rows = 3)
                )
              )
      ),
      
      tabItem(tabName = "export",
              fluidRow(
                box(title = "Export Resume", status = "primary", solidHeader = TRUE, width = 12,
                    downloadButton("downloadResume", "Download Resume as PDF")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  renderResumeContent <- function() {
    paste(
      "# Resume\n\n",
      "## Personal Information\n",
      paste("**Name:**", input$name, "\n"),
      paste("**Email:**", input$email, "\n"),
      paste("**Phone:**", input$phone, "\n"),
      paste("**Address:**", input$address, "\n"),
      
      "\n## Experience\n",
      paste("**Company:**", input$company_name, "\n"),
      paste("**Job Title:**", input$job_title, "\n"),
      paste("**Duration:**", input$job_duration, "\n"),
      paste("**Description:**", input$job_description, "\n"),
      
      "\n## Education\n",
      paste("**School/College:**", input$school_name, "\n"),
      paste("**Degree:**", input$degree, "\n"),
      paste("**Year of Graduation:**", input$grad_year, "\n"),
      
      "\n## Skills\n",
      paste("**Skills:**", input$skills, "\n")
    )
  }
  output$downloadResume <- downloadHandler(
    filename = function() {
      paste(input$name, "_resume.pdf", sep = "")
    },
    content = function(file) {
      resume_content <- renderResumeContent()
      temp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(resume_content, temp_rmd)
      render(temp_rmd, output_file = file, pdf_document())
    }
  )
}

shinyApp(ui, server)
