packages <- c("shiny", "rvest", "dplyr", "tidytext", "ggplot2", 
              "stringr", "lubridate", "textdata")
install_if_missing <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(install_if_missing)) install.packages(install_if_missing)
library(shiny)
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(lubridate)
library(textdata)
if (!file.exists("afinn_data.csv")) {
  afinn <- get_sentiments("afinn")
  write.csv(afinn, "afinn_data.csv", row.names = FALSE) # Cache AFINN
} else {
  afinn <- read.csv("afinn_data.csv")
}
scrape_headlines <- function(url) {
  tryCatch({
    headlines <- read_html(url) %>%
      html_nodes("h2, h3") %>% # Modify selectors to match headline tags on the site
      html_text() %>%
      str_trim()
    headlines <- headlines[headlines != ""] # Remove empty results
    return(headlines)
  }, error = function(e) {
    return(c("Error: Unable to fetch data"))
  })
}
analyze_sentiment <- function(text_data) {
  sentiment <- text_data %>%
    unnest_tokens(word, text) %>%
    inner_join(afinn, by = "word") %>%
    summarise(score = sum(value, na.rm = TRUE))
  return(sentiment$score)
}
ui <- fluidPage(
  titlePanel("News Sentiment Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("url", "News Website URL:", value = "https://www.bbc.com/news"),
      actionButton("fetch", "Fetch News"),
      helpText("Enter a valid news website URL that contains headlines (e.g., BBC News)."),
      hr(),
      h4("Sentiment Analysis Results"),
      verbatimTextOutput("sentiment_summary")
    ),
    mainPanel(
      h4("News Sentiment Trend"),
      plotOutput("sentiment_plot"),
      hr(),
      h4("Scraped Headlines"),
      tableOutput("headlines_table")
    )
  )
)
server <- function(input, output) {
  news_data <- reactiveVal(data.frame(Time = character(), Sentiment = numeric()))
  
  observeEvent(input$fetch, {
    headlines <- scrape_headlines(input$url)
    if ("Error: Unable to fetch data" %in% headlines) {
      showNotification("Failed to fetch news. Check the URL.", type = "error")
      return()
    }
    
    
    combined_text <- paste(headlines, collapse = " ")
    
    
    sentiment_score <- analyze_sentiment(data.frame(text = combined_text))
    
    
    new_data <- data.frame(Time = now(), Sentiment = sentiment_score)
    news_data(rbind(news_data(), new_data))
    
    
    output$sentiment_summary <- renderText({
      paste("Sentiment Score:", round(sentiment_score, 2), 
            "\nPositive: > 0 | Negative: < 0 | Neutral: 0")
    })
    output$headlines_table <- renderTable({
      data.frame(Headlines = headlines)
    })
  })
  
  
  output$sentiment_plot <- renderPlot({
    ggplot(news_data(), aes(x = Time, y = Sentiment)) +
      geom_line(color = "blue") +
      geom_point(size = 2) +
      labs(title = "Sentiment Score Over Time", x = "Time", y = "Sentiment Score") +
      theme_minimal()
  })
}
shinyApp(ui = ui, server = server)
