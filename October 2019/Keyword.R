library(shiny)
library(tidyverse)
library(tidytext)
library(tidylo)

talks <- read_csv("shiny_talks.csv")

talk <- unique(talks$title)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Automating Keyword Exploration of Journal Articles"),
  

  
###### Have them select the talk they want to evaluate
  selectInput("talk", "Choose a Talk",
            choices = talk,
            selected = talk[1]),
  
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("ngrams",
                   "Number of words:",
                   choices = c(1, 2, 3),
                   selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Most Common Words", plotOutput("contents")),
                  tabPanel("Most Unique Words", plotOutput("unique"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$contents <- renderPlot({
    
    df_raw <- talks %>%
      filter(title == input$talk)
    
    if(input$ngrams == 1) {
      
      df_unigram <- df_raw %>%
        unnest_tokens("word", text) %>%
        filter(!str_detect(word, "[:digit:]")) %>%
        anti_join(stop_words, by = "word") %>%
        mutate(source = "Article")
      
      
      df_unigram %>%
        count(word, sort = TRUE) %>%
        head(15) %>%
        mutate(word = fct_reorder(str_to_title(word), n)) %>%
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col(show.legend = FALSE) +
        labs(x = "", y = "Number of Occurences",
             title = "Most Common Words") +
        coord_flip() +
        scale_fill_gradient(low = "lightgreen", high = "green") +
        theme_minimal() 
      
      
    } else if(input$ngrams == 2) {
      
      df_bigram <- df_raw %>%
        unnest_tokens("bigram", text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        anti_join(stop_words, by = c("word1" = "word")) %>%
        anti_join(stop_words, by = c("word2" = "word")) %>%
        filter(!str_detect(word1, "[:digit:]"),
               !str_detect(word2, "[:digit:]")) %>%
        unite(bigram, c("word1", "word2"), sep = " ")
      
      df_bigram %>%
        count(bigram, sort = TRUE) %>%
        head(15) %>%
        mutate(bigram = fct_reorder(str_to_title(bigram), n)) %>%
        ggplot(aes(x = bigram, y = n, fill = n)) +
        geom_col(show.legend = FALSE) +
        labs(x = "", y = "Number of Occurences",
             title = "Most Common Words") +
        coord_flip() +
        scale_fill_gradient(low = "lightgreen", high = "green") +
        theme_minimal() 
      
    } else {
      df_trigram <- df_raw %>%
        unnest_tokens("bigram", text, token = "ngrams", n = 3) %>%
        separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
        anti_join(stop_words, by = c("word1" = "word")) %>%
        anti_join(stop_words, by = c("word2" = "word")) %>%
        anti_join(stop_words, by = c("word3" = "word")) %>%
        filter(!str_detect(word1, "[:digit:]"),
               !str_detect(word2, "[:digit:]"),
               !str_detect(word3, "[:digit:]")) %>%
        unite(trigram, c("word1", "word2", "word3"), sep = " ") 
      
      df_trigram %>%
        count(trigram, sort = TRUE) %>%
        head(15) %>%
        mutate(trigram = fct_reorder(str_to_title(trigram), n)) %>%
        ggplot(aes(x = trigram, y = n, fill = n)) +
        geom_col(show.legend = FALSE) +
        labs(x = "", y = "Number of Occurences",
             title = "Most Common Words") +
        coord_flip() +
        scale_fill_gradient(low = "lightgreen", high = "green") +
        theme_minimal() 
    }
    
  })
  
  output$unique <- renderPlot({
    
    df_raw <- talks %>%
      filter(title == input$talk)
    
    training <- talks %>%
      filter(title != input$talk) %>%
      unnest_tokens("word", text) %>%
      filter(!str_detect(word, "[:digit:]")) %>%
      anti_join(stop_words, by = "word") %>%
      mutate(source = "training")
    
    df_unigram <- df_raw %>%
      unnest_tokens("word", text) %>%
      filter(!str_detect(word, "[:digit:]")) %>%
      anti_join(stop_words, by = "word") %>%
      mutate(source = "main")
    
    combined <- rbind(df_unigram, training) %>%
      group_by(source) %>%
      count(word, sort = TRUE)
    
    combined %>%
      bind_log_odds(source, word, n) %>%
      filter(source == "main") %>%
      arrange(desc(log_odds)) %>%
      head(15) %>%
      mutate(word = fct_reorder(str_to_title(word), log_odds)) %>%
      ggplot(aes(x = word, y = log_odds, fill = log_odds)) +
      geom_col(show.legend = FALSE) +
      labs(x = "", y = "Weighted Log-Odds",
           title = "Most Unique Words") +
      coord_flip() +
      scale_fill_gradient(low = "lightgreen", high = "green") +
      theme_minimal() 
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
