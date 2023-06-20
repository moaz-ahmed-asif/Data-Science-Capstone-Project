library(shiny)

# Define UI for application
fluidPage(
    
    # Application title
    titlePanel("Predict the Next Word"),
    
    # Text box to input words
    sidebarLayout(
        sidebarPanel(
            h5('Instructions'),
            helpText("This application is for guessing the next word you are going to write."),
            helpText("The next word is guessed using the frequency of combinations of two, three and four words."),
            helpText("The frequency is calculated using text extracted from blogs, news and twitter that SwiftKey  gives for this projects"),
            helpText("The word NA is used when there are no hint for guessing of no pattern is found"),
            br(),
            h4('Enter a sentence to view which is the next word...'),
            hr(),
            textInput("inputText", "Sentence:", value = ""),
            br(),
        ),
        
        # Show the output panel
        mainPanel(
            h2("Next word"),
            h3(strong(code(textOutput('next_word')))),
            br(),
            h4(tags$b('Seconds taken to predict the very next word:')),
            textOutput('time'),
            br(),
            h4(tags$b('Word guessed using last two words:')),
            textOutput('bigram'),
            br(),
            h4(tags$b('Word guessed using last three words:')),
            textOutput('trigram'),
            br(),
            h4(tags$b('Word guessed using last four words:')),
            textOutput('quadgram'),
            br(),
            br(),
            br(),
            br(),
            h4(),
            tags$b("Data Science Capstone Project"),
            br(),
            tags$b("Author: Md. Moaz Ahmed Asif - June, 2023")
        )
    )
)