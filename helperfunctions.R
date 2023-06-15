library(tidyverse)


# create ui- and server-function for simple question

# simple question = Yes/ No Button , no additional elements


################################################################################

# UI

generateShinyUI <- function(question, lead, number) {
  ui <- fixedPage(
    # Fourth question
    tags$h3(class = "frage", question),
    
    # Fourth lead
    tags$p(class = "lead", lead),
    
    # Yes/no button 4
    actionButton(paste0("button_yes",number), "JA"),
    actionButton(paste0("button_no",number), "NEIN"),
    
    uiOutput(paste0("linkOutput",number+1)),
    
    # Hyphen after first block of topics
    tags$hr()
  )
  
  return(ui)
}


generateShinyServer <- function(input, output, session,
                                yes, no ,outputlink, link,
                                linktext,
                                nein_text = "das ist ein neintext") {
  
  observeEvent(input[[yes]], {
    output[[outputlink]] <- renderUI({
      tags$h4(
        class = "link",
        tags$a(href = paste0("https://",link), paste0(linktext))
      )
    })
    
    shinyjs::disable(no)
  })
  
  observeEvent(input[[no]], {
    output[[outputlink]] <- renderUI({
      tags$h4(
        class = "link",
        nein_text
      )
    })
  })
  
}




# dropdown function


# UI-SIDE

library(shiny)
library(shinyjs)

library(shiny)
library(shinyjs)

generateShinyUI_dropdown <- function(question, lead, number) {
  ui <- fixedPage(
    # Second question
    tags$h3(class = "frage", question),
    
    # Second lead
    tags$p(class = "lead", lead),
    
    # Yes/no button 2
    actionButton(paste0("button_yes", number), "JA"),
    actionButton(paste0("button_no", number), "NEIN"),
    
    # Dropdown menu (hidden by default)
    div(class = "knt_auswahl",
      id = paste0("dropdown", number),
      style = "display: none;",
      selectizeInput(
        inputId = paste0("topic_choice", number),
        label = "Kanton",
        selected = "placeholder",
        choices = c("Ukraine-Krieg", "CS-Debakel", "Krankenkassen", "Inflation"),
        options = list(
          placeholder = "Wählen Sie ein Thema",
          onInitialize = I('function() { this.setValue(""); this.$control.addClass("knt_auswahl"); }')
        )
      )
    ),
    
    # Output for link
    uiOutput(paste0("linkOutput", number + 1)),
    
    # Hyphen after first block of topics
    tags$hr()
  )
  
  return(ui)
}

# SERVER-SIDE



##############################################################################
# Question 3
##############################################################################

generateShinyServer_dropdown <- function(input, output, session,
                                         yes, no, outputlink, link, 
                                         linktext, 
                                         nein_text = "Das ist ein Neintext Dropdown",
                                         number) {
  
  # Show/hide dropdown menu based on button click
  observeEvent(input[[yes]], {
    shinyjs::show(paste0("dropdown", number))
    shinyjs::disable(no)
  })
  
  observeEvent(input[[no]], {
    shinyjs::hide(paste0("dropdown", number))
  })
  
  # Display second link if topic choice is selected
  observeEvent(input[[paste0("topic_choice", number)]], {
    if (input[[paste0("topic_choice", number)]] %in% c("Ukraine-Krieg", "CS-Debakel", "Krankenkassen", "Inflation")) {
      output[[outputlink]] <- renderUI({
        tags$h4(
          class = "link",
          tags$a(href = paste0("https://", link), paste0(linktext))
        )
      })
      
      # Hide the dropdown after a selection is made
      shinyjs::hide(paste0("dropdown", number))
    } else {
      output[[outputlink]] <- renderUI(NULL)
    }
  })
}