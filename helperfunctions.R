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
      renderTeaser(
        teaserimage = "https://img.chmedia.ch/2022/04/24/18046014-3ea0-4f83-b41e-132fecfa140e.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=1024,683,x0,y0",
        spitzmarke = "SPITZMARKE",
        title = "Jeder Fünfte leidet an Heuschnupfen – mit diesen Tipps kommen Sie besser durch die Pollensaison",
        link = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
        place = "Vorname, Nachname",
        time = "",
        level = ""
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

generateShinyUI_dropdown <- function(question, lead, number, choices, label) {
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
        label = label,
        selected = "placeholder",
        choices = choices,
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



generateShinyServer_dropdown <- function(input, output, session,
                                         yes, no, outputlink, link, 
                                         linktext, 
                                         nein_text = "Das ist ein Neintext Dropdown",
                                         number,
                                         choices) {
  
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
    if (input[[paste0("topic_choice", number)]] %in% choices) {
      output[[outputlink]] <- renderUI({
        renderTeaser(
          teaserimage = "https://img.chmedia.ch/2022/04/24/18046014-3ea0-4f83-b41e-132fecfa140e.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=1024,683,x0,y0",
          spitzmarke = "SPITZMARKE",
          title = "Jeder Fünfte leidet an Heuschnupfen – mit diesen Tipps kommen Sie besser durch die Pollensaison",
          link = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
          place = "Vorname, Nachname",
          time = "",
          level = ""
        )
      })
      
      # Hide the dropdown after a selection is made
      shinyjs::hide(paste0("dropdown", number))
    } else {
      output[[outputlink]] <- renderUI(NULL)
    }
  })
}




################################################################################

# Teaser-UI-Funktion zur Erstellung eines einzelnen Teasers
renderTeaser <- function(teaserimage, spitzmarke, title, link, place, time, level) {
  HTML(paste0(
    '<div class="teaser">',
    '<table><tr><td rowspan="3" class="legendimg"><a href="', link, '" target="_top"><img src="', teaserimage, '"></a></td><td class="spitzmarke logo"><div><table><tr><td class="aboplus">abo</td><td class="sup">+</td></tr></table></div></td><td class="spitzmarke text">', spitzmarke, '</td></tr><tr><td class="articletitle" colspan=2><a href="', link, '" target="_top">', title, '</a></td></tr><tr><td class="beschreibung" colspan=2><table><tr><td class="place">', place, '</td><td>', ifelse(time != "", "&centerdot;", ""), '</td><td class="time">', time, '</td><td>', ifelse(level != "", "&centerdot;", ""), '</td><td class="level">', level, '</td></tr></table></td></tr></table>',
    '</div>'
  ))
}
