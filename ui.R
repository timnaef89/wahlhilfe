library(shiny)
library(leaflet)
library(cookies)
library(htmltools)
library(shinyjs)

source("helperfunctions.R")

# add cookies / style / script

fluidPage(
  useShinyjs(),
  
  
  # check for mobile
  
  tags$script(HTML(
    "$(document).on('shiny:connected', function(event) {
        let check = false;
        
        if(window.innerWidth < 600){check = true;}      
      
        (function(a){if(/(android|bb\\d+|meego).+mobile|avantgo|bada\\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\\.(browser|link)|vodafone|wap|windows ce|xda|xiino/i.test(a)||/1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\\-(n|u)|c55\\/|capi|ccwa|cdm\\-|cell|chtm|cldc|cmd\\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\\-s|devi|dica|dmob|do(c|p)o|ds(12|\\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\\-|_)|g1 u|g560|gene|gf\\-5|g\\-mo|go(\\.w|od)|gr(ad|un)|haie|hcit|hd\\-(m|p|t)|hei\\-|hi(pt|ta)|hp( i|ip)|hs\\-c|ht(c(\\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\\-(20|go|ma)|i230|iac( |\\-|\\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\\/)|klon|kpt |kwc\\-|kyo(c|k)|le(no|xi)|lg( g|\\/(k|l|u)|50|54|\\-[a-w])|libw|lynx|m1\\-w|m3ga|m50\\/|ma(te|ui|xo)|mc(01|21|ca)|m\\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\\-2|po(ck|rt|se)|prox|psio|pt\\-g|qa\\-a|qc(07|12|21|32|60|\\-[2-7]|i\\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\\-|oo|p\\-)|sdk\\/|se(c(\\-|0|1)|47|mc|nd|ri)|sgh\\-|shar|sie(\\-|m)|sk\\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\\-|v\\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\\-|tdg\\-|tel(i|m)|tim\\-|t\\-mo|to(pl|sh)|ts(70|m\\-|m3|m5)|tx\\-9|up(\\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\\-|your|zeto|zte\\-/i.test(a.substr(0,4))) check = true;})(navigator.userAgent||navigator.vendor||window.opera);
        Shiny.setInputValue(id = 'isMobile', value = check);
      
      
      
      
      });"
  )),
  
  # add style with css
  tags$style(type = "text/css", 
             ".container-fluid {width: 100% !important; text-align: center; margin: auto;}
                .knt_auswahl { display: flex; justify-content: center;}
                #knt_auswahl { width: 50%;}     
                .btn-default {font-family: 'Ballinger';}
                .frage {font-family: 'Ballinger'; font-weight: 700;}
                .lead {font-family: 'Ballinger'; font-weight: 400;}
                .knt_auswahl {font-family: 'Ballinger'; margin: auto;}
             
             
             
             
  .teaser {
    padding: 10px;
    margin: 10px auto;
    border: 1px solid #ccc;
    background-color: white;
    border-radius: 10px;
    max-width: 600px;
  }
    .legendimg img {
      width: 100px;
      height: 100px;
      object-fit: cover;
      border-radius: 10px;
    }
    .spitzmarke.logo {
      width: 30px;
      padding-bottom: 8px;
    }
    .spitzmarke.text {
      padding-bottom: 8px;
      letter-spacing: .64px;
    }
    .articletitle {
      padding-bottom: 8px;
      font-weight: bold;
      font-size: 18px;
      line-height: 22.5px;
      text-align: left;
    }
    .beschreibung {
      font-size: 14px;
      color: #636671;
    }
    .spitzmarke {
      padding-bottom: 8px;
      color: #888B95;
      font-size: 14px;
      font-weight: 700;
      vertical-align: middle;
      text-align: left;
    }
    .legendimg {
      padding-right: 16px;
      vertical-align: top;
    }
    .spitzmarke div {
      display: inline-block;
      color: white;
      background-color: black;
      padding-left: 4.32px;
      padding-top: 1.8px;
      padding-bottom: 3.87;
      padding-right: 4.32px;
      margin-right: 4px;
    }
    table,
    tr,
    td {
      padding: 0px;
      margin: 0px;
    }
    .sup {
      padding-left: 2.2px;
      padding-bottom: 2.26px;
    }
    .place {
      padding-right: 1px;
    }
    .time {
      padding-left: 8px;
      padding-right: 8px;
    }
    .level {
      padding-left: 8px;
    }


               "),
  browsable(
    tagList(list(
      tags$head(
        tags$style(
          
          #add Ballinger font
          
          "
          @font-face {
 font-family: 'Ballinger';
 font-weight: normal; 
 src: url('fonts/Ballinger-Regular.eot');
          
 src: url('fonts/Ballinger-Regular.eot?#iefix') 
           format('embedded-opentype'),
          
      url('fonts/Ballinger-Regular.woff') 
           format('woff'), 
          
      url('fonts/Ballinger-Regular.ttf') 
           format('truetype');
         
          }

           @font-face {
 font-family: 'Ballinger';
 font-weight: bold; 
 src: url('fonts/Ballinger-Bold.eot');
          
 src: url('fonts/Ballinger-Bold.eot?#iefix') 
           format('embedded-opentype'),
          
      url('fonts/Ballinger-Bold.woff') 
           format('woff'), 
          
      url('fonts/Ballinger-Bold.ttf') 
           format('truetype');
         
}


        
"
        )
      ),






# div(
#   class = "teaser teaser__content teaser__content--articlelist-a",
#   div(
#     class = "teaser__image",
#     img(src = "https://example.com/teaser_image.jpg")
#   ),
#   div(
#     h2("Heuschnupfen: Allergologe in der Ostschweiz"),
#     p("Lesen Sie den Artikel auf der Tagblatt-Website:"),
#     a(
#       href = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
#       "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228"
#     )
#   )
# )
    
    


# teaserUI(
#   title = "Teaser 1", 
#   lead = "Einleitungstext 1", 
#   image = "https://img.chmedia.ch/2023/06/15/cb546d40-d8e7-4e18-8be7-11d919037fca.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=5516,3677,x397,y459", 
#   spitzmarke = "Spitzmarke 1", 
#   link = "https://tagblatt.ch", 
#   place = "Ort 1", 
#   time = "Zeit 1", 
#   level = "Level 1"
# ),


# Question 1

tags$h3(class="frage", "Wollen Sie wissen, wen Sie in den Nationalrat wählen können - und was die Kandiderenden zu konkreten Fragen sagen?"),

# Lead 1

tags$p(class="lead", "irgend ein lead"),

# yes / no button
actionButton("button_yes", "JA"),
actionButton("button_no", "NEIN"),

# follow-up question 1
uiOutput("frage1_1"),

# element for follow-up question 1
uiOutput("select_knt"),

# link output for question 1


uiOutput("linkOutput"),


# hyphen after first block of topics 
tags$hr(),



##############################################################################
# Question 2
##############################################################################

# second question
tags$h3(class="frage", "Wollen Sie wissen, wen Sie in den Ständerat wählen können?"),


# second lead
tags$p(class="lead", "Lead 2"),

# yes/no button 2
actionButton("button_yes2", "JA"),
actionButton("button_no2", "NEIN"),


# follow-up question 2 /
uiOutput("frage2_1"),

# element for follow-up question 2
uiOutput("select_knt2"),

# link output for question 2 / if no knt is selected in q1

conditionalPanel(
  condition = "['SG', 'AG', 'LU'].indexOf(input.knt_auswahl2) > -1",
  uiOutput("linkOutput2")
  
),




# linkoutput for question 2 / if there is already a canton selected in q1
conditionalPanel(
  condition = "['SG', 'AG', 'LU'].indexOf(input.knt_auswahl) > -1 && input.knt_auswahl2 == null",
  uiOutput("linkOutput3")
  
),

# hyphen after first block of topics 
tags$hr(),


##############################################################################
# Question 3
##############################################################################
#
# # second question
 tags$h3(class="frage", "Interessiert es Sie, welches die grossen Themen im Wahlkampf sind und wie sich die Parteien dazu positionieren?"),
#
#
# # second lead
 tags$p(class="lead", "Lead 3"),
#
# # yes/no button 2
 actionButton("button_yes3", "JA"),
 actionButton("button_no3", "NEIN"),


#
#textOutput("test2"),
uiOutput("frage3_1"),
uiOutput("linkOutput4"),

# hyphen after first block of topics 
tags$hr(),


##############################################################################
# Question 4
##############################################################################
#
# # fourth question
tags$h3(class="frage", "Wie werden sich die Wahlen auswirken auf konkrete Politfragen: Interessiert Sie das?"),
#
#
# # fourth lead
tags$p(class="lead", "Das Parlament in Bern entscheidet über wichtige Fragen, aber längst nicht über alles. Was sich je nach Wahlsieger ändern könnte..."),
#
# # yes/no button 4
actionButton("button_yes4", "JA"),
actionButton("button_no4", "NEIN"),


#

uiOutput("linkOutput5"),

# hyphen after first block of topics 
tags$hr(),


# simple yes/no questions with function


generateShinyUI(question = "Wollen Sie genauer wissen, wie Wählen funktioniert?", 
                lead = "Proporz, Majorz, Kumulieren, Panaschieren, Listenverbindung - das Wählen in der Schweiz hat es in sich. Wir zeigen Ihnen, worauf Sie beim Ausfüllen des Wahlzettels achten müssen.", 
                number = 9),


generateShinyUI(question = "Welche Rolle spielen Wahlumfragen? Was ist Smartvote?", 
                lead = "Diverse Hilfsmittel unterstützen Sie dabei, die Kandidatinnen und Kandidaten besser kennenzulernen und deren Wahlchancen einschätzen zu können", 
                number = 10),

generateShinyUI(question = "Wollen Sie erfahren, wie sich die aktuelle Ausgangslage für die Wahlen präsentiert?", 
                lead = "Wahltag ist Zahltag - aber schon jetzt haben Medien und Politexperten ihre Prognosen aufgestellt, wer gewinnt und wer verliert.", 
                number = 11),

generateShinyUI(question = "Sollen wir Ihnen schon mal aufzeigen, wie Sie am Wahltag erfahren, wer gewählt und wer abgewählt ist?", 
                lead = "Am 22. Oktober ist es soweit: Dann wissen wir, wer die nächsten vier Jahre im Parlament sitzt.", 
                number = 12),




# question with dropdown


generateShinyUI_dropdown(question = "Neugierig, wie sich die Ausgangslage für die einzelnen Parteien präsentiert?",
                         lead = "Wählen Sie aus, für welche Partei Sie sich interessieren?",
                         number = 13,
                         label = "Parteien",
                         choices = c("SP", "SVP", "Mitte", "FDP", "Grüne")),


# ,
# 
# 
# 
# ##############################################################################
# # DEBBUGING STUFF
# ##############################################################################
# 
# 
# textOutput("test"),
# 
# uiOutput("testlink2")

textOutput("testtest")
#,
# textOutput("test2")
    ))
  ),



)


# Define UI for application