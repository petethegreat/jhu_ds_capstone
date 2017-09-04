library(shiny)
library(leaflet)
library(DT)
library(shinyjs)


# App loading message and plot loading graphic based on examples found here: http://deanattali.com/blog/advanced-shiny-tips/

peteCSS<-"
#loadScreen {
    position:absolute;
    background: #FFFFFF;
    opacity: 0.8;
    z-index: 3;
    height: 100%;
    width: 100%;
    text-align: center;
    color: #AA00FF;
}
h1,h2,h3,h4 {
    color: #0000FF;
    font-family: 'Montserrat', sans-serif;
}
body {
    background: #BBBBFF
}
"

shinyUI(
  fluidPage(

    useShinyjs(),
    tags$head(tags$style(HTML('<link href="https://fonts.googleapis.com/css?family=Montserrat" rel="stylesheet">'))),
    inlineCSS(peteCSS),
    div(id='loadScreen',h1('loading app, please wait'),h4('(This may take a few seconds...)')),

    hidden(
        div(id='content',
            titlePanel('TextPred - Next Word Prediction'),
            sidebarLayout(
              sidebarPanel(
                tabsetPanel
                (
                    tabPanel("Input",
                        textInput('inputText',label='Input Text',value='Would you like to go to the')
                        ),
                    tabPanel("Help",
                        p('help text goes here')
                        )
                )

                # h3('TextPred - Text Prediction'),
                
                # tabsetPanel
                # (
                #   tabPanel("Controls",
                #            # sliderInput('ratingRange','Rating: ',min=0.0,max=5.0,step=0.5,value=c(0.0,5.0)),
                #            # uiOutput('catGroup')
                #            ), # end tabPanel
                  # tabPanel("Help",
                  #          p('The map to the right shows Toronto Restaurants, colour coded by cuisine'),
                  #          p('Adjust the slider to filter restaurants based on their Yelp rating'),
                  #          p('select/deselect checkboxes to control which cuisine types will be displayed'),
                  #          p('Restaurant names will be displayed on mouseover'),
                  #          p('A link to the restaurant\'s Yelp page (as well as it\'s rating) will be displayed when a marker is clicked')
                  #          ) # end tabPanel
                # ) # end tabsetPanel
              ), # end sidebarPanel
              mainPanel
              (
                # tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                # leafletOutput("map")
                # h3('text output'),
                # p('predicted output:'),
                # br(),
                # p('Predicted Next Word: '),
                htmlOutput("predictedText"),
                br(),
                plotOutput('wordPlot')
                # DT::dataTableOutput("predictedDT")



              ) # end mainPanel 
            )  # end sidebarLayout
        ) # end hidden div
    ) #end hidden

  ) # end fluidPage
) # end shinyUI
