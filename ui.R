library(shiny)

shinyUI(fluidPage(titlePanel("SIR Model for Spread of Disease"),

#pageWithSidebar(
 #       headerPanel("Hello Shiny!"),
        sidebarPanel(
                h3("Model Parameters"),
                
                sliderInput("Beta","Beta",
                            min = 0, max = 1,
                            value = .15, step = 0.05),
                sliderInput("Gamma", "Gamma",
                            min = 0, max = 0.1,
                            value = .05, step = 0.01),
                submitButton('Submit')
        ),
        mainPanel(
                h3('Selected Parameters'),
                h4('Beta'),
                verbatimTextOutput("OBeta"),
                h4('Gamma'),
                verbatimTextOutput("OGamma"),
                #h4('Which resulted in a prediction of '),
                #verbatimTextOutput("prediction"),
                plotOutput("distPlot")
               
        )
))
