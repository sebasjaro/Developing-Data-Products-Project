library(shiny)
library(deSolve)
library(ggplot2)

epi203 <- function(beta, gamma){
        
        pars <- c(beta, recovery = gamma, death = 0.001, birth = 0.001)
       
        times <- seq(from = 0, to = 600, by = 1)              
        yinit <- c(Susc = 0.9, Infected = 0.1, Recovered = 0) # nitial conditions
        
        SIR_model <- function(times, yinit, pars){
                
                with(as.list(c(yinit,pars)), {
                        
                        dSusc      <- birth - beta*Infected*Susc                     - death*Susc
                        dInfected  <-         beta*Infected*Susc - recovery*Infected - death*Infected
                        dRecovered <-                              recovery*Infected - death*Recovered
                        
                        return(list(c(dSusc, dInfected, dRecovered)))})
        }
        
        results <- ode(func = SIR_model, times = times, y = yinit, parms = pars)
        results <- as.data.frame(results)
        #return(results)
        g <- ggplot(results,aes(x=time)) + geom_line(mapping=aes(y=Susc,color="S"),size = .8) 
        g <- g + geom_line(mapping=aes(y=Infected,color="I"),size=.8) 
        g <- g + geom_line(mapping=aes(y=Recovered,color="R"),size=.8)
        g <- g + labs(title = "SIR Model",x="Time",y="Normalized Populations")
        g <- g +theme(
                plot.title = element_text(size=16, face="bold",hjust = 0.5),
                axis.title.x = element_text(size=16),
                axis.title.y = element_text( size=16),legend.title = element_blank()
                ) + guides(color = guide_legend(override.aes = list(size = 3)))
        
        return(g)
}




shinyServer(
        function(input, output) {
                output$OBeta <- renderPrint({input$Beta})
                output$OGamma <- renderPrint({input$Gamma})
                #output$prediction <- renderPrint({epi203(input$Beta,input$Gamma)})
                #n1 <- reactive({epi203(input$Beta,input$Gamma)})
                #output$prediction <- renderPrint({n1()})
                #output$distPlot <- renderPlot({matplot(n1[, 1],n1[, 2:4], type="l",
                #lty=1,main="SIR Model", xlab= "Time",ylab = "Normalized Populations",
                #legend("topright", col=1:3, legend=c("S", "I", "R"), lwd=1))})
                output$distPlot <- renderPlot({epi203(input$Beta,input$Gamma)})
        }
)