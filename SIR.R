library(deSolve)

epi203 <- function(pars){
        
        print(pars)
        
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
        
        return(results)
}

##############################################################################
## Plotting

test.pars <- c(beta = 0.1, recovery = 0.005, death = 0.001, birth = 0.001)
results   <- epi203(test.pars)

matplot(results[, 1], results[, 2:4], type="l", lty=1,main="SIR Model", xlab= "Time",ylab = "Normalized Populations")
legend("topright", col=1:3, legend=c("S", "I", "R"), lwd=1)
