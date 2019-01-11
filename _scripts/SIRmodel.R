### programming SIR Model 
#parameters
# y=time
# S(t) fraccion suceptible en el periodo t
# I(t) fraccion infectada en el periodo t
# b infection rate parameter
# m recovery rate parameter
# relation S(t)= -bSI
# relation I(t)= bSI-mI

# Equations
#ds_dt<- -(b)SI
#di_dt<- bSI-mI

# initial conditions
S_0<- 0.9
I_0<- 0.1

#parameters
b=2
m=0.5


require(reshape2)
require(ggplot2)
require(gridExtra)

df <- SIR(100, 100-20, 20, 0.3, 0.1, 30)
# N poblacion
# S0 condiciones iniciales de poblacion suceptible

SIR <- function(N, S0, I0, beta, alpha, t) {
      S <- numeric(t) # crea un objeto con el tamaño del periodo
      I <- numeric(t)
      S[1] <- S0
      I[1] <- I0
      
      for (i in 2:t) {
            S[i] <- S[i-1] - beta*S[i-1]/N*I[i-1]
            I[i] <- I[i-1] + beta*S[i-1]/N*I[i-1] - alpha * I[i-1]
            if (I[i] < 1 || S[i] < 1)
                  break
      }
      
      df <- data.frame(time=1:t, S=S, I=I, R=N-S-I)
      df <- df[S>1&I>1,]
      
      df$AR <- (df$I+df$R)/N
      nr <- nrow(df)
      rate <- df$I[2:nr]/df$I[1:(nr-1)]
      df$rate <- c(rate[1], rate)
      return(df)
}

plotSIR <- function(df) {
      nr <- nrow(df)
      p1 <- ggplot(df, aes(time, I))+geom_line()
      
      p1 <- p1+ggtitle("No. of Infections")
      p1 <- p1+geom_point(x=df[nr, "time"], y=df[nr, "I"], color="red", size=3)
      p2 <- ggplot(df, aes(time, AR))+geom_line()+xlab("")
      
      p2 <- p2 + ggtitle("Attack rate increases")
      p2 <- p2 + geom_point(x=df[nr, "time"], y=df[nr, "AR"], color="red", size=3)
      p3 <- ggplot(df, aes(time, S))+geom_line()
      p3 <- p3 +ggtitle("Depletion of susceptibles")
      p3 <- p3+ylim(0, max(df$S)) +
            geom_point(x=df[nr, "time"], y=df[nr, "S"], color="red", size=3)
      
      p4 <- ggplot(df, aes(time, rate))+geom_line()
      p4 <- p4 + ggtitle("R")+ ylim(range(df$rate)+c(-.2, .2)) + 
            geom_hline(yintercept=1, color="steelblue", linetype="dashed")
      p4 <- p4+geom_point(x=df[nr, "time"], y=df[nr, "rate"], color="red", size=3)
      
      grid.arrange(p1,p2,p3,p4, ncol=1)
      invisible(list(p1=p1, p2=p2, p3=p3, p4=p4))
}

# df <- SIR(1e6, 1e6-1, 1, 0.3, 0.1, 300)
df <- SIR(100, 100-20, 20, 0.3, 0.1, 30)
plotSIR(df)
