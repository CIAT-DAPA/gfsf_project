############### Umbrella CGIAR

require(reshape2)
require(ggplot2)
require(gridExtra)
require(dplyr)
require(plyr)
require(tidyr)
options(digits=3) 
options(scipen = 999)
# condiciones iniciales
t<- 30 
S0<- 799
I0<- 1
S <- numeric(t)
I <- numeric(t)
Sr<- numeric(t)
Ir<- numeric(t)
Rr<- numeric(t)
S[1] <- S0
I[1] <- I0
beta<- 0.001 # transmition 
alpha<- 1/100      

# i=2
for(i in 2:t){
      
      S[i] <-  S[i-1]- beta*S[i-1]*I[i-1]
      I[i] <-  I[i-1]+ beta*S[i-1]*I[i-1] - alpha*I[i-1]
      # if (I[i] < 1 || S[i] < 1)
      #       break
      Sr[i]<-  -beta*S[i-1]*I[i-1]
      Ir[i]<-  beta*S[i-1]*I[i-1] - alpha*I[i-1]
      Rr[i]<-  alpha*I[i-1]
}
df<- data.frame(time=1:t,S=S, I=I, Sr=Sr, Ir=Ir,Rr=Rr)

threshold<- alpha/beta

nr <- nrow(df)
rate <- df$I[2:nr]/df$I[1:(nr-1)]
df$rate <- c(rate[1], rate)

nr <- nrow(df)
p1 <- ggplot(df, aes(time, I))+geom_line()
p1 <- p1+ggtitle("No. of Infections")
p1 <- p1+geom_point(x=df[nr, "time"], y=df[nr, "I"], color="red", size=3)
plot(p1)

p3 <- ggplot(df, aes(time, S))+geom_line()
p3 <- p3 +ggtitle("Depletion of susceptibles")
p3 <- p3+ylim(0, max(df$S)) +
      geom_point(x=df[nr, "time"], y=df[nr, "S"], color="red", size=3)
plot(p3)


p5 <- ggplot(df %>% select(time,Sr,Ir) %>% gather(changes,val, 2:3), aes(time, val, color=changes))+ 
      geom_line()
p5 <- p5 + ggtitle("Curves I`(t) and S`(t)") 
p5 <- p5 + geom_hline(yintercept=threshold, color="red", size=2)
plot(p5)


grid.arrange(p1,p3,p5, ncol=1)
invisible(list(p1=p1, p3=p3, p5=p5))






