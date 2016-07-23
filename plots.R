library(fpp)
library(ggplot2)
library(zoo)
library(reshape2)
data("melsyd")
plot(melsyd[,"Economy.Class"],
     main="Economy class passengers: Melbourne-Sydney",
     xlab="Year",ylab="Thousands")
class(melsyd)
summary(melsyd)
str(melsyd)
typeof(melsyd)
melsydDf <- data.frame(year = index(melsyd), value = melt(melsyd)[, 3])
ggplot(melsydDf, aes(x = year, y = value)) + 
        geom_line() + 
        labs(
                title = "Economy class passengers: Melbourne-Sydney",
                x = "Year",
                y = "Thousands"
        ) + 
        theme(
                panel.border = element_rect(color = "black", fill = NA),
                panel.background = element_rect(fill = "white")
                
        )



melsydMeltedDf <- melt(melsyd)
plot(melsydMeltedDf[,3])
summary(melsydMeltedDf[,3])
