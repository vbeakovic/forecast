library(fpp)
library(ggplot2)
library(zoo)
library(reshape2)
library(dplyr)
#### Load data ####
data("melsyd")
data("a10")

#### Time plots Figure 2.1 ####
# original plot
plot(melsyd[,"Economy.Class"],
     main="Economy class passengers: Melbourne-Sydney",
     xlab="Year",ylab="Thousands")

# convert ts to data frame
melsydDf <- data.frame(year = index(melsyd), value = melt(melsyd)[, 3])

# ggplot2
ggplot(melsydDf, aes(x = year, y = value)) + 
        geom_line() + 
        scale_y_continuous(breaks = seq(0,30, by = 5)) + 
        labs(
                title = "Economy class passengers: Melbourne-Sydney",
                x = "Year",
                y = "Thousands"
        ) + 
        theme(
                panel.border = element_rect(color = "black", fill = NA),
                panel.background = element_rect(fill = "white"),
                plot.title = element_text(size = 15, face = "bold", margin = margin(t = 10, b = 20)),
                plot.margin = unit(c(0.5, 1, 0.5, 0.1), "cm"),
                axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5, margin = margin(l = 10, r = 10), color = "black"),
                axis.text.x = element_text(size = 13, margin = margin(t = 10, b = 10), color = "black"),
                axis.title = element_text(size = 13),
                axis.ticks = element_line(colour = "black", size = 0.5),
                axis.ticks.length = unit(0.25, "cm")
        )

#### Time plots Figure 2.2 ####
# original plot
plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")

# convert ts to data frame
a10Df <- data.frame(year = index(a10), value = melt(a10)$value)

# ggplot2
ggplot(a10Df, aes(x = year, y = value)) + 
        geom_line() + 
        scale_y_continuous(breaks = seq(5,30, by = 5)) + 
        labs(
                title = "Antidiabetic drug sales",
                x = "Year",
                y = "$ million"
        ) + 
        theme(
                panel.border = element_rect(color = "black", fill = NA),
                panel.background = element_rect(fill = "white"),
                plot.title = element_text(size = 15, face = "bold", margin = margin(t = 10, b = 20)),
                plot.margin = unit(c(0.5, 1, 0.5, 0.1), "cm"),
                axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5, margin = margin(l = 10, r = 10), color = "black"),
                axis.text.x = element_text(size = 13, margin = margin(t = 10, b = 10), color = "black"),
                axis.title = element_text(size = 13),
                axis.ticks = element_line(colour = "black", size = 0.5),
                axis.ticks.length = unit(0.25, "cm")
        )

#### Seasonal plots Figure 2.3 ####
# original plot
seasonplot(a10,ylab="$ million", xlab="Year",
           main="Seasonal plot: antidiabetic drug sales",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

# convert ts to data frame
a10Df <- data.frame(year = index(a10), value = melt(a10)$value)
a10Df$month <- c(7:12, rep(seq(1,12, by = 1), 16), 1:6)
a10Df$month <- factor(a10Df$month, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
a10Df$year <- as.factor(as.character(floor(a10Df$year)))

# ggplot2
firstAnnotation <- a10Df$value[a10Df$month == "Jan"]
lastAnnotation <- a10Df$value[a10Df$month == "Dec"]
ggplot(a10Df, aes(x = month, y = value, group = year, color = year)) + 
        geom_line() + 
        scale_y_continuous(breaks = seq(5,30, by = 5)) + 
        geom_point(size = 2) +
        scale_color_manual(values = c(rep(c("black", "red","green", "blue", "cyan", "purple", "yellow", "grey"), 2), "black", "red")) + 
        annotate("text", x = 1, y = firstAnnotation, label = 1992:2008, hjust = 1.25, 
                 color = c(rep(c("red","green", "blue", "cyan", "purple", "yellow", "grey", "black"), 2), "red")) + 
        annotate("text", x = 12, y = lastAnnotation, label = 1991:2007, hjust = -0.5, 
                 color = c(rep(c("black", "red","green", "blue", "cyan", "purple", "yellow", "grey"), 2), "black")) + 
        
        labs(
                title = "Antidiabetic drug sales",
                x = "Month",
                y = "$ million"
        ) + 
        theme(
                panel.border = element_rect(color = "black", fill = NA),
                panel.background = element_rect(fill = "white"),
                plot.title = element_text(size = 15, face = "bold", margin = margin(t = 10, b = 20)),
                plot.margin = unit(c(0.5, 1, 0.5, 0.1), "cm"),
                axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5, margin = margin(l = 10, r = 10), color = "black"),
                axis.text.x = element_text(size = 13, margin = margin(t = 10, b = 10), color = "black"),
                axis.title = element_text(size = 13),
                axis.ticks = element_line(colour = "black", size = 0.5),
                axis.ticks.length = unit(0.25, "cm"),
                legend.position = "none"
        )

#### Seasonal subseries plots Figure 2.4 ####
# original plot
monthplot(a10,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1,at=1:12,labels=month.abb,cex=0.8)

# data transformation
a10Df <- data.frame(year = index(a10), value = melt(a10)$value)
a10Df$value <- as.numeric(a10Df$value)
a10Df$month <- c(7:12, rep(seq(1,12, by = 1), 16), 1:6)
a10Df$month <- factor(a10Df$month, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
a10Df$year <- as.factor(as.character(floor(a10Df$year)))

a10Df <- arrange(a10Df, month, year)
a10Df$count <- 1:NROW(a10Df)
a10DfJan <- filter(a10Df, month == "Jan")
a10DfFeb <- filter(a10Df, month == "Feb")
a10DfMar <- filter(a10Df, month == "Mar")
a10DfApr <- filter(a10Df, month == "Apr")
a10DfMay <- filter(a10Df, month == "May")
a10DfJun <- filter(a10Df, month == "Jun")
a10DfJul <- filter(a10Df, month == "Jul")
a10DfAug <- filter(a10Df, month == "Aug")
a10DfSep <- filter(a10Df, month == "Sep")
a10DfOct <- filter(a10Df, month == "Oct")
a10DfNov <- filter(a10Df, month == "Nov")
a10DfDec <- filter(a10Df, month == "Dec")


ggplot(a10DfJan, aes(x = count, y = value)) + geom_line() + 
        geom_line(aes(x = a10DfFeb$count, y = a10DfFeb$value)) + 
        geom_line(aes(x = a10DfMar$count, y = a10DfMar$value)) + 
        geom_line(aes(x = a10DfApr$count, y = a10DfApr$value)) + 
        geom_line(aes(x = a10DfMay$count, y = a10DfMay$value)) + 
        geom_line(aes(x = a10DfJun$count, y = a10DfJun$value)) + 
        geom_line(aes(x = a10DfJul$count, y = a10DfJul$value)) + 
        geom_line(aes(x = a10DfAug$count, y = a10DfAug$value)) + 
        geom_line(aes(x = a10DfSep$count, y = a10DfSep$value)) + 
        geom_line(aes(x = a10DfOct$count, y = a10DfOct$value)) + 
        geom_line(aes(x = a10DfNov$count, y = a10DfNov$value)) + 
        geom_line(aes(x = a10DfDec$count, y = a10DfDec$value)) +         
        labs(
                title = "Seasonal deviation plot: antidiabetic drug sales",
                x = "Month",
                y = "$ million"
        ) + 
        theme(
                panel.border = element_rect(color = "black", fill = NA),
                panel.background = element_rect(fill = "white"),
                plot.title = element_text(size = 15, face = "bold", margin = margin(t = 10, b = 20)),
                plot.margin = unit(c(0.5, 1, 0.5, 0.1), "cm"),
                axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5, margin = margin(l = 10, r = 10), color = "black"),
                axis.text.x = element_text(size = 13, margin = margin(t = 10, b = 10), color = "black"),
                axis.title = element_text(size = 13),
                axis.ticks = element_line(colour = "black", size = 0.5),
                axis.ticks.length = unit(0.25, "cm"),
                legend.position = "none"
        )



  
ggplot(a10DfJan, aes(x = count, y = value)) + geom_line() + 
        geom_line(aes(x = a10DfFeb$count, y = a10DfFeb$value)) + 
        geom_line(aes(x = a10DfMar$count, y = a10DfMar$value)) + 
        geom_line(aes(x = a10DfApr$count, y = a10DfApr$value)) + 
        geom_line(aes(x = a10DfMay$count, y = a10DfMay$value)) + 
        geom_line(aes(x = a10DfJun$count, y = a10DfJun$value)) + 
        geom_line(aes(x = a10DfJul$count, y = a10DfJul$value)) + 
        geom_line(aes(x = a10DfAug$count, y = a10DfAug$value)) + 
        geom_line(aes(x = a10DfSep$count, y = a10DfSep$value)) + 
        geom_line(aes(x = a10DfOct$count, y = a10DfOct$value)) + 
        geom_line(aes(x = a10DfNov$count, y = a10DfNov$value)) + 
        geom_line(aes(x = a10DfDec$count, y = a10DfDec$value))

