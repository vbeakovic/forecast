library(fpp)
library(ggplot2)
library(zoo)
library(reshape2)
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

