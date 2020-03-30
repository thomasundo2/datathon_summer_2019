library(ggplot2)
library(tidyverse)

#Load data
spendratio <- read.csv("~/Documents/datathon_summer_2019/jiying.csv")
spendratio <- spendratio[-1]

#Rename cols
names(spendratio) <- c("Year", "ProvinceCode", "GDP", "Pop", "CPI", "PrivateSpending_percap", "PublicSpending_percap")

#Add private:public spending (per cap) col
spendratio$privpub_ratio <- spendratio$PrivateSpending_percap/spendratio$PublicSpending_percap

hist(spendratio$privpub_ratio)

spendratio %>% ggplot() +
  geom_line(aes(x = Year, y = privpub_ratio, color = ProvinceCode))


poorregions <- c("N.L.", "P.E.I.", "N.S.", "N.B.", "Nun.", "Man.", "Sask.", "Y.T.", "N.W.T.")
richregions <- c("Que.", "Ont.", "Alta.", "B.C.")

#Clean up the spaces at ends of province code
spendratio$ProvinceCode <- trimws(spendratio$ProvinceCode, which = "both", whitespace = "[ \t\r\n]")


#Create data frame for results
Year <- c(2010,2011,2012,2013,2014,2015,2016,2017)


#Wilcoxon rank sum tests
a <- wilcox.test(spendratio[which(spendratio$Year == 2010 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2010 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

b <- wilcox.test(spendratio[which(spendratio$Year == 2011 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2011 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

c <- wilcox.test(spendratio[which(spendratio$Year == 2012 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2012 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

d <- wilcox.test(spendratio[which(spendratio$Year == 2013 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2013 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

e <- wilcox.test(spendratio[which(spendratio$Year == 2014 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2014 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

f <- wilcox.test(spendratio[which(spendratio$Year == 2015 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2015 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

g <- wilcox.test(spendratio[which(spendratio$Year == 2016 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2016 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

h <- wilcox.test(spendratio[which(spendratio$Year == 2017 & spendratio$ProvinceCode %in% poorregions),"privpub_ratio"], 
            spendratio[which(spendratio$Year == 2017 & spendratio$ProvinceCode %in% richregions),"privpub_ratio"])

pval_unadj <- c(a$p.value, b$p.value, c$p.value, d$p.value, e$p.value, f$p.value, g$p.value, h$p.value)
wrst_results <- data.frame(Year, pval_unadj)
wrst_results$pval_adj <- 0.1/8
