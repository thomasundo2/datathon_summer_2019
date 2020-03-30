library(tidyverse)
library(ggplot2)
library(RColorBrewer)

#Load data
drug_spending <- read.csv("~/Documents/datathon_summer_2019/data/canada_data/drug_spending.csv")
expenditure_by_type <- read.csv("~/Documents/datathon_summer_2019/data/canada_data/expenditure_by_type.csv")
provincial<- read.csv("~/Documents/datathon_summer_2019/data/canada_data/provincial_level.csv")

###DATA WRANGLING
#GDP tracking per region over years
gdp <- provincial[which(grepl('Appendix: A.1:',provincial$Category) & provincial$Year>1980),]
gdp_narrow <- gdp %>% gather(key = "Province_Territory", value = "GDP", 4:16)

#CPI tracking per region over the years
cpi <- provincial[which(grepl('Appendix: B.2:',provincial$Category) & provincial$Year>1980),]
cpi_narrow <- cpi %>% gather(key = "Province_Territory", value = "CPI", 4:16) 


###VIZ
cbPalette <- c("#0072B2", "#D55E00", "#E69F00", "#77B4E9","#CC79A7","#000000", "#019E73", "#999999","#F0E447", "#E69F00", "#56B4E9", "#D55E00","#009E73", "#F0E442", "#0072B2",  "#CC79A7")

#GDP
gdp_narrow %>% ggplot() +
  geom_line(aes(x = Year, y = GDP, group = Province_Territory, color = Province_Territory)) +
  labs(title = "GDP per region, 1981-2018") +
  scale_colour_manual(values=cbPalette)

#CPI
cpi_narrow %>% ggplot() +
  geom_line(aes(x = Year, y = CPI, group = Province_Territory, color = Province_Territory)) +
  labs(title = "CPI (healthcare component) per region, 1981-2018")


