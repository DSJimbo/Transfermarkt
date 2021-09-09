#libs
library(dplyr)
library(dslabs)
library(tidyverse)
library(ggplot2)
#read csv
data <- read.csv(file="data.csv",header = TRUE ,sep = ";",na.strings=c("","NA"))
#reducing data
data_small <- filter(data,  Restvertrag.alter.Verein.in.Jahren == 1)
data_small <- select(data_small, Spieler, datum_transfer, Restvertrag.in.Monaten...12, abloese, marktwert_zeitpunkt)

#transformations
data_small <- data_small %>% mutate(datum_transfer = as.Date(datum_transfer,format = "%m/%d/%y"))
data_small <- data_small %>% mutate(abloese = str_replace(data_small$abloese, "ablösefrei", "0"))
data_small <- data_small %>% mutate(abloese = na_if(abloese,"-"))
data_small <- data_small %>% mutate(abloese = na_if(abloese,"\\?"))
data_small <- data_small %>% mutate(abloese = as.numeric(str_replace_all(data_small$abloese, "\\.", "")))

data_small <- data_small %>% mutate(marktwert_zeitpunkt = na_if(marktwert_zeitpunkt,"-"))
data_small <- data_small %>% mutate(marktwert_zeitpunkt = na_if(marktwert_zeitpunkt,"\\?"))
data_small <- data_small %>% mutate(marktwert_zeitpunkt = as.numeric(str_replace_all(marktwert_zeitpunkt, "\\.", "")))

#calculation
data_small <- data_small %>% mutate(factor = marktwert_zeitpunkt/abloese)
data_small[sapply(data_small, is.infinite)] <- NA

#reducing Data 2: filter ablose > 0 & Martkwert > 1000000 & Date > 2009
data_small <- filter(data_small, datum_transfer > as.Date("2009-12-31"))
data_small <- filter(data_small, abloese > 0)
over_a_million <- filter(data_small, marktwert_zeitpunkt> 1000000)

over_a_million <- over_a_million %>% mutate(year = format(over_a_million$datum_transfer,format = "%Y"))
over_a_million
ggplot(over_a_million, aes(x=year, y=factor)) + geom_boxplot() + ylim(0,5)+ ggtitle("MW > 1.000.000 und MW/ablöse Faktor")

over_5_million <- filter(over_a_million, abloese > 5000000)
ggplot(over_5_million, aes(x=year, y=factor)) + geom_boxplot() + ylim(0,5)+ggtitle("MW > 5.000.000 und MW/ablöse Faktor")

over_10_million <- filter(over_a_million, abloese > 10000000)
nrow(over_10_million)
ggplot(over_10_million, aes(x=year, y = factor)) + geom_boxplot() + ylim(0,5)+ggtitle("MW > 10.000.000 und MW/ablöse Faktor")

over_15_million <- filter(over_a_million, abloese > 15000000)
nrow(over_15_million)
ggplot(over_15_million, aes(x=year, y = factor)) + geom_boxplot() + ylim(0,5)+ggtitle("MW > 15.000.000 und MW/ablöse Faktor")

over_20_million <- filter(over_a_million, abloese > 20000000)
nrow(over_20_million)
ggplot(over_20_million, aes(x=year, y = factor)) + geom_boxplot() + ylim(0,5)+ggtitle("MW > 20.000.000 und MW/ablöse Faktor")

over_25_million <- filter(over_a_million, abloese > 25000000)
nrow(over_25_million)
ggplot(over_25_million, aes(x=year, y = factor)) + geom_boxplot() + ylim(0,5)+ggtitle("MW > 20.000.000 und MW/ablöse Faktor")
str(over_a_million)

mw <- filter(over_a_million, marktwert_zeitpunkt > 30000000)
arrange(mw, factor)

ggplot(mw, aes(x=year, y = factor)) + geom_boxplot() + ylim(0,5)+ggtitle("MW > 20.000.000 und MW/ablöse Faktor")
