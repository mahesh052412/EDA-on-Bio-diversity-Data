setwd("C:/Users/HP/OneDrive/Desktop/Dan brown/TO-DO/EDA-on-Bio-diversity-Data")
proj_assg <- read.csv("proportional_species_richness_V3.csv")
head(proj_assg)

library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(tidyverse)

proj_data <- select(proj_assg,c(2:5,7,10,11,13:17))
head(proj_data)

proj_data$dominantLandClass <- as.factor(proj_data$dominantLandClass)
proj_data$period <- as.factor(proj_data$period)

proj_data$ecostatusof7 <- rowMeans(proj_data[1:7])
head(proj_data)

#DATA_EXPLORATION:
  
x <- ggplot(proj_data, aes(x=ecologicalStatus, fill=period)) +
       geom_histogram() +
       xlim(0.25, 1)
y <- ggplot(proj_data, aes(x=ecostatusof7, fill=period)) +
       geom_histogram() +
      xlim(0.25, 1.5)
grid.arrange(x,y,ncol=2)

a <- proj_data %>%
  ggplot(aes(x=Easting, y=Bees)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(vars(period), ncol = 2, scales = "free")

b <- proj_data %>%
  ggplot(aes(x=Northing, y=Bees)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(vars(period), ncol = 2, scales = "free")
grid.arrange(a,b,ncol=2)


#Hypothesis Testing:
#1
#For all landclass in scotland, has shown increase in mean of ecological score of 7species:

scotland_data <- filter(proj_data,str_detect(dominantLandClass,"s"))

dff_in_eco_score_scotland <- scotland_data%>%group_by(dominantLandClass,period)%>%
  summarise(meanof_7=mean(ecostatusof7), .groups = 'drop')%>%
  pivot_wider(names_from = period, values_from = meanof_7,values_fill = 0 )%>%
  mutate(period_dff=Y00-Y70)

t.test(dff_in_eco_score_scotland$period_dff,alternative = "greater", mu=0, conf.level = 0.95 )

#2
#The mean of ecostatus7 for each dominatlandclass has increased over time time from y70 to Y00:

best_eco_Y00 <- proj_data%>%group_by(dominantLandClass,period)%>%
  summarise(meanofall_eco=mean(ecostatusof7), .groups = 'drop')%>%
  pivot_wider(names_from = period, values_from = meanofall_eco,values_fill = 0 )%>%
  mutate(period_dff=Y00-Y70)

t.test(best_eco_Y00$period_dff, alternative = "greater", mu=0, conf.level = 0.99 )


#Simple linear regression
proj_Y70 <- proj_data %>% filter(period=="Y70")
proj_Y00 <- proj_data %>% filter(period=="Y00")

#filtering period for Y70:
reg_proj_Y70 <- lm(proj_Y70$ecologicalStatus~proj_Y70$ecostatusof7)
summary(reg_proj_Y70)

#dataframe for predicted and ecologicalstatus values for Y70:
lm_Y70 <- data.frame(x=fitted(reg_proj_Y70),y=proj_Y70$ecologicalStatus)
#ploting the dataframe:
ggplot(lm_Y70,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method=lm,se=F)

#filtering period for Y00:
reg_proj_Y00 <- lm(proj_Y00$ecologicalStatus~proj_Y00$ecostatusof7)
summary(reg_proj_Y00)

#dataframe for predicted and ecologicalstatus values for Y00:
lm_Y00 <- data.frame(x=fitted(reg_proj_Y00),y=proj_Y00$ecologicalStatus)
#ploting the dataframe:
ggplot(lm_Y00,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method=lm,se=F)

#plotting graph for proj_Y70:
abline(reg_proj_Y70,col="green")
plot(jitter(fitted(reg_proj_Y70)),residuals(reg_proj_Y70),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(reg_proj_Y70$residuals)
qqline(reg_proj_Y70$residuals,col="red")

#plotting graph for proj_Y00:
abline(reg_proj_Y00,col="green")
plot(jitter(fitted(reg_proj_Y00)),residuals(reg_proj_Y00),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(reg_proj_Y00$residuals)
qqline(reg_proj_Y00$residuals,col="red")



