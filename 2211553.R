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
#1
x <- ggplot(proj_data, aes(x=ecologicalStatus, fill=period)) +
       geom_histogram() +
       xlim(0.25, 1)
y <- ggplot(proj_data, aes(x=ecostatusof7, fill=period)) +
       geom_histogram() +
      xlim(0.25, 1.5)
grid.arrange(x,y,ncol=2)

#2
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
scotland_data <- filter(proj_data,str_detect(dominantLandClass,"s"))

dff_in_eco_score_scotland <- scotland_data%>%group_by(dominantLandClass,period)%>%
  summarise(meanof_7=mean(ecostatusof7), .groups = 'drop')%>%
  pivot_wider(names_from = period, values_from = meanof_7,values_fill = 0 )%>%
  mutate(period_dff=Y00-Y70)

t.test(dff_in_eco_score_scotland$period_dff,alternative = "greater", mu=0, conf.level = 0.95 )

#2
best_eco_Y00 <- proj_data%>%group_by(dominantLandClass,period)%>%
  summarise(meanofall_eco=mean(ecostatusof7), .groups = 'drop')%>%
  pivot_wider(names_from = period, values_from = meanofall_eco,values_fill = 0 )%>%
  mutate(period_dff=Y00-Y70)

t.test(best_eco_Y00$period_dff, alternative = "greater", mu=0, conf.level = 0.99 )


#Simple linear regression
proj_Y70 <- proj_data %>% filter(period=="Y70")
proj_Y00 <- proj_data %>% filter(period=="Y00")

reg_proj_Y70 <- lm(proj_Y70$ecologicalStatus~proj_Y70$ecostatusof7)
summary(reg_proj_Y70)

#dataframe for values for Y70:
lm_Y70 <- data.frame(x=fitted(reg_proj_Y70),y=proj_Y70$ecologicalStatus)
ggplot(lm_Y70,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method=lm,se=F)

reg_proj_Y00 <- lm(proj_Y00$ecologicalStatus~proj_Y00$ecostatusof7)
summary(reg_proj_Y00)

#dataframe for values Y00:
lm_Y00 <- data.frame(x=fitted(reg_proj_Y00),y=proj_Y00$ecologicalStatus)

ggplot(lm_Y00,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method=lm,se=F)

#plotting graph for proj_Y70:
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

#multiple linear regression
#select the four coloum apart from given data:
proj_assg_4 <- proj_assg%>%select(c(6,8,9,12))

#find and add mean of all 4 coloum to proj_data:
proj_data <- mutate(proj_data,meanof4=rowMeans(proj_assg_4[1:4]))

#sampling for training and testing:
trainig_sample <- sample(1:nrow(proj_data),0.8*nrow(proj_data))
training_data <- proj_data[trainig_sample,]
testing_data <- proj_data[-trainig_sample,]

#Running multiple linear regression model with all 7 species against BD4:
mlr_proj1 <- lm(meanof4~.,
                data=training_data[c(1:7,14)],
                y=T)
summary(mlr_proj1)

#predicting for remaining 20%:
mlr_proj1_test <- predict(mlr_proj1,testing_data)
cor(mlr_proj1_test,testing_data$meanof4)
df_for_mlr_proj1_test <- data.frame(predicted = mlr_proj1_test,observed = testing_data$meanof4)

#excluding certain species:
#1 #3 & 6 #0.6883872
species_proj1 <- lm(meanof4~.,
                data=training_data[c(1,2,4,5,7,14)],
                y=T)
summary(species_proj1)
species_proj1_test <- predict(species_proj1,testing_data)
cor(species_proj1_test,testing_data$meanof4)
df_for_proj1 <- data.frame(predicted = species_proj1_test,observed = testing_data$meanof4)


#2 #6 #0.6642133
species_proj2 <- lm(meanof4~.,
                    data=training_data[c(1,2,3,4,5,14)],
                    y=T)
summary(species_proj2)
species_proj2_test <- predict(species_proj2,testing_data)
cor(species_proj2_test,testing_data$meanof4)
df_for_proj2 <- data.frame(predicted = species_proj2_test,observed = testing_data$meanof4)


#3 #3 #0.6959937
species_proj3 <- lm(meanof4~.,
                    data=training_data[c(1,2,4,5,6,7,14)],
                    y=T)
summary(species_proj3)
species_proj3_test <- predict(species_proj3,testing_data)
cor(species_proj3_test,testing_data$meanof4)
df_for_proj3 <- data.frame(predicted = species_proj3_test,observed = testing_data$meanof4) 


#4 #4 & 3 #0.6464906
species_proj4 <- lm(meanof4~.,
                    data=training_data[c(1,2,5,6,7,14)],
                    y=T)
summary(species_proj4)
species_proj4_test <- predict(species_proj4,testing_data)
cor(species_proj4_test,testing_data$meanof4)
df_for_proj4 <- data.frame(predicted = species_proj4_test,observed = testing_data$meanof4) 

#plotting graph 
par(mfrow=c(1,1))
p_1 <- ggplot(df_for_proj1,aes(x=predicted,y=observed))+
  geom_point()+
  geom_smooth(method = lm, se=F)+
  labs(x = "Predicted Values", y = "Observed Values")
p_2 <- ggplot(df_for_proj2,aes(x=predicted,y=observed))+
  geom_point()+
  geom_smooth(method = lm, se=F)+
  labs(x = "Predicted Values", y = "Observed Values")
p_3 <- ggplot(df_for_proj3,aes(x=predicted,y=observed))+
  geom_point()+
  geom_smooth(method = lm, se=F)+
  labs(x = "Predicted Values", y = "Observed Values")
p_4 <- ggplot(df_for_proj4,aes(x=predicted,y=observed))+
  geom_point()+
  geom_smooth(method = lm, se=F)+
  labs(x = "Predicted Values", y = "Observed Values")
p_5 <- ggplot(df_for_mlr_proj1_test,aes(x=predicted,y=observed))+
  geom_point()+
  geom_smooth(method = lm, se=F)+
  labs(x = "Predicted Values", y = "Observed Values")


grid.arrange(p_1, p_2, p_3, p_4, p_5, ncol=2)

AIC(mlr_proj1,species_proj1,species_proj2,species_proj3,species_proj4)

#open analysis:
cor(proj_data[1:7], proj_data$ecostatusof7)

cor_species <- proj_data %>% mutate(total=rowSums(proj_data[1:7],na.rm=T)) %>% 
  select(c("Bees","Butterflies","Hoverflies","Macromoths","ecostatusof7","period","dominantLandClass","total"))
head(cor_species)

cor_species <- cor_species %>% group_by(dominantLandClass,period) %>% 
  arrange(desc(ecostatusof7),.by_group = T) %>% top_n(5, wt = ecostatusof7) %>%
  mutate(count_bees=(Bees/total)*100,
         count_butterflies=(Butterflies/total)*100,
         count_hoverflies=(Hoverflies/total)*100,
         count_macromoths=(Macromoths/total)*100)
head(cor_species)

cor_species <- cor_species %>% select(c("period","dominantLandClass","count_bees","count_butterflies",
                         "count_hoverflies","count_macromoths")) %>% 
  pivot_longer(cols = c("count_bees","count_butterflies",
                        "count_hoverflies","count_macromoths"),
               names_to = "species",values_to = "count_percentage" )

#Engaland  
cor_species %>% filter(str_detect(dominantLandClass,"e")) %>%
ggplot(aes(x=species,y=count_percentage,fill=period)) +
  geom_bar(stat = "identity",position = "dodge")+
  facet_wrap(vars(dominantLandClass),scales = "free_x") +
  coord_flip()

#Scotland
cor_species %>% filter(str_detect(dominantLandClass,"s")) %>%
  ggplot(aes(x=species,y=count_percentage,fill=period)) +
  geom_bar(stat = "identity",position = "dodge")+
  facet_wrap(vars(dominantLandClass),scales = "free_x") +
  coord_flip()

#Wales
cor_species %>% filter(str_detect(dominantLandClass,"w")) %>%
  ggplot(aes(x=species,y=count_percentage,fill=period)) +
  geom_bar(stat = "identity",position = "dodge")+
  facet_wrap(vars(dominantLandClass),scales = "free_x") +
  coord_flip()
