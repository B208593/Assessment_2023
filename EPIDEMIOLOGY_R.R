library(tidyverse)
library(ggplot2)
library(BSDA)
library(janitor)
library(descriptr)

Question 1
#create subset of data
Demographic<-assessment_dataset %>%
  select(-ID)

#summary statistics
Demographic %>%
  group_by(trial_arm) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            median_age = median(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            iqr_age = IQR(age, na.rm = TRUE))

#create histogram
Demographic %>%
  ggplot(aes(x = age)) + 
  geom_histogram(col="black",fill="lightblue") +
  labs(title="a, Histogram for age distribution ") +
  theme_bw() +  
  facet_wrap(~trial_arm) +
  theme(panel.grid=element_blank())

#boxplot
Demographic %>%
  ggplot(., aes(x = trial_arm, y = age)) +
  geom_boxplot(col="black",fill="lightblue") +
  labs(title="b,Boxplot for age distribution ")+
  theme_bw() +                      
  theme(panel.grid=element_blank())

#QQ plot
Demographic %>%
  ggplot(aes(sample=age)) +
  stat_qq() +
  stat_qq_line(color=2) +
  labs(title="Q-Q Plot for age distribution") +    
  theme_bw() +                        
  theme(panel.grid=element_blank())  
#Shapiro Wilk
Demographic %>%
  pull(age) %>%
  shapiro.test()

#Kolmorov
Demographic %>%
  pull(age) %>%
  ks.test(.,"pnorm",mean=mean(.),sd=sd(.))

#Bartlet
bartlett.test(age~trial_arm, data = Demographic)

#statistics, two sample t-test
t.test(age ~ trial_arm, data = Demographic)

#age to categorical variable with 5 levels
newsubsetagegroup <- assessment_dataset %>%
  mutate(age_category = case_when(age <= 35 ~ '18-35',
                                  age > 35  & age <= 45 ~ '35-45',
                                  age > 45  & age <= 60 ~ '45-60',
                                  age > 60  & age <= 75 ~ '60-75',
                                  age > 75 ~ '75<'))%>%
  mutate(age_category = as_factor(age_category))


#order changed
newsubsetagegroup$age_category = factor(newsubsetagegroup$age_category,
                                        levels=c("18-35", "35-45", "45-60", "60-75", "75<"))

#check levels
levels(newsubsetagegroup$age_category)

#how many people in each group
table(newsubsetagegroup$age_category)

#Categorical variables
#rename variables
Demographic$sex <- recode(Demographic$sex, F = 'Female', 
                          M = 'Male')

#bar charts
Demographic %>%
  ggplot() +
  geom_bar(aes(x = trial_arm, fill = sex), position = "dodge")+
  labs(title = "a, Distribution of age in the control and intervention group",
       x = "Trial arm",
       y = "Number of people")
Demographic %>%
  ggplot() +
  geom_bar(aes(x = trial_arm, fill = location), position = "dodge")+
  labs(title = "Location status in the control and intervention group",
       x = "Trial arm",
       y = "Number of people")

Demographic %>%
  ggplot() +
  geom_bar(aes(x = trial_arm, fill = baseline_exercise), position = "dodge") +
  labs(title = "b, Baseline excercise in control and intervention group",
       x = "Control or intervention group",
       y = "Number of people")

#table for categorical variables
Demographic %>% 
  tabyl(trial_arm, sex) %>% 
  knitr::kable()

contingency_table1 <- as.data.frame.matrix(table(Demographic$trial_arm, Demographic$sex))

Demographic %>% 
  tabyl(trial_arm, baseline_exercise) %>% 
  knitr::kable()

contingency_table2 <- as.data.frame.matrix(table(Demographic$trial_arm, Demographic$baseline_exercise))

Demographic %>% 
  tabyl(trial_arm, location) %>% 
  knitr::kable()

contingency_table3 <- as.data.frame.matrix(table(Demographic$trial_arm, Demographic$location))

#chi square test for all
chisq.test(contingency_table1)
chisq.test(contingency_table2)
chisq.test(contingency_table3)

#compare each group in location
contingency_table1 <- as.data.frame.matrix(table(Demographic$trial_arm, Demographic$location))

chisq.test(contingency_table1[, c(1, 2)], correct = FALSE)
chisq.test(contingency_table1[, c(1, 3)], correct = FALSE)
chisq.test(contingency_table1[, c(2, 3)], correct = FALSE)

#print tables
install.packages("gridExtra") 
library("gridExtra")

png("location.png")
p<-tableGrob(contingency_table1)
grid.arrange(p)
dev.off()



Question 2

#creating subset of data
datasetQ2 <- assessment_dataset %>%
  select(sex, age, baseline_exercise)

#rename gender
datasetQ2$sex <- recode(datasetQ2$sex, F = 'Female', 
                        M = 'Male')

#summary statistics
age_by_gender <- datasetQ2 %>%
  group_by(sex) %>%
  summarise(mean=round(mean(age, na.rm=T), 2),
            sd = round(sd(age, na.rm=T), 2),
            var = round(var(age, na.rm=T), 2))

# boxplot 
datasetQ2 %>% 
  ggplot(aes(x = sex, y = age, fill = sex)) +
  geom_boxplot() +
  labs(title = "Figure 1: Boxplot of age by gender")

#bar chart
datasetQ2 %>%
  ggplot() +
  geom_bar(aes(x = sex, fill = baseline_exercise), position = "dodge")+
  labs(title = "Excercise by gender",
       x = "Gender",
       y = "Number of people")

#contingency table
contingency_table4 <- as.data.frame.matrix(table(datasetQ2$sex, Demographic$baseline_exercise))


Question 3

#transform age to categorical variable with 5 levels
newsubsetagegroup <- Demographic %>%
  mutate(age_category = case_when(age <= 35 ~ '18-35',
                                  age > 35  & age <= 45 ~ '35-45',
                                  age > 45  & age <= 60 ~ '45-60',
                                  age > 60  & age <= 75 ~ '60-75',
                                  age > 75 ~ '75<'))%>%
  mutate(agecat = as_factor(age_category))

#order changed
newsubsetagegroup$ age_category = factor(newsubsetagegroup$ age_category,
                                         levels=c("18-35", "35-45", "45-60", "60-75", "75<"))

#check levels
levels(newsubsetagegroup$ age_category)

#check how many people in each group
table(newsubsetagegroup$ age_category)

#bar chart for age groups
newsubsetagegroup %>%
  ggplot() +
  geom_bar(aes(x = baseline_exercise, fill = age_category), position = "dodge")+
  labs(title = "Distribution of baseline exercise by age categories",
       x = "Baseline exercise",
       y = "Number of people")

#contingency table 
contingency_table6 <- as.data.frame.matrix(table(newsubsetagegroup$baseline_exercise, newsubsetagegroup$ age_category))

#print table
png("agecat.png")
p<-tableGrob(contingency_table1)
grid.arrange(p)
dev.off()

#Chi-square test
chisq.test(contingency_table6)






#Step count histogram
assessment_dataset %>%
  ggplot(aes(x = step_count)) + 
  geom_histogram(col="black",fill="lightblue",bins = 10 ) +
  labs(title="Histogram for step count distribution ")+
  theme_bw() +  
  theme(panel.grid=element_blank())

#boxplot for stepcount
assessment_dataset %>%
  ggplot(., aes(y = step_count)) +
  geom_boxplot(col="black",fill="lightblue") +
  labs(title="Boxplot for distribution of step count ")+
  theme_bw() +                       
  theme(panel.grid=element_blank())  

#QQ plot for step count
assessment_dataset %>%
  ggplot(aes(sample=step_count)) +
  stat_qq() +
  stat_qq_line(color=2) +
  labs(title="Q-Q Plot for distribution of step count") +    
  theme_bw() +                       
  theme(panel.grid=element_blank())   

#Shapiro test for norm
assessment_dataset %>%
  pull(step_count) %>%
  shapiro.test()

# Create the scatterplot, and this time add the regression line to it
assessment_dataset %>%
  ggplot(aes(x = age, y = step_count)) + 
  geom_point() + 
  xlab("Age") + 
  ylab("Step count") +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  labs(title="Scatterplot for step count and age ")+
  theme_bw() +
  theme(panel.grid=element_blank())

#Pearson
cor.test(assessment_dataset$age,assessment_dataset$step_count)

#Spearman
cor.test(assessment_dataset$age,assessment_dataset$step_count, method="spearman")


Question 4

#create subset of data
Fitness<-assessment_dataset %>%
  select(trial_arm,step_count,baseline_exercise)

#boxplot
Fitness %>% 
  ggplot(aes(x = trial_arm, y = step_count, fill = trial_arm)) +
  geom_boxplot() +
  labs(title = "b, Boxplot of step count distribution in the two groups")

#histogram
Fitness %>%
  ggplot(aes(x = step_count)) + 
  geom_histogram(col="black",fill="lightblue") +
  labs(title = "a, Histogram of step count distribution in the two groups") +
  theme_bw() +  
  facet_wrap(~trial_arm) +
  theme(panel.grid=element_blank())

#Bartlett for equal variances
bartlett.test(Fitness$step_count~Fitness$trial_arm)

#F-test for homogeneity
Fitness %>%
  var.test(step_count ~ trial_arm, ., alternative = "two.sided")
#t-test
t.test(step_count ~ trial_arm, data = Fitness)

