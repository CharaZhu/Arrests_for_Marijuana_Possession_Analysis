#' ---
#' title: 'Arrests for Marijuana Possession Analysis Report'
#' author: ' ' 
#' date: 'Team 1'
#' output: pdf_document
#' ---
knitr::opts_chunk$set(comment=" ", error=TRUE, echo=FALSE, message=FALSE, warning=FALSE)
#+ setup, include=FALSE
 
##' # \textcolor{red}{Background} ----
##' 
##' A Toronto Star analysis of crime data shows that blacks arrested are treated 
##' more harshly than whites. Blacks have higher rate to be taken to police 
##' stations and held overnight than whites when they are facing the same charge.
##' This reveals the social phenomenon that race matters in Canadian society and 
##' people are unfairly targeted by police. Other than race, there are definitely
##' more factors will lead to discrimination. The dataset Arrests collected by 
##' Michael Friendly focus on police treatment of individuals arrested for marijuana 
##' possession in Toronto. Our team aim to develop a model that can reveals 
##' possible patterns of discrimination by using the dataset Arrests. \newline
 
##'
##' # \textcolor{red}{Data Visualization} ----
##' 
library(car)
library(spida2)

#’ summary(Arrests)
library(ggplot2)
#’ xqplot(Arrests) 
 
#' There are 5226 observations with 8 variables, and dataset not include any missing
#' value. \newline
#' 
#' \textbf{Summary of Arrests data}  
variables <- c('released','colour','year','age','sex','employed','citizen',
               'checks')
description <- c('Whether or not the arrestee was released with a summons.',
                 'Race of the arrestees.',
                 'The year that people being arrested.',
                 'The age of arrestee.',
                 'The gender of arrestee.', 
                 'Employment status of arrestee.', 
                 'Whether or not the arrestee is a citizen.',
                 'Total number of arrestee’s previous arrests, convictions, etc.')   
values <- c('No, Yes','Black, White','1997 - 2002','12 - 66','Female, Male',
            'No, Yes','No, Yes','0 - 6')
Arrests_info <- data.frame(variables,description,values)
names(Arrests_info) <- c('VARIABLES','DESCRIPTION','VALUES')

 
library(dplyr)
library(magrittr) 
library(ggpubr)
(Arrests_info <- ggtexttable(Arrests_info, rows = NULL, theme = ttheme("mOrange")))
 


# \textbf{Code to create better table}  
# Arrests_info <- Arrests_info %>% \newline
#   tab_header(title = "Summary of Arrests data") %>% \newline
#   tab_source_note(source_note = "Source: Personal communication from Michael Friendly, York University.") 
#' \newpage


#' \textbf{Correlation plot} \newline
#' 
Arrests_num<- data.frame(data.matrix(Arrests)) 
library(corrplot)
Arrests_corr <- cor(Arrests_num)
corrplot.mixed(Arrests_corr, 
               lower = 'number',
               upper = 'ellipse', 
               tl.col = 'red',
               tl.cex = .6, 
               cl.cex = .6, 
               number.cex = .8)

#' Using correlation plot rather than scatter plot to visualize the relationship 
#' between paired variables, since scatterplotMatrix does not work well for 
#' categorical data. Noted: \newline
#' 
#'   * Need to convert categorical data to numerical data firstly. \newline
#'   * Choose significance level equal to 0.05, correlation between two variables 
#'   significant if it greater than the preassigned significance level. \newline
#'   * The correlation between released and year is 0.03 doesn’t imply there’s a 
#'   positive relationship between them, since year should be a factor instead of a 
#'   continuous variable. \newline
#' 
#' From the correlation plot, released has positive association with colour, 
#' employed and citizen, and has negative association with checks. Which means 
#' arrestees are more likely to be released if they are white, employed, 
#' citizen or less criminal record. \newline
#' 
#' Expecting to build a model able to reflect this relationship, 
#' if model include them as predictor variables. \newpage

 
#' \textbf{Visualizing data} 
#' 
plot1 <- ggplot(Arrests, aes(colour, released,color=colour)) +
  geom_jitter(width = 0.4, height = 0.4,size=0.3)+
  ggtitle("POLICE TREATMENT for black v.s. white", subtitle="(count)") + 
  theme(legend.position = c(-10, 1),legend.justification = c(0, 1),
        title =element_text(size=6, face='bold'),)
 
plot2 <- ggplot(Arrests, aes(x= released,  group=colour)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.. ), stat= "count", vjust = -.5) + 
  facet_grid(~colour) +  
  ggtitle("POLICE TREATMENT for black v.s. white", subtitle="(percentage)") + 
  ylim(c(0,4000))+ 
  theme_classic()+ 
  scale_fill_discrete(name=" ",labels=c("non-released", "released")) +
  xlab(" ")+
  theme(legend.position = c(0.16, 0.66),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6),
        title =element_text(size=7, face='bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=6))

Arrests_white <- Arrests[Arrests$colour=='White',]
Arrests_black <- Arrests[Arrests$colour=='Black',]
 
colour <- rep(c('black','white'),c(6,6))
year <- rep(c('1997','1998','1999','2000','2001','2002'),2)
percent <- c(sum(Arrests_black$released=='Yes' & Arrests_black$year==1997)/sum(Arrests_black$year==1997),
             sum(Arrests_black$released=='Yes' & Arrests_black$year==1998)/sum(Arrests_black$year==1998),
             sum(Arrests_black$released=='Yes' & Arrests_black$year==1999)/sum(Arrests_black$year==1999),
             sum(Arrests_black$released=='Yes' & Arrests_black$year==2000)/sum(Arrests_black$year==2000),
             sum(Arrests_black$released=='Yes' & Arrests_black$year==2001)/sum(Arrests_black$year==2001),
             sum(Arrests_black$released=='Yes' & Arrests_black$year==2002)/sum(Arrests_black$year==2002),
             
             sum(Arrests_white$released=='Yes' & Arrests_white$year==1997)/sum(Arrests_white$year==1997),
             sum(Arrests_white$released=='Yes' & Arrests_white$year==1998)/sum(Arrests_white$year==1998),
             sum(Arrests_white$released=='Yes' & Arrests_white$year==1999)/sum(Arrests_white$year==1999),
             sum(Arrests_white$released=='Yes' & Arrests_white$year==2000)/sum(Arrests_white$year==2000),
             sum(Arrests_white$released=='Yes' & Arrests_white$year==2001)/sum(Arrests_white$year==2001),
             sum(Arrests_white$released=='Yes' & Arrests_white$year==2002)/sum(Arrests_white$year==2002))

perent_released <- data.frame(colour, year, percent)
names(perent_released) <- c('COLOUR','YEAR','PERCENTAGE OF RELEASED')
 
plot3 <- ggplot(perent_released %>% 
         group_by(COLOUR) %>%
         ungroup(), 
       aes(x=year, y=percent, group=COLOUR, colour = COLOUR)) +
  geom_line()+
  ylim(c(0.4,1))+
  geom_point(size=0.5) + 
   ggtitle("POLICE TREATMENT for black v.s. white", subtitle="over years (percentage)") + 
  xlab("Year") +
  ylab("percentage of released")+
  theme_classic()+
  theme(legend.position = c(0.76, 0.36),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6),
        title =element_text(size=7, face='bold'), 
        axis.text.y = element_text(face="bold", size=6))
 
year_per <- c('1997','1998','1999','2000','2001','2002')
black_percent<- format(c(sum(Arrests_black$released=='Yes' & Arrests_black$year==1997)/sum(Arrests_black$year==1997),
                     sum(Arrests_black$released=='Yes' & Arrests_black$year==1998)/sum(Arrests_black$year==1998),
                     sum(Arrests_black$released=='Yes' & Arrests_black$year==1999)/sum(Arrests_black$year==1999),
                     sum(Arrests_black$released=='Yes' & Arrests_black$year==2000)/sum(Arrests_black$year==2000),
                     sum(Arrests_black$released=='Yes' & Arrests_black$year==2001)/sum(Arrests_black$year==2001),
                     sum(Arrests_black$released=='Yes' & Arrests_black$year==2002)/sum(Arrests_black$year==2002)),
                   digits=2)  
black_per <- paste(round(100*as.numeric(black_percent), 2), "%", sep="")

white_percent<-  format(c(sum(Arrests_white$released=='Yes' & Arrests_white$year==1997)/sum(Arrests_white$year==1997),
                      sum(Arrests_white$released=='Yes' & Arrests_white$year==1998)/sum(Arrests_white$year==1998),
                      sum(Arrests_white$released=='Yes' & Arrests_white$year==1999)/sum(Arrests_white$year==1999),
                      sum(Arrests_white$released=='Yes' & Arrests_white$year==2000)/sum(Arrests_white$year==2000),
                      sum(Arrests_white$released=='Yes' & Arrests_white$year==2001)/sum(Arrests_white$year==2001),
                      sum(Arrests_white$released=='Yes' & Arrests_white$year==2002)/sum(Arrests_white$year==2002)),
                    digits=2)
white_per <- paste(round(100*as.numeric(white_percent), 2), "%", sep="")


table_per <- data.frame(year_per, black_per, white_per)
names(table_per) <- c('YEAR','BLACK', 'WHITE')

table_per.table <- ggtexttable(table_per, rows = NULL, 
                                     theme = ttheme("mBlue",base_size=10))


library(gridExtra)
grid.arrange(plot1, plot2, plot3, table_per.table, heights=c(4,5)) 
 
sum(Arrests$colour=='White') / sum(Arrests$colour=='Black') 

#' Form the scatterplot, the number for white is greater than black for both released 
#' and unreleased groups. The scatterplot may indicate different groups are treated fairly,
#' which is not true. This might relate to the population distribution in Toronto 
#' and not indicate different groups are treated fairly.
#' Back to year 2006, the census from Statistical Canada shows the percentage for white
#' and black among total population are 52.5% and 8.4%. The number of white arrestees 
#' is three times the number of black arrestees roughly from year 1997-2002. \newline
#' 
#' Looking at the bar plot condition on race, 86% white have been released, 
#' but only 74% black have been released. Black arrestees are less likely to be 
#' released and it reveals a social phenomenon that black racial group is treated 
#' unfairly. \newline
#' 
#' For the line plot, the blue and red lines is percentage of released for white and 
#' black. The blue line is in the top of red line between year 1997-2002, the black 
#' arrestees are less likely to be released over years. 
#' The table in the right side record the specific number for the line plot. \newline


plot4 <- ggplot(Arrests, aes(x= released,  group=sex)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.. ), stat= "count", vjust = -.5, size=3) + 
  facet_grid(~sex) + 
  ggtitle("POLICE TREATMENT for female v.s. male") +
  scale_fill_discrete(name=" ",labels=c("non-released", "released")) +
  theme_classic()+
  xlab(" ")+
  ylim(c(0,4500))+
  theme(legend.position = c(0.16, 0.82),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6),
        title =element_text(size=7, face='bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=6))

plot5 <- ggplot(Arrests, aes(x= released,  group=employed)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.. ), stat= "count", vjust = -.5, size=3) + 
  facet_grid(~employed) + 
  ggtitle("POLICE TREATMENT for unemployed v.s. employed",
          subtitle="                                    EMPLOYED") +
  scale_fill_discrete(name=" ",labels=c("non-released", "released")) +
  theme_classic()+
  # xlab(" ")+
  ylim(c(0,4500))+
  theme(legend.position = c(0.16, 0.82),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6),
        title =element_text(size=7, face='bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=6))

plot6 <- ggplot(Arrests, aes(x= released,  group=citizen)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.. ), stat= "count", vjust = -.5, size=3) + 
  facet_grid(~citizen) + 
  xlab(" ")+
  ggtitle("POLICE TREATMENT for non-citizen v.s. citizen", 
          subtitle="                                    CITIZEN") +
  scale_fill_discrete(name=" ",labels=c("non-released", "released")) +
  theme_classic()+ 
  ylim(c(0,4500))+
  theme(legend.position = c(0.16, 0.82),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6),
        title =element_text(size=7, face='bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=6))

plot7 <- ggplot(Arrests, aes(x= released,  group=checks)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.. ), stat= "count", vjust = -.5, size=1.3) + 
  facet_grid(~checks) + 
  ggtitle("POLICE TREATMENT for check 0-6", 
          subtitle="                                     CHECK") +
  scale_fill_discrete(name=" ",labels=c("non-released", "released")) +
  theme_classic()+
  xlab(" ")+
  ylim(c(0,1800))+
  theme(legend.position = c(0.76, 0.66),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6),
        title =element_text(size=7, face='bold'),
        axis.text.x = element_text(face="bold", size=6),
        axis.text.y = element_text(face="bold", size=6))
 
grid.arrange(plot4, plot5, plot6, plot7) 

#' Among all graphs, there’s a sharp gender imbalance for both released and non-released
#' arrestees. We could consider combining male and female to avoid selection effect.
#' And compare the models before and after combine to see which model fit better.\newline
#' 

 
##'
##' # \textcolor{red}{Build models} ----
#' Taking employed, citizen and checks as mediator factors or confounding factors 
#' during the building model process, mainly consider those two situations in different c
#' ondition assumptions. \newline
#' 
Arrests$year <- as.factor(Arrests$year) 
library(dagitty)
library(ggdag)
 
causal1 <- dagitty('dag {
    "colour" [pos="0,1"] 
    "released" [pos="2,1"] 
    "employed" [pos="1,-2"]
    "citizen" [pos="1,-0.5"]
    "checks" [pos="1,2"]
    
    "colour" -> "employed" -> "released" 
    "colour" -> "citizen" -> "released" 
    "colour" -> "checks" -> "released" 
    "colour" -> "released"
}')
# plot(causal1) 


#' \textcolor{red}{\textbf{CASE 1: Consider employed, citizen and checks as mediate factors}} \newline
#' From the social and ethnic perspective, it is commonly believed that racism exists in hiring,
#'  immigration and judgement process. In other word, the color of skin will have an effect on 
#'  the possibility a person to be hired, to become a citizen and to be judged guilty. 
#'  In the meantime, employed, citizen and checks will influence the decision of police 
#'  whether or not the arrest should be released. For example, black people have a lower 
#'  chance to enter the job market due to racial discrimination, police might think they 
#'  have more crime motive since they do not have stable income, so they are less likely 
#'  to be released. \newline
#'  Under this circumstance, employed, citizen and checks are considering as mediate factors. \newpage
#'  
##' \textbf{1. without combine male and female} ----
model1_1 <- glm(released ~ colour* (age+ year+ sex), family = binomial, Arrests) 
model1_2 <- glm(released ~ colour* (age+ year)+ sex, family = binomial, Arrests)
result1_1 <- anova(model1_1, model1_2, test='LRT') 
 
model1_3 <- glm(released ~ colour* year+ age+ sex, family = binomial, Arrests)  
result1_2 <- anova(model1_2, model1_3, test='LRT') 
 
model1_4 <- glm(released ~colour* age+ year+ sex, family = binomial, Arrests)  
result1_4 <- anova(model1_2, model1_4, test='LRT') 

ID <- c('1','2','3','4')
MODEL<- c('model1_1','model1_2','model1_3','model1_4')
FORMULA <- c('released ~ colour* (age+ year+ sex)',
           'released ~ colour* (age+ year)+ sex',
           'released ~ colour* year+ age+ sex',
           'released ~ colour* age+ year+ sex')
AIC <- c(4665.5, 4665.4, 4680.9, 4671.7)
(case1 <- ggtexttable(data.frame(ID,MODEL, FORMULA, AIC), rows = NULL,
                             theme = ttheme("mOrange"))) 
  
#'
LRT <- c('1 v.s. 2', '2 v.s. 3', '2 v.s. 4')
P_VAL <- c('0.1694', '2.782e-05','0.00402') 
RESULT <- c('interaction between colour and sex is not significant',
            'interaction between colour and age is significant',
            'interaction between colour and year is significant')
CHOICE <- c('model1_2','model1_2','model1_2')

(result1 <- ggtexttable(data.frame(LRT, P_VAL, RESULT,CHOICE), rows = NULL, 
                             theme = ttheme("mOrange")))

# collineararity test
# vif(model1_2)
# summary(model1_2)
# The effect of colour: 
# $$1.493 - 0.0382*age + 0.663*year1998 + 0.296*year1999 + 0.458*year2000  -0.351*year2001  - 0.479*year2002 $$ \newline
#' 
#' All coefficients of model1_2 are significant except sex. Consider 
#' combining male and female, since the number of female is too little 
#' for both released and non-released arrestees. \newline
#' 
  
##' \textbf{2. combine male and female} ----
 
model2_1 <- glm(released ~ colour* (year+ age), family = binomial, Arrests)
model2_2 <- glm(released ~ colour* year+ age , family = binomial, Arrests)  
result2.1 <- anova(model2_1, model2_2, test='LRT') 

model2_3 <- glm(released ~ colour* age+ year, family = binomial, Arrests)  
result2.2 <- anova(model2_1, model2_3, test='LRT') 

ID <- c('1','2','3')
MODEL<- c('model2_1','model2_2','model2_3')
FORMULA <- c('released ~ colour* (year+ age)',
             'released ~ colour* year+ age',
             'released ~ colour* age+ year' )
AIC <- c(4648.9, 4663.9, 4663.7) 
(case2 <- ggtexttable(data.frame(ID,MODEL, FORMULA, AIC), rows = NULL,
                      theme = ttheme("mOrange"))) 

#'
LRT <- c('1 v.s. 2','1 v.s. 3')
P_VAL <- c('3.793e-05', '0.0001516' ) 
RESULT <- c('interaction between colour and age is significant',
            'interaction between colour and year is significant')
CHOICE <- c('model2_1','model2_1' )

(result2 <- ggtexttable(data.frame(LRT, P_VAL, RESULT,CHOICE), rows = NULL, 
                        theme = ttheme("mOrange")))

# collineararity test
# vif(model2_1)
# summary(model2_1)

#' For case 1, choose model2_1 according to smaller AIC criterion.\newline
#' 
#' The effect of colour: \newline
#' 1.495160 - 0.038089 $*$ age + 0.661839 $*$ year1998 + 0.298193 $*$ year1999 + 0.460083 $*$ year2000  -0.348932 $*$ year2001 
#' - 0.479314 $*$ year2002  \newpage


causal2 <- dagitty('dag {
    "colour" [pos="0,1"] 
    "released" [pos="2,1"] 
    "employed" [pos="1,-2"]
    "citizen" [pos="1,-0.5"]
    "checks" [pos="1,2"]
    
    "citizen" -> "released" 
    "citizen" -> "colour" 
    "employed" -> "released" 
    "employed" -> "colour" 
    "checks" -> "released" 
    "checks" -> "colour" 
}')
# plot(causal2)
#'
#' \textcolor{red}{\textbf{CASE 2: Consider employed, citizen and checks as confounding factors}} \newline
#'  Firstly, taking variable “employed” as an example to make a process of reasoning If a person 
#'  is arrested by the police, the confounding variable(employed) will have an impact on his skin
#'  color and release rate at the same time. If the person is not working, it will have an adverse 
#'  impact on his skin color and release rate. It sounds a little weird, because “employed” 
#'  affects the color of the person's skin. In this condition, “employed”, “citizen” and “checks” are 
#'  supposed to be the confounding variables. They have influence both on dependent and independent 
#'  variables. \newline
#'  
##' \textbf{1. without combine male and female} ----
model3_1 <- glm(released ~ colour* (age+ year+ sex+ employed +citizen+ checks), family = binomial, Arrests) 
model3_2 <- glm(released ~ colour* (age+ year)+ sex+ employed +citizen+ checks, family = binomial, Arrests)
result3_1 <- anova(model3_1, model3_2, test='LRT') 

model3_3 <- glm(released ~ colour* age+ year+ sex+ employed +citizen+ checks, family = binomial, Arrests)
result3_2 <- anova(model3_2, model3_3, test='LRT') 

model3_4 <- glm(released ~ colour* year+ age+ sex+ employed +citizen+ checks, family = binomial, Arrests)
result3_3 <- anova(model3_2, model3_4, test='LRT') 

 
ID <- c('1','2','3','4')
MODEL<- c('model3_1','model3_2','model3_3', 'model3_4')
FORMULA <- c('released ~ colour* (age+ year+ sex+ employed +citizen+ checks)',
             'released ~ colour* (age+ year)+ sex+ employed +citizen+ checks',
             'released ~ colour* age+ year+ sex+ employed +citizen+ checks',
             'released ~ colour* year+ age+ sex+ employed +citizen+ checks')
AIC <- c(4297.8, 4293.1,4304.8, 4304.9)
(case3 <- ggtexttable(data.frame(ID,MODEL, FORMULA, AIC), rows = NULL,
                      theme = ttheme("mOrange"))) 

#'
LRT <- c('1 v.s. 2', '2 v.s. 3', '2 v.s. 4')
P_VAL <- c(' 0.5124', '0.0005923','0.0001943') 
RESULT <- c('interaction between colour and sex/employed/citizen/checks are not significant',
            'interaction between colour and age is significant',
            'interaction between colour and year is significant')
CHOICE <- c('model3_2','model3_2','model3_2')

(result3 <- ggtexttable(data.frame(LRT, P_VAL, RESULT,CHOICE), rows = NULL, 
                        theme = ttheme("mOrange",base_size=9)))

# collineararity test
# vif(model3_2)
# summary(model3_2)  
#'
 
##' \textbf{2. combine male and female} ----
model4_1 <- glm(released ~ colour* (age+ year+ employed +citizen+ checks), family = binomial, Arrests) 
model4_2 <- glm(released ~ colour* (age+ year)+ employed +citizen+ checks, family = binomial, Arrests)
result4_1 <- anova(model4_1, model4_2, test='LRT') 

model4_3 <- glm(released ~ colour* age+ year+ employed +citizen+ checks, family = binomial, Arrests)
result4_2 <- anova(model4_2, model4_3, test='LRT') 

model4_4 <- glm(released ~ colour* year+ age+ employed +citizen+ checks, family = binomial, Arrests)
result4_3 <- anova(model4_2, model4_4, test='LRT') 


ID <- c('1','2','3','4')
MODEL<- c('model4_1','model4_2','model4_3','model4_4')
FORMULA <- c('released ~ colour* (age+ year+ employed +citizen+ checks)',
             'released ~ colour* (age+ year)+ employed +citizen+ checks',
             'released ~ colour* age+ year+ employed +citizen+ checks',
             'released ~ colour* year+ age+ employed +citizen+ checks')
AIC <- c(4294.7, 4291.1, 4302.8, 4302.9)
(case4 <- ggtexttable(data.frame(ID,MODEL, FORMULA, AIC), rows = NULL,
                      theme = ttheme("mOrange"))) 

#'
LRT <- c('1 v.s. 2', '2 v.s. 3', '2 v.s. 4')
P_VAL <- c('0.498', '0.0005917','0.0001942') 
RESULT <- c('interaction between colour and employed/citizen/checks are not significant',
            'interaction between colour and age is significant',
            'interaction between colour and year is significant')
CHOICE <- c('model4_2','model4_2','model4_2')

(result4 <- ggtexttable(data.frame(LRT, P_VAL, RESULT,CHOICE), rows = NULL, 
                        theme = ttheme("mOrange", base_size=9)))

# collineararity test
# vif(model4_2)
# summary(model4_2)

#' For case 2, choose model4_2 according to smaller AIC criterion. \newline
#' 
#' The effect of colour: \newline
#' 1.212517  - 0.037373 $*$ age + 0.651956 $*$ year1998 + 0.155950 $*$ year1999 + 0.295754 $*$ year2000  -0.380541 $*$ year2001  - 0.617318 $*$ year2002  \newline
#'
#'

##' # \textcolor{red}{Analysis} ----
#'
#' model4_2: released ~ colour* (age+ year)+ employed +citizen+ checks \newline
#' 
library(effects)
effect1 <- plot(effect("colour:year", model4_2), multiline=T, ylab="Probability(released)")
effect2 <- plot(effect("colour:age", model4_2), multiline=T, ylab="Probability(released)")
 
grid.arrange(effect1, effect2, ncol=2)

#' The colour-year plot shows that the probability of being released based on races as time changed. 
#' The blue and pink lines represent the probability of being released of black and white people, 
#' respectively. As shown from the graph, there is a huge gap between these two races in 1998, 
#' and it gradually becomes narrower over the course of 4 years. The two lines finally intersect 
#' in the second half of 2001, and suddenly the relationship between black and white races is 
#' reversed since then. It implies that starting from 1999, the discrimination towards the black 
#' race had gradually reduced to a point where the government viewed the both races more equally, 
#' likely due to various Right Acts regarding race discrimination being established at the time.\newline
#' 
#' Whereas the colour-age plot reveals that the police had a bias in releasing more white young 
#' arrestees than young black arrestees. However, there is an intersection of the two lines at 
#' around age 35. We can assume that there was a flip when the criminals are in their middle age. \newline
#' 

effect3 <- plot(effect("employed", model4_2), multiline=F, ylab="Probability(released)")
effect4 <- plot(effect("citizen", model4_2), multiline=F,ylab="Probability(released)")
effect5 <- plot(effect("checks", model4_2), multiline=F, ylab="Probability(released)")
 
grid.arrange(effect3, effect4, effect5, ncol=2)

#' The three effects plot demonstrate the three confounding factors. The employed effect plot 
#' represents how the Toronto police decided whether to release a criminal with and without 
#' employment. The result shows that employed criminal had a much higher chance to be released 
#' by the police, controlling for other predictors. Having a stable job could somehow imply 
#' social responsibilities and could possibily make more contributions to the society than 
#' unemployed, and this might be one of the reasons that the policed favored in releasing 
#' employed people.The last two plots examine the relationship between the probability of release
#' and the number of arrests one had previously experienced, and the relationship between the 
#' probability of release and the number of arrests one had previously experienced, respectively. 
#' Obviously, the Toronto police had a more positive attitudes towards Canadian citizens with less 
#' checks. \newpage
#' 

#' model2_1: released ~ colour* (year+ age) \newline
#' 
effect6 <- plot(effect("colour:year", model2_1), multiline = T, ylab = "Probability(released)")
effect7 <- plot(effect("colour:age", model2_1), multiline = F, ylab = "Probability(released)")
 
grid.arrange(effect6, effect7, ncol=2)

#' This second model generates a genuinely different results from the last one. After eliminating 
#' the mediating factors, the probability of releasing the whites remained generally stable at a 
#' much higher level than the black. However, one common characteristic that both models share is 
#' that more blacks are gradually getting higher probability to be released in a relatively rapid 
#' pace. Hence, without taking the factors of whether the person arrested is employed, a citizen 
#' or received many checks into account, the chance of white arrestee being released remained high.
#' Comparing with the first model, the colour-age plot of model 2 is quite similiar. What is 
#' different is the position of the intersection point with nearly the same possibility. Without 
#' those mediators, the intersection moved horizontally to the age of 40s. However, this difference 
#' does not contribute much extra information to the interpretation of this model. Therefore, a 
#' final conclusion of the patterns of discrimination will be given in the next section. \newline
#' 
  
##' # \textcolor{red}{Conclusion} ----
#'
#' According to the logistic regression models and effect displays, racial discrimination 
#' manifests itself in the analysis results. For colour*age and colour*year effect plots, 
#' blacks and whites have different possibilities of being released and this relationship 
#' between released and race along with age changes over time. \newline
#' 
#' As time goes by, black arrestees are more likely to be released than the white since the 
#' released probability for white was decreasing from 1997 while the rate of black people's 
#' release is increasing. That means the behaviour of the police varies over time. \newline
#' 
#' The data also reveals other types of discriminations: age discrimination, discrimination against 
#' employed status and citizenship status as well as total number of arrestee’s previous arrests. 
#' Specifically, an arrestee who is an employed citizen with no checks has more probabilities of 
#' being released. \newline



