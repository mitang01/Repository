######!!!!!!Have to run all the codes again!!!! subject_nr was mistaken on no. 19!!!


#--------------import necessary packages
library(Matrix)
library(lme4)
library(MASS)
library(multcomp)
library(tidyverse)
library(dplyr)

#---------------data wash and organizing
#merge all data file (by 18/02/2022, 18 participants, sub4 excluded)
setwd("D:/RUG/MyPhDProject/opensesame adult2/data/data/csvs")
exp_data_ori <- do.call(rbind,lapply(list.files(),read.csv))

#keep the columns I need (i.e., correct; item; label; subject_nr)
exp_data_notyet <- dplyr::select(filter(exp_data_ori, !correct =='undefined'), 
                          c(subject_nr,label,item,correct))

#for the geographical info: age; country; gender; lang; subject_nr)
exp_demog <- dplyr::select(filter(exp_data_ori,!is.na(age)), 
                           c(subject_nr,age,country,gender,lang))


######################
#delete some trails for sub1-3 in exp_data
#ie: Testing items: paz2 (item: 3), pov2 (item 4), soz2 (item 11), zaef4 (item 60) (8 trails totally)
exp_data <- filter(exp_data_notyet, !(subject_nr %in% c(1,2,3) & item %in% c(3,4,11,60)))
######################


# check na value
which(!complete.cases(exp_data))
which(!complete.cases(exp_demog))

#check the vars in exp_data. Convert vars when necessary
summary(exp_data)

##########eg, need to convert the var "correct" to numeric
exp_data$correct <- as.numeric(exp_data$correct)
############
#Now the dataset(exp_data) is ready!










#-----------LME starts
#--- try to choose the best model
model1 <- glmer(correct ~ label + (1|item) + (1+label|subject_nr), 
              data = exp_data, family ="binomial") #aci: 4972.6
summary(model1)
#--- tukey comparison
summary(glht(model1, linfct = mcp(label = "Tukey")), test = (adjusted("holm")))

#---above chance-level?
#trained stimuli:
cl_trained <- dplyr::select(filter(exp_data, 
                                label == c("trained")),
                         c(subject_nr,item, correct))
summary(glmer(correct ~ 1 + (1|subject_nr) + (1|item), data=cl_trained, family='binomial'))
#legal stimuli
cl_legal <- dplyr::select(filter(exp_data, 
                                   label == c("legal")),
                            c(subject_nr,item, correct))
summary(glmer(correct ~ 1 + (1|subject_nr) + (1|item), data=cl_legal, family='binomial'))
#illegal stimuli
cl_illegal <- dplyr::select(filter(exp_data, 
                                 label == c("illegal")),
                          c(subject_nr,item, correct))
summary(glmer(correct ~ 1 + (1|subject_nr) + (1|item), data=cl_illegal, family='binomial'))

#Now, the main effect is significant. But acc on legal stimuli is under chance level p<.001
#It's necessary to see whether or not there's genralization effect

#merge "legal" & "illegal" into "untrained"
exp_data_2 <- exp_data
exp_data_2[exp_data_2 == "legal" | exp_data_2 == "illegal"] <- "untrained"

#redo the above calculation
model2 <- glmer(correct ~ label + (1|item) + (1+label|subject_nr), 
                data = exp_data_2, family ="binomial")
summary(glht(model2, linfct = mcp(label = "Tukey")), test = (adjusted("holm")))
cl_trained_2 <- dplyr::select(filter(exp_data_2, 
                                   label == c("trained")),
                            c(subject_nr,item, correct))
summary(glmer(correct ~ 1 + (1|subject_nr) + (1|item), data=cl_trained, family='binomial'))
cl_untrained <- dplyr::select(filter(exp_data_2, 
                                 label == c("untrained")),
                          c(subject_nr,item, correct))
summary(glmer(correct ~ 1 + (1|subject_nr) + (1|item), data=cl_legal, family='binomial'))













#---plotting starts--------
#----packages----
library(yarrr)
#take a look at the palettes if I want
piratepal("all")
library(ggplot2)

#---data preprocessing---
#model1
exp_plot <- summarise(group_by(exp_data, subject_nr, label), 
                       accuracy = mean(correct, na.rm = TRUE))
names(exp_plot)[names(exp_plot) == 'label'] <- 'type'
exp_plot <- drop_na(exp_plot)

#model2
exp_plot2 <- summarise(group_by(exp_data_2, subject_nr, label), 
                       accuracy = mean(correct, na.rm = TRUE))
names(exp_plot2)[names(exp_plot2) == 'label'] <- 'type'
exp_plot2 <- drop_na(exp_plot2)

#---fig 1 for model 1
pirateplot(formula = accuracy ~ type,
           data = exp_plot,
           yaxt.y = seq(0, 1, 0.2), 
           theme = 0,
           main = " ",
           pal = "cars",
           bean.f.o = 0, # Bean (ie density) fill
           bean.b.o = 0,
           bean.lwd = 2,
           point.o = .2, # Raw data points
           inf.f.o = .7, # Inference (ie confidence interval) fill 
           inf.b.o = .8, # Inference border
           avg.line.o = .5, # Average line (ie the same as bar)
           avg.line.col = "black",
           bar.f.o = .6, # Bar
           inf.f.col = gray(.3), # Inf fill col
           inf.b.col = "black", # Inf border col
           inf.method = "ci",
           point.bg = "black",
           point.pch = 21,
           point.cex = 1, 
           inf.disp = "line",
           cex.names = 1.4,
           cex.axis = 1.4,
           cex.lab = 2,
           gl.col = gray(0.5),
           gl.lwd = 1)

# fig 2 for model 2
pirateplot(formula = accuracy ~ type,
           data = exp_plot2,
           yaxt.y = seq(0, 1, 0.1), 
           theme = 0,
           main = " ",
           pal = "info",
           bean.f.o = 0, # Bean (ie density) fill
           bean.b.o = 0,
           bean.lwd = 2,
           point.o = .2, # Raw data points
           inf.f.o = .7, # Inference (ie confidence interval) fill 
           inf.b.o = .8, # Inference border
           avg.line.o = .5, # Average line (ie the same as bar)
           avg.line.col = "black",
           bar.f.o = .6, # Bar
           inf.f.col = gray(.3), # Inf fill col
           inf.b.col = "black", # Inf border col
           inf.method = "ci",
           point.bg = "black",
           point.pch = 21,
           point.cex = 1, 
           inf.disp = "line",
           cex.names = 1.4,
           cex.axis = 1.4,
           cex.lab = 2,
           gl.col = gray(0.5),
           gl.lwd = 1)
