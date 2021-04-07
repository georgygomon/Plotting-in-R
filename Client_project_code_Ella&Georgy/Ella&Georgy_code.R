#FOR THE GITHUB REPO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Loading necessary Packages
library(ggplot2)
library(tidyverse)
library(nlme)

#Setting up Data-set
n<-300
set.seed(456)
example_data<-data.frame(ID=1:300,
                         y_t1=rnorm(n, 15.4,4.6), 
                         y_t2=rnorm(n, 9.30,5.5), 
                         y_t3=rnorm(n, 11.42,6.3),
                         indicator=sample(c(0,1), replace=TRUE, size=n),
                         Sex=sample(c(0,1), replace=TRUE, size=n)
                         )

example_data$Sex<-factor(example_data$Sex,  labels=c('male', 'female'))
example_data$indicator<-factor(example_data$indicator, labels=c('healthy', 'sick'))


#Figure Number 1: Plotting Means+SD's per time point

#Calculating Means and SD's per time point
means <- colMeans(example_data[c('y_t1', 'y_t2', 'y_t3')],na.rm = T)
sds<-c(sd(example_data$y_t1, na.rm=TRUE)/sqrt(sum(!is.na(example_data$y_t1))), 
       sd(example_data$y_t2, na.rm=TRUE)/sqrt(sum(!is.na(example_data$y_t2))),
       sd(example_data$y_t3, na.rm=TRUE)/sqrt(sum(!is.na(example_data$y_t3)))
)
#Putting calculated values in a data-frame (ggplot only works with data-frames)
df_1 <- data.frame(means, Time = factor(c("Intake", "TK", "FU"), levels = c("Intake", "TK", "FU")), sds)

#Plotting using ggplot
colors=c('blue', 'red', 'green')
ggplot(data = df_1, aes(x = Time, y = means, group =1))  + ylab("Score")+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2, color=colors) +
  geom_point(color=colors)+geom_line()




#Figure Number 2: Plotting Means+SD's per time point for healthy and sick group individually
means_2 <- c( colMeans(example_data[example_data$indicator=='healthy' ,c('y_t1', 'y_t2', 'y_t3')]), 
              colMeans(example_data[example_data$indicator=='sick', c('y_t1', 'y_t2', 'y_t3')])
            )
#Determining the standard errors:
sds_2<-numeric(6)
counter<-0
for (j in c('healthy', 'sick')){
  for (i in c('y_t1', 'y_t2', 'y_t3')){
    counter=counter+1
    sds_2[counter]<-sd(example_data[example_data$indicator==j,i])/sqrt(sum(example_data[example_data$indicator==j,i]))
  }
}

#Making data frame to plot means and SD's
df_question2 <- data.frame(means = means_2, Time = rep(c("Intake", "TK", "FU"),2), group = c(0,0,0,1,1,1), sds=sds_2)
df_question2$Time <- factor(df_question2$Time, levels = c("Intake", "TK", "FU"))

#Plotting values
ggplot(data= df_question2, aes(x=Time, y = means, group = group)) + geom_line(aes(colour = factor(group))) +
  scale_color_manual(labels = c("Healthy", "Sick"), values = c("blue", "red"))+
  guides(color=guide_legend("Indicator"))+geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2)+  labs(x="Time", y="Score")




#Figure Number 3: Plotting results of Linear Mixed Model with both Population Means and some subjects
#Transforming data to long format
long_data<-pivot_longer(data=example_data, cols=c('y_t1', 'y_t2', 'y_t3'), names_to='Time', values_to='Test_Score')
#Fitting LMM model
LMM_model<-lme(Test_Score~Time*indicator+Sex, random=~Time|ID, data=long_data, na.action=na.exclude, method='REML')
#Calculating trajectories of population means
data_to_fit<-expand.grid(Time=unique(long_data$Time), indicator=c('healthy', 'sick'), Sex=c('male', 'female'))
predicted_pop_means <- data.frame(data_to_fit, Test_Score = predict(LMM_model, level = 0, newdata=data_to_fit))
#Predicted trajectories of all subjects
long_data$predicted<-predict(LMM_model, re.form = NA)

ggplot(long_data[1:12,], aes(x=Time, y =Test_Score, colour = indicator)) + 
  geom_point()+ geom_line(aes(group = ID, size = "Subjects" )) + 
  geom_line(data = predicted_pop_means, aes(y = Test_Score, x = Time, group = indicator, size = "Population")) + 
  scale_size_manual(name="Predictions", values=c("Subjects"=0.5, "Population"=3))


