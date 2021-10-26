##Worm plotting
##06/02/2021
##Sara DeLaurentis
##create summarized worm data

library(tidyverse)
library(glue)
library(stringr)

  
#No NA values for data entries, but there are UNK for some of the ID values. Will need to remove those.
  
worms <- read.csv("ItascaEarthwormData_2019.csv")
head(worms)
view(worms)

Aporr <- worms[worms$species == "Aporrectodea species",]
view(Aporr)

png("worm_scatter.png", width = 1000, height = 1000)
worm_scatter <- plot(worms, cex = 2, cex.lab = 2)
print(worm_scatter)
dev.off()
?plot

#making our own summarySE function:

wormplot.df <- worms %>% group_by(treatment) %>% 
  summarise(mean = mean(biomass_m2), SD = sd(biomass_m2))
print(wormplot.df)

wormplot.df <- wormplot.df %>%   ##Adding in SE to the dataframe
  mutate(SE = SD/sqrt(length(SD)))
print(wormplot.df)


#Error bars represent standard error of the mean
wormplot.FUN <- function(data){
  Wplot <- ggplot(wormplot.df, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE),
                width = .2,
                position = position_dodge(.9))
  Wplot <- Wplot + ggtitle("Itasca 2019 Worms") +
    theme(axis.text.x = element_text(size = rel(1.2))) +
    theme(axis.text.y = element_text(size = rel(1.2))) +
  
  print(Wplot) #print
  }


wormplot.FUN(wormplot.df) #run the function



 

print(Wplot1)

fm1 <- lm(biomass_m2 ~ treatment, data = worms)
anova(fm1)
summary(fm1)

view(worms)

#plotting residuals to check for equal variance
plot(residuals(fm1) ∼ fitted.values(fm1), main = "fm1 residuals: biomass_m2 ~ Site")

#based on summary and residual plotting, variances are not equal.


wormplot.df         

##from: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
##contains a potentially useful function using plyr (non-tidyverse) that I've recreated above. Maybe come back here for the CI information.

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(dplyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

?ddply
args(mutate)