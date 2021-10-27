#Worm plotting
#06/02/2021
#Sara DeLaurentis
#create summarized worm data


```r
library(tidyverse)
library(glue)
library(stringr)
library(magrittr)
library(dplyr)


getwd()
```

```
## [1] "C:/Users/sbaue/coding/R_TEMPRY/Itasca_project_19-21/Itasca_19-20"
```

No NA values for data entries, but there are UNK for some of the ID values. Will need to remove those.


```r
worms <- read.csv("ItascaEarthwormData_2019.csv")
head(worms)
```

```
##          notes      date site_name inv_lvl forest_type treatment rep layer
## 1              9/21/2019     C2AR1       2           C       C2A  R1   org
## 2              9/21/2019     C2AR1       2           C       C2A  R1   org
## 3  cracked cap 9/21/2019     C2AR1       2           C       C2A  R1   min
## 4 missing head 9/21/2019     C2AR1       2           C       C2A  R1   min
## 5              9/21/2019     C2AR1       2           C       C2A  R1   min
## 6              9/21/2019     C2AR1       2           C       C2A  R1   min
##                species growth_stage ecol_group length_mm LN_length
## 1 Dendrobaena octaedra        adult    epigeic        19  2.944439
## 2 Dendrobaena octaedra          juv    epigeic        12  2.484907
## 3 Dendrobaena octaedra          juv    epigeic        18  2.890372
## 4 Dendrobaena octaedra          juv    epigeic        22  3.091042
## 5 Dendrobaena octaedra          juv    epigeic        15  2.708050
## 6 Dendrobaena octaedra          juv    epigeic        10  2.302585
##   calc_LN_biomass biomass_AFDg  area_m2
## 1       -5.110987  0.006030130 0.080425
## 2       -6.322544  0.001795371 0.080425
## 3       -5.253535  0.005229002 0.080425
## 4       -4.724467  0.008875447 0.080425
## 5       -5.734226  0.003233385 0.080425
## 6       -6.803234  0.001110179 0.080425
```

```r
view(worms)
```

Add column for invasion level, then column for forest type. Overwrite csv and check.


```r
worms <- worms %>% 
  mutate(inv_lvl = case_when(
    grepl("2", treatment) ~ "2",
    grepl("5", treatment) ~ "5"
  ), .after = "site_name") 

worms <- worms %>% 
  mutate(forest_type = case_when(
    grepl("C", treatment) ~ "C",
    grepl("D", treatment) ~ "D"), .after = "inv_lvl"
  )

view(worms)

write_csv(worms, "ItascaEarthwormdata_2019.csv")

check <- read.csv("ItascaEarthwormdata_2019.csv")
view(check)
```

First need to summarize sums by replicate, then take the mean of the group.
Created summarized df, divided biomass totals by the worm ring area to get the biomass totals for each rep.


```r
worm_rep_sum <- 
  worms %>% 
  group_by(treatment, rep) %>% 
  summarise(biomass_AFDg_TOT = sum(biomass_AFDg))
```

```
## `summarise()` has grouped output by 'treatment'. You can override using the `.groups` argument.
```

```r
worm_rep_sum %<>% 
  mutate(biomass_m2 = biomass_AFDg_TOT/0.080425)

view(worm_rep_sum)  
```

redo creation of invasion level and forest type columns (post-summarize)


```r
worm_rep_sum <- worm_rep_sum %>% 
  mutate(inv_lvl = case_when(
    grepl("2", treatment) ~ "2",
    grepl("5", treatment) ~ "5"
  ), .after = "treatment") 

worm_rep_sum <- worm_rep_sum %>% 
  mutate(forest_type = case_when(
    grepl("C", treatment) ~ "C",
    grepl("D", treatment) ~ "D"), .after = "treatment"
  )

view(worm_rep_sum)
```

Attempt to select one worm group from the original worms dataframe.


```r
Aporr <- worms[worms$species == "Aporrectodea species",]
view(Aporr)
```

Worm scatterplot, checking trends and relationships.


```r
png("worm_scatter.png", width = 1000, height = 1000)
worm_scatter <- plot(worms, cex = 2, cex.lab = 2)
print(worm_scatter)
```

```
## NULL
```

```r
dev.off()
```

```
## png 
##   2
```

```r
?plot
```

making our own summarySE function:


```r
wormplot.df <- worm_rep_sum %>% group_by(treatment) %>% 
  summarise(mean = mean(biomass_m2), SD = sd(biomass_m2))
print(wormplot.df)
```

```
## # A tibble: 8 x 3
##   treatment  mean    SD
##   <chr>     <dbl> <dbl>
## 1 C2A        2.23 2.38 
## 2 C2B        1.84 0.984
## 3 C5A        2.53 1.33 
## 4 C5B        3.18 1.19 
## 5 D2A        1.67 0.656
## 6 D2B        1.56 0.505
## 7 D5A        4.06 1.46 
## 8 D5B        3.22 1.09
```

wormplot.df <- wormplot.df %>%   #Adding in SE to the dataframe


```r
  mutate(SE = SD/sqrt(length(SD)))
```

```
## Error in mutate(SE = SD/sqrt(length(SD))): object 'SD' not found
```

```r
print(wormplot.df)
```

```
## # A tibble: 8 x 3
##   treatment  mean    SD
##   <chr>     <dbl> <dbl>
## 1 C2A        2.23 2.38 
## 2 C2B        1.84 0.984
## 3 C5A        2.53 1.33 
## 4 C5B        3.18 1.19 
## 5 D2A        1.67 0.656
## 6 D2B        1.56 0.505
## 7 D5A        4.06 1.46 
## 8 D5B        3.22 1.09
```

Error bars represent standard error of the mean


```r
wormplot.FUN <- function(data){
  Wplot <- ggplot(wormplot.df, aes(x = treatment, y = mean, fill = treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE),
                width = .2,
                position = position_dodge(.9))
  Wplot <- Wplot + ggtitle("Itasca 2019 Worms") +
    theme(axis.text.x = element_text(size = rel(1.2))) +
    theme(axis.text.y = element_text(size = rel(1.2))) 
  
 # print(Wplot) #print
  }
```

wormplot.FUN(wormplot.df) run the function
anova comparing the 8 treatment groups:


```r
fm1 <- lm(biomass_m2 ~ treatment, data = worm_rep_sum)
anova(fm1)
```

```
## Analysis of Variance Table
## 
## Response: biomass_m2
##           Df Sum Sq Mean Sq F value Pr(>F)
## treatment  7 16.493  2.3562  1.3624 0.2861
## Residuals 16 27.671  1.7294
```

```r
summary(fm1)
```

```
## 
## Call:
## lm(formula = biomass_m2 ~ treatment, data = worm_rep_sum)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.76979 -0.81920 -0.06958  0.68036  2.71139 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    2.2262     0.7593   2.932  0.00977 **
## treatmentC2B  -0.3876     1.0738  -0.361  0.72281   
## treatmentC5A   0.3038     1.0738   0.283  0.78084   
## treatmentC5B   0.9489     1.0738   0.884  0.38996   
## treatmentD2A  -0.5608     1.0738  -0.522  0.60860   
## treatmentD2B  -0.6636     1.0738  -0.618  0.54528   
## treatmentD5A   1.8372     1.0738   1.711  0.10640   
## treatmentD5B   0.9943     1.0738   0.926  0.36819   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.315 on 16 degrees of freedom
## Multiple R-squared:  0.3735,	Adjusted R-squared:  0.09934 
## F-statistic: 1.362 on 7 and 16 DF,  p-value: 0.2861
```

```r
view(worm_rep_sum)
```

anova comparing the 2 forest types:


```r
fm2 <- lm(biomass_m2 ~ forest_type, data = worm_rep_sum)
anova(fm2)
```

```
## Analysis of Variance Table
## 
## Response: biomass_m2
##             Df Sum Sq Mean Sq F value Pr(>F)
## forest_type  1  0.206 0.20648  0.1033 0.7509
## Residuals   22 43.958 1.99808
```

```r
summary(fm2)
```

```
## 
## Call:
## lm(formula = biomass_m2 ~ forest_type, data = worm_rep_sum)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.9861 -1.1847 -0.4883  0.8941  3.0025 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.4425     0.4081   5.986 5.04e-06 ***
## forest_typeD   0.1855     0.5771   0.321    0.751    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.414 on 22 degrees of freedom
## Multiple R-squared:  0.004675,	Adjusted R-squared:  -0.04057 
## F-statistic: 0.1033 on 1 and 22 DF,  p-value: 0.7509
```

anova comparing the 2 invasion levels:


```r
fm3 <- lm(biomass_m2 ~ inv_lvl, data = worm_rep_sum)
anova(fm3)
```

```
## Analysis of Variance Table
## 
## Response: biomass_m2
##           Df Sum Sq Mean Sq F value   Pr(>F)   
## inv_lvl    1 12.168 12.1677  8.3662 0.008451 **
## Residuals 22 31.996  1.4544                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(fm3)
```

```
## 
## Call:
## lm(formula = biomass_m2 ~ inv_lvl, data = worm_rep_sum)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.1257 -0.6670 -0.1620  0.5298  3.1144 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.8232     0.3481   5.237 2.97e-05 ***
## inv_lvl5      1.4241     0.4923   2.892  0.00845 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.206 on 22 degrees of freedom
## Multiple R-squared:  0.2755,	Adjusted R-squared:  0.2426 
## F-statistic: 8.366 on 1 and 22 DF,  p-value: 0.008451
```

plotting residuals to check for equal variance


```r
plot(residuals(fm1) ∼ fitted.values(fm1), main = "fm1 residuals: biomass_m2 ~ Site")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

based on summary and residual plotting, variances are not equal.


```r
wormplot.df         
```

```
## # A tibble: 8 x 3
##   treatment  mean    SD
##   <chr>     <dbl> <dbl>
## 1 C2A        2.23 2.38 
## 2 C2B        1.84 0.984
## 3 C5A        2.53 1.33 
## 4 C5B        3.18 1.19 
## 5 D2A        1.67 0.656
## 6 D2B        1.56 0.505
## 7 D5A        4.06 1.46 
## 8 D5B        3.22 1.09
```

#from: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
#contains a potentially useful function using plyr (non-tidyverse) that I've recreated above. Maybe come back here for the CI information.
# Summarizes data.
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#   data: a data frame.
#   measurevar: the name of a column that contains the variable to be summariezed
#   groupvars: a vector containing names of columns that contain grouping variables
#   na.rm: a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the confidence interval (default is 95%)


```r
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
```

```
## No documentation for 'ddply' in specified packages and libraries:
## you could try '??ddply'
```

```r
args(mutate)
```

```
## function (.data, ...) 
## NULL
```

