
############### Data Science Linear Regression ################

# filter matches between year 1961 and 2001

dt <- Teams %>% filter(yearID >= 1961 & yearID <=2001)

# corelation coefficient between runs scored per game and at batters per game

dt %>% summarize(cor(R/G, AB/G))

# corelation coefficient between number of wins per game and errors per game

dt %>% summarize(cor(W/G, E/G))

# corelation coefficient between double batters per game and triple batters per game

dt %>% summarize(cor(X2B/G, X3B/G))


### Assessment For Mother Daughter Height Problem

set.seed(1989) #if you are using R 3.5 or earlier

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later

library(HistData)
library(dplyr)

# Define female_heights, a set of mother and daughter heights

data("GaltonFamilies")
female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


# Mean of mother's heights
mean(female_heights$mother)

# Standard deviation of mother's heights
sd(female_heights$mother)

# Mean of daughter's heights
mean(female_heights$daughter)

# Standard deviation of daughter's heights
sd(female_heights$daughter)

# Correlation coefficient
cor(female_heights$mother,female_heights$daughter)

## Calculate the slope and intercept of the regression line predicting
## daughters' heights given mothers' heights. Given an increase in 
## mother's height by 1 inch, how many inches is the daughter's height
## expected to change?

# Slope of regression line predicting daughters' height from mothers' heights

r <- cor(female_heights$mother, female_heights$daughter)
s_y <- sd(female_heights$daughter)
s_x <- sd(female_heights$mother)
dec_slope <- r * s_y/s_x

# Intercept of regression line predicting daughters' height from mothers' heights

mu_y <- mean(female_heights$daughter)
mu_x <- mean(female_heights$mother)
dec_intercept <- mu_y - (r * s_y/s_x)*mu_x

# Change in daughter's height in inches given 1 inch increase in mother's height

r * s_y/s_x


## What percent of the variability in daughter heights is explained by 
## the mother's height?
## Report your answer as a number between 0 and 100.

r ^ 2

## A mother has a height of 60 inches.
## What is the conditional expected value of her daughter's height 
## given the mother's

(dec_slope * 60) + dec_intercept

# (0.339 * 60) + 42.5 ==> no of decimal places are also important


### Create the dataset for batting filtered by year 2002 with players 
### having 100 games per plate

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
       mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
       filter(pa >= 100) %>%
       select(playerID, singles, bb)

### Create the dataset for batting filtered by year 1999 - 2001 with players 
### and calculating average singles and bb scored per player

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
       mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
       filter(pa >= 100) %>%
       group_by(playerID) %>%
       summarize(mean_singles = mean(singles), mean_bb = mean(bb))

## Find values of player having scored more than 0.2 in mean singles and mean bb

sum(bat_99_01$mean_singles > 0.2)

sum(bat_99_01$mean_bb > 0.2)



## Joining both the above mentioned datasets 

r_dat <- inner_join(bat_02, bat_99_01, by = "playerID")



## Find correaltion between singles and mean_singles. bb & mean_bb

r_dat %>% summarize(cor(singles, mean_singles))

r_dat %>% summarize(cor(bb,mean_bb))


## creare scattered plot to check which distribution is normal to bivariat
## in this case both the plots are noramlised by Bivariat

r_dat %>% ggplot(aes(singles, mean_singles)) +
   geom_point(alpha = 0.5)

r_dat %>% ggplot(aes(bb, mean_bb)) +
   geom_point(alpha = 0.5)


## Find interceptor by using linear model formula

## Predict singles using mean_singles. in lm formula the value to be predicted
## is written to the left of ~. The value used to predict is written 
## to the right of ~.

lm(singles ~ mean_singles, data = r_dat)

## Predicting bb using mean_bb

lm(bb ~ mean_bb, data = r_dat)




### Use the Teams data frame from the Lahman package. 
### Fit a multivariate linear regression model to obtain the effects of BB 
### and HR on Runs (R) in 1971. Use the tidy function in the broom package 
### to obtain the results in a data frame.

library(Lahman)
library(tidyverse)
library(broom)

fit <- Teams %>% filter(yearID == 1971) %>% mutate(BB = BB/G, HR = HR/G, R = R/G) %>% lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)

## Repeat the above exercise to find the effects of BB and HR on runs (R) 
## for every year from 1961 to 2018 using do and the broom package.

res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup()

res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")


## Fit a linear model on the results to determine the effect of year 
## on the impact of BB.

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate)

## What is the p-value for this effect?

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(p.value)


## Use runs (R) per game to predict average attendance.
## For every 1 run scored per game, attendance increases by how much?

# find regression line predicting attendance from R and take slope
Teams_small %>% 
  mutate(R_per_game = R/G) %>% 
  lm(avg_attendance ~ R_per_game, data = .) %>% 
  .$coef %>%
  .[2]

## Use home runs (HR) per game to predict average attendance.
## For every 1 home run hit per game, attendance increases by how much?

Teams_small %>% 
  mutate(HR_per_game = HR/G) %>% 
  lm(avg_attendance ~ HR_per_game, data = .) %>% 
  .$coef %>%
  .[2]



#######################

### For this set of exercises, we examine the data from a 2014 PNAS paper 
### that analyzed success rates from funding agencies in the Netherlands and 
### concluded:

###     "our results reveal gender bias favoring male applicants over female 
###     applicants in the prioritization of their "quality of researcher" 
###     (but not "quality of proposal") evaluations and success rates, as well 
###     as in the language used in instructional and evaluation materials."

### A response was published a few months later titled No evidence that gender 
### contributes to personal research funding success in 
### The Netherlands: A reaction to Van der Lee and Ellemers, which concluded:
  
###     However, the overall gender effect borders on statistical significance, 
###     despite the large sample. Moreover, their conclusion could be a prime 
###     example of Simpson’s paradox; if a higher percentage of women apply for 
###     grants in more competitive scientific disciplines (i.e., with low 
###     application success rates for both men and women), then an analysis 
###     across all disciplines could incorrectly show "evidence" of gender 
###     inequality. 

### Who is right here: the original paper or the response? Here, we will 
### examine the data and come to our own conclusion.

### The main evidence for the conclusion of the original paper comes down 
### to a comparison of the percentages. The information we need was originally 
### in Table S1 in the paper, which we include in dslabs:
  
  library(dslabs)
  data("research_funding_rates")
  research_funding_rates

  
  ## Construct a two-by-two table of gender (men/women) by award status 
  ## (awarded/not) using the total numbers across all disciplines.
  
  two_by_two <- research_funding_rates %>% 
    select(-discipline) %>% 
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men, 
              no_men = applications_men - awards_men, 
              yes_women = awards_women, 
              no_women = applications_women - awards_women) %>%
    gather %>%
    separate(key, c("awarded", "gender")) %>%
    spread(gender, value)
  
  ## Display results
  
  two_by_two
  
  
  ## What is the percentage of men awarded?
  
  two_by_two %>% 
    mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
    filter(awarded == "yes") %>%
    pull(men)

  ## What is the percentage of women awarded?
  
  two_by_two %>% 
    mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
    filter(awarded == "yes") %>%
    pull(women)
  
  ## Run a chi-squared test on the two-by-two table to determine whether the 
  ## difference in the two success rates is significant.
  
  library(broom)  # For tidy function 
  
  two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)
  
  
### There may be an association between gender and funding. But can we infer 
### causation here? Is gender bias causing this observed difference? The 
### response to the original paper claims that what we see here is similar to 
### the UC Berkeley admissions example. Specifically they state that this 
### "could be a prime example of Simpson’s paradox; if a higher percentage of 
### women apply for grants in more competitive scientific disciplines, then an 
### analysis across all disciplines could incorrectly show 'evidence' of gender 
### inequality."
  
### To settle this dispute, prepare a dataset with number of applications, awards, and success rate for each gender:
  
  dat <- research_funding_rates %>% 
    mutate(discipline = reorder(discipline, success_rates_total)) %>%
    rename(success_total = success_rates_total,
           success_men = success_rates_men,
           success_women = success_rates_women) %>%
    gather(key, value, -discipline) %>%
    separate(key, c("type", "gender")) %>%
    spread(type, value) %>%
    filter(gender != "total")
  
  ## Display dataset
  dat
  
  ## To check if this is a case of Simpson's paradox, plot the success rates 
  ## versus disciplines, which have been ordered by overall success, 
  ## with colors to denote the genders and size to denote the number 
  ## of applications.
  
  # scatter graph
  
  dat %>% 
    ggplot(aes(discipline, success, size = applications, color = gender)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_point()
  
  # Bar Graph
  
  dat %>% arrange(success) %>% ggplot(aes(discipline, success, fill=gender)) + geom_bar(stat = "identity", position = "stack") + geom_text(aes(label=applications))