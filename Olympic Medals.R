#importing libraries

library(dplyr)
library(GGally)
library(MASS)
library(ggfortify)
library(performance)
library(pscl)
library(usdm)
library(Metrics)

#importing the dataset

oldat <- read.csv(url('http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv'))

#converting variables into numeric

oldat[,3:43] <- lapply(oldat[,3:43], as.numeric)

#checking for NAs

colSums(is.na(oldat))

#replacing NAs with UN estimations

oldat[1,3] <- 3342
oldat[21,7] <- 91370
oldat[92,7] <- 12377

#concatenating variables separated by year
#very messy - probably a better way to do this

gdp <- c(oldat$gdp00,oldat$gdp04, oldat$gdp08, oldat$gdp12, oldat$gdp16)
gold <- c(oldat$gold00, oldat$gold04, oldat$gold08, oldat$gold12, oldat$gold16)
athletes <- c(oldat$athletes00, oldat$athletes04, oldat$athletes08, 
              oldat$athletes12, oldat$athletes16)
tot <- c(oldat$tot00, oldat$tot04, oldat$tot08, oldat$tot12, oldat$tot16)
totgold <- c(oldat$totgold00, oldat$totgold04, oldat$totgold08, oldat$totgold12,
             oldat$totgold16)
totmedals <- c(oldat$totmedals00, oldat$totmedals04, oldat$totmedals08, 
               oldat$totmedals12, oldat$totmedals16)
pop <- c(oldat$pop00, oldat$pop04, oldat$pop08, oldat$pop12, oldat$pop16)

oldat <- rbind(oldat, oldat, oldat, oldat, oldat)

oldat <- cbind(oldat, gdp, gold, athletes, tot, totmedals, totgold, pop)

#verifying if the values fit

oldat[oldat$country == "France", ]

#dropping duplicated variables

oldat <- oldat[, c(1, 13:16, 37, 43:50)]

#adding a column for years

oldat <- cbind(year=NA, oldat)

oldat[1:108, 1] <- 2000
oldat[109:216, 1] <- 2004
oldat[217:324, 1] <- 2008
oldat[325:432, 1] <- 2012
oldat[433:540, 1] <- 2016

#log-transforming the pop & gdp variables

oldat$pop <- log(oldat$pop)
oldat$gdp <- log(oldat$gdp)

#renaming the pop & gdp columns

oldat <- oldat %>% 
  rename(log_gdp = gdp,
    log_pop = pop)

#creating a training set

oldat_train <- oldat[1:432, -c(1,2)]

#computing a correlation plot

ggpairs(oldat_train,)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())

#analyzing the distribution of the target 

ggplot(oldat, aes(x=gold)) +
  geom_histogram(col='black', fill='white')

#calculating the proportion of zeros

table(oldat_train$gold > 0)

#fitting a normal linear model

linear_gold <- lm(gold~., data=oldat_train)

#computing stepwise selection using AIC as a criterion

stepAIC(linear_gold, direction='backward')

AIC(linear_gold)

#updating the model

linear_gold <- lm(formula = gold ~ soviet + oneparty + host + athletes + tot + 
                    log_pop, data = oldat_train)

#printing gof stats

summary(linear_gold)$r.squared
summary(linear_gold)$adj.r.squared

#drawing residual plots

autoplot(linear_gold ,which = 1:2)

#fitting the poisson model

poisson_gold <- glm(gold~., data=oldat_train, family='poisson')

#computing model selection

stepAIC(poisson_gold, direction='backward')

#updating the model

poisson_gold <- glm(formula = gold ~ soviet + comm + muslim + host + log_gdp + 
                      athletes + tot + totmedals, family = "poisson", 
                    data = oldat_train)


#computing gof & overdispersion tests

1-pchisq(summary(poisson_gold)$deviance, 
         summary(poisson_gold)$df.residual)

check_overdispersion(poisson_gold)

#fitting a negbi model

negbi_gold <- glm.nb(gold~., data=oldat_train, control=glm.control(maxit=50))

#computing model selection

stepAIC(negbi_gold, direction='backward')

#updating the model

negbi_gold <- glm.nb(formula = gold ~ comm + muslim + log_gdp + athletes + 
                tot + totmedals, data = oldat_train, 
                control = glm.control(maxit = 50), 
              init.theta = 2.696409903, link = log)

#fitting the hurdle model

hurdle_gold <- hurdle(gold~., data=oldat_train)

#computing model selection

stepAIC(hurdle_gold,direction='backward')

#updating the model

hurdle_gold <- hurdle(formula = gold ~ soviet + comm + altitude + host + 
                        athletes + tot, data = oldat_train)

#predicting gold medals with every model

oldat_test <- oldat[433:540,-10]

linear_predicted <- predict(linear_gold, oldat_test)
poisson_predicted <- predict(poisson_gold, oldat_test)
negbi_predicted <- predict(negbi_gold, oldat_test)
hurdle_predicted <- predict(hurdle_gold)

#computing rmse for every model

rmse(oldat[433:540,10], linear_predicted)
rmse(oldat[433:540,10], poisson_predicted)
rmse(oldat[433:540,10], negbi_predicted)
rmse(oldat[433:540,10], hurdle_predicted)

#creating a new df to plot fitted values vs predicted

fitted_vs_actuals <- data.frame(cbind(oldat[433:540,c(2,10)],linear_predicted))

#drawing a predicted vs observed plot

ggplot(data=fitted_vs_actuals, aes(x=linear_predicted,y=gold, label=country))+
  geom_point()+
  geom_text(aes(label=country),hjust=0, vjust=0, size=5)+
  geom_smooth(method = 'lm', se=F, col='purple')+
  xlab('predicted values')+
  ylab('observed values')+
  ggtitle('Gold medals won in 2016')
