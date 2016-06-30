### LESSON 4 
library(ggplot2)
pf = read.csv('pseudo_facebook.tsv', sep = '\t')

ggplot(aes(x = age, y = friend_count), data = pf) +
        geom_point(alpha = 1/20) + 
        xlim(13, 90) + 
        coord_trans(y = 'sqrt')
# geom_jitter() adds pos or neg 'noise' to graph this help interprete x-axis with catagorical variables
ggplot(aes(x = age, y = friendships_initiated), data = pf) + 
        geom_point(alpha = 1/30, position = position_jitter(h = 0)) +
        coord_trans(y = 'sqrt')

library(dplyr)
# common functions filter() group_by() mutate() arrange()

age_groups = group_by(pf, age)

pf.fc_by_age = summarise(age_groups, 
          friend_count_mean = mean(friend_count), 
          friend_count_median = median(friend_count),
          n = n()) # number of users in each group

ggplot(pf.fc_by_age, aes(x = age, y = friend_count_mean)) + geom_line()

# graphing multiple lines by quantiles 
ggplot(aes(x = age, y = friendships_initiated), data = pf) + 
        geom_point(alpha = 1/30, 
                   position = position_jitter(h = 0),
                   color = 'orange') +
        coord_trans(y = 'sqrt') + 
        geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = 0.1), linetype = 2) + 
        geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = 0.5), color = "blue") + 
        geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = 0.9), linetype = 2) +
        coord_cartesian(xlim = c(60, 75)) +
        coord_cartesian(ylim = c(0, 800))

# correlation 
cor.test(pf$age, pr$friend_count) # meathod = 'spearman'
with(pf, cor.test(age, friend_count))

ggplot(data = pf, aes(x = www_likes_received, y = likes_received)) + 
        geom_point() + 
        xlim(0, quantile(pf$www_likes_received, 0.95)) +
        ylim(0, quantile(pf$likes_received, 0.95)) +
        geom_smooth(method = 'lm', color = 'red')
        
with(pf, cor.test(www_likes_received, likes_received))

# soil data 
install.packages('alr3')
library(alr3)
data(Mitchell)

ggplot(Mitchell, aes(y = Temp, x = Month)) + geom_point() +
        scale_x_continuous(breaks = seq(0, 203, 12))

# same graph as above but using the modulus operator
ggplot(aes(x=(Month%%12),y=Temp),data=Mitchell) + 
        geom_point() 

with(Mitchell, cor.test(Temp, Month))

# Age to age months 
pf$age_with_months = pf$age + ((12 - pf$dob_month)/12)

pf.fc_by_age_months = pf %>%
        group_by(age_with_months) %>%
        summarise(friend_count_mean = mean(friend_count),
                  friend_count_median = median(friend_count),
                  n = n()) %>%
        arrange(age_with_months)

age_months_subset = subset(pf.fc_by_age_months, age_with_months < 71)

ggplot(data = age_months_subset, aes(y = friend_count_mean, x = age_with_months)) +
        geom_line()

### PROBLEM SET 4 
library(plyr)
library(tidyr)
library(dplyr)
data(diamonds)

ggplot(data = diamonds, aes(x = x, y = price)) + geom_point()

with(diamonds, cor.test(diamonds_volume$price, diamonds_volume$volume))

ggplot(data = diamonds, aes(x = depth, y = price)) + 
        geom_point(alpha = 1/100) +
        scale_x_continuous(breaks = seq(56,66,2))

carat = subset(diamonds, diamonds$carat < quantile(diamonds$carat, probs = 0.99))
price = subset(diamonds, diamonds$price < quantile(diamonds$price, probs = 0.99))
ggplot(data = diamonds, aes(x = carat, y = price)) +
        geom_point()

diamonds$volume = with(diamonds, (x*y*z))
ggplot(diamonds, aes(x = volume, y = price)) +
        geom_point()

count(diamonds$volume == 0)
diamonds_volume = subset(diamonds, volume > 0 & volume <= 800)

# http://www.ats.ucla.edu/stat/r/faq/smooths.htm (types of smoothers)
ggplot(diamonds_volume, aes(x = volume, y = price)) +
        geom_point() +
        geom_smooth()

diamondsByClarity = diamonds %>%
        group_by(clarity) %>%
        summarise(mean_price = mean(price),
                  median_price = median(price),
                  min_price = min(price),
                  max_price = max(price),
                  n = n()) %>%
        arrange(clarity)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
p1 = ggplot(data = diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) + geom_bar(stat = 'identity')
p2 = ggplot(data = diamonds_mp_by_color, aes(x = color, y = mean_price)) + geom_bar(stat = 'identity')
grid.arrange(p1, p2, ncol = 2)

data = read.csv('traffic.csv', header = T, row.names = 1, check.names = F)








