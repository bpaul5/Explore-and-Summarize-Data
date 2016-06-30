### LESSON 5 
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)

pf.fc_by_age_gender = pf %>%
        group_by(age, gender) %>%
        filter(!is.na(gender)) %>%
        summarise(mean_friend_count = mean(friend_count),
                  median_friend_count = median(friend_count),
                  n = n()) %>%
        ungroup() %>%
        arrange(age)

ggplot(data = pf.fc_by_age_gender, aes(x = age, y = median_friend_count, color = gender)) +
        geom_line()

# import tidyr can also use 'reshape2' package 
malevfemale = spread(subset(pf.fc_by_age_gender, select = c(age, gender, median_friend_count)), gender, median_friend_count)

ggplot(data = malevfemale, aes(x = age, y = female/male)) + 
        geom_line() + 
        geom_hline(yintercept = 1, alpha = 0.5, linetype = 2) # use alpha for transparency

# tenure refers to how many days since 2014 someone has had FB
# floor() to round down and ceiling() to round up
pf$year_joined = floor(2014 - (pf$tenure/365))

pf$year_joined.bucket = cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))

ggplot(data = subset(pf, !is.na(year_joined.bucket)), aes(x = age, y = friend_count)) +
        geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean) +
        geom_line(stat = 'summary', fun.y = mean, linetype = 2)

with(subset(pf, tenure >= 1), summary(friend_count/tenure))

ggplot(data = subset(pf, tenure >= 1), aes(x = tenure, y = friendships_initiated/tenure)) +
        geom_line(aes(color = year_joined.bucket), stat ='summary', fun.y = mean)

# decreasing variance increases bias 
ggplot(data = subset(pf, tenure >= 1), aes(x = tenure, y = friendships_initiated/tenure)) +
        geom_smooth(aes(color = year_joined.bucket))

yo = read.csv('yogurt.csv', header = T)
yo$id = factor(yo$id)
ggplot(yo, aes(price)) + geom_histogram()

yo = transform(yo, all.purchases = (strawberry + blueberry + pina.colada + plain + mixed.berry))

ggplot(data = yo, aes(x = time, y = price)) +
        geom_jitter(alpha = 1/4)

set.seed(3250)
sample.ids = sample(levels(yo$id), 16) # use levels for diffent ids
ggplot(aes(x = time, y = price), 
       data = subset(yo, id %in% sample.ids)) +
        facet_wrap( ~ id) +
        geom_line() + 
        geom_point(aes(size = all.purchases), pch = 1)

# Scatterplot Matrix 
library(GGally)
these_set(theme_minimal(20))
set.seed(1836)
pf_subset = pf[, c(2:7)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])

# Heat Map
library(reshape2)
nci = read.table('nci.tsv')
colnames(nci) = c(1:64)
nci.long.samp = melt(as.matrix(nci[1:200, ]))
names(nci.long.samp) = c('gene', 'case', 'value')
head(nci.long.samp)

ggplot(aes(y = gene, x = case, fill = value), 
       data = nci.long.samp) +
        geom_tile() +
        scale_fill_gradientn(colours = colorRampPalette(c('blue', 'red'))(100))

### PROBLEM SET 5 
data('diamonds')
ggplot(data = diamonds, aes(x = price, color = cut)) +
        geom_histogram() +
        facet_wrap(~color, ncol = 3)
        
ggplot(diamonds, aes(y = price, x = table, color = cut)) + 
        geom_point() + 
        scale_color_brewer(type = 'qual') +
        xlim(c(55,62))

diamonds$volume = with(diamonds, x*y*z)
ggplot(data = subset(diamonds, volume < quantile(volume, probs = .99)), aes(x = volume, y = price, color = clarity)) + 
        geom_point() +
        scale_y_log10() + 
        scale_color_brewer(type = 'div')

pf$prop_initiated = pf$friendships_initiated / pf$friend_count 

ggplot(pf, aes(y = prop_initiated, x = tenure)) +
               geom_smooth(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)

x = subset(pf, pf$year_joined.bucket == "(2012,2014]")
mean(x$prop_initiated, na.rm = T)

ggplot(diamonds, aes(x = cut, y = price/carat, color = color)) +
        geom_jitter() + 
        facet_wrap(~clarity, ncol = 3)


















