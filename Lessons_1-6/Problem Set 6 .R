### LESSON 6 
library(ggplot2)
library(GGally)
library(scales)
library(memisc)
library(lattice)
library(MASS)
library(car)
library(reshape)
library(plyr)
library(gridExtra)
library(dplyr)
library(RCurl)
library(bitops)
data(diamonds)

# simple scatter plot 
ggplot(subset(diamonds, carat < quantile(carat, probs = .99) & price < quantile(price, probs = .99)), 
       aes(x = carat, y = price)) +
        geom_point()

# 
set.seed(20022012)
diamond_samp = diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp,
        lower = list(continuous = wrap('points', shape = I('.'))),
        upper = list(combo = wrap('box', outlier.shape = I('.'))), 
        axisLabels = 'internal')

p1 = ggplot(diamonds, aes(price)) + geom_histogram() + ggtitle('Price') 
p2 = ggplot(diamonds, aes(price)) + geom_histogram() + ggtitle('Price (log10') + scale_x_log10()
grid.arrange(p1, p2, ncol = 2)

# further transforming the data 
cuberoot_trans = function() trans_new('cuberoot', 
                                      transform = function(x) x^(1/3), 
                                      inverse = function(x) x^3)
ggplot(subset(diamonds, carat < quantile(carat, probs = .99) & price < quantile(price, probs = .99)), 
       aes(x = carat, y = price)) +
        geom_point() + 
        scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3), 
                           breaks = c(0.2, 0.5, 1, 2, 3)) + 
        scale_y_continuous(trans = log10_trans(), limits = c(350, 15000), 
                           breaks = c(350, 1000, 5000, 10000, 15000)) + 
        ggtitle('Price (log10) by Cube-Root of Carat')

# Overplotting Revisited 
head(sort(table(diamonds$carat), decreasing = T))
head(sort(table(diamonds$price), decreasing = T))
ggplot(aes(carat, price), data = diamonds) + 
        geom_point(alpha = 0.5, size = 0.75, position = 'jitter') + 
        scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                           breaks = c(0.2, 0.5, 1, 2, 3)) + 
        scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                           breaks = c(350, 1000, 5000, 10000, 15000)) +
        ggtitle('Price (log10) by Cube-Root of Carat')

# color by cut or clarity 
library(RColorBrewer)
ggplot(aes(x = carat, y = price, color = color), data = diamonds) + 
        geom_point(alpha = 0.5, size = 1, position = 'jitter') +
        scale_color_brewer(type = 'div',
                           guide = guide_legend(title = 'Clarity', reverse = F,
                                                override.aes = list(alpha = 1, size = 2))) +  
        scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                           breaks = c(0.2, 0.5, 1, 2, 3)) + 
        scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                           breaks = c(350, 1000, 5000, 10000, 15000)) +
        ggtitle('Price (log10) by Cube-Root of Carat and Clarity')
        
# working with big data 
diamondsbig$logprice = log(diamondsbig$price)
m1 = lm(logprice ~ I(carat^(1/3)), 
        data = diamondsbig[diamondsbig$price < 10000 & diamondsbig$cert == 'GIA',])
m2 = update(m1, ~ . + carat)
m3 = update(m2, ~ . + cut)
m4 = update(m3, ~ . + color)
m5 = update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

thisDiamond = data.frame(carat = 1.00, cut = 'V.Good', 
                         color = 'I', clarity = 'VS1')
modelEstimate = predict(m5, newdata = thisDiamond, 
                        interval = 'prediction', level = .95)


















