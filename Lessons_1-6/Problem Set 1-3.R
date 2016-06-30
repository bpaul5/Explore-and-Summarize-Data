### LESSON 1-3 
variable <- c(variable)
nchar() # number of characters in each string 
row.names()
length()
head()
tail()
floor()
ceiling()
unique() 
read.csv(file.choose())
ggplot(data, aes(x, y)) + geom_point() + labs(x, y, title)
t() # transpose function (change rows with columns)

getwd()
setwd("Downloads")
setwd('..')
list.files()

# How to subset data 
subset(data, colname == 1)
by(data$www_likes, data$gender, sum) # adding all the likes for each gender
data[data$colname ==1, ] # prints every row use 2:4 to specify 
mtcars <- subset(mtcars, select = -year) # how to delete a column 
rm(variable) # deletes the variable from memory
levels(data$column) # shows all unique row values 

# ggplot2 
install.packages('ggplot2')
library(ggplot2)
install.packages('ggthemes', dependencies = TRUE) 
library(ggthemes) 
ggsave('priceHistogram.png')

levels(data$age.range)
new = factor(data$age.range, levels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above"), ordered = T) # how to rearrange the columns 
new2 = ordered(data$age.range, levels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above"))

qplot(x = dob_day, data = pf) +
        scale_x_discrete(breaks = 1:31) + # 'breaks' changes the x-axis 
        facet_wrap(~dob_month, ncol = 4) # creates a graph for each month
# 'facet_grid(variables split in vertical direction ~ variables split in horizontal direction) 

qplot(x = tenure, data = data, binwidth = 365, color = I('black'), fill = I('blue')) 

# transforming graphs 
summary(log10(data + 1)) # if data has '0' need to add '1' so mean is not -inf
sumarry(sqrt(data))

install.packages('gridExtra') 
library('gridExtra')
p1 = ggplot(data = data, aes(x = friend_count)) + geom_histogram()
p2 = ggplot(data = data, aes(x = friend_count)) + geom_bar() + scale_x_log10()
p3 = ggplot(data = data, aes(x = friend_count)) + geom_bar() + scale_x_sqrt()
grid.arrange(p1, p2, p3, ncol = 1)

p2 = qplot(data = data, x = log10(friend_count + 1))

# frequency polygons "multiple lines on the same graph"
qplot(data = subset(data, !is.na(gender)), x = www_likes, geom = 'freqpoly', color = gender) + 
        scale_x_continuous() + scale_x_log10()

# box plots 
# horizontal black line is mean 50% 
qplot(x = gender, y = friendships_initiated, data = subset(data, !is.na(gender)), geom = 'boxplot') + 
        coord_cartesian(ylim = c(0, 250))

### PROBLEM SET 3 
data(diamonds)
qplot(data = diamonds, x = price, bins = 2000) + xlim(c(0,2000))
mean = sum((diamonds$price/length(diamonds$price)))
median = median(diamonds$price)
# price by cut 
qplot(data = diamonds, x = price) + facet_wrap(~cut, ncol=1, scale = "free_y")
by(diamonds$price, diamonds$cut, summary, digits = 5)
# price by carat
qplot(data = diamonds, x = price/carat, bins = 1000) + facet_wrap(~cut, ncol=1) + scale_x_log10()
# price in box plot 
qplot(data = diamonds, x = color, y = price, geom = 'boxplot') + coord_cartesian(ylim = c(0,8000))
by(diamonds$price, diamonds$color, summary, digits = 5)
# interquartile range
color = subset(diamonds, diamonds$color == "J")
IQR(color$price)
# frequency polygon
qplot(data = diamonds, x = carat, geom = 'freqpoly', bins = 20000) + xlim(c(0.1, 1.2))

library(tidyr)
library(dplyr)

data = read.csv('traffic.csv', header = T, row.names = 1, check.names = F)
data = t(data)
data = as.data.frame(data)
years = seq(1950, 2008)
ggplot(data = data, aes(x = years)) + geom_line(aes(y = data[65], color = 1)) + geom_line(aes(y = data[64], color = 2))

library('lubridate')
data = as.matrix(data)
for(i in data) {
        strptime(i, format = '%y/%d/%m')
}




