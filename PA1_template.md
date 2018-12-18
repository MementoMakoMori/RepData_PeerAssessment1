---
title: "Peer-Review Project Report"
author: "R. Holley"
date: "November 26, 2018"
output: html_document
keep_md: true
---

``` {r setoptions}
knitr::opts_chunk$set(echo=TRUE)
```

This is my report for the Reproducible Research project, Course 5 in the Data Science Specialization by John Hopkins on Coursera. This analysis has five discrete parts.

1. Load and Preprocess Data
2. Mean Number of Steps per Day
3. Average Daily Patterns
4. Imput Missing Values
5. Weekday/Weekend Patterns

##1: Load and Preprocess Data

First I'm going to load any necessary packages and the data set. My analysis will be using dplyr for data wrangling and ggplot2 for some of the graphs.

```{r part1.1}
library(ggplot2)
library(dplyr)
data <- read.csv("activity.csv")

```

The next step is to remove any missing (NA) values in the dataset. I did this by subsetting all observations in the data that are not NA values.


```{r part1.2}
edit <- subset(data, is.na(data$steps) == FALSE)
```

##2: Mean Number of Steps per Day

Now that the data is cleaned up, the analysis part begins. First up is creating a histogram that shows the frequency of total steps per day. I also added lines on the chart to mark the median and mean values, but the median and mean values are so close together that only one line is visible. The final line of this code chunk generates a line of output that reports the mean and median values.

```{r part2}

sums <- aggregate(edit$steps, list(Date = edit$date), sum)
hist(sums$x, main = "Frequency of Steps per Day", xlab = "Total Steps", ylab = "% Frequency", breaks = 10)
#add mean and median
mn_day <- mean(sums$x)
md_day <- median(sums$x)
abline(v = mn_day, col = 2)
abline(v = md_day, col = 4)
paste("Median per Day:", md_day, "     Mean per Day:", mn_day, collapse = " ")

```
##3: Average Daily Patterns

Now I know how many steps are generally taken each day, but how are those steps distributed throughout the day? To examine this, I made a time-series plot. The 'interval' column in the data marks during what interval throughout the day the step measurement was taken. To make a chart examining step patterns during the day, I calculated the average steps taken at each specified interval, which are then charted. The average steps are recorded in a new dataset, means_int, that has two columns: 'Interval', and 'x'. 'x' is the automatically generated column name for the output of the aggregate function that calculated the means. Although the previous chart was created with the base plot package (hist function), this time I decided to use ggplot2 for a nicer aesthetic. Once again the last line in this chunk generates text output, this time reporting which time interval has the highest average steps.

```{r part3}

means_int <- aggregate(edit$steps, list(Interval = edit$interval), mean)
g <- ggplot(means_int, aes(x = Interval, y = x))
plot(g + geom_line() + guides(line = FALSE) +
       labs(title = "Mean Steps per Interval", x = "5 Minute Interval", y = "Mean Steps") +
       geom_vline(xintercept = means_int[which(means_int$x==max(means_int$x)), "Interval"], col = "yellow", size = 2, linetype = "dashed") + 
       annotate("text", x = 1500, y = 175, label = paste("Max Mean:", max(means_int$x), collapse = " "))
)

paste("Interval with Highest Average Steps:", means_int[which(means_int$x==max(means_int$x)), 1], collapse = " ")

```
##4: Impute Missing Values

The previous two sections use the 'edit' dataset, in which all the NA values were removed. Would the analysis change if, instead of removed completely, the NA values were replaced with something else? To find out, I used average steps per interval, calculated in means_int in the previous section, as replacement values. Each NA step value was replaced with the mean value for its corresponding interval.

```{r part4.1}

NArows <- which(is.na(data$steps))
#report # of NA values
paste("# of NA Values in Steps:", length(NArows), collapse = " ")

#replace NA values in original data with mean for that interval    
NA_ints <- data$interval[NArows]
match <- data.frame("Interval" = NA_ints)
match <- inner_join(match, means_int, by = "Interval")

new_data <- data
new_data$steps <- replace(new_data$steps, NArows, match$x)

```

'match' is a data table that matches each NA observation with the mean steps for its interval. new_data is the dataset that has all of the NA values replaced with the mean values. Next is finding out if adding these values affects the median and mean steps taken each day. Essentially, repeat Part 2 with but with this dataset. 

```{r part4.2}

new_sums <- aggregate(new_data$steps, list(Date = new_data$date), sum)
hist(new_sums$x, main = "Frequency of Steps per Day (edited data)", xlab = "Total Steps", ylab = "% Frequency", breaks = 10)
#add mean and median
new_mn_day <- mean(new_sums$x)
new_md_day <- median(new_sums$x)
abline(v = new_mn_day, col = 2)
abline(v = new_md_day, col = 4)
paste("New Median per Day:", new_md_day, "    New Mean per Day:", new_mn_day, collapse = " ")

```


##5: Weekday/Weekend Patterns

The final section of this project is determining if there are any differences in activity patterns between weekdays and weekends. First I had to again find the means per interval, but this time grouped by the added variable, 'Day', which is equal to either "Weekday" or "Weekend." 

```{r part5.1}

#convert numbered dates to weekday names
day_names <- weekdays(as.Date(new_data$date))

#find indices of either weekdays or weekends
wends <- grep("Sunday|Saturday", day_names, value = FALSE)
wdays <- grep("[^Sunday, Saturday]", day_names, value = FALSE)

#create factor, add to data as new column
days = factor(levels = c("Weekday", "Weekend"))
days[wdays] = "Weekday"
days[wends] = "Weekend"
new_data <- mutate(new_data, "Day" = days)

#find mean steps per day and interval
sep_means <- aggregate(new_data$steps, by=list(new_data$Day, new_data$interval), mean)
colnames(sep_means) <- c("Day", "Interval", "Steps")

```

There was probably a more efficient method than using grep and taking indices, but it works and made sense to me. Now, the final chart! Although the course example had to panels, I decided to make one panel with overlapping color-coded lines. I think it makes it easier to compare weekday vs weekend values a single interval.


```{r part5.2}

g2 <- ggplot(data = sep_means, aes(x=Interval, y=Steps, col=Day)) + geom_line()
plot(g2 + labs(title = "Average Activity Pattern") + guides(color = guide_legend(title = NULL)))

```



