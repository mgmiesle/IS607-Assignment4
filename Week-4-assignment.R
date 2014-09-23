# Matthew Miesle
# CUNY SPS
# IS 607 SECT 01
# Week 4 Assignment
# Due 9/23/2014 EOD

# Issued Problem:
# Which year were the best popular movies made?
# make (and document!) your reasonable assumptions about “best” and 
# “popular” and include the R code that you use to support your 
# argument.

# Algorithm description:
# "Best popular" can be broken down into 2 different considerations:
#     -Best
#     -Popular
# Best can be quantified by user rating
# Popular can be quantified by the number of user votes submitted
# 1. Identify which movies are popular for each year
# Popular movies will be defined as the movies that have received the
# 95th percentile number of votes.
# Alternate: the top 10 movies for each year?
# 2. Find the average rating for the most popular movies for each year
# The year that has the highest average rating will be considered the
# year that has the best popular movies.

filelocation <- "C:/Users/MattM/Downloads/movies.tab.gz"
movie.data <- read.table(filelocation, sep="\t", header=TRUE, quote="", comment="", stringsAsFactors = FALSE)

slim.movie.data <- movie.data[, c(1, 2, 5, 6)]

yearlist <- slim.movie.data$year[!duplicated(slim.movie.data$year)]
yearlist <- sort(yearlist, decreasing = FALSE)

pop.movies.votes <- function(year, pctile = 0.99)
{
    df <- slim.movie.data[slim.movie.data$year == year, ]
    top.votes <- sort(df$votes, decreasing = TRUE)
    if (length(top.votes) * (1 - pctile) <1)
    {
        top.votes <- top.votes[1]
    }else
    {
        top.votes <- top.votes[1:(floor(length(top.votes) * (1 - pctile)))]   
    }
    top.votes <- unique(top.votes)
    
    df <- df[df$votes %in% top.votes, ]

#     ifelse((length(top.votes) * 0.05) < 1, top.votes <- top.votes[1], top.votes <- top.votes[1:(floor(length(top.votes) * 0.05))])
#     had trouble with ifelse so did a traditional if-else instead
}

pop.movies.of.each.year <- lapply(yearlist, pop.movies.votes)

avg.rating <- function(df)
{
    data.frame(df$year[1], mean(df$rating))
}

library(plyr)
rating.pop.movies.yearly <- ldply(pop.movies.of.each.year, avg.rating)

names(rating.pop.movies.yearly) <- c("year", "avg.rating")

print ("The year(s) with the best popular movies is:")
(best.pop.movie.year <- rating.pop.movies.yearly[rating.pop.movies.yearly$avg.rating == 
                                                    max(rating.pop.movies.yearly$avg.rating), 1])

library(ggplot2)
avg.rating.barplot <- qplot(x = year, y = avg.rating, data = rating.pop.movies.yearly,
                       geom = "bar", stat = "identity") + 
                        ggtitle("Avg rating for popular movies of each year")
avg.rating.barplot
# http://www.r-bloggers.com/using-r-barplot-with-ggplot2/
# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_%28ggplot2%29/
# qplot() is the ggplot version of the base plot in R
# the actual plot is cleaner using qplot

# using ggplot():
# avg.rating.barplot2 <- ggplot(data = rating.pop.movies.yearly, aes(x = year, y = avg.rating)) + 
#                             geom_bar(stat = "identity") + 
#                             ggtitle("Avg rating for popular movies of each year")
# 
# avg.rating.barplot2
# http://www.r-bloggers.com/ggplot2-cheatsheet-for-barplots/
# this yields the same results as the qplot()

# using base barplot():
# barplot(rating.pop.movies.yearly$avg.rating, ylim = c(0, 10),
#         main = "Avg rating for popular movies of each year",
#         xlab = "year", ylab = "avg.rating")
# http://www.cyclismo.org/tutorial/R/plotting.html
# the values of the years on the x-axis are missing