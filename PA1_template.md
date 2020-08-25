Reproducible Research: Peer Assessment 1

Loading and preprocessing the data

Task 1: mean and median total steps per day

Get averages by day

setwd("/Users/testo/datasci/reprod/ass2/")
steps <- read.csv("activity.csv")
stepdays <- tapply(steps$steps, steps$date, sum)
hist(stepdays, main = "Steps per day", xla = "steps", yla = "number of days")
