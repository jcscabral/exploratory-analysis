#install.packages('FSelectorRcpp')

pacotes <- c("tidyverse","PerformanceAnalytics", "Hmisc", "psych", "ggplot2",
             "gridExtra", "moments", "goeveg", "SnowballC", "randomForest",
             "caret", "stats", "lsa")



if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

suppressMessages(library(e1071))

# library(tidyverse)
# library(data.table)
# library(ggplot2)
# #library(cowplot)
# library(gridExtra)
# #library(plotly)
# library(moments)
# library(goeveg)
# library(PerformanceAnalytics)
# library(psych)
# library(randomForest)
# library(caret)
# library(stats)
# library(lsa)

#############
# constants #
#############

set.seed(42)

SWIPE_FILE <- 'swipeData.csv'  
KEYBOARD_FILE <- 'keyboardData.csv'  
SENSORS_FILE <- 'sensorsData.csv'

PATH_PARENT <- '/home/jcscabral/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/'

# All data
FILES_A <- c('20240119', '20240122_20', '20240123', '20240124',
               '20240125', '20240126', '20240228', '20240310')
# Only data with correct keyboard sensors
FILES_B <- c('20240124', '20240125', '20240126', '20240228', '20240310')

KEYBOARD_LOGIN <- 5
KEYBOARD_AUTH <- 6
SWIPE_HOME <- 0
SWIPE_PIX_SEND <- 2 

TYPE_ACCELEROMETER <- 1
TYPE_MAGNETIC_FIELD <- 2
TYPE_GYROSCOPE <- 4

################
# load scripts #
################

source('fix_bug_20240119.R')
source('fix_bug_20240122_20.R')

#############
# functions #
#############

# Quadratic Mean
qm <- function(x){
  x.clean <- x[!is.na(x)]
  r <- sqrt(sum(x.clean ^ 2)/length(x.clean))
  return(r)
} 

# Range
rg <- function(x){
  r <-range(x, na.rm = T)
  return(r[2] - r[1])
}

#Percentil
pc <- function(x, p){
  x.na <- x[!is.na(x)]
  q <- quantile(x.na, probs = p)
  names(q) <- NULL
  return(q[1])
}

#Interquartile
qt <- function(x, nq){
  p <- c(0.0, 0.25, 0.5, 0.75,1 )
  x.na <- x[!is.na(x)]
  q <- quantile(x.na, probs = p)
  names(q) <- NULL
  return(q[nq + 1])
}

# Median Absolute Deviation
mi <- function(x){
  # median(|Yi – median(Yi|)
  x.na <- x[!is.na(x)]
  r <- median(abs(x.na - median(x.na)))
  return(r)
}

# Standard Error of Mean
se <- function(x){
  x.na <- x[!is.na(x)]
  r <- sd(x.na)/sqrt(length(x.na))
  return(r)
} 

# max deviation
mxdv <- function(x){
  x.na <- x[!is.na(x)]
  r <- max(sqrt((x.na - mean(x.na))^2))
  return(r)
}

# percentile of deviation
pcdv <- function(x, p){
  x.na <- x[!is.na(x)]
  r <- sqrt((x.na - mean(x.na))^2)
  return(pc(r, p))
}

# first
fs <- function(x){
  #x.na <- x[!is.na(x)]
  return(x[1])
}

# last
lt <- function(x){
  #x.na <- x[!is.na(x)]
  return(x[length(x)])
}

# magnitude vector
mg <- function(x, y){
  r <- norm(c(x,y), type="2")
  return(r)
}


mg3 <- function(x, y, z){
  r <- norm(c(x,y,z), type="2")
  return(r)
}

# euclidian distance
ec <- function(x1, x2, y1, y2){
  r <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(r)
}

dif <- function(x){
  x0 <- fs(x)    
  x1 <- lt(x)
  return(x1 - x0)
}

ang <- function(x1, x2, y1, y2){
  r <- atan2((y2 - y1), (x2 - x1))
  return(r)
}

avang <- function(x, y){
  y.diff <- diff(y)
  x.diff <- diff(x)
  tangs <- y.diff/x.diff
  r <-  mean(atan(tangs))
  return(r)
}


mh <- function(xyz){
  return(
    mahalanobis(
      xyz,
      colMeans(xyz),
      cov(xyz)
    )
  )
}
