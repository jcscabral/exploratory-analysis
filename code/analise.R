#install.packages('textshaping')
#install.packages("tidyverse")

library(tidyverse)
library(data.table)

###############################################################################

# Domínio

SWIPE_HOME <- 0
SWIPE_HOME_BUTTON <- 1
SWIPE_PIX_SEND <- 2 
SWIPE_PIX_SEND_BUTTON <- 3
SWIPE_PIX_RECEIVE_BUTTON <- 4
KEYBOARD_LOGIN <- 5
KEYBOARD_AUTH <- 6
KEYBOARD_PIX_MONEY <- 7
KEYBOARD_PIX_CPF <- 8

app_actions = factor(
  c(SWIPE_HOME, SWIPE_HOME_BUTTON, SWIPE_PIX_SEND,
    SWIPE_PIX_SEND_BUTTON, SWIPE_PIX_RECEIVE_BUTTON, KEYBOARD_LOGIN,
    KEYBOARD_AUTH, KEYBOARD_PIX_MONEY, KEYBOARD_PIX_CPF)
)

###############################################################################

# days 19,20,22 have bugs 


path_parent <- '/home/jcscabral/Studies/UspEsalq/pesquisa/20240123/'
file <- 'keyboardData.csv'  
path_csv <- paste(path_parent, file, sep = '')
keyboard_bug <- fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type',
            'character', 'pressure', 'x', 'y', 'uptime')  
names(keyboard_bug) <- cnames
table(keyboard_bug$app_action)
table(keyboard_bug$action_number)


path_parent <- '/home/jcscabral/Studies/UspEsalq/pesquisa/20240122/fixBug/'
file <- 'keyboardData.csv'  
path_csv <- paste(path_parent, file, sep = '')
keyboard_bug <- fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type',
            'character', 'pressure', 'x', 'y', 'uptime')  
names(keyboard_bug) <- cnames
table(keyboard_bug$app_action)

path_parent <- '/home/jcscabral/Studies/UspEsalq/pesquisa/20240119/'

# SENSORS
file = 'sensorsData.csv'  
path_csv = paste(path_parent,file, sep = '')
sensors  = fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'sensor_type', 'x', 'y', 'z', 'timestamp')  
names(sensors) <- cnames

# KEYBOARD
file = 'keyboardData.csv'  
path_csv = paste(path_parent,file, sep = '')
keyboard  = fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 'character', 'pressure', 'x', 'y', 'uptime')  
names(keyboard) <- cnames

# SWIPE
file = 'swipeData.csv'  
path_csv = paste(path_parent,file, sep = '')
swipe = fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 'pressure', 'x', 'y', 'uptime')  
names(swipe) <- cnames

# BUTTON
file = 'buttonData.csv'  
path_csv = paste(path_parent,file, sep = '')
button = fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 'pressure', 'x', 'y', 'uptime')  
names(button) <- cnames

#install.packages('DBI')
#install.packages('RSQLite')
#update.packages()
#library(DBI)
#library(RSQLite)
#path_db = "/Studies/UspEsalq/pesquisa/room/user_database"
#mydb <- dbConnect(RSQLite::SQLite(), path_db)
#dbDisconnect(mydb)

ids <- c(1,2,3,6,7,8,9,10,11,12,13,15,17,18,19,20,24,25,26,27)

# apenas dados concluídos
sensors <- sensors %>% filter(user_id %in% ids)
keyboard <- keyboard %>% filter(user_id %in% ids)
swipe <- swipe %>% filter(user_id %in% ids)
button <- button %>% filter(user_id %in% ids)

table(sensors$action_number)
table(keyboard$action_number)
table(swipe$action_number)
table(button$action_number)

# bug na primeira sessão 

head(sensors)
head(keyboard)
head(swipe)
head(button)

# SWIPE #

summary(swipe)

table(swipe$app_action) # swipe home, swipe pix home
table(swipe$pointer_event_type) # start, move, exit

swipe %>% group_by(action_number) %>%
  summarise(
   mean_pressure =  mean(pressure) 
  )
  



