
# Reference: (Buriro, 2017)

USE_SENSOR_DATA <-F  

############
# KEYBOARD #
############

loadKeyboard <- function(loadFixed = T) {

  # Load ####

  keyboard.total = NULL
  
  for(file in FILES_B){
    
    path_day <- paste(PATH_PARENT, file, '/', sep = '')
    
    full_path <- paste(path_day, KEYBOARD_FILE, sep = '')
    keyboard <- fread(full_path)
    cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type',
                'character', 'pressure', 'x', 'y', 'uptime')  
    names(keyboard) <- cnames
    
    ids_completed <- unique(keyboard[keyboard$action_number == 7]$user_id)
    keyboard.completed <- keyboard %>% filter(user_id %in% ids_completed)
    
    # it started with user_id == 51
    if (file == '20240228'){
      keyboard[keyboard$user_id>50]
    }
    if(is.null(keyboard.total)){
      keyboard.total <- keyboard.completed
    }
    else{
      keyboard.total <-rbind(keyboard.total, keyboard.completed)
    }
  }
  
  keyboard <- NULL
  keyboard.completed <- NULL
  
  ids_completed <- unique(keyboard.total$user_id)
  
  keyboard <- keyboard.total %>% filter(user_id %in% ids_completed)
  keyboard.total =  NULL
  
  if (loadFixed == T){
    # fixed data
    keyboard <- rbind(keyboard, keyboard_data_20240119())
    keyboard <- rbind(keyboard, keyboard_data_20240122_20())
  }
  
  # end load ####
  
  # new variables ####

  keyboard$time_diff <- 0
  keyboard$press_time_diff <- 0
  keyboard$pressure_var <- 0.0 
  keyboard$space_x <- 0.0
  keyboard$space_y <- 0.0
  keyboard$velocity_x <- 0.0
  keyboard$velocity_y <- 0.0

  for (id in ids_completed){
    
    id_indices <- which(keyboard$user_id == id)
    
    current_action <- -1
    
    for (indice in id_indices){
      
      former_indice <- indice - 1
      
      # app_action
      if(current_action != keyboard[indice]$app_action){
        keyboard[indice]$time_diff <- 0
        current_action <- keyboard[indice]$app_action
      } else{
        keyboard[indice]$time_diff <- diff(
          keyboard[former_indice:indice]$uptime)
      }
      
      # event_type_pointer
      current_event_type <- keyboard[indice]$pointer_event_type
      if (current_event_type == 1){ 
        keyboard[indice]$press_time_diff <- 0
        keyboard[indice]$pressure_var <- 0.0
        keyboard[indice]$velocity_x <- 0.0
        keyboard[indice]$velocity_y <- 0.0
      } else {
        
        keyboard[indice]$press_time_diff <- diff(
          keyboard[former_indice:indice]$uptime)
        
        keyboard[indice]$pressure_var <- diff(
          keyboard[former_indice:indice]$pressure) /
          keyboard[indice]$press_time_diff
        
        keyboard[indice]$space_x <- diff(keyboard[former_indice:indice]$x)
        
        keyboard[indice]$space_y <- diff(keyboard[former_indice:indice]$y)
        
        keyboard[indice]$velocity_x <- diff(
          keyboard[former_indice:indice]$space_x)/
          keyboard[indice]$press_time_diff
        
        keyboard[indice]$velocity_y <- diff(
          keyboard[former_indice:indice]$space_y)/
          keyboard[indice]$press_time_diff
      }
    }
  }
  
  # end new ####
  
  return(keyboard)

}

keyboard <- loadKeyboard(F)
ids_completed <- sort(unique(keyboard$user_id))

# plot #######

keyboard.login <- keyboard %>% filter(
  app_action %in% c(KEYBOARD_LOGIN))

keyboard.user <- keyboard.login[(keyboard.login$user_id == 74)]

p0 <- ggplot(keyboard.user, 
  aes(uptime, pressure)) + geom_point()

keyboard.user <- NULL
keyboard.login <- NULL

# end plot####

keyboard_login <- keyboard %>%
  filter(app_action == 5 | app_action == 6)

##########
# SENSOR #
##########

if (USE_SENSOR_DATA == T){
  sensors.login <- loadSensorsLogin()
}

# --------------------------------------------------------------
# teh2019 
# SOF second-order features

# Minimum (mn), Maximum (mx), Arithmetic
# Mean (am), Quadratic Mean (qm), Harmonic Mean (hm),
# Geometric Mean (gm), Median (md), Range (rg), Variance
# (vr), Standard Deviation (sd), Skewness (sk), Kurtosis (ku),
# First Quartile (fq), Third Quartile (tq), Interquartile Range
# (ir), Mean Absolute Deviation (ma), Median Absolute Deviation (mi),
# Coefficient of Variation (cv), and Standard Error of Mean (se).
# --------------------------------------------------------------

###############
# STATS CARDS #
###############

# stats functions ####

# keyboard stats
keyboardStats <- function(dfdata, digit){
  
  card_stats <- dfdata %>%
    filter(character == digit) %>%
    mutate(
      fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
      dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
    ) %>%
    summarise(
      # DT Dwelling Time
      dt_md = median(dwell_time, na.rm = T),
      dt_fq = qt(dwell_time, 1),
      dt_tq = qt(dwell_time, 3),
      dt_ir = IQR(dwell_time, na.rm = T),
      dt_ma = mean(dwell_time, na.rm = T),
      dt_vr = var(dwell_time, na.rm = T),
      dt_sd = sd(dwell_time, na.rm = T),
      dt_cv = cv(dwell_time, na.rm = T),
      dt_se = se(dwell_time),
      dt_mn = min(dwell_time, na.rm = T) ,
      dt_mx = max(dwell_time, na.rm = T) ,
      dt_qm = qm(dwell_time) ,
      dt_rg = rg(dwell_time) ,
      dt_sk = skewness(dwell_time) ,
      dt_ku = kurtosis(dwell_time) ,
      # FT Flight Time
      ft_md = median(fly_time, na.rm = T),
      ft_fq = qt(fly_time, 1),
      ft_tq = qt(fly_time, 3),
      ft_ir = IQR(fly_time, na.rm = T),
      ft_am = mean(fly_time, na.rm = T),
      ft_vr = var(fly_time, na.rm = T),
      ft_sd = sd(fly_time, na.rm = T),
      ft_cv = cv(fly_time, na.rm = T),
      ft_se = se(fly_time),
      ft_mn = min(fly_time , na.rm = T) ,
      ft_mx = max(fly_time, na.rm = T) ,
      dt_qm = qm(fly_time) ,
      ft_rg = rg(fly_time) ,
      ft_sk = skewness(fly_time) ,
      ft_ku = kurtosis(fly_time) ,
      # Pressure
      ps_md = median(pressure),
      ps_fq = qt(pressure, 1),
      ps_tq = qt(pressure, 3),
      ps_ir = IQR(pressure, na.rm = T),
      ps_am = mean(pressure, na.rm = T),
      ps_var = var(pressure, na.rm = T),
      ps_sd = sd(pressure, na.rm = T),
      ps_cv = cv(pressure, na.rm = T),
      ps_se = se(pressure),
      ps_mn = min(pressure),
      ps_mx = max(pressure),
      ps_qm = qm(pressure) ,
      ps_rg = rg(pressure) ,
      ps_sk = skewness(pressure) ,
      ps_ku = kurtosis(pressure) ,
      # Spacial x
      x_md = median(x, na.rm = T),
      x_fq = qt(x, 1),
      x_tq = qt(x, 3),
      x_ir = IQR(x, na.rm = T),
      x_am = mean(x, na.rm = T),
      x_vr = var(x, na.rm = T),
      x_sd = sd(x, na.rm = T),
      x_cv = cv(x, na.rm = T),
      x_se = se(x),
      x_mn = min(x, na.rm = T) ,
      x_mx = max(x, na.rm = T) ,
      x_qm = qm(x) ,
      x_rg = rg(x),
      x_sk =  skewness(x) ,
      x_ku = kurtosis(x) ,
      # Spacial y
      y_md = median(y, na.rm = T),
      y_fq = qt(y, 1),
      y_tq = qt(y, 3),
      y_ir = IQR(y, na.rm = T),
      y_am = mean(y, na.rm = T),
      y_vr = var(y),
      y_sd = sd(y, na.rm = T),
      y_cv = cv(y, na.rm = T),
      y_se = se(y),
      y_mn = min(y, na.rm = T) ,
      y_mx = max(y, na.rm = T) ,
      y_qm = qm(y),
      y_rg =  rg(y) ,
      y_sk =  skewness(y) ,    
      y_ku = kurtosis(y)
    )
  return(card_stats)
}

# sensor stats
sensorStats <- function(dfdata){
  
  sensor_stats <- dfdata %>%
    summarise(
      # Spacial x
      sx_md = median(x, na.rm = T),
      sx_fq = qt(x, 1),
      sx_tq = qt(x, 3),
      sx_ir = IQR(x, na.rm = T),
      sx_am = mean(x, na.rm = T),
      sx_vr = var(x, na.rm = T),
      sx_sd = sd(x, na.rm = T),
      sx_cv = cv(x, na.rm = T),
      sx_se = se(x),
      sx_mn = min(x, na.rm = T),
      sx_mx = max(x, na.rm = T),
      sx_qm = qm(x),
      sx_rg = rg(x),
      sx_sk = skewness(x),
      sx_ku = kurtosis(x),
      sx_sm = sum(x),
      
      # Spacial y
      sy_md = median(y, na.rm = T),
      sy_fq = qt(y, 1),
      sy_tq = qt(y, 3),
      sy_ir = IQR(y, na.rm = T),
      sy_am = mean(y, na.rm = T),
      sy_vr = var(y),
      sy_sd = sd(y, na.rm = T),
      sy_cv = cv(y, na.rm = T),
      sy_se = se(y),
      sy_mn = min(y, na.rm = T) ,
      sy_mx = max(y, na.rm = T) ,
      sy_qm = qm(y),
      sy_rg = rg(y) ,
      sy_sk = skewness(y) ,    
      sy_ku = kurtosis(y),
      sy_sm = sum(y),
      
      # Spacial z
      sz_md = median(z, na.rm = T),
      sz_fq = qt(z, 1),
      sz_tq = qt(z, 3),
      sz_ir = IQR(z, na.rm = T),
      sz_am = mean(z, na.rm = T),
      sz_vr = var(z),
      sz_sd = sd(z, na.rm = T),
      sz_cv = cv(z, na.rm = T),
      sz_se = se(z),
      sz_mn = min(z, na.rm = T) ,
      sz_mx = max(z, na.rm = T) ,
      sz_qm = qm(z),
      sz_rg = rg(z) ,
      sz_sk = skewness(z) ,    
      sz_ku = kurtosis(z),
      sz_sm = sum(z),
      # all vectors 
      sdp_mg = mg3(x,y, z)
      
    )
  return(sensor_stats)
}

# end card stats ####

# Renaming chars ####
col_char5 <- function(x){
  return(paste(x, "_5", '', sep= ''))
}
col_char4 <- function(x){
  return(paste(x, "_4", '', sep= ''))
}
col_char8 <- function(x){
  return(paste(x, "_8", '', sep= ''))
}
col_char3 <- function(x){
  return(paste(x, "_3", '', sep= ''))
}
col_char1 <- function(x){
  return(paste(x, "_1", '', sep= ''))
}
col_char7 <- function(x){
  return(paste(x, "_7", '', sep= ''))
}
col_char9 <- function(x){
  return(paste(x, "_9", '', sep= ''))
}
col_char0 <- function(x){
  return(paste(x, "_0", '', sep= ''))
}
# end ####

# data grouped by param
getCharStats <- function(dbase, secao = T){
  
  groupby <- c("user_id")
  ini.range <- 3
  if (secao == T){
    groupby <- c("user_id","action_number")
    ini.range <- 4
  }
  desc_login_char_5 <- keyboardStats(dbase, 5)
  desc_login_char_4 <- keyboardStats(dbase, 4)
  desc_login_char_8 <- keyboardStats(dbase, 8)
  desc_login_char_3 <- keyboardStats(dbase, 3)
  desc_login_char_1 <- keyboardStats(dbase, 1)
  desc_login_char_7 <- keyboardStats(dbase, 7)
  desc_login_char_9 <- keyboardStats(dbase, 9)
  desc_login_char_0 <- keyboardStats(dbase, 0)
  
  col.names <- names(desc_login_char_5)
  col.names <- col.names[ini.range:length(col.names)]
  
  desc_login_char_5 <- desc_login_char_5 %>% 
    rename_with(.fn = col_char5, .cols = col.names)
  desc_login_char_4 <- desc_login_char_4 %>% 
    rename_with(.fn = col_char4, .cols = col.names)
  desc_login_char_8 <- desc_login_char_8 %>% 
    rename_with(.fn = col_char8, .cols = col.names)
  desc_login_char_3 <- desc_login_char_3 %>% 
    rename_with(.fn = col_char3, .cols = col.names)
  desc_login_char_1 <- desc_login_char_1 %>% 
    rename_with(.fn = col_char1, .cols = col.names)
  desc_login_char_7 <- desc_login_char_7 %>% 
    rename_with(.fn = col_char7, .cols = col.names)
  desc_login_char_9 <- desc_login_char_9 %>% 
    rename_with(.fn = col_char9, .cols = col.names)
  desc_login_char_0 <- desc_login_char_0 %>% 
    rename_with(.fn = col_char0, .cols = col.names)
  
  desc_login_chars <- inner_join(
    desc_login_char_5, desc_login_char_4, by = groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_8, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_3, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_1, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_7, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_9, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_0, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  
  return(desc_login_chars)
  
}

# w/ variable's selection
getCardTemplate <- function(dbase, cols.ranked = NULL ){
  
  threshold <- length(ids_completed)/ (ncol(dbase) - 1)
  
  if (is.null(cols.ranked)){
    cols.not.na <-names(
      which(colSums(is.na(dbase)) == 0)
    )
    card_chars <- dbase[cols.not.na]
    
    # same weka result 
    result <- FSelectorRcpp::information_gain(
      formula = user_id ~ .,
      data = card_chars ,
      type = 'infogain'
    )
    cols_ranked  <- result %>% 
      filter(importance >= threshold) %>% select(attributes)
    cols_ranked <- c("user_id", as_vector(cols_ranked))
    
  }
  else{
    cols_ranked <- cols.ranked
    card_chars <- dbase[cols_ranked]
    card_chars[is.na(card_chars)] <- 0.0
  }
  
  card_chars_ranked <- card_chars[cols_ranked]
  results <- list("cols" = cols_ranked, "card" = card_chars_ranked)
  return(results)
  
}


# CROSS VALIDATION #####


# I. set sessions 

NUM_SESSIONS <- 5
iter <- 1
session_start <- 1 + iter
session_end <- NUM_SESSIONS + iter


# II - split data: test with only real unknown intruder 

set.seed(42)
ids <- sample(ids_completed)

PC_30 <- 0.3
PC_50 <- 0.5
PC_70 <- 0.7
PC_90 <- 0.9

ind <- round(length(ids) * PC_90)
ids.train <-ids[1:ind]
ids.val <-ids[(ind+1):length(ids)]


# III- template creation (training)

# keyboard data
keyboard.train <- keyboard_login %>% 
  filter(user_id %in% ids.train)


if (USE_SENSOR_DATA == T){
  # sensor data
  sensors.train <- sensors.login  %>% 
    filter(user_id %in% ids.train)
}

# template
keyboard.train.grouped <- keyboard.train %>%
  filter(action_number >= session_start,
         action_number <= session_end) %>%
   group_by(user_id, character, action_number)

keyboard.stats <- getCharStats(keyboard.train.grouped, secao = T)

if (USE_SENSOR_DATA == T){
  sensors.train.grouped <- sensors.train %>%  
    filter(action_number >= session_start,
           action_number <= session_end) %>%
    group_by(user_id, action_number)
  
  sensors.stats <- sensorStats(sensors.train.grouped)
  keyboard.stats <- bind_cols(keyboard.stats, sensors.stats[c(3:ncol(sensors.stats))])
  
}

results.template <- getCardTemplate(keyboard.stats)
card.train <- results.template$card


# III - login
keyboard.login <- keyboard.train %>%
  filter(action_number == session_end + 1) %>%
  group_by(user_id, character, action_number)
keyboard.login.stats <- getCharStats(keyboard.login, secao = T)

if (USE_SENSOR_DATA == T){
  sensor.login.grouped <- sensors.train %>%
    filter(action_number == session_end + 1) %>%
    group_by(user_id, action_number)
  
  sensors.login.stats <- sensorStats(sensor.login.grouped)
  keyboard.login.stats <- bind_cols(keyboard.login.stats, 
        sensors.login.stats[c(3:ncol(sensors.login.stats))])
  
}


results.login <- getCardTemplate(keyboard.login.stats, results.template$cols)
card.login <- results.login$card

# IV - validation

keyboard.val <- keyboard_login %>% 
  filter(user_id %in% ids.val)

if (USE_SENSOR_DATA == T){
  sensors.val <- sensors.login %>% 
    filter(user_id %in% ids.val)
}

keyboard.val.grouped <- keyboard.val %>%
  group_by(user_id, character, action_number)

keyboard.val.stats <- getCharStats(keyboard.val.grouped, secao = T)

if (USE_SENSOR_DATA == T){
  sensor.val.grouped <- sensors.val %>%
    group_by(user_id, action_number)
  
  sensors.val.stats <- sensorStats(sensor.val.grouped)
  keyboard.val.stats <- bind_cols(keyboard.val.stats, 
                                  sensors.val.stats[c(3:ncol(sensors.val.stats))])
  
}

results.val <- getCardTemplate(keyboard.val.stats, results.template$cols)
card.val <- results.val$card



# TRAIN #####

x_train <- card.train[,-1]
y_train <- as.factor(card.train$user_id)

x_login <- card.login[,-1]
y_login <- as.factor(card.login$user_id)

x_val <- card.val[,-1]
y_val <- as.factor(card.val$user_id)

# randomForest class
rf <- randomForest(x_train, y_train)

# with k-fold
df.train <- card.train
df.train$user_id <- paste("u", df.train$user_id, sep = "")
control <- trainControl(method = "repeatedcv", 
                        number = 10 ,
                        repeats = 2,
                        search = "grid",
                        classProbs = T)

gridsearch <- train(
  user_id ~ .,
  data = df.train ,
  method = 'rf',
  ntree = 100,
  trControl = control
)

plot(gridsearch)
gridsearch$results
gridsearch$bestTune

# I. just to realize train prob (unreal)
# pred.prob.train <- predict(object = rf, x_train, type ="prob")
# apply(pred.prob.train, 1, max, na.rm=TRUE) # max by row
# pred.class.train <- predict(object = rf, x_train, type ="class")
# print(pred.class.train)

# II. login try
pred.prob.login <- predict(object = rf, x_login, type ="prob")
#print(apply(pred.prob.login, 1, max, na.rm=TRUE)) # max by row
pred.class.login <- predict(object = rf, x_login, type ="class")
#print(pred.class.login)
sum(pred.class.login == y_login)/length(pred.class.login)

# III. attack try
pred.prob.val <- predict(object = rf, x_val, type ="prob")
#max.probs <- apply(pred.prob.val, 1, max, na.rm=TRUE) # max by row
pred.class.val <- predict(object = rf, x_val, type ="class")
sum(as.character(pred.class.val) == as.character(y_val))/
  length(pred.class.val)
#sum(max.probs > 0.5)/length(max.probs)

# IV. gridsearch

df.login.y <- paste("u", y_login, sep = "")

pred.prob.kfold <- predict(gridsearch, x_login, type = 'prob')
print(apply(pred.prob.kfold, 1, max, na.rm=TRUE)) # max by row
pred.class.kfold <- predict(gridsearch, x_login)
sum(pred.class.kfold == df.login.y)/length(df.login.y)

df.val.y <- paste("u", y_val, sep = "")

pred.prob.attack.kfold <- predict(gridsearch, x_val, type = 'prob')
print(apply(pred.prob.attack.kfold, 1, max, na.rm=TRUE)) # max by row
pred.class.attack.kfold <- predict(gridsearch, x_val)
sum(pred.class.attack.kfold == df.val.y)/length(df.val.y)















# 
# 































# ######################
# # Feature Extraction #
# ######################
# 
# cols.not.na <-names(
#   which(colSums(is.na(desc_login_chars)) == 0)
#   )
# # named as a card
# card_chars <- desc_login_chars[cols.not.na]
# 
# # Correlation #
# 
# rho <- Hmisc::rcorr(
#   as.matrix(card_chars[2:nrow(card_chars)]), type = "pearson"
# )
# corr_coef <- rho$r
# corr_sig <- round(rho$P, 5)
# 
# chart.Correlation(card_chars[2:11])
# chart.Correlation(card_chars[12:21])
# chart.Correlation(card_chars[22:31])
# chart.Correlation(card_chars[32:nrow(card_chars)])
# 
# 
# # split into train and test beforehand
# 
# #shuffle ids
# set.seed(42)
# ids <- card_chars$user_id
# ids <- sample(ids)
# ind <- round(length(ids) * 0.90)
# 
# ids.train <-ids[1:ind]
# ids.val <-ids[(ind+1):length(ids)]
# 
# # Information Gain Attribute Evaluator (IGAE) Weka
# 
# card_chars.train <- card_chars %>% filter(user_id %in% ids.train)
# card_chars.test <- card_chars %>% filter(user_id %in% ids.val)
# 
# # save csv file
# 
# setwd('/home/jcscabral/Studies/UspEsalq/Tcc/projeto/pesquisa/code')
# write_csv(card_chars.train, 'card_keyboard.csv')
# 
# 
# # Ranked attributes by Weka InfoGainAttributeVal:
# 
# # Buriro's # threshold:
# #   number os users / number os features
# 
# threshold.test <- length(ids)/ (ncol(card_chars.train) - 1)
# 
# 
# # pc_train  | weka_igae_threshold
# # 90%       | 0.09302326
# 
# 
# # same weka result
# result <- FSelectorRcpp::information_gain(
#   formula = user_id ~ .,
#   data = card_chars.train ,
#   type = 'infogain'
# )
# 
# cols_ranked  <- result %>%
#   filter(importance >= threshold.test) %>% select(attributes)
# 
# 
# # first
# # cols_ranked <- c("user_id",
# #   "y_sd_5", "y_vr_5", "ps_qm_8", "y_rg_5",
# #   "ps_md_0", "ps_am_0", "ps_am_3", "y_mn_9", "y_fq_9", "dt_ir_5",
# #   "y_cv_9", "dt_qm_1", "dt_qm_7")
# 
# cols_ranked <- c("user_id", as_vector(cols_ranked))
# card_chars_ranked <- card_chars[cols_ranked]
# 
# 
# 
# ###############
# # CLASSIFIERS #
# ###############
# 
# # Naive Bayes ) (TODO?)
# # NeuralNet (NN) (TODO?)
# # RF - best results according the paper
# 
# # ----------------- #
# # RF Random Forrest #
# # ----------------- #
# 
# 
# x_train <- card_chars_ranked %>% filter(user_id %in% ids.train)
# y_train <- as.factor(x_train$user_id)
# x_train <- x_train[,-1]
# 
# x_test <- card_chars_ranked %>% filter(user_id %in% ids.val)
# y_test <- as.factor(x_test$user_id)
# x_test <- x_test[,-1]
# 
# 
# rf <- randomForest(x_train, y_train)
# 
# # get probs instead of argmax
# results <- predict(object = rf, x_test, type ="class")
# print(results)
# results <- predict(object = rf, x_test, type ="prob")
# print(apply(results, 1, max, na.rm=TRUE)) # max by row
# 
# 
# # just to realize train prob
# results <- predict(object = rf, x_train, type ="prob")
# apply(results, 1, max, na.rm=TRUE) # max by row
# results <- predict(object = rf, x_train, type ="class")
# 
# 
# # Attack by section (More real)
# # Intruder somehow trained along sections (1,2...7)
# # Split by number action (section)
# 
# card_chars_ranked_section <- desc_login_chars_section[cols_ranked]
# card_chars_ranked_section[is.na(card_chars_ranked_section)] <- 0.0
# 
# x_test_section <- card_chars_ranked_section %>%
#   filter(user_id %in% ids.val)
# 
# results <- predict(object = rf, x_test_section, type ="prob")
# apply(results, 1, max, na.rm=TRUE) # max by row
# 
# ### Results ###
# # NOT REAL
# # 1. Considering a real threshold 0,5:
# #   high: 100% correct training and testing.
# #   low:  small dataset.
# #   can we improve that with sensors?
# 
# 
# # FIVE sections are used to build the template
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ########
# # Cpf  #
# ########
# 
# # Future studies
# 
# ##########
# # Money  #
# ##########
# 
# # Future studies
# 
# #################
# # Cpf and Money #
# #################
# 
# # Future studies
# 
# 
# # PCA
# # Before select variables, each data user is transformed
# # in order to preserve its specific features.
# 
# # fatorial <- principal(card_chars[2:11] ,
# #           nfactors = length(card_chars[2:11]),
# #           rotate = "none",
# #           scores = T)
# #
# # eigenvalues <- round(fatorial$values, 5)
# # var_shared <-as.data.frame(fatorial$Vaccounted)
# # fatorials_scores <- as.data.frame(fatorial$weights)
# # factors <- fatorial$scores
# #
# # rho.factors <- Hmisc::rcorr(as.matrix(factors), type= "pearson")
# # k <- sum(eigenvalues > 1)
# # fatorial2 <- principal(card_chars[2:11] ,
# #             nfactors = k,
# #             rotate = "none",
# #             scores = T)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###########
# # SENSORS #
# ###########
# 
# files.last <- c('20240124', '20240125', '20240126')
# 
# sensors.total = NULL
# 
# for(file in files.last){
# 
#   path_day <- paste(path_parent, file, '/', sep = '')
# 
#   # sensors #
#   full_path <- paste(path_day, sensors_file, sep = '')
#   sensors <- fread(full_path)
#   cnames <- c('user_id', 'action_number', 'app_action', 'sensor_type',
#               'x', 'y', 'z', 'timestamp')
#   names(sensors) <- cnames
# 
#   ids_completed <- unique(sensors[sensors$action_number == 7]$user_id)
#   sensors.completed <- sensors %>% filter(user_id %in% ids_completed)
# 
#   if(is.null(sensors.total)){
#     sensors.total <- sensors.completed
#   }
#   else{
#     sensors.total <-rbind(sensors.total, sensors.completed)
#   }
# }
# 
# sensors <- NULL
# sensors.completed <- NULL
# 
# ids_completed <- unique(sensors.total$user_id)
# length(ids_completed) #31
# 
# sensors <- sensors.total %>% filter(user_id %in% ids_completed)
# sensors.total =  NULL
# 
# sensors$w <- sqrt((sensors$x^2) + (sensors$y^2) + (sensors$z^2))
# 
# sensors$timestamp <- sensors$timestamp/1000000
# # 139332168 | 237742206570000
# 
# # 40
# df <- sensors[(sensors$user_id == 40) & (sensors$sensor_type == 1),] %>%
#   select(c(9, 8))
# write.csv(df, '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors40')
# 
# 
# # 41
# write.csv(
#   sensors[(sensors$user_id == 41) & (sensors$sensor_type == 1),] %>%
#        select(c(9, 8)),
#   '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors41')
# 
# # 42
# write.csv(
#   sensors[(sensors$user_id == 42) & (sensors$sensor_type == 1),] %>%
#     select(c(9, 8)),
#   '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors42')
# 
# # 50
# write.csv(
#   sensors[(sensors$user_id == 50) & (sensors$sensor_type == 1),] %>%
#     select(c(9, 8)),
#   '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors50')
# 
# 
# # 50
# write.csv(
#   sensors[(sensors$user_id == 50) & (sensors$sensor_type == 1),] %>%
#     select(c(9, 8)),
#   '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors50')
