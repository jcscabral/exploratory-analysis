# getCardTemplate <- function(dbase, 
#                             cols.ranked = NULL){
#   
#   card_chars <- dbase[cols.ranked]
#   card_chars[is.na(card_chars)] <- 0.0
#   
#   card_chars_ranked <- card_chars[cols_ranked]
#   results <- list("cols" = cols_ranked, "card" = card_chars_ranked)
#   return(results)
#   
# }

# getCardTemplate <- function(dbase, 
#                             cols.ranked = NULL){
#   
#   threshold <- length(ids_completed)/ (ncol(dbase) - 1)
#   
#   if (is.null(cols.ranked)){
#     cols.not.na <-names(
#       which(colSums(is.na(dbase)) == 0)
#     )
#     card_chars <- dbase[cols.not.na]
#     
#     # same weka result 
#     result <- FSelectorRcpp::information_gain(
#       formula = user_id ~ .,
#       data = card_chars ,
#       type = 'infogain'
#     )
#     cols_ranked  <- result %>% 
#       filter(importance >= threshold) %>% select(attributes)
#     cols_ranked <- c("user_id", as_vector(cols_ranked))
#     
#   } else{
#     cols_ranked <- cols.ranked
#     card_chars <- dbase[cols_ranked]
#     card_chars[is.na(card_chars)] <- 0.0
#   }
#   
#   card_chars_ranked <- card_chars[cols_ranked]
#   results <- list("cols" = cols_ranked, "card" = card_chars_ranked)
#   return(results)
#   
# }


# pc_train = PC_70
# ind <- round(length(ids) * pc_train)
# ids.train <-ids[1:ind]
# ids.val <-ids[(ind+1):length(ids)]
# 
# num_users <- length(ids.train)
# num_attackers <- length(ids.val)
# 
# # III- template creation (training)
# 
# # keyboard data
# keyboard.train <- keyboard_login %>% 
#   filter(user_id %in% ids.train)
# 
# 
# if (USE_SENSOR_DATA == T){
#   # sensor data
#   sensors.train <- sensors.login  %>% 
#     filter(user_id %in% ids.train)
# }
# 
# # template
# keyboard.train.grouped <- keyboard.train %>%
#   filter(action_number >= session_start,
#          action_number <= session_end) %>%
#    group_by(user_id, character, action_number)
# 
# keyboard.stats <- getCharStats(keyboard.train.grouped, secao = T)
# 
# if (USE_SENSOR_DATA == T){
#   sensors.train.grouped <- sensors.train %>%  
#     filter(action_number >= session_start,
#            action_number <= session_end) %>%
#     group_by(user_id, action_number)
#   
#   sensors.stats <- sensorStats(sensors.train.grouped)
#   keyboard.stats <- bind_cols(keyboard.stats, sensors.stats[c(3:ncol(sensors.stats))])
#   
# }
# 
# results.template <- getCardTemplate(keyboard.stats)
# card.train <- results.template$card
# 
# num_variables <- ncol(card.train)
#   
# # III - login
# keyboard.login <- keyboard.train %>%
#   filter(action_number == session_end + 1) %>%
#   group_by(user_id, character, action_number)
# keyboard.login.stats <- getCharStats(keyboard.login, secao = T)
# 
# if (USE_SENSOR_DATA == T){
#   sensor.login.grouped <- sensors.train %>%
#     filter(action_number == session_end + 1) %>%
#     group_by(user_id, action_number)
#   
#   sensors.login.stats <- sensorStats(sensor.login.grouped)
#   keyboard.login.stats <- bind_cols(keyboard.login.stats, 
#         sensors.login.stats[c(3:ncol(sensors.login.stats))])
#   
# }
# 
# 
# results.login <- getCardTemplate(keyboard.login.stats, results.template$cols)
# card.login <- results.login$card
# 
# # IV - validation
# 
# keyboard.val <- keyboard_login %>% 
#   filter(user_id %in% ids.val)
# 
# if (USE_SENSOR_DATA == T){
#   sensors.val <- sensors.login %>% 
#     filter(user_id %in% ids.val)
# }
# 
# keyboard.val.grouped <- keyboard.val %>%
#   group_by(user_id, character, action_number)
# 
# keyboard.val.stats <- getCharStats(keyboard.val.grouped, secao = T)
# 
# if (USE_SENSOR_DATA == T){
#   sensor.val.grouped <- sensors.val %>%
#     group_by(user_id, action_number)
#   
#   sensors.val.stats <- sensorStats(sensor.val.grouped)
#   keyboard.val.stats <- bind_cols(keyboard.val.stats, 
#                                   sensors.val.stats[c(3:ncol(sensors.val.stats))])
#   
# }
# 
# results.val <- getCardTemplate(keyboard.val.stats, results.template$cols)
# card.val <- results.val$card
# 
# 
# 
# # TRAIN #####
# 
# x_train <- card.train[,-1]
# y_train <- as.factor(card.train$user_id)
# 
# x_login <- card.login[,-1]
# y_login <- as.factor(card.login$user_id)
# 
# x_val <- card.val[,-1]
# y_val <- as.factor(card.val$user_id)
# 
# # randomForest class
# rf <- randomForest(x_train, y_train)
# 
# # with k-fold
# df.train <- card.train
# df.train$user_id <- paste("u", df.train$user_id, sep = "")
# control <- trainControl(method = "repeatedcv", 
#                         number = 10 ,
#                         repeats = 2,
#                         search = "grid",
#                         classProbs = T)
# 
# gridsearch <- train(
#   user_id ~ .,
#   data = df.train ,
#   method = 'rf',
#   ntree = 100,
#   trControl = control
# )
# 
# #plot(gridsearch)
# #gridsearch$results
# #gridsearch$bestTune
# 
# 
# # I. login try
# pred.prob.login <- predict(object = rf, x_login, type ="prob")
# #print(apply(pred.prob.login, 1, max, na.rm=TRUE)) # max by row
# pred.class.login <- predict(object = rf, x_login, type ="class")
# #print(pred.class.login)
# login_pos <- sum(pred.class.login == y_login)
# login_neg <- sum(pred.class.login != y_login)
# login_acc <- login_pos/length(pred.class.login)
# 
# # II. attack try
# pred.prob.val <- predict(object = rf, x_val, type ="prob")
# #max.probs <- apply(pred.prob.val, 1, max, na.rm=TRUE) # max by row
# pred.class.val <- predict(object = rf, x_val, type ="class")
# 
# attack_pos <- sum(as.character(pred.class.val) == as.character(y_val))
# attack_neg <- sum(as.character(pred.class.val) != as.character(y_val))
# 
# attack_acc <- attack_pos/length(pred.class.val)
# #sum(max.probs > 0.5)/length(max.probs)
# 
# # II. gridsearch
# 
# df.login.y <- paste("u", y_login, sep = "")
# 
# pred.prob.kfold <- predict(gridsearch, x_login, type = 'prob')
# #print(apply(pred.prob.kfold, 1, max, na.rm=TRUE)) # max by row
# pred.class.kfold <- predict(gridsearch, x_login)
# grd_login_pos <- sum(pred.class.kfold == df.login.y)
# grd_login_neg <- sum(pred.class.kfold != df.login.y)
# grd_login_acc <- grd_login_pos / length(df.login.y)
# 
# df.val.y <- paste("u", y_val, sep = "")
# 
# pred.prob.attack.kfold <- predict(gridsearch, x_val, type = 'prob')
# #print(apply(pred.prob.attack.kfold, 1, max, na.rm=TRUE)) # max by row
# pred.class.attack.kfold <- predict(gridsearch, x_val)
# 
# grd_attack_pos <- sum(pred.class.attack.kfold == df.val.y)
# grd_attack_neg <- sum(pred.class.attack.kfold != df.val.y)
# grd_attack_acc <- grd_attack_pos/length(df.val.y)
# 
# df.results[row,] <- c(pc_train, iter, num_users,
#                       num_attackers,
#                       num_variables,
#                       login_pos,
#                       login_neg,
#                       login_acc,
#                       attack_pos,
#                       attack_neg,
#                       attack_acc,
#                       grd_login_pos,
#                       grd_login_neg,
#                       grd_login_acc,
#                       grd_attack_pos,
#                       grd_attack_neg,
#                       grd_attack_acc
#                       )
# 
# row <- row + 1
# 
# 







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
