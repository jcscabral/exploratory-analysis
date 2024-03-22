
#########
# SWIPE #
#########


#############
# functions #
#############

cardStats <- function(dfdata){
  
  desc_action <- dfdata %>%
    mutate(
      press_1 = ifelse(pointer_event_type == 1, pressure, NA) ,
      press_2 = ifelse(pointer_event_type == 2, pressure, NA) ,
      press_3 = ifelse(pointer_event_type == 3, pressure, NA)
    ) %>%
    summarise(
      
      # pressure
      ps_fs = fs(pressure),
      ps_ls = lt(pressure),
      ps_md = median(pressure, na.rm = T),
      ps_fq = qt(pressure, 1),
      ps_tq = qt(pressure, 3),
      ps_ir = IQR(pressure, na.rm = T),
      ps_am = mean(pressure, na.rm = T),
      ps_vr = var(pressure, na.rm = T),
      ps_sd = sd(pressure, na.rm = T),
      ps_mxsd = mxdv(pressure),
      ps_20sd = pcdv(pressure, .2),
      ps_50sd = pcdv(pressure, .5),
      ps_80sd = pcdv(pressure, .8),
      ps_cv = cv(pressure, na.rm = T),
      ps_se = se(pressure),
      ps_mn = min(pressure, na.rm = T),
      ps_mx = max(pressure, na.rm = T),
      ps_qm = qm(pressure) ,
      ps_rg = rg(pressure) ,
      ps_sk = skewness(pressure) ,
      ps_ku = kurtosis(pressure) ,
      
      # delta pressure
      dp_fs = fs(delta_press),
      dp_ls = lt(delta_press),
      dp_md = median(delta_press, na.rm = T),
      dp_fq = qt(delta_press, 1),
      dp_tq = qt(delta_press, 3),
      dp_ir = IQR(delta_press, na.rm = T),
      dp_am = mean(delta_press, na.rm = T),
      dp_vr = var(delta_press, na.rm = T),
      dp_sd = sd(delta_press, na.rm = T),
      dp_mxsd = mxdv(delta_press),
      dp_20sd = pcdv(delta_press, .2),
      dp_50sd = pcdv(delta_press, .5),
      dp_80sd = pcdv(delta_press, .8),
      dp_cv = cv(delta_press, na.rm = T),
      dp_se = se(delta_press),
      dp_mn = min(delta_press, na.rm = T),
      dp_mx = max(delta_press, na.rm = T),
      dp_qm = qm(delta_press) ,
      dp_rg = rg(delta_press) ,
      dp_sk = skewness(delta_press) ,
      dp_ku = kurtosis(delta_press) ,

      # delta pressure 1 (start)
      dp1_fs = fs(press_1),
      dp1_ls = lt(press_1),
      dp1_md = median(press_1, na.rm = T),
      dp1_fq = qt(press_1, 1),
      dp1_tq = qt(press_1, 3),
      dp1_ir = IQR(press_1, na.rm = T),
      dp1_am = mean(press_1, na.rm = T),
      dp1_vr = var(press_1, na.rm = T),
      dp1_sd = sd(press_1, na.rm = T),
      dp1_mxsd = mxdv(press_1),
      dp1_20sd = pcdv(press_1, .2),
      dp1_50sd = pcdv(press_1, .5),
      dp1_80sd = pcdv(press_1, .8),
      dp1_cv = cv(press_1, na.rm = T),
      dp1_se = se(press_1),
      dp1_mn = min(press_1, na.rm = T),
      dp1_mx = max(press_1, na.rm = T),
      dp1_qm = qm(press_1) ,
      dp1_rg = rg(press_1) ,
      dp1_sk = skewness(press_1) ,
      dp1_ku = kurtosis(press_1) ,

      # delta pressure 2 (move)
      dp2_fs = fs(press_2),
      dp2_ls = lt(press_2),
      dp2_md = median(press_2, na.rm = T),
      dp2_fq = qt(press_2, 1),
      dp2_tq = qt(press_2, 3),
      dp2_ir = IQR(press_2, na.rm = T),
      dp2_am = mean(press_2, na.rm = T),
      dp2_vr = var(press_2, na.rm = T),
      dp2_sd = sd(press_2, na.rm = T),
      dp2_mxsd = mxdv(press_2),
      dp2_20sd = pcdv(press_2, .2),
      dp2_50sd = pcdv(press_2, .5),
      dp2_80sd = pcdv(press_2, .8),
      dp2_cv = cv(press_2, na.rm = T),
      dp2_se = se(press_2),
      dp2_mn = min(press_2, na.rm = T),
      dp2_mx = max(press_2, na.rm = T),
      dp2_qm = qm(press_2) ,
      dp2_rg = rg(press_2) ,
      dp2_sk = skewness(press_2) ,
      dp2_ku = kurtosis(press_2) ,

      # delta pressure 3 (end)
      dp3_fs = fs(press_3),
      dp3_ls = lt(press_3),
      dp3_md = median(press_3, na.rm = T),
      dp3_fq = qt(press_3, 1),
      dp3_tq = qt(press_3, 3),
      dp3_ir = IQR(press_3, na.rm = T),
      dp3_am = mean(press_3, na.rm = T),
      dp3_vr = var(press_3, na.rm = T),
      dp3_sd = sd(press_3, na.rm = T),
      dp3_mxsd = mxdv(press_3),
      dp3_20sd = pcdv(press_3, .2),
      dp3_50sd = pcdv(press_3, .5),
      dp3_80sd = pcdv(press_3, .8),
      dp3_cv = cv(press_3, na.rm = T),
      dp3_se = se(press_3),
      dp3_mn = min(press_3, na.rm = T),
      dp3_mx = max(press_3, na.rm = T),
      dp3_qm = qm(press_3) ,
      dp3_rg = rg(press_3) ,
      dp3_sk = skewness(press_3) ,
      dp3_ku = kurtosis(press_3) ,
      # 
      # Spacial x
      x_fs = fs(x),
      x_ls = lt(x),
      x_md = median(x, na.rm = T),
      x_fq = qt(x, 1),
      x_tq = qt(x, 3),
      x_ir = IQR(x, na.rm = T),
      x_am = mean(x, na.rm = T),
      x_vr = var(x, na.rm = T),
      x_sd = sd(x, na.rm = T),
      x_mxsd = mxdv(x),
      x_20sd = pcdv(x, .2),
      x_50sd = pcdv(x, .5),
      x_80sd = pcdv(x, .8),
      x_cv = cv(x, na.rm = T),
      x_se = se(x),
      x_mn = min(x, na.rm = T) ,
      x_mx = max(x, na.rm = T) ,
      x_qm = qm(x) ,
      x_rg = rg(x),
      x_sk = skewness(x) ,
      x_ku = kurtosis(x) ,
      # 
      # Spacial y
      y_fs = fs(y),
      y_ls = lt(y),
      y_md = median(y, na.rm = T),
      y_fq = qt(y, 1),
      y_tq = qt(y, 3),
      y_ir = IQR(y, na.rm = T),
      y_am = mean(y, na.rm = T),
      y_vr = var(y),
      y_sd = sd(y, na.rm = T),
      y_mxsd = mxdv(y),
      y_20sd = pcdv(y, .2),
      y_50sd = pcdv(y, .5),
      y_80sd = pcdv(y, .8),
      y_cv = cv(y, na.rm = T),
      y_se = se(y),
      y_mn = min(y, na.rm = T) ,
      y_mx = max(y, na.rm = T) ,
      y_qm = qm(y),
      y_rg = rg(y) ,
      y_sk = skewness(y) ,
      y_ku = kurtosis(y) ,
      # 
      # # Space x
      sx_fs = fs(space_x),
      sx_ls = lt(space_x),
      sx_md = median(space_x, na.rm = T),
      sx_fq = qt(space_x, 1),
      sx_tq = qt(space_x, 3),
      sx_ir = IQR(space_x, na.rm = T),
      sx_am = mean(space_x, na.rm = T),
      sx_vr = var(space_x, na.rm = T),
      sx_sd = sd(space_x, na.rm = T),
      sx_mxsd = mxdv(space_x),
      sx_20sd = pcdv(space_x, .2),
      sx_50sd = pcdv(space_x, .5),
      sx_80sd = pcdv(space_x, .8),
      sx_cv = cv(space_x, na.rm = T),
      sx_se = se(space_x),
      sx_mn = min(space_x, na.rm = T) ,
      sx_mx = max(space_x, na.rm = T) ,
      sx_qm = qm(space_x) ,
      sx_rg = rg(space_x),
      sx_sk = skewness(space_x) ,
      sx_ku = kurtosis(space_x) ,
      # 
      # Space y
      sy_fs = fs(space_y),
      sy_ls = lt(space_y),
      sy_md = median(space_y, na.rm = T),
      sy_fq = qt(space_y, 1),
      sy_tq = qt(space_y, 3),
      sy_ir = IQR(space_y, na.rm = T),
      sy_am = mean(space_y, na.rm = T),
      sy_vr = var(space_y, na.rm = T),
      sy_sd = sd(space_y, na.rm = T),
      sy_mxsd = mxdv(space_y),
      sy_20sd = pcdv(space_y, .2),
      sy_50sd = pcdv(space_y, .5),
      sy_80sd = pcdv(space_y, .8),
      sy_cv = cv(space_y, na.rm = T),
      sy_se = se(space_y),
      sy_mn = min(space_y, na.rm = T) ,
      sy_mx = max(space_y, na.rm = T) ,
      sy_qm = qm(space_y) ,
      sy_rg = rg(space_y),
      sy_sk = skewness(space_y) ,
      sy_ku = kurtosis(space_y) ,
      # 
      # Velocity x
      vx_fs = fs(velocity_x),
      vx_ls = lt(velocity_x),
      vx_md = median(velocity_x, na.rm = T),
      vx_fq = qt(velocity_x, 1),
      vx_tq = qt(velocity_x, 3),
      vx_ir = IQR(velocity_x, na.rm = T),
      vx_am = mean(velocity_x, na.rm = T),
      vx_vr = var(velocity_x, na.rm = T),
      vx_sd = sd(velocity_x, na.rm = T),
      vx_mxsd = mxdv(velocity_x),
      vx_20sd = pcdv(velocity_x, .2),
      vx_50sd = pcdv(velocity_x, .5),
      vx_80sd = pcdv(velocity_x, .8),
      vx_cv = cv(velocity_x, na.rm = T),
      vx_se = se(velocity_x),
      vx_mn = min(velocity_x, na.rm = T) ,
      vx_mx = max(velocity_x, na.rm = T) ,
      vx_qm = qm(velocity_x) ,
      vx_rg = rg(velocity_x),
      vx_sk = skewness(velocity_x) ,
      vx_ku = kurtosis(velocity_x) ,
      # 
      # # Velocity y
      vy_fs = fs(velocity_y),
      vy_ls = lt(velocity_y),
      vy_md = median(velocity_y, na.rm = T),
      vy_fq = qt(velocity_y, 1),
      vy_tq = qt(velocity_y, 3),
      vy_ir = IQR(velocity_y, na.rm = T),
      vy_am = mean(velocity_y, na.rm = T),
      vy_vr = var(velocity_y, na.rm = T),
      vy_sd = sd(velocity_y, na.rm = T),
      vy_mxsd = mxdv(velocity_y),
      vy_20sd = pcdv(velocity_y, .2),
      vy_50sd = pcdv(velocity_y, .5),
      vy_80sd = pcdv(velocity_y, .8),
      vy_cv = cv(velocity_y, na.rm = T),
      vy_se = se(velocity_y),
      vy_mn = min(velocity_y, na.rm = T) ,
      vy_mx = max(velocity_y, na.rm = T) ,
      vy_qm = qm(velocity_y) ,
      vy_rg = rg(velocity_y),
      vy_sk = skewness(velocity_y) ,
      vy_ku = kurtosis(velocity_y) ,
      # 
      # # acceleration x
      ax_fs = fs(acceleration_x),
      ax_ls = lt(acceleration_x),
      ax_md = median(acceleration_x, na.rm = T),
      ax_fq = qt(acceleration_x, 1),
      ax_tq = qt(acceleration_x, 3),
      ax_ir = IQR(acceleration_x, na.rm = T),
      ax_am = mean(acceleration_x, na.rm = T),
      ax_vr = var(acceleration_x, na.rm = T),
      ax_sd = sd(acceleration_x, na.rm = T),
      ax_mxsd = mxdv(acceleration_x),
      ax_20sd = pcdv(acceleration_x, .2),
      ax_50sd = pcdv(acceleration_x, .5),
      ax_80sd = pcdv(acceleration_x, .8),
      ax_cv = cv(acceleration_x, na.rm = T),
      ax_se = se(acceleration_x),
      ax_mn = min(acceleration_x, na.rm = T) ,
      ax_mx = max(acceleration_x, na.rm = T) ,
      ax_qm = qm(acceleration_x) ,
      ax_rg = rg(acceleration_x),
      ax_sk = skewness(acceleration_x) ,
      ax_ku = kurtosis(acceleration_x) ,
      # 
      # # Velocity y
      ay_fs = fs(acceleration_y),
      ay_ls = lt(acceleration_y),
      ay_md = median(acceleration_y, na.rm = T),
      ay_fq = qt(acceleration_y, 1),
      ay_tq = qt(acceleration_y, 3),
      ay_ir = IQR(acceleration_y, na.rm = T),
      ay_am = mean(acceleration_y, na.rm = T),
      ay_vr = var(acceleration_y, na.rm = T),
      ay_sd = sd(acceleration_y, na.rm = T),
      ay_mxsd = mxdv(acceleration_y),
      ay_20sd = pcdv(acceleration_y, .2),
      ay_50sd = pcdv(acceleration_y, .5),
      ay_80sd = pcdv(acceleration_y, .8),
      ay_cv = cv(acceleration_y, na.rm = T),
      ay_se = se(acceleration_y),
      ay_mn = min(acceleration_y, na.rm = T) ,
      ay_mx = max(acceleration_y, na.rm = T) ,
      ay_qm = qm(acceleration_y) ,
      ay_rg = rg(acceleration_y),
      ay_sk = skewness(acceleration_y) ,
      ay_ku = kurtosis(acceleration_y) ,
      # 
      # # Displacement
      # 
      dp_mg = mg(x,y),
      dp_ec = ec(x_ls, x_fs, y_fs, y_ls),
      dp_ang = ang(x_ls, x_fs, y_fs, y_ls),
      dp_avang = avang(x, y),
      dp_ra = dp_ec/dp_mg,
      # 
      # # Time Duration
      tm_dr = dif(press_time_diff),
      tm_fl = fs(press_time_diff)
    )
  return(desc_action)
}

pcaData <- function(dcard){
  # principal lib #
  # fatorial <- principal(dcard,
  #                       nfactors = length(dcard),
  #                       rotate = "none")
  # 
  # eigenvalues <- round(fatorial$values, 5)
  # k <- sum(eigenvalues > 1) # Kaiser
  # 
  # fatorial.k <- principal(dcard,
  #                         nfactors = k,
  #                         rotate = "none",
  #                         scores = T)
  # 
  # return(as.data.frame(fatorial.k$scores))
  
  rho <- rcorr(as.matrix(dcard), type = "pearson")
  
  # Kaiser
  # correlation
  corr.coef <- rho$r
  corr.coef[is.na(corr.coef)] <- 0
  corr.coef[sapply(corr.coef, is.infinite)] <- 0
  # eigenvectors
  eigen.results <- eigen(corr.coef)
  eigenvalues <- round(eigen.results$values, 5)
  k <- sum(eigenvalues > 1) 
  # prcomp | pca | princomp 
  data.pca <- prcomp(dcard, rank. = k, retx = T)
  #data.pca <- pca(dcard, n.obs = k)
  
  return(data.pca)
  
}

getCardSection <- function(swipe_user){
  # group by user, action number, swipe number
  base <- swipe_user %>%
    group_by(user_id, action_number, swipe_number)
  card <- cardStats(base)
  # remove inf and na
  card[is.na(card)] <- 0
  card[sapply(card, is.infinite)] <- 0
  return(card)
}

getCard <- function(swipe_user){
  
  # standard only numerical data (m=0, s =1)
  swipe_user.scaled <- swipe_user %>% 
    select(c(6:18)) %>% scale
  swipe_user.scaled <- bind_cols(swipe_user.scaled, 
                                 swipe_user[,c(1:5)])
  swipe_user.scaled <- swipe_user.scaled[,c(c(14:18), c(1:13))]
  
  # group by user and swipe number
  base <- swipe_user.scaled %>%
    group_by(user_id, swipe_number)
  card.user <- cardStats(base)
  # remove inf and na
  card.user[is.na(card.user)] <- 0
  card.user[sapply(card.user, is.infinite)] <- 0
  #return(card.user)
  
  # remove categorical data
  # TODO comment
  card <- card.user[,-c(1,2)]
  
  # cols.not.zero <-names(
  #   which(colSums(card) > 0)
  # )
  # card <- card[cols.not.zero]
  
  return(card)
  
}



#############
# load data #
#############

# files A and B  ###############

swipe.total = NULL

for(file in FILES_B){
  
  path_day <- paste(PATH_PARENT, file, '/', sep = '')
  
  # SWIPE #
  full_path <- paste(path_day, SWIPE_FILE, sep = '')
  swipe <- fread(full_path)
  cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 
              'pressure', 'x', 'y', 'uptime')  
  names(swipe) <- cnames
  
  ids_completed <- unique(swipe[swipe$action_number == 7]$user_id)
  swipe.completed <- swipe %>% filter(user_id %in% ids_completed)
  
  if(is.null(swipe.total)){
    swipe.total <- swipe.completed
  }
  else{
    swipe.total <-rbind(swipe.total, swipe.completed)
  }
}

swipe <- NULL
swipe.completed <- NULL

ids_completed <- unique(swipe.total$user_id)
length(ids_completed)

swipe <- swipe.total
swipe.total =  NULL

swipe <- rbind(swipe, swipe_data_20240119())
swipe <- rbind(swipe, swipe_data_20240122_20())

ids_completed <- unique(swipe$user_id)
length(ids_completed)
  


#################
# new variables #
#################

# new variables ###############
swipe$swipe_number <- 0
swipe$time_diff <- 0
swipe$press_time_diff <- 0
swipe$delta_press <- 0
swipe$space_x <- 0.0
swipe$space_y <- 0.0
swipe$velocity_x <- 0.0
swipe$velocity_y <- 0.0
swipe$acceleration_x <- 0.0
swipe$acceleration_y <- 0.0
# rearrange columns #
swipe <- swipe[,c(1,2,3,4,9,5,6,7,8,10,11,12,13,14,15,16,17,18)]

# inserts ###############
for (id in ids_completed){
  
  id_indices <- which(swipe$user_id == id)
  
  current_action <- -1
  swipe_number <- 0
  
  for (indice in id_indices){
    
    former_indice <- indice -1
    
    # app_action
    if(current_action != swipe[indice]$app_action){
      swipe[indice]$time_diff <- 0
      current_action <- swipe[indice]$app_action
    }
    else{
      swipe[indice]$time_diff <- diff(
        swipe[former_indice:indice]$uptime)
    }
    
    # event_type_pointer
    current_event_type <- swipe[indice]$pointer_event_type
    if (current_event_type == 1){ 
      swipe_number <- swipe_number + 1
      # swipe[indice]$press_time_diff <- 0
      # swipe[indice]$velocity_x <- 0.0
      # swipe[indice]$velocity_y <- 0.0
    }
    else{
      
      swipe[indice]$press_time_diff <- diff(
        swipe[former_indice:indice]$uptime)
      
      swipe[indice]$delta_press <- diff(
        (swipe[former_indice:indice]$pressure)/
          swipe[indice]$press_time_diff)
      
      swipe[indice]$space_x <- diff(swipe[former_indice:indice]$x)
      swipe[indice]$space_y <- diff(swipe[former_indice:indice]$y)
      
      swipe[indice]$velocity_x <- swipe[indice]$space_x/
          (ifelse(is_null(swipe[indice]$press_time_diff),
          Inf, swipe[indice]$press_time_diff))
      
      swipe[indice]$velocity_y <- swipe[indice]$space_y/
        (ifelse(is_null(swipe[indice]$press_time_diff),
                Inf, swipe[indice]$press_time_diff))
      
      swipe[indice]$acceleration_x <- swipe[indice]$velocity_x/
        (ifelse(is_null(swipe[indice]$press_time_diff),
                Inf, swipe[indice]$press_time_diff))
      
      swipe[indice]$acceleration_y <- swipe[indice]$velocity_y/
        (ifelse(is_null(swipe[indice]$press_time_diff),
                Inf, swipe[indice]$press_time_diff))
    }
    swipe[indice]$swipe_number <- swipe_number
  }
}

# split train and test #

set.seed(42)
ids <- sample(ids_completed)

# TODO supervisied classifier
# ind <- round(length(ids) * 0.90)
# ids.train <-ids[1:ind]
# ids.test <-ids[(ind+1):length(ids)]


#####################
# intruder's attack #
#####################

# I. Proof of concept ###############
# I. (kinda POC) #
#   first train, all swipes, all sections 
#   it doesn't happen in the real world
#   calls himself just to see self performance

# id <- 24
# 
# swipe.user <- swipe[swipe$user_id == id]
# card <- getCard(swipe.user)
# pca.user <- pcaData(card)

# user.model <- svm(pca.user$x, y = NULL, 
#                   type = 'one-classification',
#                   #nu = 0.10,
#                   kernel = 'radial') 

# df.results <- data.frame(
#   id = integer(),
#   id_attack = integer (),
#   acc = double()
# )
# 
# row <- 1
# for (id in ids){
#   
#   id.invasor <- id
#   swipe.attack <- swipe[swipe$user_id == id.invasor]
#   card.attack <- getCard(swipe.attack)
#   
#   project.attack <- predict(pca.user, card.attack)
#   
#   # SVM #
#   
#   attacker.result <- predict(user.model, project.attack)
#   acc <- sum(attacker.result[attacker.result==T])/length(attacker.result)
#   
#   df.results[row,] <- c(id, id.invasor, acc)
#   row <- row + 1
#   
# }


# II. more realistic approach ##################
# intruder accuracy by section/action number

results.section <- data.frame(
  section = integer(),
  id = integer(),
  id_attack = integer (),
  acc = double()
)

row <- 1
for (id in ids){
  
  swipe.user <- swipe[swipe$user_id == id]
  card <- getCard(swipe.user)
  pca.user <- pcaData(card)
  
  user.model <- svm(pca.user$x, y = NULL, 
                    type = 'one-classification',
                    kernel = 'radial')
  # RBF, source: https://rpubs.com/markloessi/497544
  
  ids.attack <- ids[ids!=id]   
  for(a in ids.attack){
    swipe.attack <- swipe[swipe$user_id == a]
    
    # through sections
    for (s in c(1:7)){
      swipe.attack.section <- swipe.attack[
        swipe.attack$action_number == s]
      card.attack <- getCard(swipe.attack.section)
      project.attack <- predict(pca.user, card.attack)
      
      # classifier
      attacker.result <- predict(user.model, project.attack,
                                 probability=TRUE)
      acc <- mean(attacker.result)
      row.value <- c(s, id, a, acc)
      results.section[row,] <- row.value
      row <- row + 1
    }
  }
  
}  

# accuracy - wrong
1  - nrow(results.section[results.section$acc> 0.5,])/ 
  nrow(results.section)

# param | acc
# ------------------
# 1     |  0.8702327

write.csv(results.section, 'section_results.csv')

# TODO
# weird behavior user_id == 7



# III. user accuracy by section #######################
# using 5 sections to build a model 

results.self <- data.frame(
  section = integer(),
  id = integer(),
  acc = double()
)

row <- 1
sections.model <- 5
for (id in ids){
  
  swipe.user <- swipe[swipe$user_id == id,]
  for (s in c(1:2)){
    
    swipe.user.train <- swipe.user %>%
      filter(action_number > (s -1),
             action_number < (sections.model + s))
    
    card <- getCard(swipe.user.train)
    pca.user <- pcaData(card)
    
    #View(pca.user$x)
    #View(pca.user$rotation)
    #summary(pca.user)
    
    swipe.user.test <- swipe.user[
      swipe.user$action_number == (sections.model + s) ]
    card.test <- getCard(swipe.user.test)
    project.test <- predict(pca.user, card.test)
    
    # View(project.test)
    #summary(project.test)
    
    # SVM
    user.model <- svm(pca.user$x, y = NULL, 
                      type = 'one-classification',
                      kernel = 'radial',
                      nu = 0.1,
                      gamma = 1.0e-7,
                      scale = T
                      )
    
    # classifier
    test.result <- predict(user.model, project.test)
    
    acc <- mean(test.result)
    section <- sections.model + s
    row.value <- c(section, id, acc)
    results.self[row,] <- row.value
    row <- row + 1
  }
}  


nrow(results.self[results.self$acc>= 0.5,])/ 
  nrow(results.self)

# param | svm                | acc
# -------------------------------------
# 1     | kernel = radial   | 0.25
# 1     | gamma = 10^-7     | 0.3461538
# 2     | gamma = 10^-7     | 0.125
# 2     | kernel = 'radial' | 0.1538462
#       | nu = 0.10,     
#       | scale= T
#       | scale = T
# 2     | gamma = 1.0e-7    | 0.1634615
# horrible!!! SVM is wrong here






# Export data #######################
# implement in scikit-learn

path.cards =  './swipecards/'
for (id in ids){
  swipe.user <- swipe[swipe$user_id == id,]
  card <- getCardSection(swipe.user)
  name.file <- paste('card', id, sep = '')
  name.file <- paste(name.file, '.csv', sep = '')
  path.file <- paste(path.cards, name.file, sep = '')
  write.csv(card, path.file)
}






# TODO
# CHECK DATA AGAIN #######
# id = 42: ps_sk weird





# OTHERS #######

# princomp
# cor.matrix <- cor(card)
# data.pca <- princomp(cor.matrix)
# summary(data.pca)
# data.pca$loadings[,1:9]

# # fatorial.k$Vaccounted
# as.data.frame(fatorial.k$scores)

#as.data.frame(unclass(fatorial.k$loadings))
#fatorial.k$values



# grouped 
base <- swipe %>%
  group_by(user_id, swipe_number)

# PCA feature select

# test one user

# cols.not.na <-names(
#   which(colSums(is.na(desc_action)) == 0)
# )
# named as a card
# card <- desc_action[cols.not.na]

desc_action <- cardStats(base)
user <- desc_action[(desc_action$user_id == ids_completed[1]),]
card.user <- user[,c(3:ncol(user))]
card.user[sapply(card.user, is.infinite)] <- NA
card.user[sapply(card.user, is.na)] <- 0

cols.not.na <-names(
  which(colSums(is.na(card.user)) == 0)
)
cols.zero <- which(colSums(card.user) == 0)

card <- card.user[cols.not.na]
card <- card.user[-cols.zero]


cor.matrix <- cor(card)

# princomp 

data.pca <- princomp(cor.matrix)
summary(data.pca)
data.pca$loadings[,1:9]

fatorial <- principal(card,
          nfactors = length(card),
          rotate = "none",
          scores = T)

eigenvalues <- round(fatorial$values, 5)
sum(eigenvalues)
k <- sum(eigenvalues > 1)

fatorial.k <- principal(card,
                      nfactors = k,
                      rotate = "none",
                      scores = T)

fatorial.k$Vaccounted



  
  











# user_id, app_action, action_number

base <- swipe %>%
  group_by(user_id, app_action, action_number)

description_number <- base %>%
  summarise(
    mean_pressure = mean(pressure),
    sd_pressure = sd(pressure),
    cv_pressure = (sd_pressure / mean_pressure) * 100 ,
    mean_pressure = mean(pressure),
    sd_press_time = sd(press_time_diff),
    mean_press_time = mean(press_time_diff),
    cv_press_time = (sd_press_time / mean_press_time) *100,
    obs = n()
  )

base <- swipe %>%
  group_by(app_action)

description <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff)/length(ids_completed),
    mean_pressure = mean(pressure)/length(ids_completed),
    obs = n()/length(ids_completed)
  )

base <- swipe %>%
  group_by(user_id, app_action)

description <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff),
    mean_pressure = mean(pressure),
    obs = n()
  )

mean(description[description$app_action == 0,]$mean_press_time)
mean(description[description$app_action == 2,]$mean_press_time)

swipe <- NULL
base <- NULL
description <- NULL