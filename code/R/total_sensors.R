
loadSensors <- function(){
  
  sensors.total = NULL
  
  for(file in FILES_B){
    
    path_day <- paste(PATH_PARENT, file, '/', sep = '')
    
    # sensors #
    full_path <- paste(path_day, SENSORS_FILE, sep = '')
    sensors <- fread(full_path)
    cnames <- c('user_id', 'action_number', 'app_action', 'sensor_type',
                'x', 'y', 'z', 'timestamp')  
    names(sensors) <- cnames
    
    ids_completed <- unique(sensors[sensors$action_number == 7]$user_id)
    sensors.completed <- sensors %>% filter(user_id %in% ids_completed)
    
    if(is.null(sensors.total)){
      sensors.total <- sensors.completed
    }
    else{
      sensors.total <-rbind(sensors.total, sensors.completed)
    }
  }
  
  sensors <- NULL
  sensors.completed <- NULL
  
  ids_completed <- unique(sensors.total$user_id)
  ids_completed <- ids_completed[ids_completed!=33]
  # except id 33, 2024/1/23
  #length(ids_completed) # 18
  
  sensors <- sensors.total %>% filter(user_id %in% ids_completed)
  sensors.total =  NULL
  sensors$timestamp <- sensors$timestamp / 1000000 
  
  # no longer, unfortunately 
  #sensors <- rbind(sensors, sensor_data_20240119())
  #sensors <- rbind(sensors, sensor_data_20240122_20())
  # sensors.wrong <-sensor_data_20240122_20()
  
  #ids_completed <- sort(unique(sensors$user_id))
  
  # magnitude vector
  sensors$w <- sqrt((sensors$x^2) + (sensors$y^2) + (sensors$z^2))
  
  return(sensors) 
}

loadSensorsLogin <- function(){
  sensors.login <- loadSensors() %>% filter(
    app_action %in% c(KEYBOARD_LOGIN, KEYBOARD_AUTH))
  return(sensors.login)
}

##########################################################
# sensors.wrong$w <- sqrt((sensors.wrong$x^2) + 
#   (sensors.wrong$y^2) + (sensors.wrong$z^2))
# ids_completed.wrong <- sort(unique(sensors.wrong$user_id))
##########################################################


### LOGIN ###

sensors.scroll <- sensors %>% filter(
  app_action %in% c(SWIPE_PIX_SEND)) # , SWIPE_HOME

sensors.login <- sensors %>% filter(
  app_action %in% c(KEYBOARD_LOGIN, KEYBOARD_AUTH))


##########################################################
# sensors.login.wrong <- sensors.wrong %>% filter(
#   app_action %in% c(KEYBOARD_LOGIN, KEYBOARD_AUTH))
##########################################################

sensors <- NULL # just to free memory

# set user here
user.index <- 15

sensors.user <- as.data.frame(sensors.scroll[
  (sensors.scroll$user_id == ids_completed[user.index])])
#   & (sensors.scroll$sensor_type == TYPE_ACCELEROMETER)])

#View(sensors.user)
#p1 <- 
  ggplot(
  sensors.user,  aes(timestamp, w)
) +
  geom_point()


# set user here
user.index <- 2
sensors.user <- as.data.frame(sensors.login[
  (sensors.login$user_id == ids_completed[user.index]) &
   (sensors.login$sensor_type == TYPE_ACCELEROMETER)])

p1 <- ggplot(
  sensors.user,  aes(timestamp, w)
) +
geom_point()  

##########################################################
# user.index <- 2
# sensors.user.wrong <- as.data.frame(sensors.login.wrong[
#   (sensors.login.wrong$user_id == ids_completed.wrong[user.index]) &
#     (sensors.login.wrong$sensor_type == TYPE_ACCELEROMETER)])
# 
# ggplot(
#   sensors.user.wrong,  aes(timestamp, w)
# ) +
# geom_point()
##########################################################

### Statistical template ###

### Printing data

section <- 1

# login
p0 <- ggplot(
  sensors.user[((sensors.user$action_number == section) &
                 (sensors.user$app_action == KEYBOARD_LOGIN))
               ,],  aes(timestamp, w)
) +
geom_point()

p1 <- ggplot(
  sensors.user[((sensors.user$action_number == section) &
                  (sensors.user$app_action == KEYBOARD_AUTH))
               ,],  aes(timestamp, w)
) +
  geom_point()

section <- 2
p2 <- ggplot(
  sensors.user[sensors.user$action_number == section,],  aes(timestamp, w)
) +
geom_point() 

section <- 3
p3 <- ggplot(
  sensors.user[sensors.user$action_number == section,],  aes(timestamp, w)
) +
geom_point()

section <- 4
p4 <- ggplot(
  sensors.user[sensors.user$action_number == section,],  aes(timestamp, w)
) +
geom_point()

section <- 5
p5 <- ggplot(
  sensors.user[sensors.user$action_number == section,],  aes(timestamp, w)
) +
geom_point()

section <- 6
p6 <- ggplot(
  sensors.user[sensors.user$action_number == section,],  aes(timestamp, w)
) +
geom_point()

section <- 7
p7 <- ggplot(
  sensors.user[sensors.user$action_number == section,],  aes(timestamp, w)
) +
geom_point()

grid.arrange(p0, p1, p2, p3, p4, p5, p6, p7)


# fist card

# Minimum (mn), Maximum (mx), Arithmetic
# Mean (am), Quadratic Mean (qm), Harmonic Mean (hm),
# Geometric Mean (gm), Median (md), Range (rg), Variance
# (vr), Standard Deviation (sd), Skewness (sk), Kurtosis (ku),
# First Quartile (fq), Third Quartile (tq), Interquartile Range
# (ir), Mean Absolute Deviation (ma), Median Absolute Deviation (mi),
# Coefficient of Variation (cv), and Standard Error of Mean (se).

base <- sensors.login %>% group_by(user_id, sensor_type)

mh <- function(xyz){
  return(
    mahalanobis(
      xyz,
      colMeans(xyz),
      cov(xyz)
    )
  )
}

card <- base %>%
  summarise(
    x =    sum(x),
    x_ma = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    y =    sum(y),
    z =    sum(z),
    w =    sum(w) ,
    mh =   mh(data.frame(a=x,b=y,c=z))
  )

set.seed(42)

ids <- sample(ids_completed)
#ind <- round(length(ids) * 0.90)

# empty dataframe
attack.errors <- data.frame(
  user_id = integer(),
  attack_id = integer(),
  #x_diff = double(),
  #y_diff = double(),
  #z_diff = double(),
  cos_sim =  double(),
  w_diff  = double()
)



num.row <- 0
for (i in c(1:length(ids))){
  
  id <- ids[i]
  id.others <- ids[ids!=id]
  sensor.type <- 4
  
  user <- card %>%
    filter(user_id == id, sensor_type == sensor.type)
    
  for (o in c(1:length(id.others))){
    
    num.row <- num.row + 1
    
    id.attacker <- id.others[o]
    attacker <- card %>% filter(
      user_id == id.attacker, sensor_type == sensor.type)
    
    cos.sim <-cosine(
      c(user$x, user$y, user$z) , c(attacker$x, attacker$y, attacker$z))

    w.diff <- user$w - attacker$w
    attack.errors[num.row,] <- list(
      id, id.attacker, cos.sim, w.diff)
  }
  
}

((user$x * factor) %*% (attacker$x * factor)) / 
  sqrt((sum(user$x * factor) ^2) *  (sum(attacker$x * factor) ^2))






### fourier transform

# Source: https://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}


section <- 7
X.k <- fft(sensors.user[
  sensors.user$action_number == section,]$w)
plot.frequency.spectrum(X.k)


### Cosine distance as cost function
### Overlap touch time data?
  




# plot(
#   sensors.login %>%
#     filter(
#       user_id == ids_completed[1] #|
#       #sensor_type == TYPE_MAGNETIC_FIELD
#     ) %>%
#     select(x) 
# )

ids_completed[20]
ids_completed[30]
ids_completed[38]

