# test after change sensor for all swipe screen

files <-c('20240309_test')


sensors.total = NULL

for(file in files){
  
  path_day <- paste(PATH_PARENT, file, '/', sep = '')
  
  # sensors #
  full_path <- paste(path_day, SENSORS_FILE, sep = '')
  sensors <- fread(full_path)
  cnames <- c('user_id', 'action_number', 'app_action', 'sensor_type', 'x', 'y', 'z', 'timestamp')  
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

sensors$timestamp <- sensors$timestamp / 1000000 
sensors$w <- sqrt((sensors$x^2) + (sensors$y^2) + (sensors$z^2))

unique(sensors$user_id)

sensors.scroll <- sensors %>% filter(
  app_action %in% c(KEYBOARD_LOGIN)) 
# , SWIPE_HOME

ggplot(
  sensors.scroll,  aes(timestamp, w)
) +
  geom_point()

sensors.user <- as.data.frame(sensors.scroll[
    (sensors.scroll$sensor_type == TYPE_ACCELEROMETER)])

ggplot(
  sensors.user,  aes(timestamp, w)
) +
  geom_point()  

