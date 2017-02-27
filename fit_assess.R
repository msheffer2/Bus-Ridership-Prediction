fit_assess <- function(){
  #Train Assessment
  ##############################################################################
  p1 <- data.frame(p=p_train, a=train.r, line=train$line, year=train$year, 
                   season=train$season, day=train$day, rmse=abs(p_train-train.r))
  
  rmse_train <- round(mean(p1$rmse), 1)
  
  year_train <- p1 %>% 
    group_by(year) %>%
    summarize(train=round(mean(rmse), 1)) %>%
    as.data.frame()
  
  season_train <- p1 %>% 
    group_by(season) %>%
    summarize(train=round(mean(rmse), 1)) %>%
    as.data.frame()
  
  #Test Assessment
  ##############################################################################
  p2 <- data.frame(p=p_test, a=test.r, line=test$line, year=test$year, 
                   season=test$season, day=test$day, rmse=abs(p_test-test.r))
  
  rmse_test <- round(mean(p2$rmse), 1)
  
  year_test <- p2 %>% 
    group_by(year) %>%
    summarize(train=round(mean(rmse), 1)) %>%
    as.data.frame()
  
  season_test <- p2 %>% 
    group_by(season) %>%
    summarize(train=round(mean(rmse), 1)) %>%
    as.data.frame()
  
  #Case Assessment
  ##############################################################################
  p3 <- cbind(test_cases, p_case)
  p3 <- select(p3, line, day, season, year, current, rides, p_case)
  p3$predict <- abs(p3$rides - p3$p_case)
  p3 <- select(p3, line, day, season, year, current, predict)
  
  rmse_case <- round(mean(p3$predict), 1)
  day_case<- p3 %>% 
    group_by(day) %>%
    summarize(test=round(mean(predict),1)) %>%
    as.data.frame()
  line_case <- p3 %>% 
    group_by(line) %>%
    summarize(test=round(mean(predict),1)) %>%
    as.data.frame()
  
  rmse_hist <- round(mean(p3$current), 1)
  day_hist <- p3 %>% 
    group_by(day) %>%
    summarize(current=round(mean(current),1)) %>%
    as.data.frame()
  line_hist <- p3 %>% 
    group_by(line) %>%
    summarize(current=round(mean(current),1)) %>%
    as.data.frame()
  
  #Year RMSE
  year <- left_join(year_train, year_test, by="year") %>%
    rename(Year=year, Train=train.x, Test=train.y) %>%
    mutate(Train = round(Train, digits=1),
           Test = round(Test, digits=1))
  
  #Season RMSE
  season <- left_join(season_train, season_test, by="season") %>%
    rename(Season=season, Train=train.x, Test=train.y) %>%
    mutate(Train = round(Train, digits=1),
           Test = round(Test, digits=1))
  
  #Test Case Day of Week RMSE
  day <- left_join(day_case, day_hist, by="day") %>%
    rename(Day=day, Model=test, Current=current) %>%
    mutate(Model = round(Model, digits=1),
           Current = round(Current, digits=1))
  
  #Test Case Linek RMSE
  line <- left_join(line_case, line_hist, by="line") %>%
    rename(Line=line, Model=test, Current=current) %>%
    mutate(Model = round(Model, digits=1),
           Current = round(Current, digits=1))
  
  fit_assessment <- list(train=rmse_train, test=rmse_test, hist=rmse_hist, 
                         case=rmse_case, year=year, season=season, day=day, line=line)
  return(fit_assessment)  
}



