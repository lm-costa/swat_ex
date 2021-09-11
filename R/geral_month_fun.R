geral_month <- function(arquivo){

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  for(i in seq_along(arquivo)){
    if(i==1){

      tab <- janitor::clean_names(readr::read_csv(paste0("data-raw/", arquivo[i])))

      month= lubridate::month(tab$dates)
      tab$month <- month
      Month <- seq.int(1,12,1)
      day <- c(31,28,31,30,31,30,31,31,30,31,30,31)

      P_mean <- c(tapply(tab$precipitation, tab$month, mean))
      P_mean <- P_mean*day
      P_sd <- c(tapply(tab$precipitation, tab$month, sd))
      P_min <- c(tapply(tab$precipitation, tab$month, min))
      P_max <- c(tapply(tab$precipitation, tab$month, max))
      P_med <- c(tapply(tab$precipitation, tab$month, median))
      P_mod <- c(tapply(tab$precipitation, tab$month, getmode))
      RH_mean <- c(tapply(tab$rh, tab$month, mean))
      RH_sd <- c(tapply(tab$rh, tab$month, sd))
      RH_min <- c(tapply(tab$rh, tab$month, min))
      RH_max <- c(tapply(tab$rh, tab$month, max))
      RH_med <- c(tapply(tab$rh, tab$month, median))
      RH_mod <- c(tapply(tab$rh, tab$month, getmode))
      Tm_mean <- c(tapply(tab$tm, tab$month, mean))
      Tm_sd <- c(tapply(tab$tm, tab$month, sd))
      Tm_min <- c(tapply(tab$tm, tab$month, min))
      Tm_max <- c(tapply(tab$tm, tab$month, max))
      Tm_med <- c(tapply(tab$tm, tab$month, median))
      Tm_mod <- c(tapply(tab$tm, tab$month, getmode))
      Tmax_mean <- c(tapply(tab$max_temperature, tab$month, mean))
      Tmax_sd <- c(tapply(tab$max_temperature, tab$month, sd))
      Tmax_min <- c(tapply(tab$max_temperature, tab$month, min))
      Tmax_max <- c(tapply(tab$max_temperature, tab$month, max))
      Tmax_med <- c(tapply(tab$max_temperature, tab$month, median))
      Tmax_mod <- c(tapply(tab$max_temperature, tab$month, getmode))
      Tmin_mean <- c(tapply(tab$min_temperature, tab$month, mean))
      Tmin_sd <- c(tapply(tab$min_temperature, tab$month, sd))
      Tmin_min <- c(tapply(tab$min_temperature, tab$month, min))
      Tmin_max <- c(tapply(tab$min_temperature, tab$month, max))
      Tmin_med <- c(tapply(tab$min_temperature, tab$month, median))
      Tmin_mod <- c(tapply(tab$min_temperature, tab$month, getmode))
      Qg_mean <- c(tapply(tab$solar,tab$month,mean))
      Qg_sd <- c(tapply(tab$solar, tab$month, sd))
      Qg_min <- c(tapply(tab$solar, tab$month, min))
      Qg_max <- c(tapply(tab$solar, tab$month, max))
      Qg_med <- c(tapply(tab$solar, tab$month, median))
      Qg_mod <- c(tapply(tab$solar, tab$month, getmode))
      Wind_mean <- c(tapply(tab$wind, tab$month, mean))
      Wind_sd <- c(tapply(tab$wind, tab$month, sd))
      Wind_min <- c(tapply(tab$wind, tab$month, min))
      Wind_max <- c(tapply(tab$wind, tab$month, max))
      Wind_med <- c(tapply(tab$wind, tab$month, median))
      Wind_mod <- c(tapply(tab$wind, tab$month, getmode))
      Month <- seq.int(1,12,1)

      df_month <- data.frame(Month,
                             P_mean, P_sd, P_max,P_min,P_med,P_mod,
                             Tm_mean,Tm_sd,Tm_max,Tm_min,Tm_med,Tm_mod,
                             Tmax_mean,Tmax_sd,Tmax_max,Tmax_min,Tmax_med,Tmax_mod,
                             Tmin_mean,Tmin_sd,Tmin_max,Tmin_min,Tmin_med,Tmin_mod,
                             RH_mean,RH_sd,RH_max,RH_min,RH_med,RH_mod,
                             Qg_mean,Qg_sd,Qg_max,Qg_min,Qg_med,Qg_mod,
                             Wind_mean,Wind_sd,Wind_max,Wind_min,Wind_med,Wind_mod
      )

    }else{

      tab2 <- janitor::clean_names(readr::read_csv(paste0("data-raw/", arquivo[i])))

      month= lubridate::month(tab2$dates)
      tab2$month <- month
      Month2 <- seq.int(1,12,1)
      day2 <- c(31,28,31,30,31,30,31,31,30,31,30,31)

      P_mean2 <- c(tapply(tab2$precipitation, tab2$month, mean))
      P_mean2 <- P_mean2*day2
      P_sd2 <- c(tapply(tab2$precipitation, tab2$month, sd))
      P_min2 <- c(tapply(tab2$precipitation, tab2$month, min))
      P_max2 <- c(tapply(tab2$precipitation, tab2$month, max))
      P_med2 <- c(tapply(tab2$precipitation, tab2$month, median))
      P_mod2 <- c(tapply(tab2$precipitation, tab2$month, getmode))
      RH_mean2 <- c(tapply(tab2$rh, tab2$month, mean))
      RH_sd2 <- c(tapply(tab2$rh, tab2$month, sd))
      RH_min2 <- c(tapply(tab2$rh, tab2$month, min))
      RH_max2 <- c(tapply(tab2$rh, tab2$month, max))
      RH_med2 <- c(tapply(tab2$rh, tab2$month, median))
      RH_mod2 <- c(tapply(tab2$rh, tab2$month, getmode))
      Tm_mean2 <- c(tapply(tab2$tm, tab2$month, mean))
      Tm_sd2 <- c(tapply(tab2$tm, tab2$month, sd))
      Tm_min2 <- c(tapply(tab2$tm, tab2$month, min))
      Tm_max2 <- c(tapply(tab2$tm, tab2$month, max))
      Tm_med2 <- c(tapply(tab2$tm, tab2$month, median))
      Tm_mod2 <- c(tapply(tab2$tm, tab2$month, getmode))
      Tmax_mean2 <- c(tapply(tab2$max_temperature, tab2$month, mean))
      Tmax_sd2 <- c(tapply(tab2$max_temperature, tab2$month, sd))
      Tmax_min2 <- c(tapply(tab2$max_temperature, tab2$month, min))
      Tmax_max2 <- c(tapply(tab2$max_temperature, tab2$month, max))
      Tmax_med2 <- c(tapply(tab2$max_temperature, tab2$month, median))
      Tmax_mod2 <- c(tapply(tab2$max_temperature, tab2$month, getmode))
      Tmin_mean2 <- c(tapply(tab2$min_temperature, tab2$month, mean))
      Tmin_sd2 <- c(tapply(tab2$min_temperature, tab2$month, sd))
      Tmin_min2 <- c(tapply(tab2$min_temperature, tab2$month, min))
      Tmin_max2 <- c(tapply(tab2$min_temperature, tab2$month, max))
      Tmin_med2 <- c(tapply(tab2$min_temperature, tab2$month, median))
      Tmin_mod2 <- c(tapply(tab2$min_temperature, tab2$month, getmode))
      Qg_mean2 <- c(tapply(tab2$solar,tab2$month,mean))
      Qg_sd2 <- c(tapply(tab2$solar, tab2$month, sd))
      Qg_min2 <- c(tapply(tab2$solar, tab2$month, min))
      Qg_max2 <- c(tapply(tab2$solar, tab2$month, max))
      Qg_med2 <- c(tapply(tab2$solar, tab2$month, median))
      Qg_mod2 <- c(tapply(tab2$solar, tab2$month, getmode))
      Wind_mean2 <- c(tapply(tab2$wind, tab2$month, mean))
      Wind_sd2 <- c(tapply(tab2$wind, tab2$month, sd))
      Wind_min2 <- c(tapply(tab2$wind, tab2$month, min))
      Wind_max2 <- c(tapply(tab2$wind, tab2$month, max))
      Wind_med2 <- c(tapply(tab2$wind, tab2$month, median))
      Wind_mod2 <- c(tapply(tab2$wind, tab2$month, getmode))
      Month2 <- seq.int(1,12,1)

      df_month2 <- data.frame(Month2,
                             P_mean2, P_sd2, P_max2,P_min2,P_med2,P_mod2,
                             Tm_mean2,Tm_sd2,Tm_max2,Tm_min2,Tm_med2,Tm_mod2,
                             Tmax_mean2,Tmax_sd2,Tmax_max2,Tmax_min2,Tmax_med2,Tmax_mod2,
                             Tmin_mean2,Tmin_sd2,Tmin_max2,Tmin_min2,Tmin_med2,Tmin_mod2,
                             RH_mean2,RH_sd2,RH_max2,RH_min2,RH_med2,RH_mod2,
                             Qg_mean2,Qg_sd2,Qg_max2,Qg_min2,Qg_med2,Qg_mod2,
                             Wind_mean2,Wind_sd2,Wind_max2,Wind_min2,Wind_med2,Wind_mod2
      )

      df_month <- rbind(df_month,df_month2)
    }
  }

  write.csv(df_month,"data/geral/month_climat.csv")
}
