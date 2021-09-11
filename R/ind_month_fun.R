ind_month <- function(arquivo){

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  for(i in seq_along(arquivo)){
    if(i==1){

      tab <- janitor::clean_names(readr::read_csv(paste0("data/", arquivo[i])))

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

      write.csv(df_month,paste0("data/individual/month/",arquivo[i]))
    }else{

      tab <- janitor::clean_names(readr::read_csv(paste0("data/", arquivo[i])))

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

      write.csv(df_month,paste0("data/individual/month/",arquivo[i]))
    }
  }
}
