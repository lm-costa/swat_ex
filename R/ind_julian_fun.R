ind_julian <- function(arquivo){

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  for(i in seq_along(arquivo)){
    if(i==1){
      db <- janitor::clean_names(readr::read_csv(paste0("data/", arquivo[i])))

      P_mean <- c(tapply(db$precipitation, db$jday, mean))
      P_sd <- c(tapply(db$precipitation, db$jday, sd))
      P_min <- c(tapply(db$precipitation, db$jday, min))
      P_max <- c(tapply(db$precipitation, db$jday, max))
      P_med <- c(tapply(db$precipitation, db$jday, median))
      P_mod <- c(tapply(db$precipitation, db$jday, getmode))
      Wind_mean <- c(tapply(db$wind, db$jday, mean))
      Wind_sd <- c(tapply(db$wind, db$jday, sd))
      Wind_min <- c(tapply(db$wind, db$jday, min))
      Wind_max <- c(tapply(db$wind, db$jday, max))
      Wind_med <- c(tapply(db$wind, db$jday, median))
      Wind_mod <- c(tapply(db$wind, db$jday, getmode))
      Tmax_mean <- c(tapply(db$max_temperature, db$jday, mean))
      Tmax_sd <- c(tapply(db$max_temperature, db$jday, sd))
      Tmax_min <- c(tapply(db$max_temperature, db$jday, min))
      Tmax_max <- c(tapply(db$max_temperature, db$jday, max))
      Tmax_med <- c(tapply(db$max_temperature, db$jday, median))
      Tmax_mod <- c(tapply(db$max_temperature, db$jday, getmode))
      Tmin_mean <- c(tapply(db$min_temperature, db$jday, mean))
      Tmin_sd <- c(tapply(db$min_temperature, db$jday, sd))
      Tmin_min <- c(tapply(db$min_temperature, db$jday, min))
      Tmin_max <- c(tapply(db$min_temperature, db$jday, max))
      Tmin_med <- c(tapply(db$min_temperature, db$jday, median))
      Tmin_mod <- c(tapply(db$min_temperature, db$jday, getmode))
      RH_mean <- c(tapply(db$rh, db$jday, mean))
      RH_sd <- c(tapply(db$rh, db$jday, sd))
      RH_min <- c(tapply(db$rh, db$jday, min))
      RH_max <- c(tapply(db$rh, db$jday, max))
      RH_med <- c(tapply(db$rh, db$jday, median))
      RH_mod <- c(tapply(db$rh, db$jday, getmode))
      Tm_mean <- c(tapply(db$tm, db$jday, mean))
      Tm_sd <- c(tapply(db$tm, db$jday, sd))
      Tm_min <- c(tapply(db$tm, db$jday, min))
      Tm_max <- c(tapply(db$tm, db$jday, max))
      Tm_med <- c(tapply(db$tm, db$jday, median))
      Tm_mod <- c(tapply(db$tm, db$jday, getmode))
      Qg_mean <- c(tapply(db$solar, db$jday, mean))
      Qg_sd <- c(tapply(db$solar, db$jday, sd))
      QG_min <- c(tapply(db$solar, db$jday, min))
      Qg_max <- c(tapply(db$solar, db$jday, max))
      Qg_med <- c(tapply(db$solar, db$jday, median))
      Qg_mod <- c(tapply(db$solar, db$jday, getmode))
      JDAY <- c(seq.int(1,366,1))

      df <- data.frame(JDAY,
                       P_mean, P_sd, P_max,P_min,P_med,P_mod,
                       Tm_mean,Tm_sd,Tm_max,Tm_min,Tm_med,Tm_mod,
                       Tmax_mean,Tmax_sd,Tmax_max,Tmax_min,Tmax_med,Tmax_mod,
                       Tmin_mean,Tmin_sd,Tmin_max,Tmin_min,Tmin_med,Tmin_mod,
                       RH_mean,RH_sd,RH_max,RH_min,RH_med,RH_mod,
                       Qg_mean,Qg_sd,Qg_max,QG_min,Qg_med,Qg_mod,
                       Wind_mean,Wind_sd,Wind_max,Wind_min,Wind_med,Wind_mod
      )

      write.csv(df,paste0("data/individual/julian/",arquivo[i]))
    }else{
      db <- janitor::clean_names(readr::read_csv(paste0("data/", arquivo[i])))

      P_mean <- c(tapply(db$precipitation, db$jday, mean))
      P_sd <- c(tapply(db$precipitation, db$jday, sd))
      P_min <- c(tapply(db$precipitation, db$jday, min))
      P_max <- c(tapply(db$precipitation, db$jday, max))
      P_med <- c(tapply(db$precipitation, db$jday, median))
      P_mod <- c(tapply(db$precipitation, db$jday, getmode))
      Wind_mean <- c(tapply(db$wind, db$jday, mean))
      Wind_sd <- c(tapply(db$wind, db$jday, sd))
      Wind_min <- c(tapply(db$wind, db$jday, min))
      Wind_max <- c(tapply(db$wind, db$jday, max))
      Wind_med <- c(tapply(db$wind, db$jday, median))
      Wind_mod <- c(tapply(db$wind, db$jday, getmode))
      Tmax_mean <- c(tapply(db$max_temperature, db$jday, mean))
      Tmax_sd <- c(tapply(db$max_temperature, db$jday, sd))
      Tmax_min <- c(tapply(db$max_temperature, db$jday, min))
      Tmax_max <- c(tapply(db$max_temperature, db$jday, max))
      Tmax_med <- c(tapply(db$max_temperature, db$jday, median))
      Tmax_mod <- c(tapply(db$max_temperature, db$jday, getmode))
      Tmin_mean <- c(tapply(db$min_temperature, db$jday, mean))
      Tmin_sd <- c(tapply(db$min_temperature, db$jday, sd))
      Tmin_min <- c(tapply(db$min_temperature, db$jday, min))
      Tmin_max <- c(tapply(db$min_temperature, db$jday, max))
      Tmin_med <- c(tapply(db$min_temperature, db$jday, median))
      Tmin_mod <- c(tapply(db$min_temperature, db$jday, getmode))
      RH_mean <- c(tapply(db$rh, db$jday, mean))
      RH_sd <- c(tapply(db$rh, db$jday, sd))
      RH_min <- c(tapply(db$rh, db$jday, min))
      RH_max <- c(tapply(db$rh, db$jday, max))
      RH_med <- c(tapply(db$rh, db$jday, median))
      RH_mod <- c(tapply(db$rh, db$jday, getmode))
      Tm_mean <- c(tapply(db$tm, db$jday, mean))
      Tm_sd <- c(tapply(db$tm, db$jday, sd))
      Tm_min <- c(tapply(db$tm, db$jday, min))
      Tm_max <- c(tapply(db$tm, db$jday, max))
      Tm_med <- c(tapply(db$tm, db$jday, median))
      Tm_mod <- c(tapply(db$tm, db$jday, getmode))
      Qg_mean <- c(tapply(db$solar, db$jday, mean))
      Qg_sd <- c(tapply(db$solar, db$jday, sd))
      QG_min <- c(tapply(db$solar, db$jday, min))
      Qg_max <- c(tapply(db$solar, db$jday, max))
      Qg_med <- c(tapply(db$solar, db$jday, median))
      Qg_mod <- c(tapply(db$solar, db$jday, getmode))
      JDAY <- c(seq.int(1,366,1))

      df <- data.frame(JDAY,
                       P_mean, P_sd, P_max,P_min,P_med,P_mod,
                       Tm_mean,Tm_sd,Tm_max,Tm_min,Tm_med,Tm_mod,
                       Tmax_mean,Tmax_sd,Tmax_max,Tmax_min,Tmax_med,Tmax_mod,
                       Tmin_mean,Tmin_sd,Tmin_max,Tmin_min,Tmin_med,Tmin_mod,
                       RH_mean,RH_sd,RH_max,RH_min,RH_med,RH_mod,
                       Qg_mean,Qg_sd,Qg_max,QG_min,Qg_med,Qg_mod,
                       Wind_mean,Wind_sd,Wind_max,Wind_min,Wind_med,Wind_mod
      )

      write.csv(df,paste0("data/individual/julian/",arquivo[i]))
    }
  }
}
