correction <- function(lista){

  ## Corrigindo as datas

  dates_day <- seq(from=as.Date("1979-01-01"), to=as.Date("2014-07-31"), by="day")
  day <- lubridate::day(dates_day)
  month <- lubridate::month(dates_day)
  year <- lubridate::year(dates_day)

  dates <- data.frame(day,month,year)
  dates <- dates |>
    dplyr::filter(
      month != 2 | day !=29
    )

  day <- dates$day
  month <- dates$month
  year <- dates$year

  dates_day <- as.Date(stringr::str_c(year,month,day,sep="-"))
  julian_days <- as.numeric(format(dates_day, "%j"))

  ## Escrevendo os novos arquivos
  for(i in seq_along(lista)){
    if(i==1){
      tab <- janitor::clean_names(readr::read_csv(paste0("data-raw/",lista[i])))|>
        dplyr::mutate(
          dates = dates_day,
          jday = julian_days,
          rh = relative_humidity*100,
          tm = (max_temperature + min_temperature)/2
        ) |>
        dplyr::select(
          dates,
          jday,
          longitude,
          latitude,
          elevation,
          tm,
          max_temperature,
          min_temperature,
          precipitation,
          rh,
          solar,
          wind
        )
      write.csv(tab,paste0("data/",lista[i]))
    }else{
      tab <- janitor::clean_names(readr::read_csv(paste0("data-raw/",lista[i])))|>
        dplyr::mutate(
          dates = dates_day,
          jday = julian_days,
          rh = relative_humidity*100,
          tm = (max_temperature + min_temperature)/2
        ) |>
        dplyr::select(
          dates,
          jday,
          longitude,
          latitude,
          elevation,
          tm,
          max_temperature,
          min_temperature,
          precipitation,
          rh,
          solar,
          wind
        )
      write.csv(tab,paste0("data/",lista[i]))
    }
  }
}
