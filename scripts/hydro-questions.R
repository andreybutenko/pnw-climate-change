source('./hydro-helper.R')

# inches
GetMonthlyData(measure = 'runoff_monthly_tot', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  })

# ??? W/m2
GetMonthlyData(measure = 'isr_monthly_avg', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  })

# ??? W/m2
GetMonthlyData(measure = 'olr_monthly_avg', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  })

# pretty confident inches
GetMonthlyData(measure = 'precip_monthly_tot', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  })

#
GetMonthlyData(measure = 'runoff_monthly_tot', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  })

# ??? centimeters?
GetMonthlyData(measure = 'snodep_monthly_day1', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  })

# inches
GetMonthlyData(measure = 'swe_monthly_day1', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  })

