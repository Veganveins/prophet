library(data.table)
library(zoo)
library(dplyr)
library(prophet)
library(ggplot2)



inv = read.csv('processed_investments.csv')
circ = read.csv('processed_circ.csv')
promos = read.csv('processed_promos.csv')
proph = read.csv('processed_customers.csv')



regress = merge(proph, inv, by=c('FISCAL_YR', 'FISCAL_MO', 'Segment_Channel'), all.y=TRUE)
regress$ds <- as.yearmon(paste(regress$FISCAL_YR, regress$FISCAL_MO, sep = "-"))
setnames(regress, 'monthly_customers', 'y')

regress2 = merge(regress, promos, by = c('FISCAL_YR', 'FISCAL_MO', 'FISCAL_QTR', 'Segment_Channel'), all.y=TRUE)
regress3 = merge(regress2, circ, by = c('FISCAL_YR', 'FISCAL_MO', 'FISCAL_QTR', 'Segment'), all.y=TRUE)
regress3 = as.data.table(regress3)

train = regress3[FISCAL_YR >= 2016]


make_forecast <- function(df, threshold) {
  
  seg_chan = as.character(df$Segment_Channel[1])
  print(seg_chan)
  df = as.data.table(df)
  df = df[FISCAL_YR <= threshold]
  m <- prophet(seasonality.mode = 'additive', holidays = holidays, holidays.prior.scale = .05, seasonality.prior.scale = 5, changepoint.prior.scale = .034, weekly.seasonality = FALSE, daily.seasonality = FALSE, yearly.seasonality = FALSE)
  m <- add_regressor(m, name = 'circ_score')
  #m <- add_regressor(m, name = 'peel_off_depth')
  ##m <- add_regressor(m, name = 'post_card_depth')
  #m <- add_regressor(m, name = 'free_ship_depth')
  #m <- add_regressor(m, name = 'flash_sale_depth')
  #m <- add_regressor(m, name = 'global_depth')
  #m <- add_regressor(m, name = 'fp_entire_depth')
  #m <- add_regressor(m, name = 'sp_entire_depth')
  m <- add_seasonality(m, name='yearly', period=365.25, fourier.order=10, prior.scale = 0.05)
  #m <- add_seasonality(m, name = 'quarterly', period = 365.25/4, fourier.order = 5, prior.scale = 15)
  #m <- add_seasonality(m, name = 'monthly', period = 365.25/12, fourier.order = 3, prior.scale = 10)
  m <- fit.prophet(m, df[FISCAL_YR <= (threshold - 1)])
  future <- make_future_dataframe(m, periods = 12, freq = 'month')
  future$circ_score = df$circ_score
  #future$investment = df$investment  # FIXME this needs to be correctly specified
  #future$peel_off_depth = df$peel_off_depth
  ##future$post_card_depth = df$post_card_depth
  #future$free_ship_depth = df$free_ship_depth
  #future$flash_sale_depth = df$flash_sale_depth
  #future$global_depth = df$global_depth
  #future$fp_entire_depth = df$fp_entire_depth
  #future$sp_entire_depth = df$sp_entire_depth
  fcst <- predict(m, future)
  prophet_plot_components(m, fcst)
  #plot(m, fcst)+ggtitle(seg_chan)
  return(fcst)
}



new_years <- tibble(
  holiday = 'new_years',
  ds = as.Date(c('2017-12-01', '2018-11-01',
                 '2019-12-01')),
  lower_window = 0,
  upper_window = 1
)

womens_day <- tibble(
  holiday = 'womens_day',
  ds = as.Date(c('2016-02-01', '2017-02-01', '2018-02-01',
                 '2019-02-01')),
  lower_window = 0,
  upper_window = 1
)

good_friday <- tibble(
  holiday = 'good_friday',
  ds = as.Date(c('2016-02-01', '2017-03-01', '2018-02-01',
                 '2019-03-01')),
  lower_window = 0,
  upper_window = 1
)

easter <- tibble(
  holiday = 'easter',
  ds = as.Date(c('2016-02-01', '2017-03-01', '2018-02-01',
                 '2019-03-01')),
  lower_window = 0,
  upper_window = 1
)

mothers_day <- tibble(
  holiday = 'mothers_day',
  ds = as.Date(c('2016-04-01', '2017-04-01', '2018-04-01',
                 '2019-04-01')),
  lower_window = 0,
  upper_window = 1
)

memorial <- tibble(
  # on average +17k customers
  holiday = 'memorial',
  ds = as.Date(c('2016-05-01', '2017-05-01', '2018-04-01',
                 '2019-04-01')),
  lower_window = 0,
  upper_window = 1
)

independence <- tibble(
  # on average +5k customers
  holiday = 'independence',
  ds = as.Date(c('2016-06-01', '2017-06-01', '2018-05-01',
                 '2019-05-01')),
  lower_window = 0,
  upper_window = 1
)

labor <- tibble(
  # on average +20k customers
  holiday = 'labor',
  ds = as.Date(c('2016-08-01', '2017-08-01', '2018-08-01',
                 '2019-08-01')),
  lower_window = 0,
  upper_window = 1
)

thanksgiv <- tibble(
  holiday = 'thanksgiv',
  ds = as.Date(c('2016-10-01', '2017-10-01', '2018-10-01',
                 '2019-10-01')),
  lower_window = 0,
  upper_window = 1
)

black_fri <- tibble(
  holiday = 'black_fri',
  ds = as.Date(c('2016-10-01', '2017-10-01', '2018-10-01',
                 '2019-10-01')),
  lower_window = 0,
  upper_window = 1
)

cyber_monday <- tibble(
  holiday = 'cyber_monday',
  ds = as.Date(c('2016-11-01', '2017-11-01', '2018-10-01',
                 '2019-11-01')),
  lower_window = 0,
  upper_window = 1
)

christ_eve <- tibble(
  holiday = 'christ_eve',
  ds = as.Date(c('2016-11-01', '2017-11-01', '2018-11-01',
                 '2019-11-01')),
  lower_window = 0,
  upper_window = 1
)

christ <- tibble(
  holiday = 'christ',
  ds = as.Date(c('2016-11-01', '2017-11-01', '2018-11-01',
                 '2019-11-01')),
  lower_window = 0,
  upper_window = 1
)

nye <- tibble(
  holiday = 'nye',
  ds = as.Date(c('2016-11-01', '2017-12-01', '2018-11-01',
                 '2019-11-01')),
  lower_window = 0,
  upper_window = 1
)




holidays <- bind_rows(nye, christ, cyber_monday, christ_eve,black_fri, thanksgiv, labor, independence, memorial, mothers_day, easter, good_friday, womens_day, new_years)



fcst = train %>%  
  group_by(Segment_Channel) %>%
  do(make_forecast(., 2019)) %>% 
  dplyr::select(ds, Segment_Channel, yhat, yhat_upper)




monthly_pacing = fread("percentages.txt", sep= '|')
setnames(monthly_pacing, 'Fiscal_Mo', 'FISCAL_MO')



quarterly_pacing = fread("quarter_totals.txt", sep= '|')
quarterly_pacing[, quarter := ifelse(FISCAL_MO <= 3, 1, ifelse(FISCAL_MO <= 6, 2, ifelse(FISCAL_MO <= 9, 3, 4)))]
quarterly_pacing[, quarter_sum := sum(cust_month), by=c("quarter", "Customer_Type")]
quarterly_pacing[, table := cust_quarter/quarter_sum]
quarter_table = unique(quarterly_pacing[, c("Customer_Type", "quarter", "table", "quarter_sum")])
setnames(quarter_table, "Customer_Type", "Segment_Channel")


calc_year_end <- function(cast_dat, year, hist) {
  et = as.data.table(cast_dat)
  et[, FISCAL_MO := month(ds)]
  et[, quarter := ifelse(FISCAL_MO %in% c(1,2,3), 1, ifelse(FISCAL_MO %in% c(4,5,6), 2, ifelse(FISCAL_MO %in% c(7,8,9), 3,4)))]
  et[, day := format(as.Date(ds,format="%Y-%m-%d"), "%d")]
  et[, FISCAL_YR := year(ds)]
  et = et[ day == '01']
  et[, Used_Forecast := ifelse(yhat > 0, yhat, yhat_upper)]
  
  
  
  #wrap up
  df2 = hist[, c('Segment_Channel', 'FISCAL_YR', 'FISCAL_MO', 'y', 'Segment', 'Channel')]
  et2 = merge(et, df2, by=c('Segment_Channel', 'FISCAL_MO', 'FISCAL_YR'), all.x=TRUE)
  et2[, month_forecast := ifelse(is.na(y), Used_Forecast, y)]
  
  
  year_agg = merge(et2, monthly_pacing, by = c("Segment_Channel", "FISCAL_MO"))
  year_agg = merge(year_agg, quarter_table, by = c("quarter", "Segment_Channel"))
  year_agg[, Year_Number2 := Used_Forecast * percent]
  year_agg[, Year_Number2a := month_forecast * percent]
  year_agg[, qnum := table*Used_Forecast]
  year_agg[, quarter_forecast := table * month_forecast]
  
  
  return(year_agg)
}



final_fcst = calc_year_end(fcst, 2019, regress3) 
final_fcst = final_fcst[FISCAL_YR == 2019]


estimates = c(1, 0.544586951, 0.366562835, 0.337894522)
q_s = final_fcst %>% group_by(quarter) %>% summarize(sum(quarter_forecast))
q_s
sum(q_s*estimates) #1921

final_output = final_fcst[, c('quarter', 'Segment_Channel', 'FISCAL_MO', 'FISCAL_YR','month_forecast', 'quarter_forecast' , 'Channel', 'Segment')]


write.csv(final_output, 'prophet_august.csv')


final_output %>% 
  group_by(Channel, FISCAL_MO) %>%
  summarize(sum(month_forecast))

d = final_output %>% 
  group_by(Segment, FISCAL_MO) %>%
  summarize(sum(month_forecast))


