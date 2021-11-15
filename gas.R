# A function for bill estimate
# actual_usage = c(1855.24, 1839.04, 4644.57, 4398.56, 2852.05)

library(ggplot2)
library(tidyr)
library(dplyr)
est_mbill = function(monthly_usage, dpm = 30, rate, co){
  # Feed in monthly approx. usage and rates/breaks to this function and estimate bill
  
  stopifnot(length(rate)-1 == length(co), dpm <= 31, any(rate>0), any(co>0)) # sanity check
  daily_usage = rep(monthly_usage/dpm, dpm) + sapply(rep(1,dpm), jitter)
  charge = rep(0, dpm)
  idx = 1
  for(temp in daily_usage){
    for(i in 1:length(co)){
      if((temp-co[i]) >= 0){
        charge[idx] = charge[idx] + co[i]*rate[i]
        temp = temp - co[i]
      } else {
        charge[idx] = charge[idx] + temp*rate[i]
        break
      }
    }
    if(temp > 0) charge[idx] = charge[idx] + temp*rate[length(rate)]
    idx = idx + 1
  }
  return(sum(charge)/100)
}


usage = c(rep(1000,6) + c(-100, -300, -300, -300, -100, 400), rep(2000,6) + c(0, 2000, 2000, 2000, -100, -500)) 
usage = usage + sapply(sample(12,12,replace = F), jitter)*10
# peak x6, offpeak x6
names(usage) = c("Nov","Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct")

plot_df = data.frame(months = names(usage), usage = unname(usage))
plot_df$months = factor(plot_df$months, levels = month.abb)
ggplot(plot_df, aes(x = months, y = usage)) + geom_point()

providers = list(
  AGL = list(sc_s = 53.264200, # c/day
             rate_s = c(2.321550, 2.044350, 1.767150, 1.628550, 1.559250), #c/MJ
             co_s = c(100, 100, 100, 200), # MJ cut off (# = rate-1) 
             # winter
             sc_w = 53.264200, # c/day
             rate_w = c(2.252250, 2.009700, 1.732500, 1.628550, 1.593900), #c/0-50,50-100,100+
             co_w = c(100, 100, 100, 200)), 
  AGL_new = list(sc_s = 92.400000, # c/day
             rate_s = c(2.346300, 2.083400, 1.710500, 1.482800, 1.398100), #c/MJ
             co_s = c(100, 100, 100, 200), # MJ cut off (# = rate-1) 
             # winter
             sc_w = 92.400000, # c/day
             rate_w = c(2.245100, 1.982200, 1.643400, 1.456400, 1.355200), #c/0-50,50-100,100+
             co_w = c(100, 100, 100, 200)), 
  ALINTA = list(sc_s = 51.68, # c/day
                rate_s = c(2.22, 1.90, 1.58), #c/MJ
                co_s = c(50, 50), # MJ cut off (# = rate-1) 
                # winter
                sc_w = 51.68, # c/day
                rate_w = c(2.22, 2.01, 1.58), #c/0-50,50-100,100+
                co_w = c(50, 50)),
  GLOBIRD_GS = list(sc_s = 0.5663, # c/day
                    rate_s = c(1.88, 1.42), #c/MJ
                    co_s = c(100), # MJ cut off (# = rate-1) 
                    # winter
                    sc_w = 0.5663, # c/day
                    rate_w = c(1.88, 1.42), #c/0-50,50-100,100+
                    co_w = c(100)),
  GLOBIRD_US = list(sc_s = 0.7079, # c/day
                    rate_s = c(2.18, 1.74), #c/MJ
                    co_s = c(100), # MJ cut off (# = rate-1) 
                    # winter
                    sc_w = 0.7079, # c/day
                    rate_w = c(2.18, 1.74), #c/0-50,50-100,100+
                    co_w = c(100)),
  GLOBIRD_B = list(sc_s = 0.6050, # c/day
                   rate_s = c(1.83, 1.32), #c/MJ
                   co_s = c(100), # MJ cut off (# = rate-1) 
                   # winter
                   sc_w = 0.6050, # c/day
                   rate_w = c(1.83, 1.32), #c/0-50,50-100,100+
                   co_w = c(100))
)


dpm = c(31,30,31,31,28,31,30,31,30,31,31,30)

Bills = data.frame(month = names(usage))


for(j in 1:length(providers)){
  bill = rep(0, length(usage))
  for(i in 1:6)  bill[i] = est_mbill(monthly_usage = usage[i], dpm = dpm[i], rate = providers[[j]]$rate_s, co = providers[[j]]$co_s)
  for(i in 7:12) bill[i] = est_mbill(monthly_usage = usage[i], dpm = dpm[i], rate = providers[[j]]$rate_w, co = providers[[j]]$co_w)
  Bills = cbind(Bills, bill)
}
names(Bills) = c("Month", names(providers))


Bill_pltodf = gather(Bills, 'Supplier', 'Cost', -Month)

Bill_pltodf$Month = factor(Bill_pltodf$Month, levels = month.abb)
Bill_pltodf$Supplier = factor(Bill_pltodf$Supplier, levels = unique(Bill_pltodf$Supplier))

ggplot(Bill_pltodf, aes(x = Month, y = Cost, group = Supplier, col = Supplier)) + geom_line()
Bill_pltodf %>% 
  group_by(Supplier) %>% 
  summarise(Cost = sum(Cost))
