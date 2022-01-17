## Code written by Fabrizio Benedetti, fabrizio.benedetti.82@gmail.com
## if you find it useful you can send a donation to:
## 0x86Cc82677ceFB3f6FD7dfd8f88eAcAa7ca38d83F
## The following script should help you to know when to harvest your ACS tokens from
## a FARM vault
## Take in mind that there are several unavoidable limitations as: market price volatility,
## APR changes, that cannot be factored in the script.  Therefore this is just an indicator
## of "when to harvest" if the parameters are fixed in a given period of time


library(ggplot2)


capital=10000 # capital in $ of your FARM VAULT
farm_apr_daily=0.00338  ## write the number you see in "Farm Daily:" in your FARM VAULT.  The script take care of the %
farm_apr_daily=farm_apr_daily/100
acs_vault_daily=0.104  ## write the number you see in "Farm Daily:" in your ACS VAULT.  The script take care of the %
acs_vault_daily=acs_vault_daily/100 
acs_value=9.21 ## value of a ACS token in $
acs_fee=0.03 #harvesting fee
period=365 ## I consider a period of one year

### first we find after how many days we have a FARM worth to be harvested since there is a 0.03 fee
min_day=1
for(days in 1:100){
  est_base=  (capital*farm_apr_daily*days)/acs_value
  if(est_base>acs_fee){
    min_day=days
    break
  }
}


## in res_df we store the results for all the compounding interval we will compute
res_df=NULL
## "comp_intervals" is a list that represent the intervals we want to simulate, in days, between harvesting events
## if we farm every 2 days what wold be the total ACS token in the vault?
## if we farm every 3 days what would be .... 
## if we farm every 4 days...
comp_intervals=c(min_day:(period) ) 
for(days_interval in comp_intervals){
  ## in a "period" there are a certain number of compounding events
  ## imagine we are testing an harvesting event every 30 days, in a year there woud be 12 events
  ## 30, 60, 90.... until 360
  aseq=seq(days_interval, period, days_interval)

  ##  the interest give by the FARM vault is "capital*days_in_the_vault*farm_apr_daily"
  ## if we want to find the number of acs tokens we have to divide by acs_value and subtract the harvesting fee
  harvest_val= (capital*days_interval*farm_apr_daily)/acs_value -acs_fee
  
  acs_vault=0

  for(i in aseq){
    ## first we compute the interest we obtained in the ACS vault
    for(j in 1:days_interval){
      acs_interest=acs_vault*acs_vault_daily
      acs_vault=acs_vault+acs_interest
    }
    
    ## then we add the farmed ACS, to the ACS vault
    acs_vault=acs_vault+harvest_val
  }
  
  ## there may be additional days before the end of the period
  ## as before, if we have a compounding event every 30 days, and we consider
  ## a period of 365 days, there are 5 days that are not considered. We need to take
  ## them into consideration
  harvest_val=capital*(period-max(aseq))*farm_apr_daily/acs_value
  for(j in max(aseq):period){
    acs_interest=acs_vault*acs_vault_daily
    acs_vault=acs_vault+acs_interest
  }
  acs_vault=acs_vault+acs_interest+harvest_val
  
  adf=data.frame(comp_days=days_interval, farm_acs=acs_vault, harvest_val=harvest_val)
  res_df=rbind(res_df, adf)
  
}


a_text=paste0("Harvest at: ", round(res_df$harvest_val[which.max(res_df$farm_acs)], 3 ), " every ",
              res_df$comp_days[which.max(res_df$farm_acs)], " days")

## we can also plot the results
ggplot(res_df, aes(x=comp_days, y=farm_acs))+
  geom_point()+
geom_point(data = res_df[which.max(res_df$farm_acs), ], color="red", 
           size=3)+
  ggtitle(a_text)

