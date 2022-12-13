library(stringr)
library(tidyverse)
library(lubridate)
library(survival)
library(KMsurv)
library(survMisc)

setwd("C:/Users/Hyunku Kang/Desktop/Statistics Stuff/Survival Analysis")
# Raw Data for congressperson's tenure and termination
raw = read.csv("congress_time.csv")%>%
  as_tibble()
# District Party Voting Index
pvi_df = read.csv("pvi_df.csv")%>%
  as_tibble()
# State Initial Tibble
state_ini =  read.csv("state_ini.csv")
## Constituency Information
df_demographics = as_tibble(read.csv("C:/Users/Hyunku Kang/Desktop/Statistics Stuff/Survival Analysis/df_demographics.csv"))

## Data Tidying
df = raw%>%
  replace_na(list(Death = 0, Midterm.Resign = 0, Retired = 0, Other.Office = 0, 
                  Defeated.In.Primary = 0, Defeated.In.General = 0, Reelected = 0))%>%
  mutate(Uncensored = 1 - Reelected)%>%
  ## Working with Dates and Time
  mutate(Seniority.Date = mdy(Seniority.Date), Birth.Date = mdy(Birth.Date))%>%
  mutate(End.Date = mdy("January 1st, 1970")+ ifelse(End.Date != "", mdy(End.Date), mdy("January 3, 2023")))%>%
  mutate(Time.Served = time_length(difftime(End.Date, Seniority.Date), "years"))%>%
  mutate(Start.Age = time_length(difftime(Seniority.Date, Birth.Date), "years"))%>%
  ## Creating Constituency Labels for Merging Demographic Data
  mutate(State = str_extract(District, ".*(?=\\?)"),
         Dist.Num = str_pad(str_extract(District, ("(?<=\\?).*")),2, pad = "0"))%>%
  left_join(.,state_ini)%>%
  mutate(const_size = 1/Seats.by.State)%>%
  mutate(Dist = paste(Initial, "-", Dist.Num, sep = ""))%>%
  left_join(.,pvi_df, by = "Dist")%>%
  ## Swing District
  mutate(swing = ifelse(abs(degree)< 7, "Yes", "No"))%>%
  left_join(df_demographics)%>%
  select(-c(Rank, District, Initial, PVI, Dist.Num, Clinton.., Obama.., Trump.., Romney.., 
            Seats.by.State))%>%
  select(Representative, Time.Served, everything())
write.csv(df, "df.csv")

time_augmented <- read.csv("time_augmented.csv")
## Creating Augmented Dataset for Regression with Timed Covariates
df_augmented = full_join(time_augmented,df)%>%
  mutate(Age = round(Age,2))%>%
  mutate(Time = round(time_length(difftime(Time, Seniority.Date),"years"),2))%>%
  select(Representative, Party, Status, Uncensored, Time.Served, Region, const_size, swing, 
         percent_white, percent_black, percent_asian, percent_hispanic, 
         per_capita_income, Urb_Idx, Age, Time, Modified.Legislative.Efficiency)%>%
  select(Representative, Status, Uncensored, Time, Time.Served, Age, Modified.Legislative.Efficiency,everything())

## Creating Survival Object
df1 <- tmerge(df, df, id = Representative, end = event(Time.Served, Status))
df1 <- tmerge(df1, df_augmented, id = Representative, Age = tdc(Time, Age), 
              Leg.Eff = tdc(Time, Modified.Legislative.Efficiency))
colnames(df1)

## Regression on Total Risk
df0 <- tmerge(df, df, id = Representative, end = event(Time.Served, Uncensored))
df0 <- tmerge(df0, df_augmented, id = Representative, Age = tdc(Time, Age), 
              Leg.Eff = tdc(Time, Modified.Legislative.Efficiency))
surv_fit0 = coxph(Surv(tstart, tstop, Uncensored == 1) ~ Age + Leg.Eff + swing + Party + 
                    scale(const_size) + Region + percent_white + 
                    +per_capita_income + Urb_Idx,
                  method = "efron", df0)
x = summary(surv_fit0);x
write.csv(x$coefficients, "result0.csv")

## Nested Competitive Risk Regression
surv_fit1 = coxph(Surv(tstart, tstop, end == 1) ~ Age + Leg.Eff + swing + Party + 
                   scale(const_size) + Region + percent_white + 
                 +per_capita_income + Urb_Idx,
                 method = "efron", df1)
x = summary(surv_fit1);x # Too Few Data
write.csv(x$coefficients, "result1.csv")

df2 <- df1%>%
  filter(end!= 1)
surv_fit2 = coxph(Surv(tstart, tstop, end == 2) ~ Age + Leg.Eff + swing + Party + 
          scale(const_size) + Region + percent_white + 
          +per_capita_income + Urb_Idx, method = "efron", df2)
x = summary(surv_fit2);x
write.csv(x$coefficients, "result2.csv")

df3 <- df2%>%
  filter(end!= 2)
surv_fit3 = coxph(Surv(tstart, tstop, end == 3) ~ Age + Leg.Eff + swing + Party + 
                    scale(const_size) + Region + percent_white + 
                    +per_capita_income + Urb_Idx, method = "efron", df3)
x = summary(surv_fit3);x
write.csv(x$coefficients, "result3.csv")

df4 <- df2%>%
  filter(end!= 2)
surv_fit4 = coxph(Surv(tstart, tstop, end == 4) ~ Age + Leg.Eff + swing + Party + 
                    scale(const_size) + Region + percent_white + 
                    +per_capita_income + Urb_Idx, method = "efron", df4)
x = summary(surv_fit4);x
write.csv(x$coefficients, "result4.csv")

df5 <- df2%>%
  filter(end != 3)%>%
  filter(end != 4)
surv_fit5 = coxph(Surv(tstart, tstop, end == 5) ~ Age + Leg.Eff + swing + Party + 
                    scale(const_size) + Region + percent_white + 
                    +per_capita_income + Urb_Idx, method = "efron", df5)
x = summary(surv_fit5);x
write.csv(x$coefficients, "result5.csv")

df6 <- df5%>%
  filter(end != 5)
surv_fit6 = coxph(Surv(tstart, tstop, end == 6) ~ Age + Leg.Eff + swing + Party + 
                    scale(const_size) + Region + percent_white + 
                    +per_capita_income + Urb_Idx, method = "efron", df6)
x = summary(surv_fit6);x
write.csv(x$coefficients, "result6.csv")
rm(list = c("df1", "df2", "df3", "df4", "df5", "df6", "surv_fit1", "surv_fit2", 
"surv_fit3", "surv_fit4", "surv_fit5", "surv_fit6", "x"))

## Parametric Fitting
surv_df = Surv(time=df$Time.Served, event= 1-df$Reelected, type= "right")
df_wei = survreg(surv_df ~ swing + Party + 
                   scale(const_size) + Region + percent_white + 
                   +per_capita_income + Urb_Idx, dist="weibull", data = df)
summary(df_wei)

surv_fit = survfit(formula = surv_df ~ 1)
surv_fit_sum = summary(surv_fit)
KM_surv = surv_fit_sum$surv
KM_surv = KM_surv[-length(KM_surv)]
log_time = log(surv_fit_sum$time)
log_time = log_time[-length(log_time)]
plot(log_time, log(-log(KM_surv)), type="l", lty=1, main="Weibull diagnostic plot",
     ylab="log cumulative hazard",xlab="Survival Time (log scale)")
abline(coef(lm(log(-log(KM_surv))~log_time)), col="blue")

library(lmtest)
df_exp = survreg(surv_df ~ degree*Party + const_size, dist="exponential", data=df)
df_loglogi = survreg(surv_df ~ degree*Party + const_size, dist="loglogistic", data=df)
lrtest(df_exp, df_loglogi)
lrtest(df_exp, df_wei)
rm(list = c("df_exp", "df_loglogi", "df_wei", "surv_fit_sum"))

## Plotting KM
plot(surv_fit, conf.int = TRUE, mark.time = TRUE,
     xlab = "Number of Years", 
     ylab = "Probability",
     main="K-M Estimate")

surv_fit = survfit(formula=surv_df ~ swing, data= df, conf.type="plain")
plot(surv_fit, conf.int = TRUE, col=c(4,2), mark.time = TRUE,
     xlab = "Number of Years", 
     ylab = "Probability",
     main="Survival Function by Swing")
legend('topright',c('Safe Seats','Swing Seats'), col = c(4,2), lty = c(1,1))

surv_fit = survfit(formula=surv_df ~ Party, data= df, conf.type="plain")
plot(surv_fit, conf.int = TRUE, col=c(4,2), mark.time = TRUE,
     xlab = "Number of Years", 
     ylab = "Probability",
     main="Survival Function by Party")
legend('topright',c('Democrats','Republican'), col = c(4,2), lty = c(1,1))
