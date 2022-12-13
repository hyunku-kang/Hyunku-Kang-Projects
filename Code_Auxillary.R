library(tidyverse)
library(stringr)
library(lubridate)

# Working with dates
x = "6/9/22
7/8/18"
y = str_split(x, "\\n")[[1]]
z = as_tibble(data.frame(matrix(nrow=length(y),ncol = 1)))
for (i in 1:length(y)){
    a = as.integer(str_split(y[i], "\\/")[[1]][1])
    b = as.integer(str_split(y[i], "\\/")[[1]][2])
    c = as.integer(paste("19", str_split(y[i], "\\/")[[1]][3], sep = ""))
    z[i,1] = as.character(make_datetime(year = c, month = a, day = b)) 
}

# Extracting Birth Year for Congressmen
library(jsonlite)
MySearch <- function(srsearch){
  FullSearchString <- paste("http://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=",srsearch,"&format=json",sep="")
  Response <- fromJSON(FullSearchString)
  return(Response)
}
x = "Nancy Pelosi
Paul Ryan"
y = str_split(x, "\\n")[[1]]
z = as_tibble(data.frame(matrix(nrow=length(y), ncol = 1)))
for (i in 1:length(y)){
  search_string = paste(str_split(y[i], " ")[[1]][1], 
                        str_split(y[i], " ")[[1]][2], "(politician)", sep = "_")
  Response <- MySearch(search_string)
  z[i,1] = str_extract(Response[["query"]][["search"]][["snippet"]], "(?<=born ).*(?=\\))")[1]
}

# Data Tidying on Constituency Data
urban <- read.csv("urban.csv")
df_const = tidycensus::get_acs("congressional district", table="B03002", output="wide", cache_table=TRUE, year= 2020 , survey="acs5")
df_race = as_tibble(data.frame(region = df_const$GEOID,
                               District = df_const$NAME,            
                               total_population = df_const$B03002_001E))
df_race = df_race%>%
  mutate(percent_white = round(df_const$B03002_003E / total_population * 100),
         percent_black = round(df_const$B03002_004E / total_population * 100),
         percent_asian = round(df_const$B03002_006E / total_population * 100),
         percent_hispanic = round(df_const$B03002_012E / total_population * 100))
df_income = tidycensus::get_acs("congressional district", table="B19301", output="wide", cache_table=TRUE, year = 2020, survey="acs5")
df_income = df_income[, c("GEOID", "B19301_001E")]
colnames(df_income) = c("region", "per_capita_income")
df_demographics = df_race%>%
  left_join(df_income)%>%
  filter(total_population != 0)%>%
  mutate(State = str_extract(District, "(?<=\\), ).*"), 
         Dist.Num = str_extract(District, "(?<= District ).*(?= \\(116)"))%>%
  mutate(Dist.Num = str_pad(ifelse(Dist.Num == "(at Large)", "AL", Dist.Num), 2, pad = 0))%>%
  left_join(.,state_ini)%>%
  mutate(Dist = paste(Initial, "-", Dist.Num, sep = ""))%>%
  left_join(urban)%>%
  select(-c(region, District, State, Dist.Num, Initial, Region))
rm(list = c("df_const","df_income","df_race", "urban"))
write.csv(df_demographics, "df_demographics.csv")

## Working With PVI
pvi_df%<>%
  mutate(party_lean = str_extract(PVI,".*(?=\\+)"), degree = as.integer(str_extract(PVI,"(?<=\\+).*")))%>%
  replace_na(list(party_lean = "EVEN", degree= 0))
for (i in 1:nrow(pvi_df)) {
  if(pvi_df[i, 8] == "R"){
    pvi_df[i,9] = - pvi_df[i,9]
  }
}
write.csv(pvi_df, "pvi_df.csv")


# Augmentation for Timed Covariates
nearest_general = function(new_date){
  if(month(new_date) == 1 & day(new_date) == 3){
    return(new_date)
  }
  else{
    if(year(new_date)%%2 == 1){
      year(new_date) = year(new_date) + 2
      month(new_date) = 1
      day(new_date) = 3 
      return(new_date)
    }
    else{}
    year(new_date) = year(new_date) + 1
    month(new_date) = 1
    day(new_date) = 3
    return(new_date)
  }
}

time_slice = function(x){
  Name = x[[1]]
  Birth.Date = x[[2]]
  Start.Date = x[[3]]
  End.Date = x[[4]]
  x%<>%
    select(Representative, Seniority.Date, Start.Age)%>%
    rename(Time = Seniority.Date, Age = Start.Age)
  Cur.Date = nearest_general(Start.Date)
  while(Cur.Date < End.Date){
    if(Cur.Date != Start.Date){
      x%<>%
        add_row(Representative = Name, Time = Cur.Date, Age = time_length(difftime(Cur.Date, Birth.Date), "years"))
    }
    Cur.Date = Cur.Date + years(2)
  }
  return(x)
}

x = df %>%
  select(Representative, Birth.Date, Seniority.Date, End.Date, Start.Age)
y = time_slice(x[1,])
for (i in 2:nrow(df)){
  z = time_slice(x[i,])
  y = bind_rows(y,z)
}
write.csv(y, "time_augmented.csv")
