library(tidyverse)
library(lubridate)
library(plotly)

# Load Data

## MI: mobility index
MI <- read_csv("https://raw.githubusercontent.com/descarteslabs/DL-COVID-19/master/DL-us-m50_index.csv")
## CC: number of confirmed cases
CC <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
Population <- read_csv("USPopulation.csv")
Division <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

# Process Data

# 1. MI dataset

## select data at desired unit of analysis - state

admin2_na <- which(is.na(MI$admin2))
mi_state <- MI[admin2_na,]
mi_state_sub <- cbind(mi_state %>% select('fips'),mi_state %>% select('admin1'),mi_state %>% select('2020-03-01':EndDate))

## reshape: wide to long

mi_long <- gather(mi_state_sub,Date,Mobility_Index,-fips,-admin1)
mi_long <- mi_long %>% rename(State=admin1)

## transform data type

mi_long$Date <- parse_date_time(mi_long$Date,orders = c("ymd", "mdy","dmy"))

# 2. CC dataset

## Tranform date format to align with the CC dataset
parse_date_time(EndDate, orders = c("mdy","dmy","ymd"))

month<-substr(EndDate,6,7)
day<-substr(EndDate,9,10)
year<-substr(EndDate,3,4)
EndDate_cc=paste(month,day,year,sep='/')
EndDate_cc

## aggregate data at desired unit of analysis - state
cc_sub <- cbind(CC%>% select('Province_State'),CC%>%select('3/1/20':EndDate_cc))
cc_sub <- cc_sub %>% rename(State=Province_State)
cc_sub_1 <- cc_sub %>% 
  group_by(State) %>% 
  summarise_all(funs(sum))

# 3. Population dataset

## clean dataset
Population$State <- gsub("[[:punct:]]","",Population$State)

# Integrate Data 

Pop_Div <- merge(Population, Division, by="State")
Pop_Div_CC <- merge(Pop_Div,cc_sub_1,by="State")

pop_div_cc_long <- gather(Pop_Div_CC,Date,Confirmed_Cases_Count,-State,-'State Code',-Region,-Division,-'Population, 2019 Estimate')

pop_div_cc_long$Date <- parse_date_time(pop_div_cc_long$Date, orders = c("mdy","dmy","ymd"))

df_merged <- merge(pop_div_cc_long,mi_long,by=c("State","Date"))

## Transform integrated dataframe

df <- df_merged[c(1,2,3,4,8,5,6,7,9)]
df <- df %>% rename(Code='State Code')

#subset data
df$Date <- as.character(df$Date)
df_date <- subset(df,df$Date == EndDate)

# Examine data
glimpse(df)
summary(df)

# Visualization

# 1.Motion Scatter Bubble Chart

## Transform master df into monthly aggregated df

df$Date <- parse_date_time(df$Date,orders = c("ymd", "mdy","dmy"))

df_m_avg <- df %>% group_by(month=floor_date(Date, "month"),State, Region) %>%
  summarize(cc_monthly_avg=mean(Confirmed_Cases_Count),
            mi_monthly_avg=mean(Mobility_Index),
            pop=mean(`Population, 2019 Estimate`))

##Round numeric values
is.num <- sapply(df_m_avg, is.numeric)
df_m_avg[is.num] <- lapply(df_m_avg[is.num], round, 2)
##Rename column
df_m_avg <- df_m_avg[c(2,1,3,4,5,6)]
##Transform column values
df_m_avg$month <- as.character(df_m_avg$month)
df_m_avg$month = substr(df_m_avg$month,1,7)

##Plot
fig <- df_m_avg %>%
  plot_ly(
    x = ~cc_monthly_avg, 
    y = ~mi_monthly_avg, 
    size = ~pop,
    sizes = c(10, 60),
    color = ~Region, 
    frame = ~month, 
    text = ~State,
    type = 'scatter',
    mode = 'markers',
    marker = list(opacity = 0.5, sizemode = 'diameter')
  )
fig <- fig %>% layout(
  xaxis = list(
    title = "Number of Confirmed Cases, Monthly Average<br>Log Scale",
    titlefont = list(size=8),
    type = "log",
    range = c(1, 7),
    showgrid = FALSE),
  yaxis = list(title = "Descardes Mobility Index, Monthly Average",
               titlefont = list(size=8),
               zerolinecolor = toRGB("white"),
               showgrid = FALSE),
  legend = list(x = 0.0, y = 1.1,
                orientation = 'h',
                size = 8,
                font = list(
                  family = "sans-serif",
                  size = 10,
                  color = "#000"))
  )%>%
  animation_slider(
    currentvalue = list(prefix = "Month: ", font = list(color="orange"))
  )

fig

# 2. Map - CC

## give state boundaries a white border
l <- list(color = toRGB("white"), width = 1)
## specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig_cc <- plot_geo(df_date, locationmode = 'USA-states')
fig_cc <- fig_cc %>% add_trace(
  z = ~Confirmed_Cases_Count, text = ~State, locations = ~Code,
  color = ~Confirmed_Cases_Count, 
  colors = 'Blues',
  marker = list(line = l),
  showlegend = FALSE,
  showscale=FALSE
)

fig_cc <- fig_cc %>% layout(
  geo=g,
  showlegend = FALSE
)
fig_cc

# 3. Map - MI

fig_mi <- plot_geo(df_date, locationmode = 'USA-states')
fig_mi <- fig_mi %>% add_trace(
  z = ~Mobility_Index, text = ~State, locations = ~Code,
  color = ~Mobility_Index, 
  colors = 'Greens',
  marker = list(line = l),
  showlegend = FALSE,
  showscale=FALSE
)

fig_mi <- fig_mi %>% layout(
  geo=g,
  showlegend = FALSE
)
fig_mi
