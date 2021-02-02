

```{r}
setwd("C:/Users/acer/Desktop/Data_Science_project")
library(xml2)
library(rvest)
library(xml2)
library(XML)
library(rvest)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyquant)
#install.packages("quantmod")
#install.packages("TTR")
library(quantmod)
library(TTR)
library(tidyquant)
library(timetk)
setwd("C:/Users/acer/Desktop/Data_Science_project")
```


## Import Tickers
```{r}
sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
```

```{r}
sp500 <- read_html(sp500_url) %>% 
  html_node("table") %>% 
  html_table()

sp500<-sp500 %>% select('Symbol','Security','SEC filings','GICS Sector', 'Headquarters Location')
names(sp500) <- c("Ticker", "Name", "Sector", "Industry", "HQ_Location")

save(sp500, file = "sp500.RData")
```
# importation of price Data sp&500
```{r}
sp500_price<-read.csv("^GSPC.csv")
sp500_price<-sp500_price %>% 
  mutate(Date=as.Date(Date))
sp500_price <-sp500_price %>% mutate(
  Movement = ifelse(Open< Close, "Up", "Down")
)
names(sp500_price) <- c("Date","Open","High","Low","Close","Adjusted","Volume","Movement")
sp500_price

```
#jointure de returns avec sp500_price
```{r}
stock_join<-inner_join(returns,sp500_price,by = c("Date" = "Date"), suffix = c("_stock", "_sp500_price"))
View(stock_join)
```
## Import Price Data
```{r}
returns <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adjusted", "Volume", "Ticker")

for(symbol in sp500$Ticker){
  print(symbol)
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol, "?period1=1280102400&period2=1595721600&interval=1d&events=history")
  print(url)
  ret <- try(read_csv(url))
  
  if(mode(ret) != "character"){
    ret$Ticker <- symbol
    returns <- rbind(returns, ret)
  }
  
}
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adjusted", "Volume", "Ticker")
returns<- returns %>%  select("Date","Ticker", "Open", "High", "Low", "Close", "Adjusted", "Volume")
returns <- returns %>% mutate(
  Open = as.numeric(Open),
  High = as.numeric(High),
  Low = as.numeric(Low),
  Close = as.numeric(Close),
  Adjusted= as.numeric(Adjusted),
  Volume=as.numeric(Volume),
)

returns <- returns %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
  
)
save(returns, file = "returns.RData")


returns_long <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
returns_long <- returns %>%left_join(sp500 %>% select("Ticker", "Name", "Sector", "Industry"), by = c("Ticker" = "Ticker"))
View(returns_long)
```
## Performance calcs
```{r}
performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")

i <- 1
for(Tickers in unique(returns_long$Ticker)){
  #print(Tickers)
  returns_long_by_ticker<- returns_long %>% filter(Ticker == Tickers) %>% arrange(desc(Date))
  View(returns_long_by_ticker)
  thrity_day <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[21])/returns_long_by_ticker$Adjusted[21]

  ninety_day <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[63])/returns_long_by_ticker$Adjusted[63]
  one_year <- (returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[253])/returns_long_by_ticker$Adjusted[253]
  three_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[759])/returns_long_by_ticker$Adjusted[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[1265])/returns_long_by_ticker$Adjusted[1265]))^(1/5)-1
  ten_year <- (1 + ((returns_long_by_ticker$Adjusted[1] - returns_long_by_ticker$Adjusted[2518])/returns_long_by_ticker$Adjusted[2518]))^(1/10)-1
  
  performance_summary[i, 1] <- Tickers
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  
  performance_summary[i, 7] <- ten_year
  
  i <- i + 1
}
View(performance_summary)
```


```{r}
load("sp500.RData")

performance_summary <- performance_summary %>% left_join(sp500, by = c("Ticker" = "Ticker"))
save(performance_summary, file = "performance_summary.RData")
save(returns_long,file="returns_long.RData")
View(performance_summary)



```
## Industry Chart
```{r}
sector <- sp500 %>% filter(Ticker == Tickers) %>% select(Sector) %>% as.character()
industry <- sp500 %>% filter(Ticker == Tickers) %>% select(Industry) %>% as.character()
print(industry)

industry_summary_data <- performance_summary %>% 
  filter(Sector == sector) %>% 
  mutate(
   isIndustry = ifelse(Industry=="Industrials","Industry", "Non_Industry"))
View(industry_summary_data)

industry_chart <- ggplot(industry_summary_data) +
  geom_bar(aes(x = Industry, y = One_year, fill=isIndustry), stat = "summary", fun = "mean") +
  scale_fill_manual(values = c(Industry = "#ffff00", Non_Industry = "#0066ff")) +
  ylab("One Year Return") +
  labs(
    title = "Industry Returns",
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::percent) 


industry_chart


```
#visualisation de SP&500
```{r}
sp<-sp500_price %>% ggplot(mapping=aes(x =Date,y=Adjusted) )+
  geom_line(color="red")+
  scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Adjusted_Close") +
  labs(
    title = "SP&500",
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)
sp
```
#volume sp500
```{r}
# volume 
ggplot(sp500_price,aes(x=Date,y=Volume)) + 
  geom_line(color = "green") +theme(plot.title = element_text(hjust = 0.5)) +scale_y_log10()+
  xlab("Date") + 
  ylab("Volume") +
  labs(
    title =" SP&500 index",
    
    caption = "Source: Yahoo! Finance"
  ) 

```
#moyenne mobile sp500
```{r}
moyenne1<-sp500_price %>% 
  mutate(Rm_200=rollmean(Adjusted,24,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adjusted,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adjusted,50,na.pad=TRUE, align="right")) %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Adjusted))+scale_y_log10()+
  geom_line(aes(y=Rm_200, color="MA_200")) + 
  geom_line(aes(y=Rm_100, color="MA_100")) +
  geom_line(aes(y=Rm_50, color="MA_50")) 
 
moyenne1

```
# daily_returns sp500
```{r}
daily_returns_sp <- sp500_price %>%
  tq_transmute(select = Adjusted,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "Daily_returns_sp") # renames the column
daily_returns_sp %>%
  ggplot(aes(x =Date,y =Daily_returns_sp)) +
  geom_line(color="pink")+
  theme_classic() +
  xlab("Date") + 
  ylab("daily_returns of SP&500") +
  labs(
    title = "SP_500 returns ",
    
    
    caption = "Source: Yahoo! Finance"
  )
```
#visualization with time series 
```{r}

library(tseries)
ticker_dataSelect<-sp500_price %>% select("Date" , "Open","Close","Adjusted","Volume","High","Low")
ticker_dataSelect
ticker_data_series<-xts(ticker_dataSelect[,-1],order.by =as.Date(ticker_dataSelect$Date))
str(ticker_data_series)
ticker_data_series%>%Ad()%>%chartSeries()
ticker_data_series%>%chartSeries(TA='addBBands(n=20,sd=2);addVo();addMACD();addSMA(n=24,on=1,col="blue");addSMA(n=48,on=1,col="red");addMomentum(n=1)',theme=chartTheme("white"),subset='2012')

```
# choisir l'action désiré
```{r}
ticker <- "AMZN"
```
#filtrer le dataframe
```{r}
charting_data <- returns_long %>% filter(Ticker == ticker)
```
#boxplot
```{r}
ggplot(data=charting_data) +
  geom_boxplot(aes(x = (Date), y=Adjusted, fill=Movement ))

```
#chart de prix d'action
```{r}
stock<-charting_data %>% ggplot(mapping=aes(x =Date,y=Adjusted) )+
  geom_line(color="purple")+
  scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Adjusted_Close") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)
stock

```
#Volume
```{r}
# volume 
ggplot(charting_data,aes(Date,Volume)) + 
  geom_line(color = "darkblue") +theme(plot.title = element_text(hjust = 0.5)) +scale_y_log10()+
  xlab("Date") + 
  ylab("Volume") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) 

```
#moving average 
```{r}
moyenne<-charting_data %>% mutate(Rm_200=rollmean(Adjusted,24,na.pad=TRUE, align="right")) %>%
  mutate(Rm_100=rollmean(Adjusted,100,na.pad=TRUE, align="right")) %>% 
  mutate(Rm_50=rollmean(Adjusted,50,na.pad=TRUE, align="right")) %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=Adjusted))+scale_y_log10()+
  geom_line(aes(y=Rm_200, color="MA_200")) + 
  geom_line(aes(y=Rm_100, color="MA_100")) +
  geom_line(aes(y=Rm_50, color="MA_50")) 
 
moyenne
```
#visualization with time series 
```{r}
library(tseries)
ticker_dataSelect<-charting_data %>% select("Date" , "Open", "High","Low","Close","Adjusted","Volume")
ticker_dataSelect
ticker_data_series<-xts(ticker_dataSelect[,-1],order.by =as.Date(ticker_dataSelect$Date))
str(ticker_data_series)

ticker_data_series%>%Ad()%>%chartSeries()
#ticker_data_series%>%Ad()%>%dailyReturn(type='log')
ticker_data_series%>%chartSeries(TA='addBBands(n=20,sd=2);addVo();addMACD();addSMA(n=20,on=1,col="blue");addSMA(n=200,on=1,col ="red");addRSI();addSMA(n=50,on=1,col = "red");addSMA(n=48,on=1,col="green");addROC(n=7);addMomentum(n=1)',theme=chartTheme("white"),subset='2012')

```
## Performance Charting
```{r}


performance_summary_data <- performance_summary %>% 
  filter(Ticker == ticker) %>% 
  select(Thirty_days, Ninety_days, One_year, Three_years, Five_years, Ten_years)

performance_summary_data <- performance_summary_data %>% gather("Period", "Return")

performance_summary_data <- performance_summary_data %>% mutate(
  Period = case_when(
    Period == "Thirty_days" ~ "1 Month", 
    Period == "Ninety_days" ~ "1 Quarter", 
    Period == "One_year" ~ "1 Year", 
    Period == "Three_years" ~ "3 Years", 
    Period == "Five_years" ~ "5 Years", 
    Period == "Ten_years" ~ "10 Years", 
  )
)

performance_summary_data$Period <- factor(performance_summary_data$Period, levels = c("1 Month", "1 Quarter", "1 Year", "3 Years", "5 Years", "10 Years"))

performance_chart <- ggplot(performance_summary_data) +
  geom_bar(aes(x = Period, y = Return), stat = "identity", fill = "purple") +
  ylab("Annualized Return") +
  labs(
    title =  paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = "returns",
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::percent) 
performance_summary_data
performance_chart
```
 
#group by industry

```{r}
x<-"Energy"
```
#filtrer
```{r}
data_segmentation2<-returns_long %>% filter(Industry == x)

```

```{r}
data_segmentation <- data_segmentation2 %>% arrange(desc(Date))
```


```{r}
performance_summary2 <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary2) <- c("Industry", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")
i <- 1
for(Industries in unique(returns_long$Industry)){
  #print(Industry)
  returns_long_by_Industry<- returns_long %>% filter(Industry==Industries) %>% arrange(desc(Date))
  
  thrity_day <- (returns_long_by_Industry$Adjusted[1] - returns_long_by_Industry$Adjusted[21])/returns_long_by_Industry$Adjusted[21]
  ninety_day <- (returns_long_by_Industry$Adjusted[1] - returns_long_by_Industry$Adjusted[63])/returns_long_by_Industry$Adjusted[63]
  one_year <- (returns_long_by_Industry$Adjusted[1] - returns_long_by_Industry$Adjusted[253])/returns_long_by_Industry$Adjusted[253]
   three_year <- (1 + ((returns_long_by_Industry$Adjusted[1] - returns_long_by_Industry$Adjusted[759])/returns_long_by_Industry$Adjusted[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_Industry$Adjusted[1]-returns_long_by_Industry$Adjusted[1265])/returns_long_by_Industry$Adjusted[1265]))^(1/5)-1
ten_year <- (1 + ((returns_long_by_Industry$Adjusted[1]-returns_long_by_Industry$Adjusted[2518])/returns_long_by_Industry$Adjusted[2518]))^(1/10)-1
  
  
  performance_summary2[i, 1] <- Industries
  performance_summary2[i, 2] <- thrity_day
  performance_summary2[i, 3] <- ninety_day
  performance_summary2[i, 4] <- one_year
  performance_summary2[i, 5] <- three_year
  performance_summary2[i, 6] <- five_year
  performance_summary2[i, 7] <- ten_year
  
  i <- i + 1
}
returns_long_by_Industry
performance_summary2
```
#performance_chart of l'industry 
```{r}


performance_summary_data2 <- performance_summary2 %>% 
  filter(Industry==x) %>% 
  select(Thirty_days, Ninety_days, One_year, Three_years, Five_years, Ten_years)

performance_summary_data2 <- performance_summary_data2 %>% gather("Period", "Return")

performance_summary_data2 <- performance_summary_data2 %>% mutate(
  Period = case_when(
    Period == "Thirty_days" ~ "1 Month", 
    Period == "Ninety_days" ~ "1 Quarter", 
    Period == "One_year" ~ "1 Year", 
    Period == "Three_years" ~ "3 Years", 
    Period == "Five_years" ~ "5 Years", 
    Period == "Ten_years" ~ "10 Years", 
  )
)

performance_summary_data2$Period <- factor(performance_summary_data2$Period, levels = c("1 Month", "1 Quarter", "1 Year", "3 Years", "5 Years", "10 Years"))

performance_chart2 <- ggplot(performance_summary_data2) +
  geom_bar(aes(x = Period, y = Return), stat = "identity", fill = "red") +
  
  ylab("Annualized Return") +
  labs(
    title =paste0(data_segmentation2$Industry[1], " (", x, ")"),
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::percent)

performance_chart2
```
#rendement_journalier
```{r}
rendement_journalier <- data_segmentation %>%
  tq_transmute(select = Adjusted,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "rendement_journalier") # renames the column
rendement_journalier %>%
  ggplot(aes(x =Date,y =rendement_journalier)) +
  geom_line(color="green")+
  theme_classic() +
  xlab("Date") + 
  ylab("rendement_journalier") +
  labs(
    title =paste0(data_segmentation2$Industry[1], " (", x, ")"),
    
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  )
```
# calcule des indicateurs techniqus :
```{r}
library(TTR)

```
#Simple Moving Average

```{r}
sma1 <-SMA(Cl(charting_data),n=20)
sma2 <-SMA(Cl(charting_data),n=50)
sma3 <-SMA(Cl(charting_data),n=200)
View(sma1)
View(sma2)
View(sma3)
```
#Exponential moving average
```{r}

ema1 <-EMA(Cl(charting_data),n=20)
ema2 <-EMA(Cl(charting_data),n=50)
ema3 <-EMA(Cl(charting_data),n=200)
View(ema1)
View(ema2)
View(ema3)

```
#bonds de Bollinger
```{r}
bb <-BBands(Cl(charting_data),s.d=2)
View(bb)
```
#momentum
```{r}
M <- momentum(Cl(charting_data), n=2)
head (M,n=1000000)

```
#ROC(Rate of Change)
```{r}
ROC <- ROC(Cl(charting_data),n=2)
# 2-day ROC
View(ROC)
#head(ROC,n=100000)
```
#MACD
```{r}
macd <- MACD(Cl(charting_data), nFast=12, nSlow=26,
             nSig=9, maType=SMA)
#tail(macd,n=5)
View(macd)
```
#RSI
```{r}
rsi = RSI(Cl(charting_data), n=14)
#tail(rsi,n=5)
View(rsi)
```
hicham 
---
title: "Stocks Data science project"
output: html_notebook
---

```{r}
setwd("~/ensa/FID2/DATA SCIENCE/project")
library(xml2)
library(rvest)
library(xml2)
library(XML)
library(rvest)
library(dplyr)
library(readr)
library(tidyr)
library(tidyquant)
library(TTR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xts)
#splitting data
library(rpart)
library(vip)
library(tidymodels)
setwd("~/ensa/FID2/DATA SCIENCE/project")
```


```{r}
## Import Tickers
sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
```


```{r}
sp500 <- read_html(sp500_url) %>% 
  html_node("table") %>% 
  html_table()

sp500<-sp500 %>% select('Symbol','Security','SEC filings','GICS Sector', 'Headquarters Location')
names(sp500) <- c("Ticker", "Name", "Sector", "Industry", "HQ_Location")

save(sp500, file = "sp500.RData")
```

```{r}
sp500
```

```{r}
# importation of price Data sp&500
#options("getSymbols.warning4.0"=FALSE)
# options("getSymbols.yahoo.warning"=FALSE)
# tickers<-c("^GSPC","AAPL")
# getSymbols(tickers, from = '2000-01-01',
#            to = "2020-12-30",warnings = FALSE,
#            auto.assign = TRUE)
# sp500_price<-GSPC
# stock_price<-AAPL
# str(sp500_price)
# sp500_price %>% 
#   select(Date,Adj.Close,Volume,Close,Open) %>% 
#   
# sp500_price <-sp500_price %>% mutate(
#   Movement = ifelse(Close > Open, "Up", "Down")
# )
# sp500_price
# View(sp500_price)
```

```{r}
#head(sp500_price)

```


```{r}
#stock_join<-inner_join(returns,sp500_price,by = c("Date" = "Date"), suffix = c("_stock", "_sp500_price"))
#View(stock_join)
```


```{r}

## Import Price Data
returns <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume", "Ticker")

for(symbol in sp500$Ticker){
  print(symbol)
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol, "?period1=1280102400&period2=1595721600&interval=1d&events=history")
  print(url)
  ret <- try(read_csv(url))
  
  if(mode(ret) != "character"){
    ret$Ticker <- symbol
    returns <- rbind(returns, ret)
  }
  
}
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume", "Ticker")
returns<- returns %>%  select("Date","Ticker", "Open", "High", "Low", "Close", "Adj_Close", "Volume")
returns <- returns %>% mutate(
  Open = as.numeric(Open),
  High = as.numeric(High),
  Low = as.numeric(Low),
  Close = as.numeric(Close),
  Adj_Close= as.numeric(Adj_Close),
  Volume=as.numeric(Volume),
)

returns <- returns %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
  
)
save(returns, file = "returns.RData")

returns_long <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
returns_long <-returns %>% left_join(sp500 %>% select("Ticker", "Name", "Sector", "Industry"), by = c("Ticker" = "Ticker"))
save(returns, file = "returns_long.RData")

```

```{r}
## Performance calcs

performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")

i <- 1
for(ticker in unique(returns_long$Ticker)){
  print(ticker)
  
  returns_long_by_ticker <- returns_long %>% filter(Ticker == ticker ) %>% arrange(desc(Date))
  
  thrity_day <- (returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[21])/returns_long_by_ticker$Adj_Close[21]
  ninety_day <- (returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[63])/returns_long_by_ticker$Adj_Close[63]
  one_year <- (returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[253])/returns_long_by_ticker$Adj_Close[253]
  three_year <- (1 + ((returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[759])/returns_long_by_ticker$Adj_Close[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[1265])/returns_long_by_ticker$Adj_Close[1265]))^(1/5)-1
  ten_year <- (1 + ((returns_long_by_ticker$Adj_Close[1] - returns_long_by_ticker$Adj_Close[2518])/returns_long_by_ticker$Adj_Close[2518]))^(1/10)-1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  performance_summary[i, 7] <- ten_year
  
  i <- i + 1
}

load("sp500.RData")

performance_summary <- performance_summary %>% left_join(sp500, by = c("Ticker" = "Ticker"))
save(performance_summary, file = "performance_summary.RData")



```

```{r}
library(ggplot2)
library(dplyr)
library(tidyr)
library(xts)
```

```{r}

ticker <- "AMZN"
```

```{r}
returns

charting_data <- returns_long %>% filter(Ticker == ticker,Date >= "2020-01-01")
ggplot(data=charting_data) +
  geom_line(aes(x = (Date), y=Adj_Close ))

```


```{r}
returns
#install.packages("plotly")
library(plotly)
```

```{r}
# # Candlestick patherb of the stock 
charting_data <- returns_long %>% filter(Ticker == ticker,Date >= 	"2020-01-01")
#charting_data<-data.frame(Date=index(charting_data),coredata(charting_data))
candle_chart<-charting_data %>% plot_ly(
  type="candlestick",
  x=~Date,
  open=~Open,
  close=~Adj_Close,
  high=~High,
  low=~Low
) 
candle_chart<-candle_chart %>% layout(title = paste0(charting_data$Name[1], " (", ticker, ")"))
candle_chart
```
```{r}
#Data frame preparation for visualisation 
(ticker_data<-returns_long %>% filter(Ticker == ticker)%>%
     rename(Adjusted = Adj_Close))
```



```{r}
#Data Frame to time series OHLC ready for visualisation
library(tidyverse)
ticker_dataSelection<-ticker_data %>% select("Date","Open", "High", "Low", "Close", "Adjusted","Volume")
ticker_data_series <- xts(ticker_dataSelection[, -1], order.by=as.Date(ticker_dataSelection$Date))
str(ticker_data_series)
```

```{r}
# Visualisation using candlestick patterns and technical analysis
chartSeries(ticker_data_series,
            subset='2020',
            theme=chartTheme('white'),
            name = ticker)
addSMA(n=24,on=1,col = "blue")
addSMA(n=48,on=1,col = "red")
addBBands(n=20,sd=2)
addMomentum(n=1)
```
```{r}
#don' run 
hlac <- as.xts(data.frame(x=Hi(ticker_data_series), y=Lo(ticker_data_series), z=Ad(ticker_data_series)))
```


```{r}
#The response features
ticker_data<-data.frame(ticker_data_series)
ticker_data_test<-ticker_data %>% mutate(difference = Adjusted- lag(Adjusted, default=NaN),
                       response= ifelse(difference>=0,"UP","DOWN"))
```

```{r}
library(rpart)
library(vip)
library("rpart.plot")
```

```{r}
rsi <- RSI(Ad(ticker_data_series), n=14)
SMA<-SMA(Ad(ticker_data_series),n=20)
bb <-BBands(Ad(ticker_data_series),s.d=2)
bb<- subset(bb,select=pctB)
hlac <- as.xts(data.frame(x=Hi(ticker_data_series), y=Lo(ticker_data_series), z=Ad(ticker_data_series)))
sto <- stoch(hlac, nFastK = 14) *100
wpr <-WPR(hlac, n=14) * (-100)
macd <- MACD(Ad(ticker_data_series), nFast=12, nSlow=26, nSig=9)  
roc <- ROC(Ad(ticker_data_series), n=14) *100
obv <- OBV(Ad(ticker_data_series), Vo(ticker_data_series))
ticker_data_test<-ticker_data %>% mutate(difference = Adjusted- lag(Adjusted, default=NaN),
                       response= ifelse(difference>=0,"UP","DOWN"))
EFI<-ticker_data_test$difference*ticker_data_test$Volume
EFI_13<-EMA(EFI,n=13)
  indic <- data.frame(rsi, bb, sto, wpr, macd, roc, obv,EFI_13,as.factor(ticker_data_test$response))
  colnames(indic) <- c("RSI","pctB" ,"StoFASTK","StoFASTD","StoSLOWD", 
                          "WilliamPR", "MACD","MACDSignal", "PriceRateOfChange", 
                          "OnBalanceVolume","EFI_13","response"
                          )
  indic<-indic[-1:-35,]
```

```{r}
#splitting data
library(rpart)
library(vip)
library(tidymodels)
library(randomForest)
set.seed(28676)

# The set. seed() function sets the starting number used to generate a sequence of random numbers ??? it ensures that you get the same result if you start with that same seed each time you run the same process. For example, if I use the sample() function immediately after setting a seed, I will always get the same sample.

indicators_split <- initial_split(indic, prop=.8, strata='response')
indicators_train <- training(indicators_split)
indicators_test <- testing(indicators_split)
```

The default Model
```{r}
DM_Random_Forest<-randomForest(response ~ ., data = indicators_train)
```

Out of bags sample= The "absent" examples od data set that's not been taken in bootstrapped sample are what's called "out of bag" samples
The classification error across all the out of bag samples is called the Out of Bag Error. The Out-of-bag error matrix is stored in the Random Forest model, and the rows of this matrix represent the number of trees in the forest. The i-th row reports the OOB error rate for all trees up to and including the the i-th tree. The first column shows the error across all the classes and then there are additional columns for per-class OOB error. 

```{r}
err <- DM_Random_Forest$err.rate
oob_err <- err[nrow(err), "OOB"]
print(oob_err)
plot(DM_Random_Forest,main="Default Random Forest Model")
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))

```
```{r}
#predire la classe.
predicted_DM <- DM_Random_Forest %>%                 
              predict(indicators_test,type = "class")                   
indicators_test <- indicators_test %>%  mutate(prediction_DM = predicted_DM)
```

```{r}
#ces mesures requierent la cible reelle Status et la classe predite par le modele
yardstick::conf_mat(indicators_test,response,prediction_DM)   #matrice de confusion 
yardstick::accuracy(indicators_test,response,prediction_DM)   #accuracy
yardstick::recall(indicators_test,response,prediction_DM)     #recall 
yardstick::precision(indicators_test,response,prediction_DM)  #precision
yardstick::f_meas(indicators_test,response,prediction_DM)     #f mesure
````
```{r}
#confusion matrix visualization :
library(caret)
library(e1071)
cm_DM <- confusionMatrix(data = indicators_test$prediction_DM, reference = indicators_test$response)
```

Evaluation Metrics visualisation
```{r}
draw_confusion_matrix <- function(cm) {

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)

  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'DOWN', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'UP', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'DOWN', cex=1.2, srt=90)
  text(140, 335, 'UP', cex=1.2, srt=90)

  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}
draw_confusion_matrix(cm_DM)
```

Find the optimal mtry value (tune the hyperparametre)

```{r}
# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(3, ncol(indicators_train) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(indicators_train) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {

    # Train a Random Forest model
    model <- randomForest::randomForest(formula = response ~ ., 
                          data = indicators_train,
                          mtry = hyper_grid$mtry[i],
                          nodesize = hyper_grid$nodesize[i],
                          sampsize = hyper_grid$sampsize[i])
                          
    # Store OOB error for the model                      
    oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
```
Upgrade Model
```{r}
UM_Random_Forest<-randomForest(response ~ ., data = indicators_train,
                                             mtry=hyper_grid[opt_i,]$mtry,
                                             nodesize=hyper_grid[opt_i,]$nodesize,
                                             sampsize=round(hyper_grid[opt_i,]$sampsize))
err1 <- UM_Random_Forest$err.rate
oob_err1 <- err1[nrow(err1), "OOB"]
print(oob_err1)
plot(UM_Random_Forest,main = "Upgrade Random Forest Model")
legend(x = "right", 
       legend = colnames(err1),
       fill = 1:ncol(err1))

```

```{r}
predicted_UM <- UM_Random_Forest %>%                 
              predict(indicators_test,type = "class")        #predire la classe.           
indicators_test <- indicators_test %>%  mutate(prediction_UM = predicted_UM)
```

```{r}
#ces mesures requierent la cible reelle Status et la classe predite par le modele
yardstick::conf_mat(indicators_test,response,prediction_UM)   #matrice de confusion 
yardstick::accuracy(indicators_test,response,predicted_UM)   #accuracy
yardstick::recall(indicators_test,response,predicted_UM)     #recall 
class(yardstick::precision(indicators_test,response,predicted_UM))  #precision
yardstick::f_meas(indicators_test,response,predicted_UM )     #f mesure
cm_UM <- confusionMatrix(data = indicators_test$prediction_UM, reference = indicators_test$response)
draw_confusion_matrix(cm_UM)
```
```{r}
vip(UM_Random_Forest)
```

```{r}
#probs
predicted_UM_prob <- UM_Random_Forest %>%                 
              predict(indicators_test,type = "prob")           
prob_DOWN <- predicted_UM_prob %>%  as_tibble() %>% pull(DOWN) 
indicators_test<-indicators_test %>% mutate(prob_Down=prob_DOWN)
Metrics::auc(actual = ifelse(indicators_test$response == "DOWN", 1, 0), 
    predicted = predicted_UM_prob[,"DOWN"])
```
```{r}
## Boosted tree (avec parnsnip et XGBOOST ) Creation du model 
 ?boost_tree
  ?xgboost::xgboost
  set.seed(1234)
  model_boost_tree <- boost_tree(
                        mode = "classification") %>%
                        set_engine("xgboost") %>%
                        fit(response ~ ., data = indicators_train )
```


```{r}
xgboost::xgb.importance(model = model_boost_tree$fit) %>%
    as_tibble() %>% arrange(desc(Gain))
```

```{r}
vip(model_boost_tree$fit)
```
## Evaluation de xgboost sur le testing set 
```{r}
#classe 
prediction_XGB <-model_boost_tree %>%
    predict(new_data = indicators_test,type="class")
indicators_test <- indicators_test %>% mutate(prediction_XGB = prediction_XGB$.pred_class)

yardstick::conf_mat(indicators_test,response,prediction_XGB)   #matrice de confusion 
yardstick::accuracy(indicators_test,response,prediction_XGB)   #accuracy
yardstick::recall(indicators_test,response,prediction_XGB)     #recall 
yardstick::precision(indicators_test,response,prediction_XGB)  #precision
yardstick::f_meas(indicators_test,response,prediction_XGB )     #f mesure
cm_XGB <- confusionMatrix(data = indicators_test$prediction_XGB, reference = indicators_test$response)
draw_confusion_matrix(cm_XGB)

#probs
  model_boost_tree_pred_probs <- model_boost_tree %>%
    predict(new_data = indicators_test,type = "prob") %>% 
    bind_cols( indicators_test %>% select(response) ) 
  glimpse(model_boost_tree_pred_probs)
  yardstick::roc_auc(model_boost_tree_pred_probs,truth = response, .pred_DOWN)
  
```

```{r}
library(ROCR)
library(pROC)
# List of predictions
preds_list <- list(predicted_UM_prob)
indicators_test$res <- ifelse(indicators_test$response == "DOWN", 1, 0)
# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(indicators_test$res), m)
roc(indicators_test$res,predicted_UM_prob,plot=TRUE)
# Plot the ROC curves
pred <- prediction(predicted_UM_prob, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM"),
       fill = 1:m)
```

function --------------------------------------------------------------------------------------------------------------------------

```{r}
get_indicators <- function(stock, period){
  #creating response variable. Predicting next days price, by using lag function in price_change
  #price_change <- Ad(lag(stock,-period)) - Ad(stock)
  #response <- ifelse(price_change > 0, "UP", "DOWN")
  #Calculating RSI
  rsi <- RSI(Ad(stock), n=14)
  #calculing SMA
  SMA<-SMA(Ad(stock),n=20)
  #High, Low, and adjusted close xts object 
  hlac <- as.xts(data.frame(x=Hi(stock), y=Lo(stock), z=Ad(stock)))
  
  #Stochastic Oscillator
  sto <- stoch(hlac, nFastK = 14) *100
  #Williams %R
  wpr <-WPR(hlac, n=14) * (-100)
  
  #MACD
  macd <- MACD(Ad(stock), nFast=12, nSlow=26, nSig=9)  
  #Price Rate of Change
  roc <- ROC(Ad(stock), n=14) *100
  #On Balance Volume
  obv <- OBV(Ad(stock), Vo(stock))
  
  #create data set with all indicators and labeled columns 
  indicators <- data.frame(rsi, sto, wpr, macd, roc, obv)
  colnames(indicators) <- c("RSI", "StoFASTK","StoFASTD","StoSLOWD", 
                          "WilliamPR", "MACD","MACDSignal", "PriceRateOfChange", 
                          "OnBalanceVolume"
                          )
  #removing na values from calculations and keeping sizes of columns same
  indicators <- indicators[-1:-35,]
  
  #removing na values due to lag
  indicators <- head(indicators,-period)
  return(indicators)
}
```
--------------------------------------------------------------------------------------------------------------------------------------------

