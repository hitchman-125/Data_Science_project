

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
-----------------------------------------------------------------------------------------------------------------------------------------
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

ticker <- "AAPL"
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
hlac <- as.xts(data.frame(x=Hi(ticker_data_series), y=Lo(ticker_data_series), z=Ad(ticker_data_series)))
sto <- stoch(hlac, nFastK = 14) *100
wpr <-WPR(hlac, n=14) * (-100)
macd <- MACD(Ad(ticker_data_series), nFast=12, nSlow=26, nSig=9)  
roc <- ROC(Ad(ticker_data_series), n=14) *100
obv <- OBV(Ad(ticker_data_series), Vo(ticker_data_series))
ticker_data_test<-ticker_data %>% mutate(difference = Adjusted- lag(Adjusted, default=NaN),
                       response= ifelse(difference>=0,"UP","DOWN"))
  indic <- data.frame(rsi, SMA, sto, wpr, macd, roc, obv,as.factor(ticker_data_test$response))
  colnames(indic) <- c("RSI","SMA", "StoFASTK","StoFASTD","StoSLOWD", 
                          "WilliamPR", "MACD","MACDSignal", "PriceRateOfChange", 
                          "OnBalanceVolume","response"
                          )
  indic<-indic[-1:-35,]
```

```{r}
#splitting data
library(rpart)
library(vip)
set.seed(28676)

# The set. seed() function sets the starting number used to generate a sequence of random numbers ??? it ensures that you get the same result if you start with that same seed each time you run the same process. For example, if I use the sample() function immediately after setting a seed, I will always get the same sample.

indicators_split <- initial_split(indic, prop=.8, strata='response')
indicators_train <- training(indicators_split)
indicators_test <- testing(indicators_split)
```

```{r}
#model Random Forest
model_Random_Forest <- rand_forest(mode='classification') %>% 
                      set_engine('ranger') %>%
                      fit(response ~ ., data = indicators_train )
```

```{r}
model_Random_Forest
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
#classe 
model_Random_Forest %>%
    predict(new_data = indicators_test %>% filter(complete.cases(.)) ) %>%
    bind_cols(indicators_test %>% filter(complete.cases(.))  %>% select(response)) %>%
    yardstick::accuracy(truth = response, estimate = .pred_class)

#probs
  model_Random_forest_pred_probs <- model_boost_tree %>%
    predict(new_data = indicators_test,type = "prob") %>% 
    bind_cols( indicators_test %>% select(response) ) 
  glimpse(model_Random_forest_pred_probs)
  yardstick::roc_auc(model_Random_forest_pred_probs,truth = response , .pred_DOWN)
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
model_boost_tree %>%
    predict(new_data = indicators_test) %>%
    bind_cols(indicators_test %>% select(response)) %>%
    yardstick::accuracy(truth = response, estimate = .pred_class)

#probs
  model_boost_tree_pred_probs <- model_boost_tree %>%
    predict(new_data = indicators_test,type = "prob") %>% 
    bind_cols( indicators_test %>% select(response) ) 
  glimpse(model_boost_tree_pred_probs)
  yardstick::roc_auc(model_boost_tree_pred_probs,truth = response, .pred_DOWN)
  
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
Statistique:

```{r}
charting_data <- returns_long %>% filter(Ticker == ticker,Date >= 	"2020-01-01")
Candelstick<-ggplot(data=charting_data) +
  geom_boxplot(aes(x = (Date), y=Adj_Close  ,fill=Movement))+
  scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
  xlab("Date") + 
  ylab("Stock Price") +
  labs(
    title = paste0(charting_data$Name[1], " (", ticker, ")"),
    subtitle = charting_data$Sector[1],
    caption = "Source: Yahoo! Finance"
  ) +
  scale_y_continuous(labels = scales::dollar)
Candelstick

```

```{r}
charting_data <- returns_long %>% filter(Ticker == ticker,Date >= 	"2013-06-24")
ggplot(charting_data,aes(Date,Adj_Close,fill =Movement)) +
  geom_boxplot()
```

