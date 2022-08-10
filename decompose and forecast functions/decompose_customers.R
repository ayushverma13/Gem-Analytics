## Malik top Customers value decompose
decompose_customers<-function(x, index, measure){


### DATA SETUP 
# msd_pp
# x<-msd_pp
# x<-master_ltr
# index=3
  
if(measure == 1)
{
x$Date = as.Date(x$Date,"%d-%m-%Y")

party_value<-x%>%
  dplyr::select(Party,Amt.)%>%
  group_by(Party)%>%
  summarise(total_sales=sum(Amt.))%>%
  arrange(desc(total_sales))%>%
  mutate(cumsum=cumsum(total_sales),freq=round(total_sales/sum(total_sales),3),cum_freq=cumsum(freq))


  x<-x%>%dplyr::select(Date,Amt.,Party)%>%rowwise()%>%filter( Party==party_value[index,1])%>%
    dplyr::select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))
  
  dh<-head(x$Date, n=1)
  d<-tail(x$Date, n=1)
  dates_se<-seq.Date(from = dh, to = d, by = "day")%>%as.data.frame()
  dates_se<-dates_se%>%mutate(Date = seq.Date(from = dh, to = d, by = "day"))%>%dplyr::select(Date)
  x<-merge(x,dates_se, by="Date", all = T)
  x[is.na(x)] <- 0
  
  dh2<-head(x$Date, n=1)
  d2<-tail(x$Date, n=1)
  
  x<-xts(x$amt,order.by = x$Date)%>%apply.monthly(FUN = sum)
  index(x)<-as.yearmon(index(x))
  x<-as.data.frame(x)
  date_seq<-seq.Date(from = dh2 , by ="month", length.out = length(x$V1)+12)%>%tail(n=12)
  x$Date=seq.Date(from = dh2 , by ="month", length.out = length(x$V1))
  x$amt=x$V1
  
  
  # # # # # REOMVING OUTLIERS
  amt_clean<- ts(x[,c("amt")])
  
  x$amt_clean = tsclean(amt_clean)
  x<-as.data.frame(x)
  
  
  # # # SMOOTHENING THE DATA FURTHER (REMOVING NOISE COMPONENT BY MOVING AVERAGE)
  
  #####     IF STATEMENT NEEDED ON LENGTH FOR OUTLIERS
  
  
  # x$amt_ma =ma(x$amt, order=3) #WEEKLY MOVING AVERAGE
  x$amt_ma =x$amt
  
  
  #     APPLYING HMISC TO REMOVE N.A. VALUES
  
  x$amt_ma_impute<-with(x,impute(amt_ma,mean))
  # x$amt_ma_30_impute<-with(x,impute(amt_ma_30,mean))
  x<-as.data.frame(x)
  
  b<-ggplot(x, aes(Date, amt_clean ))  +geom_line(data = x,aes(Date,amt_clean,colour = "Original"))+
    geom_line(data = x,aes(Date,amt_ma_impute,colour = "Weekly Moving Average"))+
    xlab("Date") +  scale_x_date(name = "month_year",date_labels = "%b %Y", date_breaks = "1 month" )
  
  
  # #  DECOMPOSING THE SERIES
  
  x$amt_ma_impute<-as.numeric(x$amt_ma_impute)
  find.freq <- function(x)
  {
    n <- length(x)
    spec <- spec.ar(c(x),plot=FALSE)
    if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
    {
      period <- round(1/spec$freq[which.max(spec$spec)])
      if(period==Inf) # Find next local maximum
      {
        j <- which(diff(spec$spec)>0)
        if(length(j)>0)
        {
          nextmax <- j[1] + which.max(spec$spec[j[1]:500])
          period <- round(1/spec$freq[nextmax])
        }
        else
          period <- 1
      }
    }
    else
      period <- 1
    return(period)
  }
  
  f<-find.freq(x$amt_ma_impute) 
  if(is.na(f)){
    f = findfrequency(x$amt_ma_impute)
  }
  
  # amt_ma_ts<-ts(na.omit(x$amt_ma_impute),frequency =f)
  dh3<-dh2%>%str_split('-')%>%unlist()
  amt_ma_ts<-ts(na.omit(x$amt_ma_impute), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
  x$amt_ma_ts=amt_ma_ts
  
  if(length(amt_ma_ts)>60 ){
    if(!isError(stl(amt_ma_ts, s.window="periodic"))){
      decomp = mstl(amt_ma_ts, s.window="periodic")
      # autoplot(decomp)
      deseasonal_qty <- seasadj(decomp)%>%as.data.frame()
      decomp<-as.data.frame(decomp)
      
      decomp$Date= x$Date
      deseasonal_qty$Date = x$Date
      
      t_data<-decomp%>%dplyr::select(Trend)%>%xts(order.by = x$Date)
      s_data<-decomp%>%dplyr::select(contains("season"))
      sadj_data<-deseasonal_qty}else{
        # t_data<-loess(data = x,amt_ma_ts~Date)
        tdata<-supsmu(as.numeric(x$Date),amt_ma_ts)
        t_data<-tdata$y%>%xts(order.by = x$Date)
        s_data<-NULL}
    
    a <- list()
    a$temp<-dygraph(t_data, main = paste("Decomposed trend for total sales by value for ",party_value[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    
    if(length(s_data[1,]) == "0"){
      a$temp1 <- "No seasonality"
      return(a)
    }else{
      s_data<-s_data%>%xts(order.by = x$Date)
      a$temp1<-dygraph(s_data, main = paste("Decomposed seasonality for total sales by value for ",party_value[index,1]))%>%
        dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
        dyCrosshair(direction = "vertical")
    }
    return(a)}
  
  
  
  else{
    # t_data<-loess(data = x,amt_ma_ts~Date)
    # tdata<-supsmu(as.numeric(x$Date),amt_ma_ts)
    # t_data<-tdata$y%>%xts(order.by = x$Date)
    # s_data<-NULL
    dh3<-dh2%>%str_split('-')%>%unlist()
    amt_ma_ts<-ts(na.omit(x$amt_ma_impute), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    
    decompose_df <- tslm(amt_ma_ts ~ trend + fourier(amt_ma_ts, 2))
    
    trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(amt_ma_ts)
    components <- cbind(
      data = amt_ma_ts,
      trend = trend,
      season = amt_ma_ts - trend - residuals(decompose_df),
      remainder = residuals(decompose_df)
    )
    autoplot(components, facet=TRUE)
    components<-as.data.frame(components)
    components$Date<-x$Date
    t_data<-ts(na.omit(components$trend), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    s_data<-ts(na.omit(components$season), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    r_data<-ts(na.omit(components$remainder), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    
    
    a <- list()
    a$temp<-dygraph(t_data, main = paste("Decomposed trend for total sales by value, for ",party_value[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    a$temp1<-dygraph(s_data, main = paste("Decomposed seasonality for total sales by value, with frequency ",findfrequency(s_data)," for ",party_value[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    a$temp2<-dygraph(t_data+r_data, main = paste("Deseasonalized series for total sales by value (Trend+Remainder), for ",party_value[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    return(a)
  }
}
  
  
  
  
  
  
  
  
  

# x<-master_ltr
  if(measure == 2)
{
  x$Date = as.Date(x$Date,"%d-%m-%Y")
  
  party_volume<-x%>%
    dplyr::select(Party,`Order Qty`)%>%
    group_by(Party)%>%
    summarise(total_qty=sum(`Order Qty`))%>%
    arrange(desc(total_qty))%>%
    mutate(cumsum=cumsum(total_qty),freq=round(total_qty/sum(total_qty),3),cum_freq=cumsum(freq))
  
  x<-x%>%dplyr::select(Date,`Order Qty`,Party)%>%rowwise()%>%filter( Party==party_volume[index,1])%>%
    dplyr::select(Date,`Order Qty`)%>%group_by(Date)%>%summarise(qty=sum(`Order Qty`))
  
  dh<-head(x$Date, n=1)
  d<-tail(x$Date, n=1)
  dates_se<-seq.Date(from = dh, to = d, by = "day")%>%as.data.frame()
  dates_se<-dates_se%>%mutate(Date = seq.Date(from = dh, to = d, by = "day"))%>%dplyr::select(Date)
  x<-merge(x,dates_se, by="Date", all = T)
  x[is.na(x)] <- 0
  
  dh2<-head(x$Date, n=1)
  d2<-tail(x$Date, n=1)
  
  x<-xts(x$qty,order.by = x$Date)%>%apply.monthly(FUN = sum)
  index(x)<-as.yearmon(index(x))
  x<-as.data.frame(x)
  date_seq<-seq.Date(from = dh2 , by ="month", length.out = length(x$V1)+12)%>%tail(n=12)
  x$Date=seq.Date(from = dh2 , by ="month", length.out = length(x$V1))
  x$qty=x$V1
  
  
  # # # # # REOMVING OUTLIERS
  qty_clean<- ts(x[,c("qty")])
  
  x$qty_clean = tsclean(qty_clean)
  x<-as.data.frame(x)
  
  
  # # # SMOOTHENING THE DATA FURTHER (REMOVING NOISE COMPONENT BY MOVING AVERAGE)
  
  #####     IF STATEMENT NEEDED ON LENGTH FOR OUTLIERS
  
  
  x$qty_ma =ma(x$qty, order=3) #WEEKLY MOVING AVERAGE
  
  
  #     APPLYING HMISC TO REMOVE N.A. VALUES
  
  x$qty_ma_impute<-with(x,impute(qty_ma,mean))
  # x$qty_ma_30_impute<-with(x,impute(qty_ma_30,mean))
  x<-as.data.frame(x)
  
  b<-ggplot(x, aes(Date, qty_clean ))  +geom_line(data = x,aes(Date,qty_clean,colour = "Original"))+
    geom_line(data = x,aes(Date,qty_ma_impute,colour = "Weekly Moving Average"))+
    xlab("Date") +  scale_x_date(name = "month_year",date_labels = "%b %Y", date_breaks = "1 month" )
  
  
  # #  DECOMPOSING THE SERIES
  
  x$qty_ma_impute<-as.numeric(x$qty_ma_impute)
  find.freq <- function(x)
  {
    n <- length(x)
    spec <- spec.ar(c(x),plot=FALSE)
    if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
    {
      period <- round(1/spec$freq[which.max(spec$spec)])
      if(period==Inf) # Find next local maximum
      {
        j <- which(diff(spec$spec)>0)
        if(length(j)>0)
        {
          nextmax <- j[1] + which.max(spec$spec[j[1]:500])
          period <- round(1/spec$freq[nextmax])
        }
        else
          period <- 1
      }
    }
    else
      period <- 1
    return(period)
  }
  
  f<-find.freq(x$qty_ma_impute)
  if(is.na(f)){
    f = findfrequency(x$qty_ma_impute)
  }
  
  
  # qty_ma_ts<-ts(na.omit(x$amt_ma_impute),frequency =f)
  dh3<-dh2%>%str_split('-')%>%unlist()
  qty_ma_ts<-ts(na.omit(x$qty_ma_impute), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
  x$qty_ma_ts=qty_ma_ts
  
  if(length(qty_ma_ts)>60 ){
    if(!isError(stl(qty_ma_ts, s.window="periodic"))){
      decomp = mstl(qty_ma_ts, s.window="periodic")
      # autoplot(decomp)
      deseasonal_qty <- seasadj(decomp)%>%as.data.frame()
      decomp<-as.data.frame(decomp)
      
      decomp$Date= x$Date
      deseasonal_qty$Date = x$Date
      
      t_data<-decomp%>%dplyr::select(Trend)%>%xts(order.by = x$Date)
      s_data<-decomp%>%dplyr::select(contains("season"))
      sadj_data<-deseasonal_qty}else{
        # t_data<-loess(data = x,qty_ma_ts~Date)
        tdata<-supsmu(as.numeric(x$Date),qty_ma_ts)
        t_data<-tdata$y%>%xts(order.by = x$Date)
        s_data<-NULL}
    
    a <- list()
    a$temp<-dygraph(t_data, main = paste("Decomposed trend for total sales by value for",party_volume[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    
    if(length(s_data[1,]) == "0"){
      a$temp1 <- "No seasonality"
      return(a)
    }else{
      s_data<-s_data%>%xts(order.by = x$Date)
      a$temp1<-dygraph(s_data, main = paste("Decomposed seasonality for total sales by value for",party_volume[index,1]))%>%
        dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
        dyCrosshair(direction = "vertical")
    }
    return(a)}
  
  
  
  else{
    # t_data<-loess(data = x,qty_ma_ts~Date)
    # tdata<-supsmu(as.numeric(x$Date),qty_ma_ts)
    # t_data<-tdata$y%>%xts(order.by = x$Date)
    # s_data<-NULL
    dh3<-dh2%>%str_split('-')%>%unlist()
    qty_ma_ts<-ts(na.omit(x$qty_ma_impute), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    
    decompose_df <- tslm(qty_ma_ts ~ trend + fourier(qty_ma_ts, 2))
    
    trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(qty_ma_ts)
    components <- cbind(
      data = qty_ma_ts,
      trend = trend,
      season = qty_ma_ts - trend - residuals(decompose_df),
      remainder = residuals(decompose_df)
    )
    autoplot(components, facet=TRUE)
    components<-as.data.frame(components)
    components$Date<-x$Date
    t_data<-ts(na.omit(components$trend), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    s_data<-ts(na.omit(components$season), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    r_data<-ts(na.omit(components$remainder), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    
    
    a <- list()
    a$temp<-dygraph(t_data, main = paste("Decomposed trend for total sales by volume, for ",party_volume[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    a$temp1<-dygraph(s_data, main = paste("Decomposed seasonality for total sales by volume, with frequency ",findfrequency(s_data)," for ",party_volume[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    a$temp2<-dygraph(t_data+r_data, main = paste("Deseasonalized series for total sales by volume (Trend+Remainder), for ",party_volume[index,1]))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    return(a)
  }
  }
}