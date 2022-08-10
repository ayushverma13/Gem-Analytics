forecast_customers<-function ( x, measure, index, horizontype){
  
  # measure arguement is 1 for by value or 2 for by volume
  # horizontype is for choosing monthly(1) or quarterly(2)
  # x<-master
  # # x<-htpldata
  # index<-3
  # horizontype=1
  # measure<-1
  
  if(measure== 1){
    
    dh<-head(x$Date, n=1)
    d<-tail(x$Date, n=1)
    party_value<-x%>%
      select(Party,Amt.)%>%
      group_by(Party)%>%
      summarise(total_sales=sum(Amt.))%>%
      arrange(desc(total_sales))%>%
      mutate(cumsum=cumsum(total_sales),freq=round(total_sales/sum(total_sales),3),cum_freq=cumsum(freq))
    
     x<-x%>%select(Date,Amt.,Party)%>%rowwise()%>%filter( Party==party_value[index,1])%>%
      select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))

    dates_se<-seq.Date(from = dh, to = d, by = "day")%>%as.data.frame()
    dates_se<-dates_se%>%mutate(Date = seq.Date(from = dh, to = d, by = "day"))%>%select(Date)
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
    
    
    x$amt_ma =ma(x$amt, order=3) #WEEKLY MOVING AVERAGE
    
    
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
    f<-find.freq(x$amt_ma_impute)
    if(is.na(f)){
      f = findfrequency(x$amt_ma_impute)
    }
    
    # f<-findfrequency(x$amt_ma_impute)
    
    amt_ma_ts<-ts(na.omit(x$amt_ma_impute), frequency =f)
    x$amt_ma_ts=amt_ma_ts
    if(length(amt_ma_ts)>2*f){
      decomp = mstl(amt_ma_ts)
      # autoplot(decomp)
      deseasonal_qty <- seasadj(decomp)%>%as.data.frame()
      decomp<-as.data.frame(decomp)
      
      decomp$Date= x$Date
      deseasonal_qty$Date = x$Date
      
      s_data<-decomp%>%select(contains("season"))
      sadj_data<-deseasonal_qty}else{
        s_data<-NULL
      }
    
    
    
    ########      FORECASTING
    df_ag<-as.data.frame(x)
    df_ag$amt_ma_impute<-as.numeric(df_ag$amt_ma_impute)
    
    # timeseries_input<-xts(df_ag$amt_ma_impute,order.by = df_ag$Date)
    # View(timeseries_input)
    # timeseries_input
    # dygraph(timeseries_input)%>%dyRangeSelector()
    
    #  FITTING THE MODELS
    
    h1= 12
    
    if(length(s_data) !=0){
      #   Naive    
      naive_out<- stlf(amt_ma_ts, method = "naive" , h = h1)
      # accuracy_naive<-accuracy(naive_out)
      
      f_naive <- function(y, h){stlf(y, method = "naive", h = h)}
      f_naive(amt_ma_ts,1)
      e <- tsCV(y = amt_ma_ts,f_naive, h=1)
      r_naive<-sqrt(mean(e^2, na.rm=TRUE))
      
      #   Meanf     
      meanf_out<-meanf(amt_ma_ts, h = h1)
      # accuracy_meanf<-accuracy(meanf_out)
      
      f_meanf <- function(y, h){meanf(y, h = h)}
      f_meanf(amt_ma_ts,1)
      e <- tsCV(y = amt_ma_ts,f_meanf, h=1)
      r_meanf<-sqrt(mean(e^2, na.rm=TRUE))
      
      #   Drift method    
      drift_out<-stlf(amt_ma_ts, method = "rwdrift" , h = h1)
      # accuracy_drift<-accuracy(drift_out)
      
      f_drift <- function(y, h){stlf(y, method = "rwdrift", h = h)}
      f_drift(amt_ma_ts,1)
      e <- tsCV(y = amt_ma_ts,f_drift, h=1)
      r_drift<-sqrt(mean(e^2, na.rm=TRUE))
      
      #   Holts methods
      
      #   FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(sadj_data$x,approximation = FALSE , stepwise = FALSE, seasonal = FALSE )  
      ord<-arimaorder(output_arima1)
      v1<-as.vector(ord)
      output_arima2<- stlm(amt_ma_ts, modelfunction=Arima, order=v1,)
      arima_out<-forecast(output_arima2, h = h1)
      # accuracy_arima<-accuracy(arima_out)
      
      f_ar <- function(y, h){forecast(stlm(y, modelfunction=Arima, order=v1), h=h)}
      f_ar(amt_ma_ts,1)
      e <- tsCV(y = amt_ma_ts,f_ar, h=1)
      r_arima<-sqrt(mean(e^2, na.rm=TRUE))
      
      #   FITTING THE ETS MODEL
      ets_out<-stlf(amt_ma_ts, h = h1)
      # accuracy_ets<-accuracy(ets_out)
      
      f_ets <- function(y, h){stlf(y, h = h)}
      f_ets(amt_ma_ts,1)
      e <- tsCV(y = amt_ma_ts,f_ets, h=1)
      r_ets<-sqrt(mean(e^2, na.rm=TRUE))
      
    }else{
      
      #   Naive    
      naive_out<- naive(amt_ma_ts , h = h1) 
      # accuracy_naive<-accuracy(naive_out)
      
      e<-tsCV(amt_ma_ts, naive, h=1)
      r_naive<-sqrt(mean(e^2, na.rm = TRUE))
      
      #   Meanf     
      meanf_out<-meanf(amt_ma_ts, h = h1)
      # accuracy_meanf<-accuracy(meanf_out)
      
      e<- tsCV(amt_ma_ts, meanf, h=1)
      r_meanf<-sqrt(mean(e^2, na.rm = TRUE))
      
      #   Drift method    
      drift_out<-rwf(amt_ma_ts, drift = TRUE, h = h1)
      # accuracy_drift<-accuracy(drift_out)
      
      m4<- tsCV(amt_ma_ts, rwf, drift=TRUE, h=1)
      r_drift<-sqrt(mean(e^2, na.rm = TRUE))
      #   Holts methods
      
      #   FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(amt_ma_ts,seasonal = TRUE,approximation = FALSE, stepwise = FALSE)
      arima_out<-forecast(output_arima1, h = h1)
      # accuracy_arima<-accuracy(arima_out)
      
      f_ar <- function(x, h){forecast(Arima(x, order=as.vector(arimaorder(output_arima1))), h=h)}
      e <- tsCV(amt_ma_ts, f_ar, h=1)
      r_arima<-sqrt(mean(e^2, na.rm = TRUE))
      
      #   FITTING THE ETS MODEL
      output_ets1<-ets(amt_ma_ts)
      ets_out<-forecast(output_ets1, h = h1)
      # accuracy_ets<-accuracy(ets_out)
      
      fets <- function(x, h){forecast(ets(x), h=h)}
      e <- tsCV(amt_ma_ts, fets, h=1)
      r_ets<-sqrt(mean(e^2, na.rm = TRUE))
    }
    
    
    
    #      COMPARING THE MODELS
    v_min<-c("r_arima"= r_arima,"r_drift"=r_drift,"r_meanf"=r_meanf,"r_naive"=r_naive,"r_ets"=r_ets)
    model<-names(v_min)[which.min(v_min)]%>%str_split('_')%>%unlist()
    print(paste("best fit is ",model[2] ,"with an RMSE of ",round(min(r_arima,r_drift,r_naive,r_ets,r_meanf),3)))
    
    if(model[2]=="ets"){
      model_final = ets_out
    }else if(model[2] == "arima"){
      model_final = arima_out
    }else if(model[2] == "naive"){
      model_final = naive_out
    }else if(model[2] == "drift"){
      model_final = drift_out
    }else if(model[2] == "meanf"){
      model_final = meanf_out
    }
    
    
    ## pull AIC, BIC AICc values using $
    
    temp<-list()
    
    d3<-head(date_seq,n = 1)
    
    df_fc<-as.data.frame(model_final)
    df_fc$Date<-date_seq
    
    
    if(horizontype==1){
      
      
      df_ag<-df_ag%>%select(Date,amt_ma_impute)
      df_ag$amt_ma_impute<-as.numeric(df_ag$amt_ma_impute)
      
      df_bind<-merge(df_ag,df_fc, by="Date", all = T)
      df_final<-df_bind%>%select(-Date)
      # setnames(df_final, old = c("amt_ma_impute", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95"), new=c("actual", "forecast", "low_80", "high_80", "low_95", "high_95"))
      df_final<-xts(df_final, order.by = df_bind$Date)
      
      a<-xts(df_fc%>%select(-Date),order.by = df_fc$Date)
      c1<-a$`Point Forecast`%>%apply.monthly(FUN = sum)
      c2<-a$`Lo 80`%>%apply.monthly(FUN = sum)
      c3<-a$`Hi 80`%>%apply.monthly(FUN = sum)
      c4<-a$`Lo 95`%>%apply.monthly(FUN = sum)
      c5<-a$`Hi 95`%>%apply.monthly(FUN = sum)
      a<-cbind(c1,c2,c3,c4,c5)
      index(a)<-as.yearmon(index(a))
      
      temp$temp1<-as.data.frame(a)
      
      temp$temp2<-dygraph(df_final, main = paste("total sales by value for ",party_value[index,1]))%>%
        dySeries("amt_ma_impute", label = "Actual")%>%
        dySeries(c("Lo 95", "Point Forecast","Hi 95"), label = "Predicted")%>%
        dyRangeSelector()%>% dyCrosshair(direction = "vertical")
      
    }else if(horizontype == 2)
    {
      
      
      df_ag<-df_ag%>%select(Date,amt_ma_impute)
      df_ag$amt_ma_impute<-as.numeric(df_ag$amt_ma_impute)
      df_ag_q<-df_ag%>%select(-Date)%>%xts(order.by = df_ag$Date)%>%apply.quarterly(FUN = sum)
      index(df_ag_q)<-as.yearqtr(index(df_ag_q))
      df_ag_q<-as.data.frame(df_ag_q)
      df_ag_q$Date<-seq.Date(from = dh2, length.out = length(df_ag_q$amt_ma_impute), by = "quarter" )
      
      a<-xts(df_fc%>%select(-Date),order.by = df_fc$Date)
      c1<-a$`Point Forecast`%>%apply.quarterly(FUN = sum)
      c2<-a$`Lo 80`%>%apply.quarterly(FUN = sum)
      c3<-a$`Hi 80`%>%apply.quarterly(FUN = sum)
      c4<-a$`Lo 95`%>%apply.quarterly(FUN = sum)
      c5<-a$`Hi 95`%>%apply.quarterly(FUN = sum)
      a<-cbind(c1,c2,c3,c4,c5)
      index(a)<-as.yearqtr(index(a))
      a<-as.data.frame(a)
      a$Date<-seq.Date(from = d3, length.out = length(a$Point.Forecast), by = "quarter")
      
      df_bind<-merge(df_ag_q,a, by="Date", all = T)
      df_final<-df_bind%>%select(-Date)
      df_final<-xts(df_final, order.by = df_bind$Date)
      
      temp$temp1<-a
      
      temp$temp2<-dygraph(df_final, main = paste("total sales by value for ",party_value[index,1]))%>%
        dySeries("amt_ma_impute", label = "Actual")%>%
        dySeries(c("Lo.95", "Point.Forecast","Hi.95"), label = "Predicted")%>%
        dyRangeSelector()%>% dyCrosshair(direction = "vertical")
    }
    return(temp)
  }

    
 # x<-master_ltr
 # index<-1
   else if(measure==2){
 
     dh<-head(x$Date, n=1)
     d<-tail(x$Date, n=1)
  party_volume<-x%>%select(Party,`Order Qty`)%>%
    group_by(Party)%>%
    summarise(total_qty=sum(`Order Qty`))%>%
    arrange(desc(total_qty))%>%
    mutate(cumsum=cumsum(total_qty),freq=round(total_qty/sum(total_qty),3),cum_freq=cumsum(freq))
  
  x<-x%>%select(Date,`Order Qty`,Party)%>%rowwise()%>%filter( Party==party_volume[index,1])%>%
    select(Date,`Order Qty`)%>%group_by(Date)%>%summarise(qty=sum(`Order Qty`))
  

  dates_se<-seq.Date(from = dh, to = d, by = "day")%>%as.data.frame()
  dates_se<-dates_se%>%mutate(Date = seq.Date(from = dh, to = d, by = "day"))%>%select(Date)
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
  
  qty_ma_ts<-ts(na.omit(x$qty_ma_impute),frequency =f)
  x$qty_ma_ts=qty_ma_ts
  
  if(length(qty_ma_ts)>2*f){
    decomp = mstl(qty_ma_ts, s.window="periodic")
    # autoplot(decomp)
    deseasonal_qty <- seasadj(decomp)%>%as.data.frame()
    decomp<-as.data.frame(decomp)
    
    decomp$Date= x$Date
    deseasonal_qty$Date = x$Date
    
    s_data<-decomp%>%select(contains("season"))
    sadj_data<-deseasonal_qty}else{
      s_data<-NULL
    }
  
  
  
  ########      FORECASTING
  df_ag<-as.data.frame(x)
  df_ag$qty_ma_impute<-as.numeric(df_ag$qty_ma_impute)
  
  # timeseries_input<-xts(df_ag$qty_ma_impute,order.by = df_ag$Date)
  # View(timeseries_input)
  # timeseries_input
  # dygraph(timeseries_input)%>%dyRangeSelector()
  
  #  FITTING THE MODELS
  h1= 12
  
  if(length(s_data) !=0){
    #   Naive    
    naive_out<- stlf(qty_ma_ts, method = "naive" , h = h1)
    # accuracy_naive<-accuracy(naive_out)
    
    f_naive <- function(y, h){stlf(y, method = "naive", h = h)}
    f_naive(qty_ma_ts,1)
    e <- tsCV(y = qty_ma_ts,f_naive, h=1)
    r_naive<-sqrt(mean(e^2, na.rm=TRUE))
    
    #   Meanf     
    meanf_out<-meanf(qty_ma_ts, h = h1)
    # accuracy_meanf<-accuracy(meanf_out)
    
    f_meanf <- function(y, h){meanf(y, h = h)}
    f_meanf(qty_ma_ts,1)
    e <- tsCV(y = qty_ma_ts,f_meanf, h=1)
    r_meanf<-sqrt(mean(e^2, na.rm=TRUE))
    
    #   Drift method    
    drift_out<-stlf(qty_ma_ts, method = "rwdrift" , h = h1)
    # accuracy_drift<-accuracy(drift_out)
    
    f_drift <- function(y, h){stlf(y, method = "rwdrift", h = h)}
    f_drift(qty_ma_ts,1)
    e <- tsCV(y = qty_ma_ts,f_drift, h=1)
    r_drift<-sqrt(mean(e^2, na.rm=TRUE))
    
    #   Holts methods
    
    #   FITTING THE ARIMA MODEL
    output_arima1<-auto.arima(sadj_data$x,approximation = FALSE , stepwise = FALSE, seasonal = FALSE )  
    ord<-arimaorder(output_arima1)
    v1<-as.vector(ord)
    output_arima2<- stlm(qty_ma_ts, modelfunction=Arima, order=v1,)
    arima_out<-forecast(output_arima2, h = h1)
    # accuracy_arima<-accuracy(arima_out)
    
    f_ar <- function(y, h){forecast(stlm(y, modelfunction=Arima, order=v1), h=h)}
    f_ar(qty_ma_ts,1)
    e <- tsCV(y = qty_ma_ts,f_ar, h=1)
    r_arima<-sqrt(mean(e^2, na.rm=TRUE))
    
    #   FITTING THE ETS MODEL
    ets_out<-stlf(qty_ma_ts, h = h1)
    # accuracy_ets<-accuracy(ets_out)
    
    f_ets <- function(y, h){stlf(y, h = h)}
    f_ets(qty_ma_ts,1)
    e <- tsCV(y = qty_ma_ts,f_ets, h=1)
    r_ets<-sqrt(mean(e^2, na.rm=TRUE))
    
  }else{
    
    #   Naive    
    naive_out<- naive(qty_ma_ts , h = h1) 
    # accuracy_naive<-accuracy(naive_out)
    
    e<-tsCV(qty_ma_ts, naive, h=1)
    r_naive<-sqrt(mean(e^2, na.rm = TRUE))
    
    #   Meanf     
    meanf_out<-meanf(qty_ma_ts, h = h1)
    # accuracy_meanf<-accuracy(meanf_out)
    
    e<- tsCV(qty_ma_ts, meanf, h=1)
    r_meanf<-sqrt(mean(e^2, na.rm = TRUE))
    
    #   Drift method    
    drift_out<-rwf(qty_ma_ts, drift = TRUE, h = h1)
    # accuracy_drift<-accuracy(drift_out)
    
    m4<- tsCV(qty_ma_ts, rwf, drift=TRUE, h=1)
    r_drift<-sqrt(mean(e^2, na.rm = TRUE))
    #   Holts methods
    
    #   FITTING THE ARIMA MODEL
    output_arima1<-auto.arima(qty_ma_ts,seasonal = TRUE,approximation = FALSE, stepwise = FALSE)
    arima_out<-forecast(output_arima1, h = h1)
    # accuracy_arima<-accuracy(arima_out)
    
    f_ar <- function(x, h){forecast(Arima(x, order=as.vector(arimaorder(output_arima1))), h=h)}
    e <- tsCV(qty_ma_ts, f_ar, h=1)
    r_arima<-sqrt(mean(e^2, na.rm = TRUE))
    
    #   FITTING THE ETS MODEL
    output_ets1<-ets(qty_ma_ts)
    ets_out<-forecast(output_ets1, h = h1)
    # accuracy_ets<-accuracy(ets_out)
    
    fets <- function(x, h){forecast(ets(x), h=h)}
    e <- tsCV(qty_ma_ts, fets, h=1)
    r_ets<-sqrt(mean(e^2, na.rm = TRUE))
  }
  
  
  
  #      COMPARING THE MODELS
  v_min<-c("r_arima"= r_arima,"r_drift"=r_drift,"r_meanf"=r_meanf,"r_naive"=r_naive,"r_ets"=r_ets)
  model<-names(v_min)[which.min(v_min)]%>%str_split('_')%>%unlist()
  print(paste("best fit is ", model[2] ,"with an RMSE of ",round(min(r_arima,r_drift,r_naive,r_ets,r_meanf),3)))
  
  if(model[2]=="ets"){
    model_final = ets_out
  }else if(model[2] == "arima"){
    model_final = arima_out
  }else if(model[2] == "naive"){
    model_final = naive_out
  }else if(model[2] == "drift"){
    model_final = drift_out
  }else if(model[2] == "meanf"){
    model_final = meanf_out
  }
  
  
  ## pull AIC, BIC AICc values using $
  
  temp<-list()
  
  d3<-head(date_seq,n = 1)
  
  df_fc<-as.data.frame(model_final)
  df_fc$Date<-date_seq
  
  
  if(horizontype==1){
    
    
    df_ag<-df_ag%>%select(Date,qty_ma_impute)
    df_ag$qty_ma_impute<-as.numeric(df_ag$qty_ma_impute)
    
    df_bind<-merge(df_ag,df_fc, by="Date", all = T)
    df_final<-df_bind%>%select(-Date)
    # setnames(df_final, old = c("qty_ma_impute", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95"), new=c("actual", "forecast", "low_80", "high_80", "low_95", "high_95"))
    df_final<-xts(df_final, order.by = df_bind$Date)
    
    a<-xts(df_fc%>%select(-Date),order.by = df_fc$Date)
    c1<-a$`Point Forecast`%>%apply.monthly(FUN = sum)
    c2<-a$`Lo 80`%>%apply.monthly(FUN = sum)
    c3<-a$`Hi 80`%>%apply.monthly(FUN = sum)
    c4<-a$`Lo 95`%>%apply.monthly(FUN = sum)
    c5<-a$`Hi 95`%>%apply.monthly(FUN = sum)
    a<-cbind(c1,c2,c3,c4,c5)
    index(a)<-as.yearmon(index(a))
    
    temp$temp1<-as.data.frame(a)
    
    temp$temp2<-dygraph(df_final, main =paste("total sales by volume for ",party_volume[index,1]))%>%
      dySeries("qty_ma_impute", label = "Actual")%>%
      dySeries(c("Lo 95", "Point Forecast","Hi 95"), label = "Predicted")%>%
      dyRangeSelector()%>% dyCrosshair(direction = "vertical")
    
  }else if(horizontype == 2)
  {
    
    
    df_ag<-df_ag%>%select(Date,qty_ma_impute)
    df_ag$qty_ma_impute<-as.numeric(df_ag$qty_ma_impute)
    df_ag_q<-df_ag%>%select(-Date)%>%xts(order.by = df_ag$Date)%>%apply.quarterly(FUN = sum)
    index(df_ag_q)<-as.yearqtr(index(df_ag_q))
    df_ag_q<-as.data.frame(df_ag_q)
    df_ag_q$Date<-seq.Date(from = dh2, length.out = length(df_ag_q$qty_ma_impute), by = "quarter" )
    
    a<-xts(df_fc%>%select(-Date),order.by = df_fc$Date)
    c1<-a$`Point Forecast`%>%apply.quarterly(FUN = sum)
    c2<-a$`Lo 80`%>%apply.quarterly(FUN = sum)
    c3<-a$`Hi 80`%>%apply.quarterly(FUN = sum)
    c4<-a$`Lo 95`%>%apply.quarterly(FUN = sum)
    c5<-a$`Hi 95`%>%apply.quarterly(FUN = sum)
    a<-cbind(c1,c2,c3,c4,c5)
    index(a)<-as.yearqtr(index(a))
    a<-as.data.frame(a)
    a$Date<-seq.Date(from = d3, length.out = length(a$Point.Forecast), by = "quarter")
    
    df_bind<-merge(df_ag_q,a, by="Date", all = T)
    df_final<-df_bind%>%select(-Date)
    df_final<-xts(df_final, order.by = df_bind$Date)
    
    temp$temp1<-a
    
    temp$temp2<-dygraph(df_final, main =paste("total sales by volume for ",party_volume[index,1]))%>%
      dySeries("qty_ma_impute", label = "Actual")%>%
      dySeries(c("Lo.95", "Point.Forecast","Hi.95"), label = "Predicted")%>%
      dyRangeSelector()%>% dyCrosshair(direction = "vertical")
  }
  return(temp)
  }
}