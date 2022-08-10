forecast_product<-function ( x, measure, index, horizontype){
  
  # measure arguement is 1 for by value or 2 for by volume
  # horizontype is for choosing monthly(1) or quarterly(2)
  # x<-master
  # # x<-htpldata
  # index<-3
  # horizontype=1
  # measure<-1
  # x<-umar_total_territory
  # index<-2
  
  if(measure== 1){
    dh<-head(x$Date, n=1)
    d<-tail(x$Date, n=1)
    productgroup_value<-x%>%
      dplyr::select(product_group,Amt.)%>%
      group_by(product_group)%>%
      summarise(total_sales=sum(Amt.))%>%
      arrange(desc(total_sales))%>%
      mutate(cumsum=cumsum(total_sales),freq=round(total_sales/sum(total_sales),3),cum_freq=cumsum(freq))
    
    x<-x%>%dplyr::select(Date,Amt.,product_group)%>%rowwise()%>%filter( product_group==productgroup_value[index,1])%>%
      dplyr::select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))

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
    f1<-findfrequency(x$amt_ma_impute)
    if(is.na(f)){
      f = findfrequency(x$amt_ma_impute)
      f1 = findfrequency(x$amt_ma_impute)
    }
    
    
    
    dh3<-dh2%>%str_split('-')%>%unlist()
    amt_ma_ts<-ts(na.omit(x$amt_ma_impute), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    x$amt_ma_ts=amt_ma_ts
    
    if(length(amt_ma_ts)>60 ){
      if(!has_error(stl(amt_ma_ts, s.window="periodic"))){
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
      
      
      if(length(s_data[1,]) == "0"){
        a$temp1 <- "No seasonality"
      }else{
        s_data<-s_data%>%xts(order.by = x$Date)
        
      }
    }
    
    
    
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
    
    ## STLF Models
    
    if(!has_error( stlf(amt_ma_ts, method = "naive" , h = h1))){
      
      #   Naive    
      naive_out_s<- stlf(amt_ma_ts, method = "naive" , h = h1)
      # accuracy_naive<-accuracy(naive_out)
      
      f_naive <- function(y, h){stlf(y, method = "naive", h = h)}
      f_naive(amt_ma_ts,h1)
      r_naive_s<-accuracy(f_naive(amt_ma_ts,h1))
      r_naive_s<-r_naive_s[3]
      # e <- tsCV(y = amt_ma_ts,f_naive, h=1)
      # r_naive<-sqrt(mean(e^2, na.rm=TRUE))
    }else{
      r_naive_s<-Inf
    }
    
    
    
    if(!has_error(stlf(amt_ma_ts, method = "rwdrift" , h = h1))){   
      
      #   Drift method    
      drift_out_s<-stlf(amt_ma_ts, method = "rwdrift" , h = h1)
      # accuracy_drift<-accuracy(drift_out)
      
      f_drift <- function(y, h){stlf(y, method = "rwdrift", h = h)}
      f_drift(amt_ma_ts,h1)
      r_drift_s<-accuracy(f_drift(amt_ma_ts,h1))
      r_drift_s<-r_drift_s[3]
      # e <- tsCV(y = amt_ma_ts,f_drift, h=1)
      # r_drift<-sqrt(mean(e^2, na.rm=TRUE))
      
    }else{
      r_drift_s<-Inf
    }
    
    
    # if(!has_error( ))   
    #   Holts methods
    
    
    # if(!has_error( ))   
    #   BATS methods
    
    
    
    # if(!has_error( ))   
    #   TBATS methods
    
    
    
    if(!has_error( stlm(amt_ma_ts, modelfunction=Arima, order=v1))) {  
      
      # FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(sadj_data$x,approximation = FALSE , stepwise = FALSE, seasonal = FALSE )  
      ord<-arimaorder(output_arima1)
      v1<-as.vector(ord)
      output_arima2<- stlm(amt_ma_ts, modelfunction=Arima, order=v1)
      arima_out_s<-forecast(output_arima2, h = h1)
      accuracy_arima<-accuracy(arima_out)
      r_arima_s<-accuracy_arima[3]
      # f_ar <- function(y, h){forecast(stlm(y, modelfunction=Arima, order=v1), h=h)}
      # f_ar(amt_ma_ts,1)
      # e <- tsCV(y = amt_ma_ts,f_ar, h=1)
      # r_arima<-sqrt(mean(e^2, na.rm=TRUE))
    }else{
      r_arima_s<-Inf
    }
    
    
    
    if(!has_error(stlf(amt_ma_ts, h = h1))){   
      
      #   FITTING THE ETS MODEL
      ets_out_s<-stlf(amt_ma_ts, h = h1)
      # accuracy_ets<-accuracy(ets_out)
      
      f_ets <- function(y, h){stlf(y, h = h)}
      f_ets(amt_ma_ts,h1)
      r_ets_s<-accuracy(f_ets(amt_ma_ts,h1))
      r_ets_s<-r_ets_s[3]
      # e <- tsCV(y = amt_ma_ts,f_ets, h=1)
      # r_ets<-sqrt(mean(e^2, na.rm=TRUE))
    }else{
      r_ets_s<-Inf
    }
    
    
    
    
    # Normal models 
    if(!has_error(naive(amt_ma_ts , h = h1))){ 
      #   Naive    
      naive_out<- naive(amt_ma_ts , h = h1) 
      accuracy_naive<-accuracy(naive_out)
      r_naive<-accuracy_naive[3]
      
      # e<-tsCV(amt_ma_ts, naive, h=1)
      # r_naive<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_naive<-Inf
    }
    
    if(!has_error(meanf(amt_ma_ts, h = h1))){ 
      #   Meanf     
      meanf_out<-meanf(amt_ma_ts, h = h1)
      accuracy_meanf<-accuracy(meanf_out)
      r_meanf<-accuracy_meanf[3]
      
      # e<- tsCV(amt_ma_ts, meanf, h=1)
      # r_meanf<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_meanf<-Inf
    }
    
    
    if(!has_error(rwf(amt_ma_ts, drift = TRUE, h = h1))){   
      #   Drift method    
      drift_out<-rwf(amt_ma_ts, drift = TRUE, h = h1)
      accuracy_drift<-accuracy(drift_out)
      r_drift<-accuracy_drift[3]
      
      # e<- tsCV(amt_ma_ts, rwf, drift=TRUE, h=1)
      # r_drift<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_drift<-Inf
    }
    
    # if(!has_error( ))    
    #   Holts methods
    
    # if(!has_error( ))    
    #   BATS methods
    
    
    # if(!has_error( ))    
    #   TBATS methods
    
    
    
    if(!has_error(auto.arima(amt_ma_ts,seasonal = TRUE,approximation = FALSE, stepwise = FALSE))) {  
      #   FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(amt_ma_ts,seasonal = TRUE,approximation = FALSE, stepwise = FALSE)
      arima_out<-forecast(output_arima1, h = h1)
      accuracy_arima<-accuracy(arima_out)
      r_arima<-accuracy_arima[3]
      
      
      # f_ar <- function(x, h){forecast(Arima(x, order=as.vector(arimaorder(output_arima1))), h=h)}
      # e <- tsCV(amt_ma_ts, f_ar, h=1)
      # r_arima<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_arima<-Inf
    }
    
    
    if(!has_error(ets(amt_ma_ts)))  {  
      #   FITTING THE ETS MODEL
      output_ets1<-ets(amt_ma_ts)
      ets_out<-forecast(output_ets1, h = h1)
      accuracy_ets<-accuracy(ets_out)
      r_ets<-accuracy_ets[3]
      
      # fets <- function(x, h){forecast(ets(x), h=h)}
      # e <- tsCV(amt_ma_ts, fets, h=1)
      # r_ets<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_ets<-Inf
    }
    
    
    
    
    
    
    # Normal models For the short time series using fourier 
    
    amt_ma_ts_d<-components$trend+components$remainder
    # amt_ma_ts_d<-components$remainder
    
    if(!has_error(naive(amt_ma_ts_d , h = h1))){ 
      #   Naive    
      naive_out_d<- naive(amt_ma_ts_d , h = h1)
      naive_out_d$mean<-naive_out_d$mean+tail(components$season,12)
      naive_out_d$lower[,1]<-naive_out_d$lower[,1]+tail(components$season,12)
      naive_out_d$lower[,2]<-naive_out_d$lower[,2]+tail(components$season,12)
      naive_out_d$upper[,1]<-naive_out_d$upper[,1]+tail(components$season,12)
      naive_out_d$upper[,2]<-naive_out_d$upper[,2]+tail(components$season,12)
      accuracy_naive<-accuracy(naive_out_d)
      r_naive_d<-accuracy_naive[3]
      
      # e<-tsCV(amt_ma_ts, naive, h=1)
      # r_naive<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_naive_d<-Inf
    }
    
    if(!has_error(meanf(amt_ma_ts_d, h = h1))){ 
      #   Meanf     
      meanf_out_d<-meanf(amt_ma_ts_d, h = h1)
      meanf_out_d$mean<-meanf_out_d$mean+tail(components$season,12)
      meanf_out_d$lower[,1]<-meanf_out_d$lower[,1]+tail(components$season,12)
      meanf_out_d$lower[,2]<-meanf_out_d$lower[,2]+tail(components$season,12)
      meanf_out_d$upper[,1]<-meanf_out_d$upper[,1]+tail(components$season,12)
      meanf_out_d$upper[,2]<-meanf_out_d$upper[,2]+tail(components$season,12)
      accuracy_meanf<-accuracy(meanf_out_d)
      r_meanf_d<-accuracy_meanf[3]
      
      # e<- tsCV(amt_ma_ts, meanf, h=1)
      # r_meanf<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_meanf_d<-Inf
    }
    
    
    if(!has_error(rwf(amt_ma_ts_d, drift = TRUE, h = h1))){   
      #   Drift method    
      drift_out_d<-rwf(amt_ma_ts_d, drift = TRUE, h = h1)
      drift_out_d$mean<-drift_out_d$mean+tail(components$season,12)
      drift_out_d$lower[,1]<-drift_out_d$lower[,1]+tail(components$season,12)
      drift_out_d$lower[,2]<-drift_out_d$lower[,2]+tail(components$season,12)
      drift_out_d$upper[,1]<-drift_out_d$upper[,1]+tail(components$season,12)
      drift_out_d$upper[,2]<-drift_out_d$upper[,2]+tail(components$season,12)
      accuracy_drift<-accuracy(drift_out_d)
      r_drift_d<-accuracy_drift[3]
      
      # e<- tsCV(amt_ma_ts, rwf, drift=TRUE, h=1)
      # r_drift<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_drift_d<-Inf
    }
    
    # if(!has_error( ))    
    #   Holts methods
    
    # if(!has_error( ))    
    #   BATS methods
    
    
    # if(!has_error( ))    
    #   TBATS methods
    
    
    
    if(!has_error(auto.arima(amt_ma_ts_d,seasonal = TRUE,approximation = FALSE, stepwise = FALSE))) {  
      #   FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(amt_ma_ts_d,seasonal = FALSE,approximation = FALSE, stepwise = FALSE)
      arima_out_d<-forecast(output_arima1, h = h1)
      arima_out_d$mean<-arima_out_d$mean+tail(components$season,12)
      arima_out_d$lower[,1]<-arima_out_d$lower[,1]+tail(components$season,12)
      arima_out_d$lower[,2]<-arima_out_d$lower[,2]+tail(components$season,12)
      arima_out_d$upper[,1]<-arima_out_d$upper[,1]+tail(components$season,12)
      arima_out_d$upper[,2]<-arima_out_d$upper[,2]+tail(components$season,12)
      accuracy_arima<-accuracy(arima_out_d)
      r_arima_d<-accuracy_arima[3]
      
      
      # f_ar <- function(x, h){forecast(Arima(x, order=as.vector(arimaorder(output_arima1))), h=h)}
      # e <- tsCV(amt_ma_ts, f_ar, h=1)
      # r_arima<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_arima_d<-Inf
    }
    
    
    if(!has_error(ets(amt_ma_ts_d)))  {  
      #   FITTING THE ETS MODEL
      output_ets1<-ets(amt_ma_ts_d)
      ets_out_d<-forecast(output_ets1, h = h1)
      ets_out_d$mean<-ets_out_d$mean+tail(components$season,12)
      ets_out_d$lower[,1]<-ets_out_d$lower[,1]+tail(components$season,12)
      ets_out_d$lower[,2]<-ets_out_d$lower[,2]+tail(components$season,12)
      ets_out_d$upper[,1]<-ets_out_d$upper[,1]+tail(components$season,12)
      ets_out_d$upper[,2]<-ets_out_d$upper[,2]+tail(components$season,12)
      accuracy_ets<-accuracy(ets_out_d)
      r_ets_d<-accuracy_ets[3]
      
      # fets <- function(x, h){forecast(ets(x), h=h)}
      # e <- tsCV(amt_ma_ts, fets, h=1)
      # r_ets<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_ets_d<-Inf
    }
    
    
    # fruit <- c("apple", "banana", "pear", "pinapple")
    # str_detect(model[2], "arima")
    # str_detect(fruit, "^a")
    # str_detect(fruit, "a$")
    # str_detect(fruit, "b")
    # str_detect(fruit, "[aeiou]")
    
    
    
    #      COMPARING THE MODELS
    v_min<-c("r_arima"= r_arima,"r_drift"=r_drift,"r_meanf"=r_meanf,"r_naive"=r_naive,"r_ets"=r_ets,"r_arima.seasonal"= r_arima_s,"r_drift.seasonal"=r_drift_s,"r_naive.seasonal"=r_naive_s,"r_ets.seasonal"=r_ets_s,"r_arima.decomposed"= r_arima_d,"r_drift.decomposed"=r_drift_d,"r_meanf.decomposed"=r_meanf_d,"r_naive.decomposed"=r_naive_d,"r_ets.decomposed"=r_ets_d)
    model<-names(v_min)[which.min(v_min)]%>%str_split('_')%>%unlist()
    # print(paste("best fit is ",model[2] ,"with an RMSE of ",round(min(r_arima,r_drift,r_naive,r_ets,r_meanf),3)))
    
    if(model[2]=="ets"){
      model_final = ets_out}
    if(model[2] == "arima"){
      model_final = arima_out}
    if(model[2] == "naive"){
      model_final = naive_out}
    if(model[2] == "drift"){
      model_final = drift_out}
    if(model[2] == "meanf"){
      model_final = meanf_out}
    if(model[2] == "ets.seasonal"){
      model_final = ets_out_s}
    if(model[2] == "arima.seasonal"){
      model_final = arima_out_s}
    if(model[2] == "naive.seasonal"){
      model_final = naive_out_s}
    if(model[2] == "drift.seasonal"){
      model_final = drift_out_s}
    if(model[2]=="ets.decomposed"){
      model_final = ets_out_d}
    if(model[2] == "arima.decomposed"){
      model_final = arima_out_d}
    if(model[2] == "naive.decomposed"){
      model_final = naive_out_d}
    if(model[2] == "drift.decomposed"){
      model_final = drift_out_d}
    if(model[2] == "meanf.decomposed"){
      model_final = meanf_out_d}
    
    
    
    
    
    ## pull AIC, BIC AICc values using $
    
    temp<-list()
    
    d3<-head(date_seq,n = 1)
    
    df_fc<-as.data.frame(model_final)
    df_fc$Date<-date_seq
    
    
    if(horizontype==1){
      
      
      df_ag<-df_ag%>%dplyr::select(Date,amt_ma_impute)
      df_ag$amt_ma_impute<-as.numeric(df_ag$amt_ma_impute)
      
      df_bind<-merge(df_ag,df_fc, by="Date", all = T)
      df_final<-df_bind%>%dplyr::select(-Date)
      # setnames(df_final, old = c("amt_ma_impute", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95"), new=c("actual", "forecast", "low_80", "high_80", "low_95", "high_95"))
      df_final<-xts(df_final, order.by = df_bind$Date)
      
      a<-xts(df_fc%>%dplyr::select(-Date),order.by = df_fc$Date)
      c1<-a$`Point Forecast`%>%apply.monthly(FUN = sum)
      c2<-a$`Lo 80`%>%apply.monthly(FUN = sum)
      c3<-a$`Hi 80`%>%apply.monthly(FUN = sum)
      c4<-a$`Lo 95`%>%apply.monthly(FUN = sum)
      c5<-a$`Hi 95`%>%apply.monthly(FUN = sum)
      a<-cbind(c1,c2,c3,c4,c5)
      index(a)<-as.yearmon(index(a))
      
      temp$temp1<-as.data.frame(a)
      
      temp$temp2<-dygraph(df_final, main = paste("total sales by value for ",productgroup_value[index,1]))%>%
        dySeries("amt_ma_impute", label = "Actual")%>%
        dySeries(c("Lo 95", "Point Forecast","Hi 95"), label = "Predicted")%>%
        dyRangeSelector()%>% dyCrosshair(direction = "vertical")
      
    }else if(horizontype == 2)
    {
      
      
      df_ag<-df_ag%>%dplyr::select(Date,amt_ma_impute)
      df_ag$amt_ma_impute<-as.numeric(df_ag$amt_ma_impute)
      df_ag_q<-df_ag%>%dplyr::select(-Date)%>%xts(order.by = df_ag$Date)%>%apply.quarterly(FUN = sum)
      index(df_ag_q)<-as.yearqtr(index(df_ag_q))
      df_ag_q<-as.data.frame(df_ag_q)
      df_ag_q$Date<-seq.Date(from = dh2, length.out = length(df_ag_q$amt_ma_impute), by = "quarter" )
      
      a<-xts(df_fc%>%dplyr::select(-Date),order.by = df_fc$Date)
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
      df_final<-df_bind%>%dplyr::select(-Date)
      df_final<-xts(df_final, order.by = df_bind$Date)
      
      temp$temp1<-a
      
      temp$temp2<-dygraph(df_final, main = paste("total sales by value for ",productgroup_value[index,1]))%>%
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
    
    productgroup_volume<-x%>%dplyr::select(product_group,`Order Qty`)%>%
      group_by(product_group)%>%
      summarise(total_qty=sum(`Order Qty`))%>%
      arrange(desc(total_qty))%>%
      mutate(cumsum=cumsum(total_qty),freq=round(total_qty/sum(total_qty),3),cum_freq=cumsum(freq))
    
    x<-x%>%dplyr::select(Date,`Order Qty`,product_group)%>%rowwise()%>%filter( product_group==productgroup_volume[index,1])%>%
      dplyr::select(Date,`Order Qty`)%>%group_by(Date)%>%summarise(qty=sum(`Order Qty`))
    

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
    
    dh3<-dh2%>%str_split('-')%>%unlist()
    qty_ma_ts<-ts(na.omit(x$qty_ma_impute), frequency = 12, start = c(as.numeric(dh3[1]),as.numeric(dh3[2])))
    x$qty_ma_ts=qty_ma_ts
    
    if(length(qty_ma_ts)>60 ){
      if(!has_error(stl(qty_ma_ts, s.window="periodic"))){
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
      
      
      if(length(s_data[1,]) == "0"){
        a$temp1 <- "No seasonality"
      }else{
        s_data<-s_data%>%xts(order.by = x$Date)
        
      }
    }
    
    
    
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
    
    ## STLF Models
    
    if(!has_error( stlf(qty_ma_ts, method = "naive" , h = h1))){
      
      #   Naive    
      naive_out_s<- stlf(qty_ma_ts, method = "naive" , h = h1)
      # accuracy_naive<-accuracy(naive_out)
      
      f_naive <- function(y, h){stlf(y, method = "naive", h = h)}
      f_naive(qty_ma_ts,h1)
      r_naive_s<-accuracy(f_naive(qty_ma_ts,h1))
      r_naive_s<-r_naive_s[3]
      # e <- tsCV(y = qty_ma_ts,f_naive, h=1)
      # r_naive<-sqrt(mean(e^2, na.rm=TRUE))
    }else{
      r_naive_s<-Inf
    }
    
    
    
    if(!has_error(stlf(qty_ma_ts, method = "rwdrift" , h = h1))){   
      
      #   Drift method    
      drift_out_s<-stlf(qty_ma_ts, method = "rwdrift" , h = h1)
      # accuracy_drift<-accuracy(drift_out)
      
      f_drift <- function(y, h){stlf(y, method = "rwdrift", h = h)}
      f_drift(qty_ma_ts,h1)
      r_drift_s<-accuracy(f_drift(qty_ma_ts,h1))
      r_drift_s<-r_drift_s[3]
      # e <- tsCV(y = qty_ma_ts,f_drift, h=1)
      # r_drift<-sqrt(mean(e^2, na.rm=TRUE))
      
    }else{
      r_drift_s<-Inf
    }
    
    
    # if(!has_error( ))   
    #   Holts methods
    
    
    # if(!has_error( ))   
    #   BATS methods
    
    
    
    # if(!has_error( ))   
    #   TBATS methods
    
    
    
    if(!has_error( stlm(qty_ma_ts, modelfunction=Arima, order=v1))) {  
      
      # FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(sadj_data$x,approximation = FALSE , stepwise = FALSE, seasonal = FALSE )  
      ord<-arimaorder(output_arima1)
      v1<-as.vector(ord)
      output_arima2<- stlm(qty_ma_ts, modelfunction=Arima, order=v1)
      arima_out_s<-forecast(output_arima2, h = h1)
      accuracy_arima<-accuracy(arima_out)
      r_arima_s<-accuracy_arima[3]
      # f_ar <- function(y, h){forecast(stlm(y, modelfunction=Arima, order=v1), h=h)}
      # f_ar(qty_ma_ts,1)
      # e <- tsCV(y = qty_ma_ts,f_ar, h=1)
      # r_arima<-sqrt(mean(e^2, na.rm=TRUE))
    }else{
      r_arima_s<-Inf
    }
    
    
    
    if(!has_error(stlf(qty_ma_ts, h = h1))){   
      
      #   FITTING THE ETS MODEL
      ets_out_s<-stlf(qty_ma_ts, h = h1)
      # accuracy_ets<-accuracy(ets_out)
      
      f_ets <- function(y, h){stlf(y, h = h)}
      f_ets(qty_ma_ts,h1)
      r_ets_s<-accuracy(f_ets(qty_ma_ts,h1))
      r_ets_s<-r_ets_s[3]
      # e <- tsCV(y = qty_ma_ts,f_ets, h=1)
      # r_ets<-sqrt(mean(e^2, na.rm=TRUE))
    }else{
      r_ets_s<-Inf
    }
    
    
    
    
    # Normal models 
    if(!has_error(naive(qty_ma_ts , h = h1))){ 
      #   Naive    
      naive_out<- naive(qty_ma_ts , h = h1) 
      accuracy_naive<-accuracy(naive_out)
      r_naive<-accuracy_naive[3]
      
      # e<-tsCV(qty_ma_ts, naive, h=1)
      # r_naive<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_naive<-Inf
    }
    
    if(!has_error(meanf(qty_ma_ts, h = h1))){ 
      #   Meanf     
      meanf_out<-meanf(qty_ma_ts, h = h1)
      accuracy_meanf<-accuracy(meanf_out)
      r_meanf<-accuracy_meanf[3]
      
      # e<- tsCV(qty_ma_ts, meanf, h=1)
      # r_meanf<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_meanf<-Inf
    }
    
    
    if(!has_error(rwf(qty_ma_ts, drift = TRUE, h = h1))){   
      #   Drift method    
      drift_out<-rwf(qty_ma_ts, drift = TRUE, h = h1)
      accuracy_drift<-accuracy(drift_out)
      r_drift<-accuracy_drift[3]
      
      # e<- tsCV(qty_ma_ts, rwf, drift=TRUE, h=1)
      # r_drift<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_drift<-Inf
    }
    
    # if(!has_error( ))    
    #   Holts methods
    
    # if(!has_error( ))    
    #   BATS methods
    
    
    # if(!has_error( ))    
    #   TBATS methods
    
    
    
    if(!has_error(auto.arima(qty_ma_ts,seasonal = TRUE,approximation = FALSE, stepwise = FALSE))) {  
      #   FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(qty_ma_ts,seasonal = TRUE,approximation = FALSE, stepwise = FALSE)
      arima_out<-forecast(output_arima1, h = h1)
      accuracy_arima<-accuracy(arima_out)
      r_arima<-accuracy_arima[3]
      
      
      # f_ar <- function(x, h){forecast(Arima(x, order=as.vector(arimaorder(output_arima1))), h=h)}
      # e <- tsCV(qty_ma_ts, f_ar, h=1)
      # r_arima<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_arima<-Inf
    }
    
    
    if(!has_error(ets(qty_ma_ts)))  {  
      #   FITTING THE ETS MODEL
      output_ets1<-ets(qty_ma_ts)
      ets_out<-forecast(output_ets1, h = h1)
      accuracy_ets<-accuracy(ets_out)
      r_ets<-accuracy_ets[3]
      
      # fets <- function(x, h){forecast(ets(x), h=h)}
      # e <- tsCV(qty_ma_ts, fets, h=1)
      # r_ets<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_ets<-Inf
    }
    
    
    
    
    
    
    # Normal models For the short time series using fourier 
    
    qty_ma_ts_d<-components$trend+components$remainder
    # qty_ma_ts_d<-components$remainder
    
    if(!has_error(naive(qty_ma_ts_d , h = h1))){ 
      #   Naive    
      naive_out_d<- naive(qty_ma_ts_d , h = h1)
      naive_out_d$mean<-naive_out_d$mean+tail(components$season,12)
      naive_out_d$lower[,1]<-naive_out_d$lower[,1]+tail(components$season,12)
      naive_out_d$lower[,2]<-naive_out_d$lower[,2]+tail(components$season,12)
      naive_out_d$upper[,1]<-naive_out_d$upper[,1]+tail(components$season,12)
      naive_out_d$upper[,2]<-naive_out_d$upper[,2]+tail(components$season,12)
      accuracy_naive<-accuracy(naive_out_d)
      r_naive_d<-accuracy_naive[3]
      
      # e<-tsCV(qty_ma_ts, naive, h=1)
      # r_naive<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_naive_d<-Inf
    }
    
    if(!has_error(meanf(qty_ma_ts_d, h = h1))){ 
      #   Meanf     
      meanf_out_d<-meanf(qty_ma_ts_d, h = h1)
      meanf_out_d$mean<-meanf_out_d$mean+tail(components$season,12)
      meanf_out_d$lower[,1]<-meanf_out_d$lower[,1]+tail(components$season,12)
      meanf_out_d$lower[,2]<-meanf_out_d$lower[,2]+tail(components$season,12)
      meanf_out_d$upper[,1]<-meanf_out_d$upper[,1]+tail(components$season,12)
      meanf_out_d$upper[,2]<-meanf_out_d$upper[,2]+tail(components$season,12)
      accuracy_meanf<-accuracy(meanf_out_d)
      r_meanf_d<-accuracy_meanf[3]
      
      # e<- tsCV(qty_ma_ts, meanf, h=1)
      # r_meanf<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_meanf_d<-Inf
    }
    
    
    if(!has_error(rwf(qty_ma_ts_d, drift = TRUE, h = h1))){   
      #   Drift method    
      drift_out_d<-rwf(qty_ma_ts_d, drift = TRUE, h = h1)
      drift_out_d$mean<-drift_out_d$mean+tail(components$season,12)
      drift_out_d$lower[,1]<-drift_out_d$lower[,1]+tail(components$season,12)
      drift_out_d$lower[,2]<-drift_out_d$lower[,2]+tail(components$season,12)
      drift_out_d$upper[,1]<-drift_out_d$upper[,1]+tail(components$season,12)
      drift_out_d$upper[,2]<-drift_out_d$upper[,2]+tail(components$season,12)
      accuracy_drift<-accuracy(drift_out_d)
      r_drift_d<-accuracy_drift[3]
      
      # e<- tsCV(qty_ma_ts, rwf, drift=TRUE, h=1)
      # r_drift<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_drift_d<-Inf
    }
    
    # if(!has_error( ))    
    #   Holts methods
    
    # if(!has_error( ))    
    #   BATS methods
    
    
    # if(!has_error( ))    
    #   TBATS methods
    
    
    
    if(!has_error(auto.arima(qty_ma_ts_d,seasonal = TRUE,approximation = FALSE, stepwise = FALSE))) {  
      #   FITTING THE ARIMA MODEL
      output_arima1<-auto.arima(qty_ma_ts_d,seasonal = FALSE,approximation = FALSE, stepwise = FALSE)
      arima_out_d<-forecast(output_arima1, h = h1)
      arima_out_d$mean<-arima_out_d$mean+tail(components$season,12)
      arima_out_d$lower[,1]<-arima_out_d$lower[,1]+tail(components$season,12)
      arima_out_d$lower[,2]<-arima_out_d$lower[,2]+tail(components$season,12)
      arima_out_d$upper[,1]<-arima_out_d$upper[,1]+tail(components$season,12)
      arima_out_d$upper[,2]<-arima_out_d$upper[,2]+tail(components$season,12)
      accuracy_arima<-accuracy(arima_out_d)
      r_arima_d<-accuracy_arima[3]
      
      
      # f_ar <- function(x, h){forecast(Arima(x, order=as.vector(arimaorder(output_arima1))), h=h)}
      # e <- tsCV(qty_ma_ts, f_ar, h=1)
      # r_arima<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_arima_d<-Inf
    }
    
    
    if(!has_error(ets(qty_ma_ts_d)))  {  
      #   FITTING THE ETS MODEL
      output_ets1<-ets(qty_ma_ts_d)
      ets_out_d<-forecast(output_ets1, h = h1)
      ets_out_d$mean<-ets_out_d$mean+tail(components$season,12)
      ets_out_d$lower[,1]<-ets_out_d$lower[,1]+tail(components$season,12)
      ets_out_d$lower[,2]<-ets_out_d$lower[,2]+tail(components$season,12)
      ets_out_d$upper[,1]<-ets_out_d$upper[,1]+tail(components$season,12)
      ets_out_d$upper[,2]<-ets_out_d$upper[,2]+tail(components$season,12)
      accuracy_ets<-accuracy(ets_out_d)
      r_ets_d<-accuracy_ets[3]
      
      # fets <- function(x, h){forecast(ets(x), h=h)}
      # e <- tsCV(qty_ma_ts, fets, h=1)
      # r_ets<-sqrt(mean(e^2, na.rm = TRUE))
    }else{
      r_ets_d<-Inf
    }
    
    
    # fruit <- c("apple", "banana", "pear", "pinapple")
    # str_detect(model[2], "arima")
    # str_detect(fruit, "^a")
    # str_detect(fruit, "a$")
    # str_detect(fruit, "b")
    # str_detect(fruit, "[aeiou]")
    
    
    
    #      COMPARING THE MODELS
    v_min<-c("r_arima"= r_arima,"r_drift"=r_drift,"r_meanf"=r_meanf,"r_naive"=r_naive,"r_ets"=r_ets,"r_arima.seasonal"= r_arima_s,"r_drift.seasonal"=r_drift_s,"r_naive.seasonal"=r_naive_s,"r_ets.seasonal"=r_ets_s,"r_arima.decomposed"= r_arima_d,"r_drift.decomposed"=r_drift_d,"r_meanf.decomposed"=r_meanf_d,"r_naive.decomposed"=r_naive_d,"r_ets.decomposed"=r_ets_d)
    model<-names(v_min)[which.min(v_min)]%>%str_split('_')%>%unlist()
    # print(paste("best fit is ",model[2] ,"with an RMSE of ",round(min(r_arima,r_drift,r_naive,r_ets,r_meanf),3)))
    
    if(model[2]=="ets"){
      model_final = ets_out}
    if(model[2] == "arima"){
      model_final = arima_out}
    if(model[2] == "naive"){
      model_final = naive_out}
    if(model[2] == "drift"){
      model_final = drift_out}
    if(model[2] == "meanf"){
      model_final = meanf_out}
    if(model[2] == "ets.seasonal"){
      model_final = ets_out_s}
    if(model[2] == "arima.seasonal"){
      model_final = arima_out_s}
    if(model[2] == "naive.seasonal"){
      model_final = naive_out_s}
    if(model[2] == "drift.seasonal"){
      model_final = drift_out_s}
    if(model[2]=="ets.decomposed"){
      model_final = ets_out_d}
    if(model[2] == "arima.decomposed"){
      model_final = arima_out_d}
    if(model[2] == "naive.decomposed"){
      model_final = naive_out_d}
    if(model[2] == "drift.decomposed"){
      model_final = drift_out_d}
    if(model[2] == "meanf.decomposed"){
      model_final = meanf_out_d}
    
    
    
    
    
    ## pull AIC, BIC AICc values using $
    
    temp<-list()
    
    d3<-head(date_seq,n = 1)
    
    df_fc<-as.data.frame(model_final)
    df_fc$Date<-date_seq
    
    
    if(horizontype==1){
      
      
      df_ag<-df_ag%>%dplyr::select(Date,qty_ma_impute)
      df_ag$qty_ma_impute<-as.numeric(df_ag$qty_ma_impute)
      
      df_bind<-merge(df_ag,df_fc, by="Date", all = T)
      df_final<-df_bind%>%dplyr::select(-Date)
      # setnames(df_final, old = c("qty_ma_impute", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95"), new=c("actual", "forecast", "low_80", "high_80", "low_95", "high_95"))
      df_final<-xts(df_final, order.by = df_bind$Date)
      
      a<-xts(df_fc%>%dplyr::select(-Date),order.by = df_fc$Date)
      c1<-a$`Point Forecast`%>%apply.monthly(FUN = sum)
      c2<-a$`Lo 80`%>%apply.monthly(FUN = sum)
      c3<-a$`Hi 80`%>%apply.monthly(FUN = sum)
      c4<-a$`Lo 95`%>%apply.monthly(FUN = sum)
      c5<-a$`Hi 95`%>%apply.monthly(FUN = sum)
      a<-cbind(c1,c2,c3,c4,c5)
      index(a)<-as.yearmon(index(a))
      
      temp$temp1<-as.data.frame(a)
      
      temp$temp2<-dygraph(df_final, main = paste("total sales by volume for ",productgroup_volume[index,1]))%>%
        dySeries("qty_ma_impute", label = "Actual")%>%
        dySeries(c("Lo 95", "Point Forecast","Hi 95"), label = "Predicted")%>%
        dyRangeSelector()%>% dyCrosshair(direction = "vertical")
      
    }else if(horizontype == 2)
    {
      
      
      df_ag<-df_ag%>%dplyr::select(Date,qty_ma_impute)
      df_ag$qty_ma_impute<-as.numeric(df_ag$qty_ma_impute)
      df_ag_q<-df_ag%>%dplyr::select(-Date)%>%xts(order.by = df_ag$Date)%>%apply.quarterly(FUN = sum)
      index(df_ag_q)<-as.yearqtr(index(df_ag_q))
      df_ag_q<-as.data.frame(df_ag_q)
      df_ag_q$Date<-seq.Date(from = dh2, length.out = length(df_ag_q$qty_ma_impute), by = "quarter" )
      
      a<-xts(df_fc%>%dplyr::select(-Date),order.by = df_fc$Date)
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
      df_final<-df_bind%>%dplyr::select(-Date)
      df_final<-xts(df_final, order.by = df_bind$Date)
      
      temp$temp1<-a
      
      temp$temp2<-dygraph(df_final, main = paste("total sales by volume for ",productgroup_volume[index,1]))%>%
        dySeries("qty_ma_impute", label = "Actual")%>%
        dySeries(c("Lo.95", "Point.Forecast","Hi.95"), label = "Predicted")%>%
        dyRangeSelector()%>% dyCrosshair(direction = "vertical")
    }
    return(temp)
  }
}