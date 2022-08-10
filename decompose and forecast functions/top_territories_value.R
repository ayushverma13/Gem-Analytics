
seasonalsales_territory_value<-function(x, index){
  
  a<-list()
  # par(mfrow=c(2,2))
  # par(mar = rep(2,4))
  # x<-msd_pp
  # x<-master
  # index<-1
  territory_value<-x%>%select(Territory,Amt.)%>%
    group_by(Territory)%>%summarise(amt_sold=sum(Amt.))%>%
    arrange(desc(amt_sold))%>%
    mutate(cumsum=cumsum(amt_sold),freq=round(amt_sold/sum(amt_sold),3),cum_freq=cumsum(freq))
  
  principles_sales<-x%>%select(Date,Territory,`Order Qty`,Amt.)%>%rowwise()%>%filter( Territory==territory_value[index,1])
  
  totalsales_value<-principles_sales%>%select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))
  totalsales_value$Date<-as.Date(totalsales_value$Date,"%d-%m-%Y")
  totalsales_value_ts<-xts(totalsales_value$amt,order.by = totalsales_value$Date)
  # plot<-dygraph(totalsales_value_ts)
  a$temp1<-dygraph( totalsales_value_ts, main = paste(index," - Historical sales trend for",territory_value[index,1],"by value"))%>%
    dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
    dyCrosshair(direction = "vertical")
  
  dh<-head(totalsales_value$Date, n=1)
  table<-xts(totalsales_value$amt,order.by = totalsales_value$Date)%>%apply.monthly(FUN = sum)
  index(table)<-as.yearmon(index(table))
  table<-as.data.frame(table)
  table$amt=table$V1
  a$temp2<-table%>%select(amt)
  return(a)
}

