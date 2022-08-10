
seasonalsales_principles_value<-function(x, index){
  

  # par(mfrow=c(2,2))
  # par(mar = rep(2,4))
  # x<-msd_pp
  principles_value<-x%>%select(`Item Group`,Amt.)%>%
    group_by(`Item Group`)%>%summarise(amt_sold=sum(Amt.))%>%
    arrange(desc(amt_sold))%>%
    mutate(cumsum=cumsum(amt_sold),freq=round(amt_sold/sum(amt_sold),3),cum_freq=cumsum(freq))
  
    principles_sales<-x%>%select(Date,Item,`Item Group`,`Order Qty`,Amt.)%>%rowwise()%>%filter( `Item Group`==principles_value[index,1])
    
    totalsales_value<-principles_sales%>%select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))
    totalsales_value$Date<-as.Date(totalsales_value$Date,"%d-%m-%Y")
    totalsales_value_ts<-xts(totalsales_value$amt,order.by = totalsales_value$Date)
    # plot<-dygraph(totalsales_value_ts)
    temp<-dygraph( totalsales_value_ts, main = paste("Historical sales trend for",principles_value[index,1],"by value"))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    return(list(temp, totalsales_value))
  }
  