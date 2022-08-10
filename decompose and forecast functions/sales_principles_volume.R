seasonalsales_principles_volume<-function(x, index){
  

  # par(mfrow=c(2,2))
  # par(mar = rep(2,4))
  # x=msd_pp
  
  principles_volume<-x%>%select(`Item Group`,`Order Qty`)%>%group_by(`Item Group`)%>%summarise(qty_sold=sum(`Order Qty`))%>%arrange(desc(qty_sold))%>%mutate(cumsum=cumsum(qty_sold),freq=round(qty_sold/sum(qty_sold),3),cum_freq=cumsum(freq))
  
  

    principles_sales<-x%>%select(Date,Item,`Item Group`,`Order Qty`,Amt.)%>%rowwise()%>%filter( `Item Group`== principles_volume[index,1])   
    totalsales_volume<-principles_sales%>%select(Date,`Order Qty`)%>%group_by(Date)%>%summarise(qty=sum(`Order Qty`))
    totalsales_volume$Date<-as.Date(totalsales_volume$Date,"%d-%m-%Y")
    totalsales_volume_ts<-xts(totalsales_volume$qty,order.by = totalsales_volume$Date)
    # plot<-dygraph(totalsales_volume_ts)
    temp<-dygraph(totalsales_volume_ts, main = paste("Historical sales for",principles_volume[index,1],"by volume"))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    return(list(temp,product_sales_volume_1))
  }
 
  
 