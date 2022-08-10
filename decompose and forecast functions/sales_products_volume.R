seasonalsales_product_volume<-function(x, index){
  
  # par(mfrow=c(3,2))
  # par(mar = rep(2,4))
 # x<-master_ltr
 # index=2
  productgroup_volume<-x%>%select(product_group,`Order Qty`)%>%group_by(product_group)%>%summarise(qty_sold=sum(`Order Qty`))%>%arrange(desc(qty_sold))%>%mutate(cumsum=cumsum(qty_sold),freq=round(qty_sold/sum(qty_sold),3),cum_freq=cumsum(freq))

  productgroup_value<-x%>%select(product_group,Amt.)%>%group_by(product_group)%>%summarise(amt_sold=sum(Amt.))%>%arrange(desc(amt_sold))%>%mutate(cumsum=cumsum(amt_sold),freq=round(amt_sold/sum(amt_sold),3),cum_freq=cumsum(freq))
  
  value <- match("Uncategorized", productgroup_volume$product_group)
  a <- list()
  
  if (index == "unc"){
    if(value > 5){
      product_sales_volume_un<-x%>%select(Date,`Order Qty`,Amt.,product_group)%>%rowwise()%>%filter( product_group=="Uncategorized") 
      product_sales_volume_un<-product_sales_volume_un%>%select(Date,`Order Qty`)%>%group_by(Date)%>%summarise(qty=sum(`Order Qty`))
      product_sales_volume_un$Date<-as.Date(product_sales_volume_un$Date,"%d-%m-%Y")
      product_sales_volume_un_ts<-xts(product_sales_volume_un$qty,order.by = product_sales_volume_un$Date)
      # sixthplot<-dygraph( product_sales_volume_un_ts)
      return(list(product_sales_volume_un_ts, product_sales_volume_un,"Uncategorized"))
    }
  }else{
    product_sales_volume_1<-x%>%select(Date,`Order Qty`,Amt.,product_group)%>%rowwise()%>%filter( product_group==productgroup_volume[index,1])
    
    product_sales_volume_1<-product_sales_volume_1%>%select(Date,`Order Qty`)%>%group_by(Date)%>%summarise(qty=sum(`Order Qty`))
    product_sales_volume_1$Date<-as.Date(product_sales_volume_1$Date,"%d-%m-%Y")
    product_sales_volume_1_ts<-xts(product_sales_volume_1$qty,order.by = product_sales_volume_1$Date)
    # firstplot<-dygraph( product_sales_volume_1_ts)
    a$temp1<-dygraph(product_sales_volume_1_ts, main = paste(index," - Historical sales for",productgroup_volume[index,1],"by volume"))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
    
    dh<-head(product_sales_volume_1$Date, n=1)
    table<-xts(product_sales_volume_1$qty,order.by = product_sales_volume_1$Date)%>%apply.monthly(FUN = sum)
    index(table)<-as.yearmon(index(table))
    table<-as.data.frame(table)
    table$qty=table$V1
    a$temp2<-table%>%select(qty)
    
    return(a)
  }
  
  
}