seasonalsales_product_value<-function(x, index){
  
  # par(mfrow=c(3,2))
  # par(mar = rep(2,4))
  # 
  
  a <- list()
  productgroup_value<-x%>%select(product_group,Amt.)%>%group_by(product_group)%>%summarise(amt_sold=sum(Amt.))%>%arrange(desc(amt_sold))%>%mutate(cumsum=cumsum(amt_sold),freq=round(amt_sold/sum(amt_sold),3),cum_freq=cumsum(freq))
  
  value <- match("Uncategorized", productgroup_value$product_group)

if(index == "unc"){
  if(value > 5){
      product_sales_value_un<-x%>%select(Date,`Order Qty`,Amt.,product_group)%>%rowwise()%>%filter( product_group=="Uncategorized")
      product_sales_value_un<-product_sales_value_un%>%select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))
      product_sales_value_un$Date<-as.Date(product_sales_value_un$Date,"%d-%m-%Y")
      product_sales_value_un_ts<-xts(product_sales_value_un$amt,order.by = product_sales_value_un$Date)
      # sixthplot<-dygraph(product_sales_value_un_ts)
      return(list(product_sales_value_un_ts,product_sales_value_un,"Uncategorized"))
  }
}
  else{
    product_sales_value_1<-x%>%select(Date,`Order Qty`,Amt.,product_group)%>%rowwise()%>%filter( product_group==productgroup_value[index,1])
    
    product_sales_value_1<-product_sales_value_1%>%select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))
    product_sales_value_1$Date<-as.Date(product_sales_value_1$Date,"%d-%m-%Y")
    product_sales_value_1_ts<-xts(product_sales_value_1$amt,order.by = product_sales_value_1$Date)
    # firstplot<-dygraph(product_sales_value_1_ts)
    a$temp1<-dygraph(product_sales_value_1_ts, main = paste(index," - Historical sales trend for",productgroup_value[index,1],"by value"))%>%
      dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
      dyCrosshair(direction = "vertical")
  
    dh<-head(product_sales_value_1$Date, n=1)
    table<-xts(product_sales_value_1$amt,order.by = product_sales_value_1$Date)%>%apply.monthly(FUN = sum)
    index(table)<-as.yearmon(index(table))
    table<-as.data.frame(table)
    table$amt=table$V1
    a$temp2<-table%>%select(amt)
    
    
    return(a)

  }
  
  }

