seasonalsales_customers_volume<-function(x,index){

  
a <- list()

party_volume<-x%>%
  select(Party,`Order Qty`)%>%
  group_by(Party)%>%
  summarise(total_qty=sum(`Order Qty`))%>%
  arrange(desc(total_qty))%>%
  mutate(cumsum=cumsum(total_qty),freq=round(total_qty/sum(total_qty),3),cum_freq=cumsum(freq))

party_sales_volume_1<-x%>%select(Date,`Order Qty`,Amt.,Party)%>%rowwise()%>%filter( Party==party_volume[index,1])

customer_sales_volume_1<-party_sales_volume_1%>%select(Date,`Order Qty`)%>%group_by(Date)%>%summarise(qty=sum(`Order Qty`))
customer_sales_volume_1$Date<-as.Date(customer_sales_volume_1$Date,"%d-%m-%Y")
customer_sales_volume_1_ts<-xts(customer_sales_volume_1$qty,order.by = customer_sales_volume_1$Date)
# firstplot<-dygraph( customer_sales_volume_1_ts)
a$temp1<-dygraph(customer_sales_volume_1_ts, main = paste(index," - Historical sales for",party_volume[index,1],"by volume"))%>%
  dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
  dyCrosshair(direction = "vertical")

dh<-head(customer_sales_volume_1$Date, n=1)
table<-xts(customer_sales_volume_1$qty,order.by = customer_sales_volume_1$Date)%>%apply.monthly(FUN = sum)
index(table)<-as.yearmon(index(table))
table<-as.data.frame(table)
table$qty=table$V1
a$temp2<-table%>%select(qty)


return(a)
}



