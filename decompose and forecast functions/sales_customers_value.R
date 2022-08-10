seasonalsales_customers_value<-function(x, index){
 
  
  a <- list()
  party_value<-x%>%
    select(Party,Amt.)%>%
    group_by(Party)%>%
    summarise(total_sales=sum(Amt.))%>%
    arrange(desc(total_sales))%>%
    mutate(cumsum=cumsum(total_sales),freq=round(total_sales/sum(total_sales),3),cum_freq=cumsum(freq))
 
    
 customer_sales_value_1<-x%>%select(Date,`Order Qty`,Amt.,Party)%>%rowwise()%>%filter( Party==party_value[index,1])%>%
      select(Date,Amt.)%>%group_by(Date)%>%summarise(amt=sum(Amt.))
   customer_sales_value_1$Date<-as.Date(customer_sales_value_1$Date,"%d-%m-%Y")
     
   
   dh<-head(customer_sales_value_1$Date, n=1)
   table<-xts(customer_sales_value_1$amt,order.by = customer_sales_value_1$Date)%>%apply.monthly(FUN = sum)
   index(table)<-as.yearmon(index(table))
   table<-as.data.frame(table)
   table$amt=table$V1
   a$temp2<-table%>%select(amt)
    
   customer_sales_value_1_ts<-xts(customer_sales_value_1$amt,order.by = customer_sales_value_1$Date)
    # dygraph( customer_sales_value_1_ts)
   a$temp1<-dygraph(customer_sales_value_1_ts, main = paste(index," - Historical sales for",party_value[index,1],"by value"))%>%
     dyLegend(show = "follow",width = 150)%>%dyOptions(fillGraph = TRUE,drawPoints = TRUE,pointSize = 2)%>%dyRangeSelector()%>%
     dyCrosshair(direction = "vertical")
    return(a)
  }
  
  
  
  
  
  