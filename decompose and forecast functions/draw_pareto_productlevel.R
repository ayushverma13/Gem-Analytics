" 'col2' maybe sales by value or volume, 
  'col1'' is the cut on which sales is based e.g.Party, product,customer etc
  'territory' =1 to get the top purchasing territory for a particular product, =2 for the top territory in terms of the rate at which product is purchased"

draw_pareto_productlevel<-function(d_frame, col1, col2, index){
  
  library(plotly)

  # col1 = "Territory"
  # col2 =  "Order Qty"
  # d_frame <- umar_total_territory
  # index<-3

  
  productgroup_value<-d_frame%>%
    select(product_group,Amt.)%>%
    group_by(product_group)%>%
    summarise(total_sales=sum(Amt.))%>%
    arrange(desc(total_sales))%>%
    mutate(cumsum=cumsum(total_sales),freq=round(total_sales/sum(total_sales),3),cum_freq=cumsum(freq))
  
  
  d_frame<-d_frame%>%select(Date,Rate,product_group,Territory,`Order Qty`)%>%rowwise()%>%filter( product_group==productgroup_value[index,1])
  
  d_frame_territories<-d_frame
  
  name = paste("top",col1, "by",col2, sep=" "," for ",productgroup_value[index,1])
  a <-list()
  d_frame$col1<-d_frame[[col1]]
  d_frame$col2<-d_frame[[col2]]%>%as.numeric()
  
  d_frame<-d_frame%>%select(col1,col2)%>%group_by(col1)%>%summarise(col2=sum(col2))%>%arrange(desc(col2))
  
  d_frame<-d_frame%>%mutate(cumsum = cumsum(col2),freq=round((col2)/sum(col2),3),cum_freq=cumsum(freq))
  
  
  d_frame$col1 <- factor(d_frame$col1, levels = unique(d_frame$col1)[order(d_frame$col2, decreasing = TRUE)])
  
  a$pl <- plot_ly(d_frame) %>%
    add_trace(x = ~col1, y = ~col2, type = 'bar', name = 'Value',
              marker = list(color = 'grey'),
              hoverinfo = "text",
              text = ~paste(col1,", percentage of total business =",100*freq,"%")) %>%
    add_trace(x = ~col1, y = ~cum_freq, type = 'scatter', mode = 'lines', name = 'Cummulative Percentage Buisness', yaxis = 'y2',
              line = list(color = 'cyan'),
              hoverinfo = "text",
              text = ~paste(cum_freq*100,"% ",col1)) %>%
    layout(title = name,
           xaxis = list(title = col1),
           yaxis = list(side = 'left', title = col2, showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = 'right', overlaying = "y", title = '% values', showgrid = FALSE, zeroline = FALSE))
  
  
  
  
  
  d_frame_display<-d_frame%>%select(col1,col2,cumsum,freq,cum_freq)
  d_frame_display$freq<-d_frame_display$freq*100
  d_frame_display$cum_freq<-d_frame_display$cum_freq*100
  
  setnames(d_frame_display, old = c("col1", "col2", "cumsum", "freq", "cum_freq"), new=c(col1, col2 , paste("cummulative",col2,sep = " "), "Percentage of total business", "Cummulative Business Offered"))
  a$data_frame <- d_frame_display
  
  
  d_frame_territories<-d_frame_territories%>%select(Territory,Rate)%>%group_by(Territory)%>%summarise(Rate=mean(NA^(Rate==0)*Rate, na.rm=TRUE))%>%arrange(desc(Rate))
  
  d_frame_territories_disp<-d_frame_territories
  
  d_frame_territories<-d_frame_territories%>%mutate(cumsum = cumsum(Rate),freq=round((Rate)/sum(Rate),3),cum_freq=cumsum(freq))
  
  
  d_frame_territories$Territory <- factor(d_frame_territories$Territory, levels = unique(d_frame_territories$Territory)[order(d_frame_territories$Rate, decreasing = TRUE)])
  
  a$pl1 <- plot_ly(d_frame_territories) %>%
    add_trace(x = ~Territory, y = ~Rate, type = 'bar', name = 'Value',
              marker = list(color = 'light blue'),
              hoverinfo = "text",
              text = ~paste(Territory,", Average rate quoted =", round(Rate,2)))%>%
    layout(title = "Territory wise average rates")
  a$data_frame1 <- d_frame_territories_disp
  
  
  return(a)
  
}


# d_frame_display<-d_frame
# d_frame_display$freq<-d_frame_display$freq*100
# d_frame_display$cum_freq<-d_frame_display$cum_freq*100
# 
# setnames(d_frame_display, old = c("col1", "col2", "cumsum", "freq", "cum_freq"), new=c(col1, col2 , paste("cummulative",col2,sep = " "), "Percentage of total business", "Cummulative Business Offered"))
# 
# a$data_frame <- d_frame_display
