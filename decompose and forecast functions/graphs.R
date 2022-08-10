" 'col2' maybe sales by value or volume, 
  'col1'' is the cut on which sales is based e.g.Party, product,customer etc "

draw_pareto<-function(d_frame, col1, col2, name, pngFlag){
  
  library(plotly)
  pngFlag = FALSE
  # col1 = "Party"
  # col2 =  "Amt."
  # d_frame <- master
  
  # col1 = "Territory"
  # col2 =  "Order Qty"
  # d_frame <- umar_total_territory
  # 
  
  
  name = paste("top",col1, "by",col2, sep=" ")
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

  

  
  d_frame<-d_frame%>%mutate(ID=row_number())
  d_frame_display<-d_frame%>%select(ID,col1,col2,cumsum,freq,cum_freq)
  d_frame_display$freq<-d_frame_display$freq*100
  d_frame_display$cum_freq<-d_frame_display$cum_freq*100


  setnames(d_frame_display, old = c("col1", "col2", "cumsum", "freq", "cum_freq"), new=c(col1, col2 , paste("cummulative",col2,sep = " "), "Percentage of total business", "Cummulative Business Offered"))
  a$data_frame <- d_frame_display
  
  
  return(a)
  
}


# d_frame_display<-d_frame
# d_frame_display$freq<-d_frame_display$freq*100
# d_frame_display$cum_freq<-d_frame_display$cum_freq*100
# 
# setnames(d_frame_display, old = c("col1", "col2", "cumsum", "freq", "cum_freq"), new=c(col1, col2 , paste("cummulative",col2,sep = " "), "Percentage of total business", "Cummulative Business Offered"))
# 
# a$data_frame <- d_frame_display
