
pareto_customer<-function(x){
  
par(mfrow=c(2,1))

  party_value<-x%>%select(Party,Amt.)%>%group_by(Party)%>%summarise(total_sales=sum(Amt.))%>%arrange(desc(total_sales))%>%mutate(cumsum=cumsum(total_sales),freq=round(total_sales/sum(total_sales),3),cum_freq=cumsum(freq))
 
  party_volume1<-x%>%select(Party,`Order Qty`)%>%group_by(Party)%>%summarise(total_qty=sum(`Order Qty`))%>%arrange(desc(total_qty))%>%mutate(cumsum=cumsum(total_qty),freq=round(total_qty/sum(total_qty),3),cum_freq=cumsum(freq))
  
  pval<-barplot(party_value$total_sales, 
              width = 1, space = 0.2, border = NA, axes = F, 
              ylim = c(0, 1.05 * max(party_value$total_sales, na.rm = T)), 
              ylab = "Counts",cex.names = 0.7, 
              names.arg = party_value$Party,main = "Pareto Chart (value wise)")
  
  axis(side = 2, at = c(0, party_value$Party), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
  
  box(col = "grey62")
  
  px <- party_value$cum_freq * max(party_value$total_sales, na.rm = T)
  pvalp<-lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
  
  axis(side = 4, at = c(0, px), labels = paste(c(0, round(party_value$cum_freq * 100)) ,"%",sep=""), 
       las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
  
  pvol<-barplot(party_volume1$total_qty, 
                width = 1, space = 0.2, border = NA, axes = F, 
                ylim = c(0, 1.05 * max(party_volume1$total_qty, na.rm = T)), 
                ylab = "Counts",cex.names = 0.7, 
                names.arg = party_volume1$Party,main = "Pareto Chart (volume wise)")
  
  axis(side = 2, at = c(0, party_volume1$Party), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
  
  box(col = "grey62")
  
  px <- party_volume1$cum_freq * max(party_volume1$total_qty, na.rm = T)
  pvolp<-lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
  
  axis(side = 4, at = c(0, px), labels = paste(c(0, round(party_volume1$cum_freq * 100)) ,"%",sep=""), 
       las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
  
}