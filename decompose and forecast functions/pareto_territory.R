## 

pareto_territory<-function(x,index){
  # par(mfrow=c(2,1))
  # par(mar = rep(2,4))
  # x<-master_kg
  # territory_volume<-x%>%select(Territory,`Order Qty`)%>%group_by(Territory)%>%summarise(qty_sold=sum(`Order Qty`))%>%arrange(desc(qty_sold))%>%mutate(cumsum=cumsum(qty_sold),freq=round(qty_sold/sum(qty_sold),3),cum_freq=cumsum(freq))
  if(index==1){
    territory_value<-x%>%select(Territory,Amt.)%>%group_by(Territory)%>%summarise(amt_sold=sum(Amt.))%>%arrange(desc(amt_sold))%>%mutate(cumsum=cumsum(amt_sold),freq=round(amt_sold/sum(amt_sold),3),cum_freq=cumsum(freq))
    
    pval<-barplot(territory_value$amt_sold, 
                  width = 1, space = 0.2, border = NA, axes = F, 
                  ylim = c(0, 1.05 * max(territory_value$amt_sold, na.rm = T)), 
                  ylab = "Counts",cex.names = 0.7, 
                  # names.arg = territory_value$Territory,
                  main = "Pareto Chart (value wise)")
    
    axis(side = 2, at = c(0, territory_value$Territory), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
    
    box(col = "grey62")
    
    px <- territory_value$cum_freq * max(territory_value$amt_sold, na.rm = T)
    pvalp<-lines(pval, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
    
    axis(side = 4, at = c(0, px), labels = paste(c(0, round(territory_value$cum_freq * 100)) ,"%",sep=""), 
         las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
    return(territory_value)
  }
  
  if(index==2)
  {
    territory_volume<-x%>%select(Territory,`Order Qty`)%>%group_by(Territory)%>%summarise(qty_sold=sum(`Order Qty`))%>%arrange(desc(qty_sold))%>%mutate(cumsum=cumsum(qty_sold),freq=round(qty_sold/sum(qty_sold),3),cum_freq=cumsum(freq))
    
    pvol<-barplot(territory_volume$qty_sold, 
                  width = 1, space = 0.2, border = NA, axes = F, 
                  ylim = c(0, 1.05 * max(territory_volume$qty_sold, na.rm = T)), 
                  ylab = "Counts",cex.names = 0.7, 
                  # names.arg = territory_volume$Territory,
                  main = "Pareto Chart (volume wise)")
    
    
    axis(side = 2, at = c(0, territory_volume$Territory), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
    
    box(col = "grey62")
    
    px <- territory_volume$cum_freq * max(territory_volume$qty_sold, na.rm = T)
    pvolp<-lines(pvol, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
    
    axis(side = 4, at = c(0, px), labels = paste(c(0, round(territory_volume$cum_freq * 100)) ,"%",sep=""), 
         las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
    return(territory_volume)
  }
  
}
