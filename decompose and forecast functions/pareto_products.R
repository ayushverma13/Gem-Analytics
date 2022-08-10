pareto_product<-function(x, index){
  # par(mfrow=c(2,1))
  # par(mar = rep(2,4))
  

if(index == 1){
  productgroup_value<-x%>%select(product_group,Amt.)%>%group_by(product_group)%>%summarise(amt_sold=sum(Amt.))%>%arrange(desc(amt_sold))%>%mutate(cumsum=cumsum(amt_sold),freq=round(amt_sold/sum(amt_sold),3),cum_freq=cumsum(freq))
  
  pval<-barplot(productgroup_value$amt_sold, 
                width = 1, space = 0.5, border = NA, axes = F, 
                ylim = c(0, 1.05 * max(productgroup_value$amt_sold, na.rm = T)), 
                ylab = "Counts",cex.names = 0.7, 
                # names.arg = productgroup_value$product_group,
                main = "Pareto Chart (value wise)")
  
  axis(side = 2, at = c(0, productgroup_value$product_group), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
  
  box(col = "grey62")
  
  px <- productgroup_value$cum_freq * max(productgroup_value$amt_sold, na.rm = T)
  pvalp<-lines(pval, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
  
  axis(side = 4, at = c(0, px), labels = paste(c(0, round(productgroup_value$cum_freq * 100)) ,"%",sep=""), 
       las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
  return(productgroup_value)
}

else if(index == 2){
  productgroup_volume<-x%>%select(product_group,`Order Qty`)%>%group_by(product_group)%>%summarise(qty_sold=sum(`Order Qty`))%>%arrange(desc(qty_sold))%>%mutate(cumsum=cumsum(qty_sold),freq=round(qty_sold/sum(qty_sold),3),cum_freq=cumsum(freq))
  
  pvol<-barplot(productgroup_volume$qty_sold, 
                width = 1, space = 0.5, border = NA, axes = F, 
                ylim = c(0, 1.05 * max(productgroup_volume$qty_sold, na.rm = T)), 
                ylab = "Counts",cex.names = 0.7, 
                # names.arg = productgroup_volume$product_group,
                main = "Pareto Chart (volume wise)")
  
  
  axis(side = 2, at = c(0, productgroup_volume$product_group), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
  
  box(col = "grey62")
  
  px <- productgroup_volume$cum_freq * max(productgroup_volume$qty_sold, na.rm = T)
  pvolp<-lines(pvol, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
  
  axis(side = 4, at = c(0, px), labels = paste(c(0, round(productgroup_volume$cum_freq * 100)) ,"%",sep=""), 
       las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
  
  return(productgroup_volume)
}
}

