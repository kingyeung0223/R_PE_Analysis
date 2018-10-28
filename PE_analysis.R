#check and set directory
if(getwd() != "C:/Users/KingYiu/Desktop/King/University/Year3/RMBI4000D/stock") {
  setwd("C:/Users/KingYiu/Desktop/King/University/Year3/RMBI4000D/stock")
}

#import data
original = read.csv("stock_list.csv", header = T)
current_pe = read.csv("current_pe.csv", header = T)

#store stock name
stock_list = names(original)

for (i in 1:ncol(original)) {
  
  #Retrieving non-NA PE of a stock
  target_range = original[,i]
  select_bool = !is.na(target_range)
  current_stock = target_range[select_bool]
  
  #plotting the graph
  graph = hist(current_stock, nclass = 14)
  curve_x = graph$mids
  curve_y = graph$counts
  
  #tell compiler that we are going to generate a jpeg file
  jpeg(quality = 100, filename = paste(substr(stock_list[i], start = 1, stop = 5), ".jpg", sep = ""))
  #plot histo
  plot(graph, xlab = "PE ratio", main = paste("PE distribution of", stock_list[i]))
  
  #plot PE curve
  lines(spline(curve_x,curve_y, method = 'n', n = 300), col = 'red', lwd = 3)
  
  #plot current PE
  abline(v = current_pe[i], col = 'blue', lwd = 3)
  
  dev.off()
}
