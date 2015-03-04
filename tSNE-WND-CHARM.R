#require(gdata)
#df = read.xls ("/Users/stealth/GitHub/ShinyFeaturesWNDCHARM/TestWorkbook.xlsx", sheet = 1, header = TRUE)

require(XLConnect)
wb = loadWorkbook("/Users/stealth/GitHub/ShinyFeaturesWNDCHARM/crop_allSigFiles_Mm01.xls")
df = readWorksheet(wb, sheet = "Hoja1", header = TRUE)

colors = rainbow(length(unique(df$label)))
names(colors) = unique(df$label)

ecb = function(x,y){ 
  plot(x,t='n'); 
  text(x,labels=df$label, col=colors[df$label]) 
}
tsne_iris = tsne(df[,2:length(df[1,])], epoch_callback = ecb, perplexity=50)

