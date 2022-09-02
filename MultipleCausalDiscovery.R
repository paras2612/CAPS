library("pcalg")

output <- read.csv("C:\\Users\\coole\\Downloads\\TimeWiseOutput.csv")
input <- read.csv("C:\\Users\\coole\\Downloads\\AllDataAvgOutput.csv")
input <- input[-c(39:40)]
filename<-paste(c("~/AllData/AllData.csv"), collapse = "")
for (i in (487:730)){
  #filename<-paste(c("~/CausalGraphs/CausalGraph", i,".csv"), collapse = "")
  filename2<-paste(c("~/AllData/AllDataForDay", i,".csv"), collapse = "")
  area <- c()
  wyld <- c()
  for (j in (680001:682715)){
    area <- c(area,output[output$HRU_NUM == j,]['AREAkm2'][i,])
    wyld <- c(wyld,output[output$HRU_NUM == j,]['WYLDmm'][i,])
  }
  input['WYLDmm'] <- wyld
  input['AREAkm2'] <- area
  scaled.md <- scale(input)
  scaled.mdnna <- scaled.md[, colSums(is.na(scaled.md)) == 0]
  suffStat <- list(C=cor(input), n =nrow(input))
  varName <- colnames(input)
  skel <- skeleton(suffStat,indepTest = gaussCItest,labels = varName,alpha = 1)
  pc <- pc(suffStat,indepTest=gaussCItest,labels = varName,alpha=1)
  abc<-unlist(showEdgeList(pc))
  total = length(abc)
  from<- colnames(input)[abc[1:(total/2)]]
  to<- colnames(input)[abc[(total/2)+1:(total/2)]]
  edgelist_df<-cbind(data.frame(from), data.frame(to))
  if(i==0)
  {
    write.table(input,filename,sep=",",row.names=FALSE,append = TRUE)
  }
  else
  {
    write.table(input,filename,sep=",",row.names=FALSE,append = TRUE,col.names = FALSE)
  }
  write.table(input,filename2,sep=",",row.names=FALSE)
  sprintf("Done for %d",i)
}
