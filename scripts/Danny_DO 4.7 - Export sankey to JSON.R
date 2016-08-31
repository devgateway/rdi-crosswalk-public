# Exporting pr_ALL DONORS file to json with names and nodes

library(XLConnect)
library(jsonlite)

sankeys <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/pr_ALL DONORS_edited_v2.xlsx"),sheet=1,header=TRUE)

sector <- data.frame(ag="A",health="H")
all_datas <- data.frame()
for(j in seq(ncol(sector)))
{
  sankeeyer <- sankeys[sankeys[,2]==sector[1,j],]
  o_un <- data.frame(unique(sankeeyer[,1]))
  sub_data <- data.frame()
  for(g in seq(nrow(o_un)))
  {
    san_sub <- sankeeyer[sankeeyer[,1]==o_un[g,1],]
    t_un <- data.frame(unique(san_sub[,7]))
    bumber <- data.frame()
    for(b in seq(nrow(t_un)))
    {
      bumber2 <- data.frame(source=o_un[g,1],target=t_un[b,1],value=nrow(san_sub[san_sub[,1]==o_un[g,1]&san_sub[,7]==t_un[b,1],]))
      bumber <- rbind(bumber,bumber2)
    }
    sub_data <- rbind(sub_data,bumber)
  }
  sub_data <- cbind(data.frame(sector=matrix(sector[1,j],nrow(sub_data),1)),sub_data)
  all_datas <- rbind(all_datas,sub_data)
}
all_datas[is.na(all_datas[,3]),3] <- "Not specified"

all_datas2 <- data.frame()
for(j in seq(ncol(sector)))
{
  sankeeyer <- sankeys[sankeys[,2]==sector[1,j],]
  o_un <- data.frame(unique(sankeeyer[,7]))
  sub_data <- data.frame()
  for(g in seq(nrow(o_un)))
  {
    san_sub <- sankeeyer[sankeeyer[,7]==o_un[g,1],]
    t_un <- data.frame(unique(san_sub[,8]))
    bumber <- data.frame()
    for(b in seq(nrow(t_un)))
    {
      bumber2 <- data.frame(source=o_un[g,1],target=t_un[b,1],value=nrow(san_sub[san_sub[,7]==o_un[g,1]&san_sub[,8]==t_un[b,1],]))
      bumber <- rbind(bumber,bumber2)
    }
    sub_data <- rbind(sub_data,bumber)
  }
  sub_data <- cbind(data.frame(sector=matrix(sector[1,j],nrow(sub_data),1)),sub_data)
  all_datas2 <- rbind(all_datas2,sub_data)
}
all_datas2 <- all_datas2[!is.na(all_datas2[,2])&!is.na(all_datas2[,3]),]

# write.csv(all_datas,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/flow_counts1.csv")
# write.csv(all_datas2,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/flow_counts2.csv")

h_sub1_san <- all_datas[all_datas[,1]=="H",]
h_sub2_san <- all_datas2[all_datas2[,1]=="H",]
ag_sub1_san <- all_datas[all_datas[,1]=="A",]
ag_sub2_san <- all_datas2[all_datas2[,1]=="A",]

h_sub2_san <- h_sub2_san[h_sub2_san[,2]!="Compiled in-house",]
ag_sub2_san <- ag_sub2_san[ag_sub2_san[,2]!="Compiled in-house",]

json_it <- function(x)
{
  all_paster <- paste()
  ender <- nrow(x)
  for(z in seq(nrow(x)))
  {
    if(z==ender)
    {
      part7 <- paste('\n{\n"source":','"',x[z,2],'",\n','"target":','"',x[z,3],'",\n','"value":',x[z,4],'\n}',sep="")
      all_paster <- paste(all_paster,part7)
    }
    else
    {
      part7 <- paste('\n{\n"source":','"',x[z,2],'",\n','"target":','"',x[z,3],'",\n','"value":',x[z,4],'\n},',sep="")
      all_paster <- paste(all_paster,part7)
    }
  }
  all_paster <- paste('{\n"links": [',all_paster,'\n],',sep="")
  new_paster <- paste()
  whatever <- data.frame(name=unique(rbind(data.frame(name=x[,2]),data.frame(name=x[,3]))))
  ender2 <- nrow(whatever)
  for(o in seq(nrow(whatever)))
  {
    if(o==ender2)
    {
      part20 <- paste('\n{\n"name":','"',whatever[o,1],'"\n}',sep="")
      new_paster <- paste(new_paster,part20)
    }
    else
    {
      part20 <- paste('\n{\n"name":','"',whatever[o,1],'"\n},',sep="")
      new_paster <- paste(new_paster,part20)
    }
  }
  new_paster <- paste('\n"nodes": [',new_paster,'\n]\n}',sep="")
  all_paster <- paste(all_paster,new_paster,sep="")
  return(all_paster)
}
json_h1 <- json_it(h_sub1_san)
json_h2 <- json_it(h_sub2_san)
json_a1 <- json_it(ag_sub1_san)
json_a2 <- json_it(ag_sub2_san)
writeLines(json_h1,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/san_H1_json11.txt",sep="\n")
writeLines(json_h2,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/san_H2_json11.txt",sep="\n")
writeLines(json_a1,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/san_A1_json11.txt",sep="\n")
writeLines(json_a2,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/san_A2_json11.txt",sep="\n")

# Used this tool to verify: http://jsonlint.com/
