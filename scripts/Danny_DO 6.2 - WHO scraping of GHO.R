# The web scrape

library(rvest)
boscoe <- data.frame()
papa_bear <- html_session("http://apps.who.int/gho/data/node.imr")
da_list <- data.frame(link=(papa_bear %>% html_nodes(".list_dash a") %>% html_text()))
small_list <- as.data.frame(da_list[1:100,])
for(j in seq(nrow(small_list)))
{
  index_1 <- papa_bear %>% follow_link(paste(da_list[j,]))
  index_2 <- index_1 %>% follow_link("Indicator definition")
  name <- paste(da_list[j,])
  group <- index_2 %>% html_nodes("#ctl00_labPageTitle") %>% html_text()
  if(length(group)==0){next}
  infos <- as.data.frame(index_2 %>% html_nodes(".indicatorViewLongStringDiv") %>% html_text())
  momma_bear <- data.frame(name=name,group=group,rations=infos[1,],defins=infos[2,])
  boscoe <- rbind(boscoe,momma_bear)
}

write.csv(BIG_disco, file="C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_DISCO_v1.csv",row.names=FALSE)
