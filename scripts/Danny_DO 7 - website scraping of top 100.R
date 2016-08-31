# TOP 100 health indicators
# Compiling a metadata spreadsheet

library(tm)
whatever <- readPDF(engine=)


library(rvest)
topper <- data.frame()
big_papa <- html_session("http://www.who.int/healthinfo/indicators/2015/metadata/en/")
lister <- data.frame(link=(big_papa %>% html_nodes("#primary a") %>% html_text()))
for(i in seq(nrow(lister)))
{
  indexer <- big_papa %>% follow_link(paste(lister[1,]))
  name <- as.character(lister[1,])
  domain <- indexer %>% html_nodes("div:nth-child(8)") %>% html_text()
  tags <- ...
  definition <- ...
  disag <- ...
  measure <- ...
  estimate <- ...
  cycle <- ...
  descrip <- paste((indexer %>% html_nodes(".field-label-inline p") %>% html_text()),collapse=", ")
  type <- indexer %>% html_nodes(".field-name-field-data-type .even") %>% html_text()
  collect <- paste((indexer %>% html_nodes("#node_form_full_group_data_collection .field-item") %>% html_text()),collapse=", ")
  freq <- indexer %>% html_nodes(".field-name-field-measurement-frequency .even") %>% html_text()
  level <- indexer %>% html_nodes(".field-name-field-indicator-level .even") %>% html_text()
  geo_level <- paste((indexer %>% html_nodes(".field-name-field-geo-location .even") %>% html_text()),collapse=", ")
  org <- indexer %>% html_nodes(".field-name-field-agency-source .even") %>% html_text()
  mic_mac <- indexer %>% html_nodes(".field-name-field-goal .even") %>% html_text()
  if(length(id)==0){id <- NA}
  if(length(descrip)==0){descrip <- NA}
  if(length(type)==0){type <- NA}
  if(length(collect)==0){collect <- NA}
  if(length(freq)==0){freq <- NA}
  if(length(level)==0){level <- NA}
  if(length(geo_level)==0){geo_level <- NA}
  if(length(org)==0){org <- NA}
  if(length(mic_mac)==0){mic_mac <- NA}
  mcguin <- data.frame(id=id,label=lister[i,],descrip=descrip,type=type,collect=collect,freq=freq,level=level,geo_level=geo_level,org=org,mic_mac=mic_mac)
  papa_disco <- rbind(papa_disco,mcguin)
}

BIG_disco <- rbind(papa_disco, papa_disco2)
write.csv(BIG_disco, file="C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_DISCO_v1.csv",row.names=FALSE)
