library(tidyverse)
library(stringr)
library(data.table)
library(taxize)
library(reshape2)
library(ggplot2)

aphids<-readLines("./RockyMountainAphids/AphidsRMR.txt") %>%
  str_trim() %>%
  data.frame(fullstring=.,stringsAsFactors=F) %>%
  # slice(1:50) %>%  # cut this after unit testing
  mutate(host=str_trim(str_extract(fullstring,
                                   pattern="^[A-z]+\\s[a-z-\\.]+\\s*[a-z]*")),
         host_common=gsub("[()]","",str_extract(fullstring,pattern="\\((.*?)\\)")),
         aphids=gsub("^\\)\\s","",str_extract(fullstring,"\\)\\s*.*"))) %>%
  select(-fullstring) %>%
  full_join(tnrs(.$host,source="iPlant_TNRS",splitby=100) %>%
              select(submittedname,acceptedname),by=c("host"="submittedname")) %>%
  rename(host_tnrs = acceptedname) %>%
  cbind(tstrsplit(.$aphids,'(?<=.)(?=[[:upper:]])',perl=TRUE,fill=NA,names=TRUE)) %>%
  select(-aphids) %>%
  melt(id.vars=c('host','host_common','host_tnrs'),value.name='aphid') %>%
  select(-variable) %>%
  arrange(host) %>%
  na.omit()

# manually correct annomylously non-matched plant names from iPlant TNRS web service
aphids$host_tnrs[aphids$host=="Oenothera caespitosa"]<-"Oenothera caespitosa"
aphids$host_tnrs[aphids$host=="Pistacia terebinthus"]<-"Pistacia terebinthus"
aphids$host_tnrs[aphids$host=="Betula alba"]<-"Betula pubescens"
aphids$host_tnrs[aphids$host=="Gilia linearis"]<-"Collomia linearis"
aphids$host_tnrs[aphids$host=="Quamasia hyacinthia"]<-"Camassia scilloides"
aphids$host_tnrs[aphids$host=="Salix elegantissima"]<-"Salix ×pendulina"
aphids$host_tnrs[aphids$host=="Ligusticum porteri"]<-"Ligusticum porteri"

# number of plant 'species'
length(unique(aphids$host_tnrs))
# number of plant genera
length(unique(stringr::word(aphids$host_tnrs,1L,1L)))

## Clean aphid names
aphids_cleaned<-aphids %>%
  mutate(aphid=str_trim(aphid)) %>%
  full_join(gnr_resolve(.$aphid,canonical=T,best_match_only=T) %>%
              select(submitted_name,matched_name2),by=c("aphid"="submitted_name")) %>%
  rename(aphid_tnrs=matched_name2)

# number of aphid species
length(unique(aphids_cleaned$aphid_tnrs))
# number of plant genera
length(unique(stringr::word(aphids_cleaned$aphid_tnrs,1L,1L)))

## Prepare fully cleaned dataset and export
final_cleaned<-aphids_cleaned %>%
  select(host_tnrs,host_common,aphid_tnrs) %>%
  rename(host=host_tnrs,aphid=aphid_tnrs)

write.csv(final_cleaned,"./RockyMountainAphids/Palmer1952hostlist.csv",
          row.names=F)
