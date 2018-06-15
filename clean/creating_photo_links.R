#creating photo links

share_links <- read.csv("share_links.csv")
direct_links <- read.csv("direct_links.csv")
pic_list <- read.csv("needed_pics.csv")


names(pic_list) <- c("pic_id", "d_link")
names(direct_links) <- c("pic_id", "d_link")
names(share_links) <- c("pic_id", "s_link")


library(tidyverse)
pic_list <- pic_list %>%
  mutate_all(as.character)

direct_links <- direct_links %>%
  mutate_all(as.character)

share_links <- share_links %>%
  mutate_all(as.character)


#select <- seq(from = 1, to = (length(pic_list$pic_id)*2), by = 2)
#pic_list$pic_id <- unlist(strsplit(x = as.character(pic_list$pic_id), split = ".", fixed = T))[select]


select <- seq(from = 1, to = (length(direct_links$pic_id)*2), by = 2)
direct_links$pic_id <- unlist(strsplit(x = as.character(direct_links$pic_id), split = ".", fixed = T))[select]

select <- seq(from = 1, to = (length(share_links$pic_id)*2), by = 2)
share_links$pic_id <- unlist(strsplit(x = as.character(share_links$pic_id), split = ".", fixed = T))[select]


pic_list$pic_id
share_links$pic_id
direct_links$pic_id


pics_need_dlink <- pic_list[43:123,]


dlink_row <- match(x = pics_need_dlink$pic_id, table = direct_links$pic_id)

pics_need_dlink$d_link_new <- direct_links$d_link[dlink_row]

pics_need_dlink <- select(pics_need_dlink, pic_id, d_link_new)
names(pics_need_dlink) <- c("pic_id", "d_link")


pics_with_dlink <- rbind(pic_list[1:41,], pics_need_dlink)


pic_list$pic_id %in% share_links$pic_id

s_link_row <- match(pics_with_dlink$pic_id, table = share_links$pic_id)
pics_with_dlink$s_link_new <- share_links$s_link[s_link_row]


write.csv(pics_with_dlink, "links_vmillion.csv")



merge(pics_need_dlink, direct_links, by = "pic_id")


write.csv(pic_list_have_dlink, "have_dlink.csv")





direct_links$pic_id <- direct_link_clean_names


matching <- match(direct_links$pic_id, pic_list_needed$pic)


pic_list_needed$ddownlink1 <- direct_links$d_link[matching]

pic_list_needed


#cleaning names of d_link



direct_link_clean_names <- unlist(strsplit(x = as.character(direct_links$pic_id), split = ".", fixed = T))[select]

pic_list$pic_id %in% direct_link_clean_names

ids <- read.csv("ids.csv")
names(ids) <- c("pic_id")
ids$pic_id <- as.character(ids$pic_id)
ids$pic_id[is.na(ids$pic_id)] <- "chill"



ids$dlink <- all_links_new$d_link.x[match(ids$pic_id, all_links_new$pic_id)]


#pick up here
latest_df <- merge(pic_list, tester, by = "pic_id")

select.x <- seq(from = 1, to = (length(share_links$pic_id)*2), by = 2)
share_link_clean_names <- unlist(strsplit(x = as.character(share_links$pic_id), split = ".", fixed = T))[select.x]
names(share_links) <- c("old_id", "s_link")
share_links$pic_id <- share_link_clean_names



all_links_new <- merge(latest_df, share_links, by = "pic_id")

all_links_final <- merge(ids,all_links_new, by = "pic_id", all.x = T, sort = FALSE, incomparables = F)


all_links_final <- 
  
rows <- match(x =  ids$pic_id, table = all_links_new$pic_id)


write.csv(cbind(ids, all_links_new[rows,]), "links_Test.csv")

write.csv(all_links_final, "final_links.csv")



?merge


#rename the 


tester <- cbind(direct_link_clean_names, direct_links)
str(tester)

names(tester) <- c("pic_id", "old_id", "d_link")


tail(direct_links$pic_id)
tail(direct_link_clean_names)

?strsplit


i= y%%2==0



