# Andrew Messamore
# 11.13.19

library("dplyr")
library("writexl")
#Name Matching


rm(list=ls())

df <- read.csv("./extracted_df.csv")

class(df$author_fullname)

df$author_fullname <- as.character(df$author_fullname)

df_namematch <- df %>% select(X, author_fullname)
df_namematch <- df_namematch %>% subset(author_fullname != "")
df_namematch$bins <- sapply(df_namematch$author_fullname, function(n)
  paste(as.integer(agrepl(n, df_namematch$author_fullname, max.distance = .1)), collapse=""))
df_namematch$unique_id <- as.integer(as.factor(df_namematch$bins))
df_namematch$bins <- NULL
df_namematch$author_fullname <-NULL

df <- left_join(df, df_namematch, by = "X")
write.csv(df, "df_uniqueids.csv")

df_orgmatch <- df %>% select(X, affiliate)
df_orgmatch <- df_orgmatch %>% subset(affiliate != "")
df_orgmatch$bins <- sapply(df_orgmatch$affiliate, function(n)
  paste(as.integer(agrepl(n, df_orgmatch$affiliate, max.distance = .1)), collapse=""))
df_orgmatch$unique_id_org <- as.integer(as.factor(df_orgmatch$bins))
df_orgmatch$bins <- NULL
df_orgmatch$affiliate <-NULL

df <- left_join(df, df_orgmatch, by = "X")

df_2 <- df 

df_2 <- df_2 %>% group_by(unique_id) %>% mutate(person_sum = sum(n())) %>% ungroup()
df_2 <- df_2 %>% group_by(unique_id_org) %>% mutate(org_sum = sum(n())) %>% ungroup()

top_people <- df_2  %>% select(author_fullname, unique_id, person_sum)
top_people <- top_people[!duplicated(top_people$unique_id),]
top_people <- top_people %>%  arrange(desc(person_sum))
top_orgs <- df_2  %>% select(affiliate, unique_id_org, org_sum)
top_orgs <- top_orgs[!duplicated(top_orgs$unique_id_org),]
top_orgs <- top_orgs %>%  arrange(desc(org_sum))

write_xlsx(top_people, "top_people.xlsx")
write_xlsx(top_orgs, "top_orgs.xlsx")

                                      
