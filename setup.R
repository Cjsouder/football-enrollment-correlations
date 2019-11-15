library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)

#

big_10 <- c("http://web1.ncaa.org/mfb/2000/Internet/conf%20stats/2000000000827T.HTML",
            "http://web1.ncaa.org/mfb/2001/Internet/conf%20stats/2001000000827T.HTML",
            "http://web1.ncaa.org/mfb/2002/Internet/conf%20stats/2002000000827T.HTML",
            "http://web1.ncaa.org/mfb/2003/Internet/conf%20stats/2003000000827T.HTML",
            "http://web1.ncaa.org/mfb/2004/Internet/conf%20stats/2004000000827T.HTML",
            "http://web1.ncaa.org/mfb/2005/Internet/conf%20stats/2005000000827T.HTML",
            "http://web1.ncaa.org/mfb/2006/Internet/conf%20stats/2006000000827T.HTML",
            "http://web1.ncaa.org/mfb/2007/Internet/conf%20stats/2007000000827T.HTML",
            "http://web1.ncaa.org/mfb/2008/Internet/conf%20stats/2008000000827T.HTML",
            "http://web1.ncaa.org/mfb/2009/Internet/conf%20stats/2009000000827T.HTML",
            "http://web1.ncaa.org/mfb/2010/Internet/conf%20stats/2010000000827T.HTML",
            "http://web1.ncaa.org/mfb/2011/Internet/conf%20stats/2011000000827T.HTML",
            "http://web1.ncaa.org/mfb/2012/Internet/conf%20stats/2012000000827T.HTML"
)

big_12 <- c("http://web1.ncaa.org/mfb/2000/Internet/conf%20stats/2000000025354T.HTML",
            "http://web1.ncaa.org/mfb/2001/Internet/conf%20stats/2001000025354T.HTML",
            "http://web1.ncaa.org/mfb/2002/Internet/conf%20stats/2002000025354T.HTML",
            "http://web1.ncaa.org/mfb/2003/Internet/conf%20stats/2003000025354T.HTML",
            "http://web1.ncaa.org/mfb/2004/Internet/conf%20stats/2004000025354T.HTML",
            "http://web1.ncaa.org/mfb/2005/Internet/conf%20stats/2005000025354T.HTML",
            "http://web1.ncaa.org/mfb/2006/Internet/conf%20stats/2006000025354T.HTML",
            "http://web1.ncaa.org/mfb/2007/Internet/conf%20stats/2007000025354T.HTML",
            "http://web1.ncaa.org/mfb/2008/Internet/conf%20stats/2008000025354T.HTML",
            "http://web1.ncaa.org/mfb/2009/Internet/conf%20stats/2009000025354T.HTML",
            "http://web1.ncaa.org/mfb/2010/Internet/conf%20stats/2010000025354T.HTML",
            "http://web1.ncaa.org/mfb/2011/Internet/conf%20stats/2011000025354T.HTML",
            "http://web1.ncaa.org/mfb/2012/Internet/conf%20stats/2012000025354T.HTML"
)

acc <- c("http://web1.ncaa.org/mfb/2000/Internet/conf%20stats/2000000000821T.HTML",
         "http://web1.ncaa.org/mfb/2001/Internet/conf%20stats/2001000000821T.HTML",
         "http://web1.ncaa.org/mfb/2002/Internet/conf%20stats/2002000000821T.HTML",
         "http://web1.ncaa.org/mfb/2003/Internet/conf%20stats/2003000000821T.HTML",
         "http://web1.ncaa.org/mfb/2004/Internet/conf%20stats/2004000000821T.HTML",
         "http://web1.ncaa.org/mfb/2005/Internet/conf%20stats/2005000000821T.HTML",
         "http://web1.ncaa.org/mfb/2006/Internet/conf%20stats/2006000000821T.HTML",
         "http://web1.ncaa.org/mfb/2007/Internet/conf%20stats/2007000000821T.HTML",
         "http://web1.ncaa.org/mfb/2008/Internet/conf%20stats/2008000000821T.HTML",
         "http://web1.ncaa.org/mfb/2009/Internet/conf%20stats/2009000000821T.HTML",
         "http://web1.ncaa.org/mfb/2010/Internet/conf%20stats/2010000000821T.HTML",
         "http://web1.ncaa.org/mfb/2011/Internet/conf%20stats/2011000000821T.HTML",
         "http://web1.ncaa.org/mfb/2012/Internet/conf%20stats/2012000000821T.HTML"
)

pac_12 <-c("http://web1.ncaa.org/mfb/2000/Internet/conf%20stats/2000000000905T.HTML",
           "http://web1.ncaa.org/mfb/2001/Internet/conf%20stats/2001000000905T.HTML",
           "http://web1.ncaa.org/mfb/2002/Internet/conf%20stats/2002000000905T.HTML",
           "http://web1.ncaa.org/mfb/2003/Internet/conf%20stats/2003000000905T.HTML",
           "http://web1.ncaa.org/mfb/2004/Internet/conf%20stats/2004000000905T.HTML",
           "http://web1.ncaa.org/mfb/2005/Internet/conf%20stats/2005000000905T.HTML",
           "http://web1.ncaa.org/mfb/2006/Internet/conf%20stats/2006000000905T.HTML",
           "http://web1.ncaa.org/mfb/2007/Internet/conf%20stats/2007000000905T.HTML",
           "http://web1.ncaa.org/mfb/2008/Internet/conf%20stats/2008000000905T.HTML",
           "http://web1.ncaa.org/mfb/2009/Internet/conf%20stats/2009000000905T.HTML",
           "http://web1.ncaa.org/mfb/2010/Internet/conf%20stats/2010000000905T.HTML",
           "http://web1.ncaa.org/mfb/2011/Internet/conf%20stats/2011000000905T.HTML",
           "http://web1.ncaa.org/mfb/2012/Internet/conf%20stats/2012000000905T.HTML"
)

sec <- c("http://web1.ncaa.org/mfb/2000/Internet/conf%20stats/2000000000911T.HTML",
         "http://web1.ncaa.org/mfb/2001/Internet/conf%20stats/2001000000911T.HTML",
         "http://web1.ncaa.org/mfb/2002/Internet/conf%20stats/2002000000911T.HTML",
         "http://web1.ncaa.org/mfb/2003/Internet/conf%20stats/2003000000911T.HTML",
         "http://web1.ncaa.org/mfb/2004/Internet/conf%20stats/2004000000911T.HTML",
         "http://web1.ncaa.org/mfb/2005/Internet/conf%20stats/2005000000911T.HTML",
         "http://web1.ncaa.org/mfb/2006/Internet/conf%20stats/2006000000911T.HTML",
         "http://web1.ncaa.org/mfb/2007/Internet/conf%20stats/2007000000911T.HTML",
         "http://web1.ncaa.org/mfb/2008/Internet/conf%20stats/2008000000911T.HTML",
         "http://web1.ncaa.org/mfb/2009/Internet/conf%20stats/2009000000911T.HTML",
         "http://web1.ncaa.org/mfb/2010/Internet/conf%20stats/2010000000911T.HTML",
         "http://web1.ncaa.org/mfb/2011/Internet/conf%20stats/2011000000911T.HTML",
         "http://web1.ncaa.org/mfb/2012/Internet/conf%20stats/2012000000911T.HTML"
)

#

season <- c("_2000", "_2001", "_2002", "_2003", "_2004", "_2005", "_2006", "_2007", "_2008", "_2009", "_2010", "_2011", "_2012")

#

table_maker <- function(power5){
  
#
  
  dfList <- lapply(power5, function(i){
    read_html(i) %>%
      html_nodes(".statstable") %>%
      extract2(1) %>%
      html_table() %>%
      select("Name", "Win", "Loss") 
    })
  
#
  
  df_final <-dfList[[1]]
  
#  
  
  for(i in head(seq_along(dfList), -1)){
    df_final <- merge(df_final, dfList[[i+1]], all = TRUE, suffixes = season[i:(i+1)], by = "Name")
  }
  
#  
  
  df_final
  
}

# Create a dataset for each of the 5 major football conferences via the
# table_maker function. Add a column to each table indicating the conference
# name

big_10_table <- table_maker(big_10) %>%
  mutate(Conference = "Big 10")
big_12_table <- table_maker(big_12) %>%
  mutate(Conference = "Big 12")
acc_table    <- table_maker(acc) %>%
  mutate(Conference = "ACC")
pac_12_table <- table_maker(pac_12) %>%
  mutate(Conference = "Pac 12")
sec_table    <- table_maker(sec) %>%
  mutate(Conference = "SEC")

# The SEC data lists Mississippi under two names, "Ole Miss" and "Mississippi",
# so I combine the results for both listings under the name "Mississippi"

sec_table$Name <- replace(sec_table$Name, sec_table$Name=="Ole Miss", "Mississippi")

# Merge results for "Ole Miss" and "Mississippi". In the previous step I renamed
# the entry for "Ole Miss" to "Mississippi", but the results for this college
# remain spread out over two rows. I group by college, then duplicate results
# such that the blank spaces in each row for Mississippi are filled in. I keep
# one of the resulting entries and delete the duplicate.

sec_table <- sec_table %>%
  group_by(Name) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)

# Here I combine the data for all Power 5 conferences into one table, removing
# instances in which data is missing. I do this to exclude teams that changed
# conferences over the time period of interest.

football_data <- bind_rows(big_10_table, sec_table, big_12_table, pac_12_table, acc_table) %>%
  filter(!is.na(Win_2000) & !is.na(Win)) 

# This code fixes an error where for some reason data columns for 2012 don't
# have a suffix in their title

colnames(football_data)[colnames(football_data)=="Win"] <- "Win_2012"
colnames(football_data)[colnames(football_data)=="Loss"] <- "Loss_2012"

#

for(i in 2000:2012) {
  football_data[[paste0("win_pct_", i)]] <-
    (football_data[[paste0("Win_", i)]]) / (football_data[[paste0("Win_", i)]] + football_data[[paste0("Loss_", i)]])
}

# I use the clean_names() function to make the titles in both datasets easier to
# read and reference.

academic_data <- read.csv("university_data_clean.csv") %>%
  clean_names()
football_data <- football_data %>%
  clean_names()

# This chunk of code changes all university names in the football_data table
# such that "St." now reads "State". I do this so that the football_data and
# academic_data tables reference universities by a common name, allowing me to
# merge the tables in a later step.

football_data <- football_data %>%
  mutate(name = gsub("St\\.", "State", name)) 

#

academic_data <- academic_data %>%
  select(-x) %>%
  mutate(instnm = gsub("The ", "", instnm)) %>%
  mutate(instnm = gsub("University of California-Los Angeles", "UCLA", instnm)) %>%
  mutate(instnm = gsub("-Main Campus", "", instnm)) %>%
  mutate(instnm = gsub("Institute of Technology", "Tech", instnm)) %>%
  mutate(instnm = gsub(" University", "", instnm)) %>%
  mutate(instnm = gsub("University of ", "", instnm)) %>%
  mutate(instnm = gsub("-.*", "", instnm)) %>%
  mutate(instnm = gsub(" at.*", "", instnm)) %>%
  mutate(instnm = gsub("Pennsylvania", "Penn", instnm)) %>%
  mutate(instnm = gsub("Louisiana State and Agricultural & Mechanical College", "LSU", instnm)) %>%
  mutate(pubprime_ic2017 = gsub("-2", "Private", pubprime_ic2017)) %>%
  mutate(pubprime_ic2017 = gsub("2", "Public", pubprime_ic2017))

#

names(academic_data) <- gsub("_rv", "", names(academic_data))

# I merge academic_data and football_data into a new table, merged_data, with
# results joined by institution name

merged_data <- left_join(academic_data, football_data, by=c("instnm" = "name"))

#

for(i in 2002:2012){
  merged_data[[paste0("applcn_diff_",i+1)]] <- merged_data[[paste0("applcn_ic", i+1)]] - merged_data[[paste0("applcn_ic", i)]]
  merged_data[[paste0("applcn_pct_chng_",i+1)]] <- (merged_data[[paste0("applcn_ic", i+1)]] - merged_data[[paste0("applcn_ic", i)]])/merged_data[[paste0("applcn_ic", i)]]
}

#

for(i in 2001:2011){
  merged_data[[paste0("win_diff_",i+1)]] <- merged_data[[paste0("win_", i+1)]] - merged_data[[paste0("win_", i)]] 
  merged_data[[paste0("loss_diff_",i+1)]] <- merged_data[[paste0("loss_", i+1)]] - merged_data[[paste0("loss_", i)]]
  merged_data[[paste0("win_pct_diff_",i+1)]] <- merged_data[[paste0("win_pct_", i+1)]] - merged_data[[paste0("win_pct_", i)]]
}

#

merged_data <- merged_data %>%
  select(
    instnm,
    pubprime_ic2017,
    conference,
    starts_with("win_pct_diff"),
    starts_with("applcn_pct_chng_")
  ) %>%
  gather(
    key = "year",
    value = "win_pct_diff",-instnm,-pubprime_ic2017,-conference,-starts_with("applcn_pct_chng_")
  ) %>%  
  mutate(year = as.integer(gsub("win_pct_diff_", "", year))) %>%
  gather(
    key = "year2",
    value = "applcn_pct_chng", -instnm,-pubprime_ic2017,-conference,-year,-win_pct_diff
  ) %>% 
  mutate(year2 = as.integer(gsub("applcn_pct_chng_", "", year2))) %>%
  filter(year2 == year + 1) %>%
  rename("pub_status" = pubprime_ic2017) %>%
  mutate(win_pct_diff = round(win_pct_diff * 100, digits = 1), 
         applcn_pct_chng = round(applcn_pct_chng * 100, digits = 1)) %>%
  write.csv(file = "./Shiny-Football-Enrollment/football_academic_data.csv")



