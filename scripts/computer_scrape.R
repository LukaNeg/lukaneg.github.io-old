### Attempt at code to loop through computer and extract all file info

dir()


setwd("~/Documents/Awning")
setwd("~/Documents")

#setwd("../")
getwd()


library(tidyverse)
library(lubridate)

setwd("~/Documents")

setwd("~/Documents/Research/Georgia Island Study")
all_dir <- list.dirs()
file_data <- NULL
for(d in all_dir){
  files <- list.files(path=d)
  for(f in files){
    file_data <- rbind(file_data,as.data.frame(file.info(paste(d,f, sep="/"))))
  }
}


file_data$path <- row.names(file_data)
file_data <- as.tibble(file_data)

file_dat_clean <- mutate(file_data,
                         name = sub('.*/', '', path)) %>%
  select(size, isdir, mtime, ctime, atime, path, name) %>%
  arrange(mtime) %>%
  mutate(index = cumsum(size>=0),
         date_create = as.Date(ctime),
         date_mod = as.Date(mtime),
         ext = sub('^[^.]*', '', name))

file_dat_mod <- filter(file_dat_clean, date_create < as.Date("2017-01-01")) %>%
  mutate(kb = size/1024) %>%
  group_by(isdir) %>%
  summarize(total = length(size>0))

#write_csv(file_dat_clean, "private_data/scraped_files_info.csv")

file_dat_R <- filter(file_dat_clean, ext==".R") %>%
  mutate(index = cumsum(size>=0))

plot(index~date_mod, data=file_dat_R, type="l")
abline(v=as.Date("2018-05-13"))
abline(v=as.Date("2019-04-01"))
plot(cumsum(size)/1012~date_mod, data=file_dat_R, type="l", ylab="Total Mb worth of R scripts")

file_dat_doc <- filter(file_dat_clean, ext==".doc" | ext==".docx") %>%
  mutate(index = cumsum(size>=0))
plot(index~date_mod, data=file_dat_doc, type="l")
abline(v=as.Date("2018-05-13"))
abline(v=as.Date("2019-04-01"))
abline(v=as.Date("2011-06-06"))
plot(cumsum(size)/1012~date_mod, data=file_dat_doc, type="l", ylab="Total Mb worth of R scripts")

# 1000 characters ~ 1kb, so one character ~ 1 byte
# average word in english is 4.7 characters, but lets say 10 since this is scientific haha
# and because of other things that increase document size.
# this doesn't work though because all files contain other images and stuff 
# that increases file size
file_dat_doc <- mutate(file_dat_doc,
                       kb= size/1024,
                       characters = size,
                       words = size/10
                       )





