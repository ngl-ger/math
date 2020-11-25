
#Read data as tibble 
climate <- 
  read_delim("C:/Users/jjohn/OneDrive/MScthesis/data/clim.csv",";",) %>%
  filter (date != 0)#deletes empty days (artifact from data mining)
climate$date <- as.Date(climate$date,"%d.%m.%Y") #formats the date correct 
  
#Condense data, mean it

summary <- climate %>%
  mutate(month = format(date,"%m"), year = format (date, "%y"))%>%
  group_by(id, month, year)%>%
  summarise(
    avg = mean(value)
  )
summary$id <- as.factor(summary$id)

summary$time <- 
  lubridate::ymd(paste0(summary$year,summary$month,"01") #reintroducing date format for ggplot2
                 
grp.clrs <-
  c(temp_min= "#1f64e6", temp_max= "#e81d17", temp_avg="#000000", rainfall="#51fff2", sunshine ="#fcff4b" )


a <- ggplot(aggro, aes(time,avg,group=id)) +
  geom_line() +
  facet_wrap(~id, scales ="free")+
  scale_fill_manual(values=grp.clrs)+
  #scale_color_manual(values = my_colour) +
  NULL
a
