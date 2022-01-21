library(ggplot2)
# library(ggradar)
library(fmsb)
library(lubridate)

df = read.csv('data/AccidentesLatLong.csv')

df <- read.csv("data/AccidentesBicicletas.csv") %>% mutate(fecha = as.Date(fecha, "%d/%m/%Y"), acc_id = 1, fecha = as.Date(fecha, "%d/%m/%Y"), month = months(fecha), year =  as.character(year(fecha))) %>% 
  group_by(year, month) %>% summarise(accident_num = sum(acc_id, na.rm = TRUE)) %>% ungroup() %>%  spread(month, accident_num) %>% as.data.frame()

df <- mutate_all(df, ~replace(., is.na(.), 0))

max_accidents <- df %>% gather(month, acc_n, -year) %>% select(acc_n) %>% filter(acc_n == max(acc_n)) %>% pull()

rownames(df) <- select(df, year) %>% pull()

df_2 <- df %>% select(-year)

df_2 <- rbind(rep(max_accidents,12) , rep(0,5) , df_2)

radarchart(df_2)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart( df_2  , axistype=1 , 
            #custom polygon
            pcol=colors_border , 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

legend(x=1.3, y=1.2, legend = rownames(df_2[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

# df <- mutate_all(df, ~replace(., is.na(.), 0))
# 
# rownames(df) <- select(df, year) %>% pull()
# 
# df_2 <- df %>% select(-year)
# 
# df_2 <- rbind(rep(150,12) , rep(0,5) , df_2)
# 
# radarchart(df_2)
# 
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# 
# legend(x=0.7, y=1, legend = rownames(df_2[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
# 

# df$fecha <- as.Date(df$fecha,'%d/%m/%Y')
# df$mes <- format(df$fecha,"%m")
# df <- df %>% 
#   filter(format(df$fecha, "%Y") == "2019")%>%
#   group_by(mes) %>%
#   tally()
# months = c('Jan','Feb',"Mar",'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
# 
# new.df <- as.data.frame(t(as.matrix(df)))[c('n'),]
# colnames(new.df) <- months
# 
# maxval <- as.numeric(max(as.matrix(new.df[1,])))
# max_min <- rbind(rep(maxval,12),rep(0,12))
# row.names(max_min) <- c("max","min")
# colnames(max_min) <- months
# new.df <- rbind(max_min, new.df)
#  
# radarchart(new.df)
