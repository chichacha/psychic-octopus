
## 16 Personalities

library(rvest)  ## to get data from website
library(stringr)
library(tidyverse)
library(circlize)
library(hrbrthemes)
library(ggthemes)
library(magick)

## Get 16 Different Personalities from 16 Personalities Website
personalities <-read_html("https://www.16personalities.com/personality-types")
per.type <- personalities %>% html_nodes(".type-item h4") %>% html_text() %>% gsub(pattern="(“|”)",replacement="")
per.type.code <- personalities %>% html_nodes("h5") %>% html_text() 
per.type.snippet <- personalities %>% html_nodes(".type-snippet") %>% html_text()
per.type.img <- personalities %>% html_nodes(".type-item img") %>% html_attr("src")

per.df <- tibble(
  type = per.type,
  type.code = per.type.code,
  short.descr = per.type.snippet,
  img = per.type.img
)

per.df <- per.df %>% mutate(type.code=substr(type.code,1,4), mind=substr(type.code,1,1),
                            energy = substr(type.code,2,2), nature = substr(type.code,3,3),
                            tactic = substr(type.code,4,4))

## There are 4 "Roles" based on followig rules 
per.df <- per.df %>% mutate(roles = case_when(
  str_detect(.$type.code,".NT.") ~ "Analyst",
  str_detect(.$type.code,".NF.") ~ "Diplomat",
  str_detect(.$type.code,".S.J") ~ "Sentinels",
  str_detect(.$type.code,".S.P") ~ "Explorers"
))

### Personality Distribution
per.dist <- read_html("https://personalitymax.com/personality-types/population-gender/") %>% html_table()
per.dist[[1]][1,]
per.dist.df <-bind_rows(per.dist[[1]][2:5,],per.dist[[2]][2:5,],per.dist[[3]][2:5,],per.dist[[4]][2:5,])

names(per.dist.df) <- c("personality.type","overall","male","female")

## Below function will extract number and convert to numeric object
numextract <- function(x){as.numeric(str_extract(x,"\\d*\\.*\\d*"))}

per.dist.df <- per.dist.df %>% mutate_at(vars(overall:female),numextract) %>% mutate(type.code=substr(personality.type,1,4))

per.df.comb <- per.df %>% inner_join(per.dist.df, by="type.code")



#####  Now Get to Fun Stuff!!!



# col.pal[1:16]
# 
# circos.clear()
# 
# 
# layout(matrix(1:9, 1,3))
#par(mfrow=c(1,1))
# 
# circos.par(start.degree=90,cell.padding=c(0,0,0,0),track.height=0.5)
# circos.initialize(factors = per.df.comb$type.code, xlim=c(0,1), 
#                   sector.width= per.df.comb$overall/100)
# circos.track(ylim=c(0,1), 
#              panel.fun = function(x,y){
#                circos.rect(xleft = CELL_META$xlim[1], ybottom=0, 
#                            xright=CELL_META$xlim[2], ytop=1, 
#                            col=col.pal[CELL_META$sector.numeric.index])
#                circos.text(x=CELL_META$xcenter, y=CELL_META$ycenter, 
#                            labels=per.df.comb$type[CELL_META$sector.numeric.index], 
#                            facing = "clockwise",
#                            niceFacing = TRUE, font=2)
# })
# circos.clear()
# 
# 
# circos.par(start.degree=90,cell.padding=c(0,0,0,0),track.height=0.5)
# circos.initialize(factors = per.df.comb$type.code, xlim=c(0,1), 
#                   sector.width= per.df.comb$male/100)
# circos.track(ylim=c(0,1), 
#              panel.fun = function(x,y){
#                circos.rect(xleft = CELL_META$xlim[1], ybottom=0, 
#                            xright=CELL_META$xlim[2], ytop=1, 
#                            col=col.pal[CELL_META$sector.numeric.index])
#                circos.text(x=CELL_META$xcenter, y=CELL_META$ycenter, 
#                            labels=per.df.comb$type[CELL_META$sector.numeric.index], 
#                            facing = "clockwise",
#                            niceFacing = TRUE, font=2)
#              })
# circos.clear()
# 
# 
# circos.par(start.degree=90,cell.padding=c(0,0,0,0),track.height=0.5)
# circos.initialize(factors = per.df.comb$type.code, xlim=c(0,1), 
#                   sector.width= per.df.comb$female/100)
# circos.track(ylim=c(0,1), 
#              panel.fun = function(x,y){
#                circos.rect(xleft = CELL_META$xlim[1], ybottom=0, 
#                            xright=CELL_META$xlim[2], ytop=1, 
#                            col=col.pal[CELL_META$sector.numeric.index])
#                circos.text(x=CELL_META$xcenter, y=CELL_META$ycenter, 
#                            labels=per.df.comb$type[CELL_META$sector.numeric.index], 
#                            facing = "clockwise",
#                            niceFacing = TRUE, font=2)
#              })
# circos.clear()

## ggplot
per.df.comb %>% ggplot(aes(color=fct_inorder(type),fill=mind)) + 
  geom_bar(stat="identity", aes(x=1, y=overall), position="fill") +
  geom_bar(stat="identity", aes(x=2, y=male), position = "fill") +
  geom_bar(stat="identity", aes(x=3, y=female), position="fill") +
  theme_ipsum_rc() +
  scale_color_tableau("cyclic") +
  scale_fill_grey()+
  scale_x_continuous(breaks=c(1:3), labels=c("Overall","Male","Female")) 


#par(new = TRUE) # <- magic
####  Drawing all 16 Personalities as Circos Chart

png(filename="PersonalitiesChart_240.png", width=2000, height=2000, res=240)
circos.clear()
circos.par(start.degree=90)
circos.initialize(factor=per.df.comb$type, xlim=c(0,1))
circos.track(ylim=c(0,1), 
             panel.fun = function(x,y){
               circos.text(x=CELL_META$xcenter, y=CELL_META$ycenter,
                           labels=CELL_META$sector.index,
                           facing = "inside", 
                           niceFacing= TRUE, font=2)
             })

highlight.sector(per.df.comb$type[1:4], 
                 track.index = 1, 
                 text = per.df.comb$roles[1],
                 facing = "bending.inside", niceFacing = TRUE, 
                 text.vjust = "15mm", cex = 1.5, col="#734C5F60")
highlight.sector(per.df.comb$type[5:8], 
                 track.index = 1, 
                 text = per.df.comb$roles[5],
                 facing = "bending.inside", niceFacing = TRUE, 
                 text.vjust = "15mm", cex = 1.5, col="#728D5660")
highlight.sector(per.df.comb$type[9:12], 
                 track.index = 1, 
                 text = per.df.comb$roles[9],
                 facing = "bending.inside", niceFacing = TRUE, 
                 text.vjust = "15mm", cex = 1.5, col="#51A9AB60")
highlight.sector(per.df.comb$type[13:16], 
                 track.index = 1, 
                 text = per.df.comb$roles[13],
                 facing = "bending.inside", niceFacing = TRUE, 
                 text.vjust = "15mm", cex = 1.5, col="#E5C82E60")

circos.track(ylim=c(0,1), 
             panel.fun = function(x,y){
               circos.raster(image=image_read(per.df.comb$img[CELL_META$sector.numeric.index]),
                             x=CELL_META$xcenter, y=CELL_META$ycenter, 
                             width="2.5cm", facing = "inside", niceFacing=TRUE)
             })
circos.track(ylim=c(0,1), 
             panel.fun = function(x,y){
               circos.text(x=CELL_META$xcenter, y=CELL_META$ycenter,
                           labels=per.df.comb$type.code[CELL_META$sector.numeric.index],
                           facing = "inside", 
                           niceFacing= TRUE)
             })
dev.off()

