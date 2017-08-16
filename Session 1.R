library(tidyverse)
## If error: install.packages("tidyverse")

## Explore
starwars %>% View

## Play
starwars %>% 
  filter(height<100) %>%
  arrange(mass)

## model
model <- lm(price ~ cut + color + carat + clarity,diamonds)
summary(model)

## Even more complexly
diamonds %>%
  nest(-cut) %>% pull %>%
  map(~ lm(price ~ color + carat + clarity, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>% round(2)

## Play with Plotting
diamonds %>%  ggplot(aes(x=carat,y=price)) + geom_point()

diamonds %>%
  ggplot(aes(x=carat,y=price,color=color)) + geom_point()

diamonds %>%
  ggplot(aes(x=carat,y=price,color=color,shape=cut)) + geom_point()


diamonds %>%
  ggplot(aes(x=carat,y=price,color=color,shape=cut)) + geom_point() + facet_wrap(~clarity)
## ^ powerful? Yes. Useful? No... go simpler:

diamonds %>%
  ggplot(aes(x=carat,y=price)) + geom_point() + geom_smooth()

## Maps -----------------
library(leaflet)
## If error: install.packages("leaflet")

head(quakes)

leaflet(data = quakes) %>% addTiles() %>% 
  addCircles(~long, ~lat,opacity = 0.1)

leaflet(data = quakes) %>% addTiles() %>% 
  addCircles(~long, ~lat,radius = ~mag)
## ^ not very visible.... what are magnitude ranges anyway?

plot(quakes$mag)
## ^ ok, so between 4:6... not huge difference. Maybe try with colors?

pal <- colorNumeric(c("yellow", "red"), 4:6)

quakes <- quakes %>%
  mutate(mag=ifelse(.$mag>6,6,.$mag))
         
leaflet(data = quakes) %>% addTiles() %>% 
  addCircles(~long, ~lat,color = ~pal(mag),radius=10)

## and finally add a note
quakes$note <- paste("Depth of ",quakes$depth,"; stations = ",quakes$stations,sep="")

leaflet(data = quakes) %>% addTiles() %>% 
  addCircles(~long, ~lat,label = ~note)

## Scrape list of pubs in canterbury
library(rvest)
## If error: install.packages("rvest")

path <- "https://www.pubsgalore.co.uk/towns/canterbury/kent"

page <- read_html(path)

## Get ONLY the pubnames and links (use SelectorGadget in Chrome to find the right css element, in this case "#pagelist a")
URLs <- page %>%
  html_nodes("#pagelist a")

df <- data.frame(name= html_text(URLs),
           link= html_attr(URLs,"href"))

## But paste in the missing part of the link
df$link <- paste("https://www.pubsgalore.co.uk", df$link,sep="")

## ok now if I go to one of those pages, how do I get it's address? Using "#pubaddress"
read_html(df$link[1]) %>%
  html_nodes("#pubaddress") %>% html_text()

## So create a function that will do that for any pub
addressGetter <- function(x){
  read_html(x) %>%
    html_nodes("#pubaddress") %>% html_text()
}

## BE NICE AND SCRAPE RESPONSIBLY... LETS ONLY DO THIS FOR 10 PUBS
df_small <- head(df,10)
df_small$addresses <- map_chr(df_small$link,addressGetter)
df_small$addresses1 <- gsub("\\r\\n +","",df_small$addresses)
df_small$addresses1 <- gsub("Canterbury",", Canterbury, UK ",df_small$addresses1)

## Map scraped stuff
library(ggmap)
## If error: install.packages("ggmap")

## now geocode
Locs <- geocode(df_small$addresses1)

## And put them on a map!
df_small %>%
  bind_cols(Locs) %>% 
  filter(!is.na(lat)) %>%
  leaflet() %>% addTiles() %>% 
  addCircles(~lon, ~lat,label = ~name,radius = 10) %>%
  addProviderTiles(providers$Stamen.Toner)
  # addProviderTiles(providers$Stamen.Watercolor)

