library(tidyverse)
library(haven)

try(dir.create("data/casen"))

url <- "http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/docs/casen_2015_spss.rar"
file <- file.path("data", "casen", basename(url))


if(!file.exists(file)){
  
  download.file(url, file)
  
}


# ya descomprimido el archivo ---------------------------------------------
casen <- read_sav("data/casen/Casen 2015.sav")
casen

glimpse(casen)
str(casen)

str(casen$o13)
names(casen$o13)

attr(casen$o13, "label")

pregs <- map_chr(casen, ~ attr(.x, "label"))
pregs <- data_frame(col = names(casen)) %>% 
  mutate(preg = pregs)
pregs

pregs %>% 
  # filter(str_detect(tolower(preg), "gresos")) %>% 
  writexl::write_xlsx("data/casen/casen_preguntas.xlsx")

casen %>% 
  select(starts_with("y")) %>% 
  glimpse()



casen %>% count(o10)
ggplot(casen) + geom_histogram(aes(o10))

casen %>% count(y1, sort = TRUE)
ggplot(casen) + geom_histogram(aes(y1)) + scale_x_log10()
casen$y1 %>% summary()
