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


casen <- casen %>%
  # head(10) %>% 
  select(-folio, -o, -id_vivienda, -provincia, -zona, -hogar)

# casen num  
casen_num <- casen %>% 
  group_by(region, comuna) %>% 
  summarise_if(negate(is.labelled), mean, na.rm = TRUE)

# casen cat
casen_cat <- ""

casen %>%
  head(200) %>% 
  group_by(region, comuna) %>% 
  select(-ends_with("esp")) %>% 
  select(matches("^[a-z][1-3][a-z]?"))
  select_if(is.labelled) %>% 
  select_if(negate(is.character)) %>% 
  mutate_if(is.labelled, as_factor) %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "(NA)") %>% 
  mutate_if(is.factor, fct_lump)

casen_cat <- casen_cat %>% 
  ungroup() %>% 
  # sample_n(2000) %>% 
  gather(key, value, -region, -comuna) %>% 
  count(region, comuna, key, value) %>% 
  group_by(region, comuna, key) %>% 
  mutate(n = n/sum(n)) %>% 
  ungroup() %>% 
  unite("key_value", c("key", "value")) %>% 
  spread(key_value, n) %>% 
  mutate_all(replace_na, 0)

