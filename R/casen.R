library(tidyverse)
library(haven)
# devtools::install_github("jlmelville/smallvis/smallvis")
library(smallvis)
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


# preguntas ---------------------------------------------------------------
str(casen$o13)
names(casen$o13)

attr(casen$o13, "label")

pregs <- map_chr(casen, ~ attr(.x, "label"))
pregs <- data_frame(col = names(casen)) %>% 
  mutate(preg = pregs)
pregs

# pregs %>% 
#   filter(str_detect(tolower(preg), "gresos")) %>%
#   writexl::write_xlsx("data/casen/casen_preguntas.xlsx")


# variables seleccionadas -------------------------------------------------
vars <- c("region", "comuna", 
          "tot_hog", "tot_par", 
          "tot_nuc", "tot_per", 
          "sexo", "edad", 
          "ecivil", "pareja", 
          "e1", "e2a", 
          "o1", "o2", 
          "y1", "y2d", 
          "y3a", "y27a", 
          "y27b", "y27c", 
          "y27d", "y27e", 
          "s4", "s5", 
          "s12", "s13", 
          "r1a", "v1", 
          "v2", "v8", 
          "v10", "v11", 
          "v13", "v18", 
          "v19", "v23", 
          "v26", "y0101", 
          "y0301", "ESC", 
          "educ", "depen", 
          "hacinamiento")

pregs %>% 
  filter()

casen <- select(casen, vars)
gc()

glimpse(casen)


# cluster -----------------------------------------------------------------
# casendict <- casen %>% 
#   distinct(region, comuna) %>% 
#   mutate(comuna_lbl = as_factor(comuna))

casen <- casen %>% 
  mutate_if(is.labelled, as_factor)

casen <- casen %>% 
  mutate_at(vars(y1, y2d, s4, s5, v8), function(x) as.numeric(as.character(x)))

# casen %>% 
#   map(class) %>% 
#   unlist() %>% 
#   table()
# 
# casen <- casen %>% 
#   select_if(negate(is.character))

# casen num  
casen_num <- casen %>% 
  group_by(region, comuna) %>% 
  select_if(is.numeric) %>% 
  # mutate_all(replace_na, 0) %>% 
  ungroup()

names(casen_num)

# casen factor
casen_fac <- casen %>% 
  group_by(region, comuna) %>% 
  select_if(is.factor) %>% 
  ungroup() %>% 
  mutate_all(fct_explicit_na, na_level = "NA") %>% 
  mutate_at(vars(-1, -2), fct_lump, other_level = "Otra") 

nms <- names(casen_fac)
casen_fac <- map2_dfc(casen_fac, nms, function(v, n){
  levels(v) <- paste(n, levels(v), sep = "_")
  v
})

casen_fac <- casen_fac %>% 
  mutate_at(vars(1, 2), as.character) %>% 
  mutate_at(vars(1, 2), str_remove, "^.*_")


# por comuna --------------------------------------------------------------
casen_num_c <- casen_num %>% 
  group_by(region, comuna) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  mutate_all(replace_na, 0) %>% 
  ungroup()

table_list <- function(x) {
  # x <- sample(LETTERS[1:5], 20, 1:5, replace = TRUE)
  t <- table(x)
  
  data_frame(
    cat = names(t),
    val = as.numeric(prop.table(t))
  ) %>% 
    spread(cat, val) %>% 
    list()
}


casen_fac_c <- casen_fac %>% 
  group_by(region, comuna) %>% 
  summarise_all(table_list) %>% 
  unnest()


casen_c <- left_join(casen_num_c, casen_fac_c)
casen_c <- casen_num_c
glimpse(casen_c)


tsne_casen_c <- smallvis(casen_c, perplexity = 25,verbose = TRUE)
umap_casen_c <- smallvis(casen_c, method = "umap", perplexity = 25, eta = 0.01)


casen_c %>% 
  bind_cols(as.data.frame(tsne_casen_c)) %>% 
  ggplot(aes(V1, V2, color = region, label = comuna)) +
  geom_point() +
  ggrepel::geom_text_repel(size = 2) +
  facet_wrap(~region, scales = "free")

casen_c %>% 
  bind_cols(as.data.frame(umap_casen_c)) %>% 
  ggplot(aes(V1, V2, color = region, label = comuna)) +
  geom_point() +
  ggrepel::geom_text_repel(size = 2, color = "black") +
  scale_color_viridis_d() +
  facet_wrap(~region)



  
