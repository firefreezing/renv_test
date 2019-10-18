# TFS: http://tfsgit.mathematica.net:8080/tfs/MPR/_git/R4PkgMgt

# if (!requireNamespace("remotes")) install.packages("remotes")
# 
# remotes::install_github("rstudio/renv")
# renv::init()

# renv::snapshot()

library(pacman)
library(tmap)
library(tidyverse)
library(sf)


# load the data -----------------------------------------------------------

dat <- read_csv("./data/ghi_hrrp.csv")

names(dat)

# read the shape file 
state_sf <- sf::st_read("data/cb_2016_us_state_20m/cb_2016_us_state_20m.shp") 
county_sf <- sf::st_read("data/cb_2016_us_county_20m/cb_2016_us_county_20m.shp") 

# dat_cnty_lst <- dat %>% select(county_name, state) %>% distinct() %>%
#   mutate(st_key = map(state, rep, times = 10) %>% map(str_c, collapse = "") %>% unlist,
#          key = str_c(st_key, tolower(county_name), sep = "_"))
# 
# cnty_dict <- county_sf %>% st_drop_geometry() %>% select(STATEFP, GEOID, county_name = NAME) %>% 
#   left_join(state_sf %>% st_drop_geometry() %>% select(STATEFP, state = STUSPS), 
#                        by = "STATEFP") %>%
#   mutate(st_key = map(state, rep, times = 10) %>% map(str_c, collapse = "") %>% unlist,
#          key = str_c(st_key, tolower(county_name), sep = "_"))


# add geoid as unique identifiers for states and counties -----------------

# state dictionary
st_dict <- state_sf %>% st_drop_geometry() %>% 
  dplyr::select(geoid_st = STATEFP, state = STUSPS)

dat_cnty_lst <- dat %>% dplyr::select(county_name, state) %>% distinct() %>%
  mutate(key = str_c(state, tolower(county_name), sep = "_"))

# side note Q: some counties could have more than one GEOIDs, why?
# for demo purpose: just pick one
# cnty_dict_sf <- county_sf %>% st_drop_geometry() %>% select(STATEFP, GEOID, county_name = NAME) %>%
#   group_by(STATEFP, county_name) %>%
#   mutate(n = n()) %>%
#   arrange(desc(n))

cnty_dict_sf <- county_sf %>% st_drop_geometry() %>% 
  dplyr::select(STATEFP, GEOID, county_name = NAME) %>% 
  left_join(state_sf %>% st_drop_geometry() %>% 
              dplyr::select(STATEFP, state = STUSPS), 
            by = "STATEFP") %>%
  mutate(key = str_c(state, tolower(county_name), sep = "_"))

dat_cnty_lst <- dat_cnty_lst %>%
  left_join(cnty_dict_sf, by = "key", suffix = c("_hrrp", "_cnty_sf")) %>%
  group_by(key) %>%
  mutate(cnt = n(),
         num = row_number()) %>%
  filter(
    !is.na(STATEFP),
    row_number() == 1  # only select the first record if there is multiple geoids per county
    ) %>%
  ungroup()

cnty_dict <- dat_cnty_lst %>%
  dplyr::select(county_name = county_name_hrrp, 
                state = state_hrrp, geoid_cnty = GEOID)

dat_w_key <- dat %>%
  left_join(st_dict, by = "state") %>%
  left_join(cnty_dict, by = c("state", "county_name"))



# begin the analysis ------------------------------------------------------

fct_summarise_hrrp <- function(dat, report_lvl, measr){
  
  # input:
  #   dat - a data frame, with records of readmission rate per hospital per measure
  #   report_lvl - a string, either "state" or "county"
  #   measr - a string, measure name. 
  # output:
  #   corr_mtrx - correlation matrix between measures
  #   map - a list of tmap objects: continental, Hawaii, and Alaska
  
  index <- list(
    state = list(shape_file = state_sf, key = "geoid_st"),
    county = list(shape_file = county_sf, key = "geoid_cnty")
  )
  
  var_key <- sym(index[[report_lvl]][["key"]])
  
  dat_clean <- dat %>%
    dplyr::select(hospital_name, provider_id, !!var_key, measure_name,
           excess_readmission_ratio, 
           predicted_readmission_rate, 
           expected_readmission_rate) %>%
    filter(!is.na(excess_readmission_ratio) & !is.na(!!var_key)) %>%
    group_by(!!var_key, measure_name) %>%
    summarise(err = sum(predicted_readmission_rate)/sum(expected_readmission_rate)) %>%
    spread(key = measure_name, value = err) %>%
    ungroup
  
  corr_mtrx <- dat_clean %>% 
    dplyr::select(-!!var_key) %>%
    cor(method = "pearson", use = "pairwise.complete.obs")
  
  hrrp_sf <- index[[report_lvl]][["shape_file"]] %>%
    left_join(dat_clean, by = c("GEOID" = index[[report_lvl]][["key"]]))
  
  # HI - 15, AK - 02
  us_continental_map <- tm_shape(hrrp_sf %>% 
                                   filter(!str_sub(GEOID, 1, 2) %in% c("15", "02")), 
                                 projection = 2163) +
    tm_fill(col = measr, palette = "OrRd", n = 4) +
    tm_borders(col = "grey50") +
    tm_layout(frame = FALSE, 
              legend.position = c("right","bottom"))
  
  hawaii_map <- tm_shape(hrrp_sf %>% filter(str_sub(GEOID, 1, 2) == "15")) +
    tm_fill(col = measr, palette = "OrRd", n = 4, legend.show = F) +
    tm_borders(col = "grey50")+
    tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, 
              title.position = c("left", "bottom"))
  
  alaska_map <- tm_shape(hrrp_sf %>% filter(str_sub(GEOID, 1, 2) == "02"), 
                         projection = 2163) +
    tm_fill(col = measr, palette = "OrRd", n = 4, legend.show = F) +
    tm_borders(col = "grey50")+
    tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)
  
  return(list(
    corr_mtrx = corr_mtrx,
    map = list(
      continental = us_continental_map,
      hawaii = hawaii_map,
      alaska = alaska_map)
  ))
}

output <- fct_summarise_hrrp(dat_w_key, "county", "COPD")

output$corr_mtrx %>%
  corrplot::corrplot.mixed(lower.col = "black", number.cex = .7, 
                           tl.pos = "lt", tl.cex = .75, tl.col = "black")

output$map$continental
print(output$map$alaska, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))
print(output$map$hawaii, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))

# ggplot ------------------------------------------------------------------

# ggplot(hrrp_sf) + 
#   geom_sf(mapping = aes(fill = AMI)) +
#   scale_fill_viridis_c()


