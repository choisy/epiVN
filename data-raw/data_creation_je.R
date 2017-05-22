library(magrittr) # for the "%>%" pipe operator
library(dplyr)    # for "filter", "select" , "contains"
library(tidyr)    # for "gather", "spread", "separate"

# Prerequisites ----------------------------------------------------------------
provinces <- readRDS("data-raw/province.RDS")

# Functions --------------------------------------------------------------------

# Function to clean the column names and regroup them in one row.
#|No| Province | YEAR 1997 | NA    |NA     | NA   |NA | NA    |  NA    | NA |...
#|NA|    NA    | District  | NA    |2Doses | NA   |NA | Dose3 |  NA    | NA |...
#|NA|    NA    | Implement | Result|Target |Result| % | Target| Result | %  |...
make_col_names <- function(df) {
  # delete first column, the columns names are written in three rows.
  df <- df[, -1]
  # rename more clearly the columns names in the third rows
  sel <- which(is.na(df[2, ]) != TRUE)
  df[2, sel] <- rep(c("district_implement", "district_total", "target 2doses",
    "result 2doses", "percentage 2doses", "target 3doses", "result 3doses",
    "percentage 3doses"), 20)
  # rename the empty columns names
  names(df)[sel] <- rep(na.omit(names(df)[sel]), each = 8)
  # regroup the columns names in one row
  df <- df[-1, ]
  names(df) <- apply(cbind(names(df), unlist(df[1, ])), 1, paste, collapse = "_") %>%
    gsub("NA", "", .) %>%
    gsub("NA NA", "", .)
  # select only the important data and rename the column province
  df %<>%
    filter (is.na(.$Province_) == FALSE) %>%
    select(-contains("percentage"), province = Province_)
}

# Translates (by default: province's name).
translate <- function(df, col_names = "province", hash = provinces) {
  df[, col_names] <- hash[unlist(df[, col_names])]
  df
}

# Load data --------------------------------------------------------------------

je <- read_excel("data-raw/JE VACCINATION DATA IN VIET NAM, 1997-2016.xlsx", skip = 1) %>%
  make_col_names %>%
  gather(key, value, contains("19"), contains("20")) %>%
  separate(key, letters[1:2], "_", extra = "merge") %>%
  spread(b, value) %>%
  rename(year = a) %>%
  gather(key, value, contains("doses")) %>%
  separate(key, letters[1:2], " ") %>%
  spread(a, value) %>%
  rename(number_doses = b) %>%
  mutate(
    year               = as.numeric(year),
    number_doses       = as.numeric(gsub("doses", "", number_doses)),
    result             = as.numeric(result),
    target             = as.numeric(target),
    district_implement = as.numeric(district_implement),
    district_total     = as.numeric(district_total)) %>%
  translate %>%
  arrange(province, year)

# Test provinces names ---------------------------------------------------------
provinces_names <- readRDS(file = "data-raw/provinces_names.rds")

extract_prov <- function(ye, df = je) {
  filter(df, year == ye) %>%
    select(province,result,target) %>%
    na.omit %>%
    select(province) %>%
    unlist() %>%
    unique()
}

test_function <- function(ye, test) {
  testthat::expect_equal(
    grep(paste(extract_prov(ye = ye), collapse = "|"),
      unlist(provinces_names[test]) %>% as.vector, value=T),
    extract_prov(ye = ye))
}

lapply(c(1997:2003), function(x) test_function(x, test = "1997"))
lapply(c(2004:2007), function(x) test_function(x, test = "2004"))
lapply(c(2008:2015), function(x) test_function(x, test = "2008"))

# ------------------------------------------------------------------------------

je <- setNames(as.data.frame(je, stringsAsFactors = FALSE),
               c("province", "year", "nb_districts", "districts_tot", "nb_doses",
                 "nb_people", "people_tg"))

je <- je[, c("province", "year", "nb_doses", "nb_districts", "districts_tot",
             "nb_people", "people_tg")]

je %<>% mutate_if(is.numeric, as.integer) %>% str

devtools::use_data(je, overwrite = TRUE)
