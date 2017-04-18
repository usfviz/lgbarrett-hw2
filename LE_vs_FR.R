rm(list = ls())
cat('\014')

if (!require("tidyr")) {
  install.packages("tidyr", repos = "http://cran.us.r-project.org")
}

if (!require("dplyr")) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
}

life_expectancy <- read.csv("Data/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv",
                           header = TRUE, stringsAsFactors = FALSE, skip = 4, check.names = FALSE)
fertility_rate <- read.csv("Data/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv",
                            header = TRUE, stringsAsFactors = FALSE, skip = 4, check.names = FALSE)
population <- read.csv("Data/API_SP.POP.TOTL_DS2_en_csv_v2.csv",
                       header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

metadata <- read.csv("Data/Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv",
                       header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

metadata <- metadata[, -6] %>% filter(Region != "")

fertility_rate <- fertility_rate[-c(which(apply(fertility_rate[, -c(3, 4, 60, 61, 62)], MARGIN = 1, function(x) all(is.na(x[3:57])))), 211),
                                 -c(3, 4, 60, 61, 62)]
life_expectancy <- life_expectancy[-c(which(apply(life_expectancy[, -c(3, 4, 60, 61, 62)], MARGIN = 1, function(x) all(is.na(x[3:57])))), 211),
                                 -c(3, 4, 60, 61, 62)]
population <- population[-c(which(apply(population[, -c(3, 4, 60, 61, 62)], MARGIN = 1, function(x) all(is.na(x[3:57])))), 211),
                                 -c(3, 4, 60, 61, 62)]

# between_years <- seq(1960, 2014, 0.25)
# new_cols <- between_years[!(between_years%%1 == 0)]
# for (i in seq(58, length(new_cols)+57, 1)){
#   fertility_rate[,i] <- NA
#   life_expectancy[,i] <- NA
#   population[,i] <- NA
# }
# colnames(fertility_rate) <- c(colnames(fertility_rate)[1:57], new_cols)
# colnames(life_expectancy) <- c(colnames(life_expectancy)[1:57], new_cols)
# colnames(population) <- c(colnames(population)[1:57], new_cols)


fertility_rate <- gather(fertility_rate, key = "year", value = "fertility_rate", 3:57)
life_expectancy <- gather(life_expectancy, key = "year", value = "life_expectancy", 3:57)
population <-gather(population, key = "year", value = "population", 3:57)

country_data <- inner_join(inner_join(fertility_rate, life_expectancy, by = c("Country Name", "Country Code", "year")),
                          population, by = c("Country Name", "Country Code", "year"))

# na_count <- country_data %>% 
#   group_by(`Country Name`) %>% 
#   arrange(year) %>% 
#   summarise(fr_non_na = sum(!is.na(fertility_rate)),
#             le_non_na = sum(!is.na(life_expectancy)),
#             pop_non_na = sum(!is.na(population)))

linear_interpolation <- function(data){
  missindx <- is.na(data)
  n <- length(data)
  allindx <- 1:n
  indx <- allindx[!missindx]
  data.vec <- as.vector(data)
  return(approx(indx, data.vec[indx], 1:n, rule = 2)$y)
}


country_data <- country_data %>% 
  group_by(`Country Name`) %>% 
  arrange(year) %>% 
  mutate(fr_interp = linear_interpolation(fertility_rate),
         le_interp = linear_interpolation(life_expectancy),
         pop_interp = linear_interpolation(population))

country_data <- left_join(country_data, metadata[, c("Country Code", "Region")], by = "Country Code")
country_data <- country_data %>% filter(!is.na(Region))
country_data$point_size <- sapply(country_data$pop_interp/max(country_data$pop_interp)*3000, function(x) max(x, 25))
country_data$hover_point_size <- 2*country_data$point_size
