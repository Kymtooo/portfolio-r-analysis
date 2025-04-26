library(Ecdat)
library(tidyverse)

library(readr)
point <- read_csv("L01-2024P-2K.csv", 
                         locale = locale(encoding = "SHIFT-JIS"))
View(point)

names(point)[3] <- "code"
names(point)[6] <- "year"

print(names(point))
names(point) <- gsub("価格", "", names(point))

convert_to_seireki <- function(name) {
  if (grepl("^S[0-9]+", name)) {
    year <- as.numeric(sub("S", "", name)) + 1925
  } else if (grepl("^H[0-9]+", name)) {
    year <- as.numeric(sub("H", "", name)) + 1988
  } else if (grepl("^R[0-9]+", name)) {
    year <- as.numeric(sub("R", "", name)) + 2018
  } else {
    year <- name
  }
  return(as.character(year))
}

current_names <- names(point)

new_names <- sapply(current_names, convert_to_seireki)

names(point) <- new_names

filter_codes <- c(01235, 15108, 07547, 07212, 24212, 24472, 24471, 24543, 30421, 30428, 30382, 30401, 
                  28585, 28586, 26212, 31201, 35201, 35341, 08214, 36204, 38203, 39412, 44205, 45207)

filtered_point <- point[point$code %in% filter_codes, ]

# 行政区域コードに基づくダミー変数の条件
ones_codes <- c(14036, 24244, 45811, 83411, 152056, 155047, 222232, 173843, 182028, 184420, 184837, 184811,
                322016, 384429, 413879, 462152)

zeros_codes <- c(01235, 15108, 07547, 07212, 24212, 24472, 24471, 24543, 30421, 30428, 30382, 30401, 28585, 
                 28586, 26212, 31201, 35201, 35341, 08214, 36204, 38203, 39412, 44205, 45207)

# 新しい列「原発ダミー」を作成
filtered_point$treatment <- ifelse(filtered_point$code %in% ones_codes, 1, 
                      ifelse(filtered_point$code %in% zeros_codes, 0, NA))


#地価データの縦結合
Landprices <- rbind(X2007,X2008,X2009,X2010,X2011,X2012,X2013_,X2014,X2015,X2016,X2017,X2018)


#必要な自治体のみ抽出
filtercodes_all <- c(14036, 24244, 45811, 83411, 15205, 15504, 22223, 17384, 18202, 18442, 18483, 18481,
                  32201, 38442, 41387, 46215,1235, 15108, 7547, 7212, 24212, 24472, 24471, 24543, 30421, 30428, 30382, 30401, 28585, 
                  28586, 26212, 31201, 35201, 35341, 35204, 36204, 38203, 39412, 44205, 45207,1371,3202,17205,43215,18204)

filteredpoint_all <- Landprices[Landprices$code %in% filtercodes_all, ]


#行政区域コードによるダミー変数の作成？
ones_codes_all <- c(14036, 24244, 45811, 83411, 15205, 15504, 22223, 17384, 18202, 18442, 18483, 18481,
                32201, 38442, 41387, 46215)

zeros_codes_all <- c(1235, 15108, 7547, 7212, 24212, 24472, 24471, 24543, 30421, 30428, 30382, 30401, 28585, 
                 28586, 26212, 31201, 35201, 35341, 35204, 36204, 38203, 39412, 44205, 45207,1371,3202,17205,43215,18204)

# 新しい列「原発ダミー」を作成
filteredpoint_all$treatment <- ifelse(filteredpoint_all$code %in% ones_codes_all, 1, 
                                   ifelse(filteredpoint_all$code %in% zeros_codes_all, 0, NA))

filteredpoint_all_after <- filteredpoint_all_after %>%
  mutate(log_price = log(filteredpoint_all_after$price))

temp_point <- filteredpoint_all_after %>%
  filter(year == 2018) %>%
  filter(treatment == 0)
mean(temp_point$log_price)

filteredpoint_all_after <- filteredpoint_all %>%
  mutate(after = ifelse(year <= 2011, 0, 1))

did_model <- lm(log(price) ~ treatment * after + 駅距離 + 容積率 + 建ぺい率 + 地積, data = filteredpoint_all_after)
summary(did_model)

#データの削除
rm(X2013_)

people <- 市区町村人口$code %in% filtercodes_all
