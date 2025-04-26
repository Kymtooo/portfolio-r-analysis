#必要な自治体のみ抽出
filtercodes_all_2 <- c(14036, 24244, 45811, 83411, 15504, 17384, 18442, 18483, 18481,
                     38442, 41387,1235, 15108, 7547, 7212, 24212, 24472, 24471, 24543, 30421, 30428, 30382, 30401, 28585, 
                     28586, 26212, 31201, 35201, 35341, 35204, 36204, 38203, 39412, 44205, 45207,1371,3202,17205,43215,18204)

filteredpoint_all_2 <- Landprices[Landprices$code %in% filtercodes_all_2, ]


#行政区域コードによるダミー変数の作成？
ones_codes_all_2 <- c(14036, 24244, 45811, 83411, 15205, 15504, 22223, 17384, 18202, 18442, 18483, 18481,
                    32201, 38442, 41387, 46215)

zeros_codes_all_2 <- c(1235, 15108, 7547, 7212, 24212, 24472, 24471, 24543, 30421, 30428, 30382, 30401, 28585, 
                     28586, 26212, 31201, 35201, 35341, 35204, 36204, 38203, 39412, 44205, 45207,1371,3202,17205,43215,18204)

# 新しい列「原発ダミー」を作成
filteredpoint_all_2$treatment <- ifelse(filteredpoint_all_2$code %in% ones_codes_all_2, 1, 
                                      ifelse(filteredpoint_all_2$code %in% zeros_codes_all_2, 0, NA))

temp_point_2 <- filteredpoint_all_2 %>%
  filter(year == 2018) %>%
  filter(treatment == 1)
mean(temp_point$price)

filteredpoint_all_after_2 <- filteredpoint_all_2 %>%
  mutate(after = ifelse(year <= 2011, 0, 1))

旧did_model <- lm(log(price) ~ treatment * after + 駅距離 + 容積率 + 建ぺい率 + 地積, data = filteredpoint_all_after_2)
summary(旧did_model)

people2 <- 市区町村人口2[市区町村人口2$code %in% filtercodes_all_2, ]

merged2 <- merge(filteredpoint_all_after_2, people2, by = "code", all = TRUE)

lm(log(price) ~ treatment * after * 若年割合 
   + 総数 
   + 駅距離 
   + 容積率 
   + 建ぺい率 
   + 地積, data = merged) %>%
  summary()

library(fixest)
fixest::feols(log(price) ~ after:treatment
              + 駅距離
              + 容積率
              + 建ぺい率
              + 地積
              |code + year, data = merged2) %>%
  summary()

fixest::feols(log(price) ~ after:treatment:年少人口割合
              + after:treatment:生産年齢前期人口割合
              + after:treatment:生産年齢後期人口割合
              + after:treatment:高齢者人口割合
              + 駅距離
              + 容積率
              + 建ぺい率
              + 地積
              |code + year, data = merged2) %>%
  summary()

colnames(merged2)

dynamic_did <- feols(log(price) ~ i(year, treatment, 2011)
                     + 駅距離
                     + 容積率
                     + 建ぺい率
                     + 地積 
                     | code + year, data = merged2)

iplot(dynamic_did)

# 必要なパッケージの読み込み
library(ggplot2)
library(dplyr)

# 年度ごとにtreatmentごとの平均地価を計算
data_summary <- merged2 %>%
  group_by(year, treatment) %>%
  summarize(mean_price = mean(price, na.rm = TRUE), .groups = "drop")


# 欠損値を取り除く
data_summary <- data_summary %>% filter(!is.na(year))

# プロットの作成
plot <- ggplot(data_summary, aes(x = year, y = log(mean_price), color = as.factor(treatment))) +
  geom_line(aes(group = treatment), linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0" = "red", "1" = "blue"), labels = c("原発計画地の平均", "原発所在地の平均")) +
  labs(x = "year", y = "log(price)", color = "Treatment") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(min(data_summary$year), max(data_summary$year), by = 1)) + # 全年度表示
  geom_vline(xintercept = 2011, linetype = "dashed", color = "black") # 2011年に縦線

# プロットの表示
print(plot)

# 必要な列のみを選択して統計量を計算
summary_stats <- merged2 %>%
  select(駅距離, 容積率, 建ぺい率, 地積, after, treatment) %>%
  summarise_all(list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    count = ~ sum(!is.na(.))
  ))

# 結果を縦長に整形して表示
summary_stats_long <- summary_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("column", "statistic"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

# 全行を表示
print(summary_stats_long, n = Inf)

# 必要な列のみを選択して統計量を計算
summary_stats2 <- Landprices %>%
  select(駅距離, 容積率, 建ぺい率, 地積) %>%
  summarise_all(list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    count = ~ sum(!is.na(.))
  ))

# 結果を縦長に整形して表示
summary_stats_long2 <- summary_stats2 %>%
  pivot_longer(
    cols = everything(),
    names_to = c("column", "statistic"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

# 全行を表示
print(summary_stats_long2, n = Inf)

# 必要な列のみを選択して統計量を計算
summary_stats <- people2 %>%
  select(年少人口割合,生産年齢前期人口,生産年齢後期人口,高齢者人口) %>%
  summarise_all(list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    count = ~ sum(!is.na(.))
  ))

# 結果を縦長に整形して表示
summary_stats_long <- summary_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("column", "statistic"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

# 全行を表示
print(summary_stats_long, n = Inf)


# 年少人口割合と生産年齢前期人口の列を数値型に変換
people2$年少人口割合 <- as.numeric(people2$年少人口割合)
people2$生産年齢前期人口 <- as.numeric(people2$生産年齢前期人口)
people2$生産年齢後期人口 <- as.numeric(people2$生産年齢後期人口)
people2$高齢者人口 <- as.numeric(people2$高齢者人口)

# 変換後のデータ型を確認
str(市区町村人口2)


# 必要な列のみを選択して統計量を計算
summary_stats_all <- 市区町村人口2 %>%
  select(年少人口割合,生産年齢前期人口,生産年齢後期人口,高齢者人口) %>%
  summarise_all(list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    count = ~ sum(!is.na(.))
  ))

# 結果を縦長に整形して表示
summary_stats_long_all <- summary_stats_all %>%
  pivot_longer(
    cols = everything(),
    names_to = c("column", "statistic"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

# 全行を表示
print(summary_stats_long_all, n = Inf)

dim(merged2)
dim(市区町村人口2)

identical(merged2$年少人口割合, 市区町村人口2$年少人口割合)
identical(merged2$生産年齢前期人口, 市区町村人口2$生産年齢前期人口)
identical(merged2$生産年齢後期人口, 市区町村人口2$生産年齢後期人口)
identical(merged2$高齢者人口, 市区町村人口2$高齢者人口)

# 同じ行のみを抽出（共通部分）
common_rows <- merged2[merged2$年少人口割合 == 市区町村人口2$年少人口割合 &
                         merged2$生産年齢前期人口 == 市区町村人口2$生産年齢前期人口 &
                         merged2$生産年齢後期人口 == 市区町村人口2$生産年齢後期人口 &
                         merged2$高齢者人口 == 市区町村人口2$高齢者人口, ]

# 共通するキーで内部結合を行う（例として "ID" を使用）
merged_compare <- merge(merged2, 市区町村人口2, by = "code", suffixes = c("_merged2", "_市区町村人口2"))

# 列ごとに違いを確認
diff_rows <- merged_compare %>%
  filter(年少人口割合_merged2 != 年少人口割合_市区町村人口2 |
           生産年齢前期人口_merged2 != 生産年齢前期人口_市区町村人口2 |
           生産年齢後期人口_merged2 != 生産年齢後期人口_市区町村人口2 |
           高齢者人口_merged2 != 高齢者人口_市区町村人口2)

# summary_stats と summary_stats_all を比較
summary_stats
summary_stats_all
