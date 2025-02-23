setwd("C:\\Users\\17376\\Desktop\\CYX\\学习\\5243 Applied DS\\Possible Data Sets")
df <- read.csv("SupplyChainGHGEmissionFactors_v1.3.0_NAICS_CO2e_USD2022.csv")

install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("readr")
library(readr)

library(dplyr)
library(stringr)
library(readr)

# read data
df <- read.csv("SupplyChainGHGEmissionFactors_v1.3.0_NAICS_CO2e_USD2022.csv")

# 1. **制造 GHG 列的乱码字符**
set.seed(42)
special_chars <- c("#", "@", "&", "*", "%", "!")
df$GHG <- ifelse(runif(nrow(df)) < 0.3, 
                 paste0(df$GHG, sample(special_chars, 1)), 
                 df$GHG)

# 2. **单位（Unit）不统一：让错误更像真实单位**
unit_variants <- c("kg CO2e/m2", "g CO2e/kg", "lbs CO2e/m2", "mg CO2e/kg", "ton CO2e")
df$Unit <- ifelse(runif(nrow(df)) < 0.2, sample(unit_variants, nrow(df), replace = TRUE), df$Unit)

# 3. **改变数值精度 & 插入异常值**
numeric_cols <- names(df)[sapply(df, is.numeric)]
for (col in numeric_cols) {
  df[[col]] <- ifelse(runif(nrow(df)) < 0.3, round(df[[col]], sample(0:5, 1)), df[[col]])
  
  # 插入极端异常值，但要在正常范围基础上扩大
  df[sample(nrow(df), 5), col] <- sample(c(df[[col]] * 10, df[[col]] * -5, Inf, -Inf, NA), 5, replace = TRUE)
}

# 4. **插入 200 行乱码数据**
random_strings <- function(n, length = 10) {
  apply(matrix(sample(c(letters, LETTERS, 0:9, "@", "#", "!", "%", "&"), n * length, replace = TRUE), 
               ncol = length), 1, paste, collapse = "")
}

garbage_data <- data.frame(matrix(random_strings(200, ncol(df)), ncol = ncol(df)))
colnames(garbage_data) <- colnames(df)
df <- bind_rows(df, garbage_data)

# 5. **复制 50 行数据并插入**
duplicate_rows <- df[sample(1:nrow(df), 50), ]
df <- bind_rows(df, duplicate_rows)

# 6. **插入文本列乱码字符（更隐蔽）**
categorical_cols <- names(df)[sapply(df, is.character)]
for (col in categorical_cols) {
  df[[col]] <- ifelse(runif(nrow(df)) < 0.2, 
                      paste0(substr(df[[col]], 1, nchar(df[[col]]) - 1), 
                             sample(c("@", "#", "!", "%", "&"), 1)), 
                      df[[col]])
}

# 7. **数据错位（让某些单元格交换位置）**
for (i in sample(1:nrow(df), 5)) {
  cols <- sample(names(df), 2)
  temp <- df[i, cols[1]]
  df[i, cols[1]] <- df[i, cols[2]]
  df[i, cols[2]] <- temp
}

# 8. **打乱数据行顺序**
df <- df[sample(1:nrow(df)), ]

# 保存脏数据
output_path <- "extremely_messy_data_v2.csv"
write_csv(df, output_path)

print(paste("更自然的脏数据已生成，保存在:", output_path))



# 1. **修改 2017 NAICS Title 列的 20% 为全大写或小写**
set.seed(42)  # 确保每次运行结果一致
title_col <- "2017 NAICS Title"  # 确保列名正确

# 确保列为字符类型
df[[title_col]] <- as.character(df[[title_col]])

# 随机选择 20% 的数据行进行修改
rows_to_modify <- sample(1:nrow(df), size = round(0.2 * nrow(df)), replace = FALSE)

# 随机转换为大写或小写
df[rows_to_modify, title_col] <- ifelse(runif(length(rows_to_modify)) < 0.5, 
                                        toupper(df[rows_to_modify, title_col]), 
                                        tolower(df[rows_to_modify, title_col]))

# 2. **将 Reference USEEIO Code 列中的部分 'A' 改成 'a'**
reference_col <- "Reference USEEIO Code"  # 确保列名正确
df[[reference_col]] <- gsub("A", function(x) ifelse(runif(1) < 0.3, "a", x), df[[reference_col]])

# 保存脏数据
output_path <- "extremely_messy_data_v3.csv"
write_csv(df, output_path)

print(paste("增强版脏数据已生成，保存在:", output_path))

