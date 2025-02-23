
library(dplyr)
library(stringr)
library(readr)


file_path <- "extremely_messy_data_v3.csv"
df <- read_csv(file_path)

# 1. **去掉第一列、第二列列名开头的 "X"**
colnames(df) <- gsub("^X", "", colnames(df))

# 2. **按照第一列升序排序**
first_col <- colnames(df)[1]  # 获取第一列列名
df <- df %>% arrange(.data[[first_col]])  # 按第一列排序

# 3. **GHG 列去掉特殊符号，统一变成 "All GHGs"**
ghg_col <- "GHG"
df[[ghg_col]] <- "All GHGs"

# 4. **Unit 列单位统一**
unit_col <- "Unit"
df[[unit_col]] <- "kg CO2e/2022 USD, purchaser price"

# 5. **所有数值保留三位小数**
numeric_cols <- names(df)[sapply(df, is.numeric)]
df[numeric_cols] <- lapply(df[numeric_cols], function(x) round(x, 3))

# 6. **去除乱码行、重复行、极端值**
df <- df %>%
  filter(!apply(df, 1, function(x) all(grepl("[@#!%&]", x)))) %>%  # 去除乱码行
  distinct() %>%  # 去重
  mutate(across(all_of(numeric_cols), ~ ifelse(abs(.) > 1e6 | is.infinite(.), NA, .)))  # 处理极端值
df <- df %>%
  filter(!if_any(everything(), ~ is.na(.) | abs(.) > 1e6 | is.infinite(.))) 


# 7. **去掉 Reference USEEIO Code 列中的特殊符号**
reference_col <- "Reference USEEIO Code"
df[[reference_col]] <- gsub("[@#!%&]", "", df[[reference_col]])
df[[reference_col]] <- str_pad(df[[reference_col]], width = 5, side = "left", pad = "0")

# 8. **去掉最后一列**
df <- df[, -ncol(df)]

# 保存清理后的数据
cleaned_file_path <- "fully_cleaned_data_final.csv"
write_csv(df, cleaned_file_path)

print(paste("数据清理完成，已保存至:", cleaned_file_path))
