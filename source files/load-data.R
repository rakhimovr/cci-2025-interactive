  # Survey data
survey_2024 <- read_sav("~/GitHub/cci-2024/Survey Exports/2024/spss_5_7_2024.sav")
survey_2023 <- read_csv("~/GitHub/cci-2024/Survey Exports/national_2023.csv")
survey_2021 <- read.csv("~/GitHub/cci-2024/Survey Exports/national_2021.csv")
survey_2021_boston <-  read_sav("~/GitHub/cci-2024/Survey Exports/boston_2021.sav")

  # 2024 Lucid data
# US
lucid_2024_1 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/US/RespondentAnswerReport_48714562_CC_-_Index_2024_-_National_2024-05-07_0847.csv")
lucid_2024_2 <- read_csv("Lucid Exports/05-07/US/RespondentAnswerReport_50912475_CC_-_Index_2024_-_National_-_V2_2024-05-07_0845.csv")

# Boston
lucid_2024_boston_1 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/Boston/RespondentAnswerReport_48757406_CC_-_Index_2024_-_Boston_2024-05-07_0846.csv")
lucid_2024_boston_2 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/Boston/RespondentAnswerReport_50912470_CC_-_Index_2024_-_Boston_-_V2_2024-05-07_0845.csv")

# Denver
lucid_2024_denver_1 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/Denver/RespondentAnswerReport_48757136_CC_-_Index_2024_-_Denver_2024-05-07_0846.csv")
lucid_2024_denver_2 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/Denver/RespondentAnswerReport_50913982_CC_-_Index_2024_-_Denver_-_V2_2024-05-07_0844.csv")