# set working directory
setwd("your/working/directory")

# import libraries
library(readxl)
library(tidyverse)
library(gtsummary)
library(flextable)
library(ggpubr)
library(ggsci)

# import data
df <- read_excel("your/working/directory/data.xlsx")
attach(df)
df %>% colnames

# create a summary table
table1 <- df[,-1] %>% # remove the first column (ID)
            tbl_summary(by = "T Score Categories",
                        type = list(where(is.numeric) ~ "continuous"),
                        missing_text = "Missing Data") %>%
            add_p() %>%
            add_overall()

# save the table
table1 %>%
  as_flex_table() %>%
  save_as_docx(path = "Table 1.docx")

# create a univariate linear regression table for T-scores
table2 <- df[,c(13,2,3,15,8,6,10)] %>%
            tbl_uvregression(method = lm, 
                             y = "T Score")

# save the table
table2 %>%
  as_flex_table() %>%
  save_as_docx(path = "Table 2.docx")

# create mean and CI plots for categorical variables and 
# scatterplots with regression lines for continuous variables
plot1 <- ggerrorplot(data = df,
                     x = "Sex",
                     y = "T Score",
                     desc_stat = "mean_ci",
                     ggtheme = theme_gray())

ggsave(plot1, 
       filename = "Plot 1.png", 
       height = 3, 
       width = 6, 
       dpi = 900)

df$Age <- df$`Age (years)`

plot2 <- ggscatter(data = df,
                   x = "Age",
                   y = "T Score",
                   add = "reg.line",
                   conf.int = T,
                   ggtheme = theme_gray())

ggsave(plot2, 
       filename = "Plot 2.png", 
       height = 3, 
       width = 6, 
       dpi = 900)

plot3 <- ggscatter(data = df,
                     x = "BMI",
                   y = "T Score",
                   add = "reg.line",
                   conf.int = T,
                   ggtheme = theme_gray())

ggsave(plot3, 
       filename = "Plot 3.png", 
       height = 3, 
       width = 6, 
       dpi = 900)

plot4 <- ggerrorplot(data = df,
                     x = "HTN",
                     y = "T Score",
                     desc_stat = "mean_ci",
                     ggtheme = theme_gray())

ggsave(plot4, 
       filename = "Plot 4.png", 
       height = 3, 
       width = 6, 
       dpi = 900)

plot5 <- ggerrorplot(data = df,
                     x = "DM",
                     y = "T Score",
                     desc_stat = "mean_ci",
                     ggtheme = theme_gray())

ggsave(plot5, 
       filename = "Plot 5.png", 
       height = 3, 
       width = 6, 
       dpi = 900)

plot6 <- ggerrorplot(data = df,
                     x = "ART",
                     y = "T Score",
                     desc_stat = "mean_ci",
                     ggtheme = theme_gray())

ggsave(plot6, 
       filename = "Plot 6.png", 
       height = 3, 
       width = 6, 
       dpi = 900)
