library(data.table)
curr_file <- "C:/Users/Md.Ali/OneDrive - FDA/yousuf/00_github_projects/toxSummary/my_data_fda/fda_db.csv"
df <- data.table::fread(curr_file)
df[]
#8110
df[IND_num %in% c("IND163843")]
rmst[]
new_file <- data.table::data.table(application_type = rep("IND",2),
IND_num = c("IND163843", "IND163843"),
studyID = c("A2021029-T011-01", "A2021029-T014-01")
)
?data.table::rbindlist
# bind correctly by names
new_file
ls_rbind  <-  list(df, new_file)
new_df <- data.table::rbindlist(ls_rbind)
new_df <- new_df[order(application_type)]
new_df
data.table::fwrite(new_df, curr_file)

fda_data <- "C:/Users/Md.Ali/OneDrive - FDA/yousuf/00_github_projects/toxSummary/my_data_fda/fda_db.csv"

df_fda_data <- data.table::fread(fda_data)
identical(df,df_fda_data)

A2021029-T011-01
A2021029-T014-01
new_ind <- c("IND163843")
new_ind <- c("IND163843")