# set the file location
file_location = "bhbc_dk_gen_data.csv"

library(dplyr)
library(lubridate)
library(stringr)

# load data
df <- read.csv(file= file_location, stringsAsFactors=FALSE)
df$Date <- ymd_hms(df$Date.visite)

# separate systolic and diastolic BP
df$bp_sys <- gsub("/(.+)", "", df$BP)
df$bp_dia <- gsub("(.+)/", "", df$BP)

# convert all indications of antihypertenseur1 Nom to '1' and NaNs to '0'
df$antihypertenseur1.Nom[df$antihypertenseur1.Nom == ''] <- 0
df$antihypertenseur1.Nom[df$antihypertenseur1.Nom != 0] <- 1

# generate the indications

# generate the quarter number and key
completed_quarter <- function(dt) {
  prev_quarter_map <- c(1, 2, 3, 4)
  quarter <- prev_quarter_map[((month(dt) - 1) %/% 3) + 1]
  q = paste(toString(year(dt)), '_q', toString(quarter))
  return (gsub(" ", "", q))
}

df$quarter = lapply(df$Date, completed_quarter)

generate_key <- function(uuid, date_visit) {
  return(gsub(" ", "", paste(toString(uuid), "_", toString(date_visit))))
}
generate_key_v <- Vectorize(generate_key)
df$key = generate_key_v(df$UUID, df$Date)

# convert all instances of 'MHD et Medicament' to 'treated' and all others to NA
df$Type.de.traitement[df$Type.de.traitement == "MHD"] <- NA
df$Type.de.traitement[!is.na(df$Type.de.traitement)] <- "treated"

# set systolic and diastolic BP as numeric type
df$bp_sys <- as.numeric(as.character(df$bp_sys))
df$bp_dia <- as.numeric(as.character(df$bp_dia))

# display final dataset
head(df)

# create function that generates the metrics
measures <- function(df) {
  # all patients
  n_all <- length(unique(df$UUID))
  
  # diagnosed patients
  n_diag <- n_all
  
  # diagnosed and treated
  df_tmp <- subset(df, Type.de.traitement == "treated")
  df_tmp <- subset(df_tmp, antihypertenseur1.Nom == "1")
  n_trat <- length(unique(df_tmp$UUID))
  
  # eligible patients with at least 2 visits in the period
  pat_elig <- df %>%
    group_by(UUID) %>%
    summarize(count = n_distinct(Type.de.traitement)) %>%
    filter(count >= 2)
  pat_elig <- pat_elig$UUID
  
  # diagnosed, treated and controlled
  df_con <- df_tmp %>%
    filter(df_tmp$UUID %in% pat_elig) %>%
    group_by(UUID) %>%
    summarize(last_visit = max(Date))
  df_con$key <- generate_key_v(df_con$UUID, df_con$last_visit)
  df_cont <- merge(df_tmp, df_con, by="key")
  
  df_count <- df_cont %>%
    filter(bp_sys < 120) %>%
    filter(bp_dia < 80)
  df_count <- df_count %>%
    summarize(count = n_distinct(UUID.y))
  n_cont <- df_count[1, 'count']
  
  # return values
  return(c(n_all, n_diag, n_trat, n_cont))
}

# generate the list of unique quarters
q_list <- str_sort(unique(df$quarter))

print("Cascade per quarter - NOT cumulative")
cas_nc_list <- vector("list", length(q_list) + 1)
cas_nc_list[[1]] <- c("All Patients", "Diagnosed", "Treated", "Controlled")
for (i in 1:length(q_list)) {
  cas_nc_list[[i+1]] <- measures(df %>% filter(df$quarter %in% q_list[i]))
}

cas_nc <- as.data.frame(cas_nc_list)
colnames(cas_nc) <- c("Category", q_list)
head(cas_nc)

print("Cascade per quarter - cumulative")
cas_c_list <- vector("list", length(q_list) + 1)
cas_c_list[[1]] <- c("All Patients", "Diagnosed", "Treated", "Controlled")
for (i in 1:length(q_list)) {
  cas_c_list[[i+1]] <- measures(df %>% filter(df$quarter %in% q_list[1:i]))
}

cas_c <- as.data.frame(cas_c_list)
colnames(cas_c) <- c("Category", q_list)
head(cas_c)

# save as csv
write.csv(cas_nc, "./cascade_nc.csv", row.names=FALSE)
write.csv(cas_c, "./cascade_c.csv", row.names=FALSE)

# missingness
df_m <- df[, c("UUID", "Type.de.traitement", "antihypertenseur1.Nom", "quarter")] %>%
  filter(Type.de.traitement == "treated")
x <- length(df_m$Type.de.traitement)
df_md <- df_m %>%
  filter(antihypertenseur1.Nom == '1')
y <- length(df_md$Type.de.traitement)

print(paste("Missingness: ", y / x * 100, "%"))
