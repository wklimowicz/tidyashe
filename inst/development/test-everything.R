library(data.table)
library(tictoc)
library(ggplot2)
library(prettyunits)

load_all()

setwd(here::here())

# Setup ----------

keep_columns <- c("year", "piden", "thrs", "age", "ft", "sex", "gpay", "sic07",
                  "sic03", "occ00", "occ10")

ashe_convert(folder = "../ashe_data/",
                    new_folder = "../ashe_fst_data")
                    # select_columns = keep_columns)


# TODO: Add duplicate years argument for generality

ashe <- ashe_compile("../ashe_fst_data",
            # select_columns = keep_columns,
            save_to_folder = TRUE)

ashe <- ashe_load()

ashe |>
object.size() |>
as.numeric() |>
prettyunits::pretty_bytes()

# Extra Columns ----------------------------
ashe[,`:=`(last_job = shift(occupation, 1),
          last_industry = shift(industry, 1)),
     by = piden]

# QA ------------------------

# Present values by year
ashe[, .N, by = .(year, occ10)][, .N, by = year]
ashe[, .N, by = .(year, occ00)][, .N, by = year]
ashe[, .N, by = .(year, sic03)][, .N, by = year]
ashe[, .N, by = .(year, sic07)][, .N, by = year]
ashe[, .N, by = .(year, occupation)][, .N, by = year]
ashe[, .N, by = .(year, industry)][, .N, by = year]


# How long do people stay in sample
ashe[piden != "", years_in_sample := .N, by = piden
     ][,.N, by = years_in_sample
     ][order(years_in_sample)] |>
  ggplot() +
    geom_col(aes(x = years_in_sample, y = N)) +
    geom_vline(aes(xintercept = ashe[,max(year) - min(year) + 1]),
    size = 2, linetype = "dotted")

# Case studies for data accuracy

interesting_vars <- c("file", "year", "thrs", "age", "ft", "sex", "gpay",
                      "occ00", "occ10",  "occupation", "industry")

ashe[piden == "01999932", ..interesting_vars]
ashe[piden == "00170978", ..interesting_vars]
ashe[piden == "01942244", ..interesting_vars]


# TODO: Figure out duplicates
# 2 observations in 2004 - remove?
ashe[piden == "01024264", ..interesting_vars]
ashe[piden == "01964028", ..interesting_vars]
ashe[piden == "00286597", ..interesting_vars] 
ashe[piden == "00222354", ..interesting_vars]


# Analysis ----------------------
# Characteristics of people who were historically leaders
ashe[last_job %in% c("Senior professionals of educational establishments"), .N, by = occupation
     ][order(-N)]

ashe[grep("Teach", occupation, ignore.case = TRUE)]

# TODO: If ever leader - 
# ever_leader = by piden max(leader_flag = 1)
