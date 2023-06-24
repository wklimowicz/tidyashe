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

# Median salaries

ashe <- fst::read_fst("../ashe_fst_data/ashe_2019_2020_gb_v18.fst",
                      as.data.table = TRUE)

ashe <- fst::read_fst("../ashe_fst_data/ashe_1997_2011_gb_vdfe.fst",
                      as.data.table = TRUE)

ashe[year == 2008 &
     adr == 1 &
     sjd == 1 & 
     # lop == 2 & 
     # wgor != 12 & 
     # agp > 0 &
# serno != "1336120" &
# serno != "1236504" &
# serno != "1142885" &
# piden != "" &
     age > 15 & 
     wgor > 0,
   .(x = matrixStats::weightedMedian(agp, calwght),
     y =  matrixStats::weightedMedian(agp, calwght, ties = "mean"),
     y =  weighted.mean(agp, calwght),
     y =  mean(agp)
     ),
     # k = substr(occ10, 1, 1)]
     k = occ00]

    #1115 78,457		116,434
#1115 prov 79,633		111,760

ashe[occ10 == 1115 & year == 2020 &
     adr == 1 &
     sjd == 1,
     N := .N,
     piden]


ashe[occ10 == 1115 &
     year == 2020 &
     adr == 1 &
     sjd == 1]
 


ashe[occ10 == 1115 &
     year == 2020 &
     adr == 1 &
     sjd == 1,
   .N,
   piden]




ashe[year == 2020 &
     adr == 1 &
     lop == 2 & 
     agp > 0 &
     # sjd == 1 & 
     # wgor != 12 & 
     age > 15 & 
     wgor > 0,
   .(x = matrixStats::weightedMedian(gpay/thrs, calwght),
     y =  matrixStats::weightedMedian(gpay/thrs, calwght, ties = "mean"),
     y =  weighted.mean(gpay/thrs, calwght),
     y =  sum(gpay * calwght) / sum(thrs * calwght)
     ),
     # k = substr(occ10, 1, 1)]
     k = occ10]



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
