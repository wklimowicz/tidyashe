library(data.table)
library(tictoc)
library(fst)
# library(ggplot2)
# library(prettyunits)
# library(tidyverse)

setwd(here::here())


get_all_combinations <- function(variables, empty_set = 0) {
combinations <- unlist(lapply(1:length(variables),
  utils::combn,
  x = variables,
  simplify = F
),
recursive = F
)
if (empty_set) combinations <- append("", combinations)
return(combinations)
}


test_dataset <- function(ashe_function, duplication_check, true_mean) {

  stopifnot(nrow(ashe) == 177930)

# variables <- c("a", "b", "c", "e")

# combinations <- get_all_combinations(variables)


# ashe_function <- unique(ashe_function, by = duplication_check)

# tests <- function(vars) {
data_frame_of_combinations <- ashe_function[adr == 1 &
        lopf == 2 &
        wgor > 0 &
        age > 15,
   .(median_weekly_pay = matrixStats::weightedMedian(gpay,
                                                           w = calwght, na.rm = TRUE), 
                    mean_weekly_pay = weighted.mean(gpay, w = calwght, na.rm = TRUE),
                    n = sum(calwght))]
# }


# list_of_combinations <- lapply(combinations, tests)

# data_frame_of_combinations <- rbindlist(list_of_combinations,
#                                         use.names = TRUE,
#                                         fill = TRUE)

mean_outcome <- data_frame_of_combinations[, mean_weekly_pay]
median_outcome <- data_frame_of_combinations[, median_weekly_pay]

# print(round(mean_outcome, 1))


# print(true_mean %in% round(mean_outcome, 1))
# print(true_median %in% round(median_outcome, 1))

correct_condition <- (true_mean %in% round(mean_outcome, 1))
# | (true_median %in% round(median_outcome, 1))

if (correct_condition) {
   print(data_frame_of_combinations[round(mean_weekly_pay, 1) == true_mean])
         # round(median_weekly_pay, 1) == true_median])
    print(duplication_check)
}

print(correct_condition)
return(correct_condition)


}


ashe <- read_fst("../data_ashe_fst/ashe_2019_2020_gb_v18.fst", as.data.table = TRUE)
ashe <- ashe[year == 2019]
# & age %in% c(18:21)

# ashe[, `:=`(a = adr == 1,
#     b = lop == 2,
#     c = wgor > 0,
#     d = sjd == 1,
#     e = agp > 0]


  duplication_check_variables <- c("piden", "thrs", "age", "ft", "sex",
                                   "gpay",  "occ10", "serno")
  

  # duplication_check_variables <- c("gpay", "occ10")


  duplication_check_list <- get_all_combinations(duplication_check_variables)

s <- lapply(duplication_check_list,
            test_dataset,
            ashe_function = ashe,
            true_mean = 571.2
)

xh

# Revised ------------
# true_mean <- 571.7 Weekly pay gross - Revised
# median = 479.1
# n = 26703

# Excluding overtime
#  558.4
# 464.1
# 26703

# Including other pay

# 546.6 
# 454.5
# 26703

# Provisional ---------------

# true_mean <- 571.2 Weekly pay gross - Revised
# median = 479.1
# n = 26704

ashe

        a median_weekly_pay mean_weekly_pay        n
   <lgcl>             <num>           <num>    <num>
1:     NA          478.7413        571.2135 25976672
2:     NA          478.7413        571.2135 25976672
        b      c      e
   <lgcl> <lgcl> <lgcl>
1:   TRUE     NA     NA
2:   TRUE   TRUE     NA
[1] c("thrs",  "age",   "sex",   "gpay",  "occ10")
[1] "thrs"  "age"   "ft"    "sex"   "gpay"  "occ10"

# Excluding overtime
#  557.9
# 463.6
# 26704

        a median_weekly_pay mean_weekly_pay        n
   <lgcl>             <num>           <num>    <num>
1:     NA          460.5155         557.887 24833035
        b      c      e
   <lgcl> <lgcl> <lgcl>
1:     NA   TRUE     NA
[1] "ft"    "sex"   "gpay"  "occ10"

# Including other pay

# 546.1 
# 454.0
# 26704


# true_median <- 479.1

s <- as.logical(s)

test_dataset(ashe, duplication_check_list[[1]])


ashe2 <- unique(ashe, by = c("serno"))

ashe[, .N, by = piden][N == 9]

ashe[piden == "01080873"]

  ashe2[age %in% 18:21 &
       adr == 1 &
        lop == 2 &
        wgor > 0,
    .(median_weekly_pay = matrixStats::weightedMedian(gpay,
                                                           w = calwght, na.rm = TRUE), 
                    mean_weekly_pay = weighted.mean(gpay, w = calwght, na.rm = TRUE),
                    n = sum(calwght, na.rm = T))]





# Results
# mean exact, median  within 0.5
# [1] "thrs"  "age"   "sex"   "gpay"  "occ10" 
# b + c
# [1] "thrs"  "age"   "ft"    "sex"   "gpay"  "occ10"
# b + c




# -----------------------------------


# Setup ----------


# thrs # Hours
# year # Year
# age # Age
# SEX
# occ10
# gweight
# gpay

ashe_summarise_salary <- function(ashe, ...) {

  ashe %>%
    mutate(Wpay = gpay * calwght,
           Wthrs = thrs * calwght,) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
    # median_weekly_pay2 = median(.data$gpay, na.rm = TRUE),
    # median_weekly_pay = matrixStats::weightedMedian(.data$gpay, w = .data$calwght, na.rm = TRUE),
    mean_weekly_pay2 = mean(.data$gpay, na.rm = TRUE),
    mean_weekly_pay = weighted.mean(.data$gpay, w = .data$calwght, na.rm = TRUE),
    # mean_hourly_pay = mean(Wpay)/mean(Wthrs)
    sum_pay = sum(gpay * calwght),
    sum_hrs = sum(thrs * calwght)
    ) %>%
    dplyr::mutate(
                  mean_hourly = sum_pay/sum_hrs
    ) %>%
    select(-starts_with("sum")) %>%
    select(mean_weekly_pay)

}

