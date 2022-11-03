ashe_convert_file <- function(ashe, new_folder, folder, select_columns) {

    file_path <- paste0(folder, "/", ashe)

    cli::cli_alert_info(paste0("Converting ", ashe))

    df <- haven::read_sav(file_path, encoding = "latin1") |>
      janitor::clean_names() |>
      data.table::data.table()

    # Keep columns from select_columns if they exist
    if (!is.null(select_columns)) {
    select_columns <- select_columns[select_columns %in% names(df)]
    df <- df[, ..select_columns]
    }


    save_file_path <- tools::file_path_sans_ext(paste0(new_folder, "/", ashe))


    fst::write_fst(df, path = paste0(save_file_path, ".fst"))



}

#' Convert to fst for faster loading
#'
#' @param folder Folder with ASHE .sav files
#' @param new_folder Name of new folder for files to be converted to
#' @param select_columns Vector of columns to limit conversion
#' @param incremental Only convert files which don't exist in output directory
#'
#' @export
ashe_convert <- function(folder,
                          new_folder,
                          select_columns = NULL,
                          incremental = TRUE) {

  cli::cli_h1(paste0("Converting sav files to fst"))

  files_in_directory <- list.files(folder)

  if (length(files_in_directory) == 0) {
    cli::cli_alert_danger("No Files Found")
    invokeRestart("abort")
  }

  dir.create(file.path(new_folder), showWarnings = FALSE)

  files <- list.files(folder)


  if (incremental == TRUE) {
    # Check what files exist and diff
   exist_in_output <- tools::file_path_sans_ext(list.files(new_folder))
   exist_in_input <- tools::file_path_sans_ext(list.files(folder))
   files_in_directory <- setdiff(exist_in_input, exist_in_output)

   # Reattach extension
   files_in_directory <- paste0(files_in_directory, ".sav")
  }

  invisible(lapply(files_in_directory, ashe_convert_file,
         new_folder = new_folder,
         folder = folder,
         select_columns = select_columns))

}


read_ashe_fst <- function(ashe) {

    cli::cli_alert_info(paste0("Loading ", ashe))

    df <- fst::read_fst(ashe, as.data.table = TRUE)

    return(df)
  }


ashe_load_data <- function(ashe_files) {

  # Read in all files in folder
  files <- lapply(ashe_files, read_ashe_fst)

  # Convert columns which are sometimes numeric and sometimes character

  convert_to_char <- function(dt) {
    char_num_columns <- c("warea", "wttw", "wla", "harea", "hla", "httw")

    char_num_columns <- char_num_columns[char_num_columns %in% names(dt)]

    dt <- dt[, lapply(.SD, as.character), .SDcols = char_num_columns]
  }

  cli::cli_h2("Tidying")
  cli::cli_alert_info("Binding to one dataset")

  lapply(files, convert_to_char)

  names(files) <- basename(ashe_files)

  ashe <- data.table::rbindlist(files, use.names = TRUE, fill = TRUE,
                    idcol = "file")

  return(ashe)

}


ashe_add_descriptions <- function(ashe) {

  coding_frames_path <- system.file("coding_frames/", package = "tidyashe")

  coding_soc00 <- data.table::fread(paste0(coding_frames_path,
                                           "/",
                                           "occ00_coding.csv"),
                                  colClasses = c("numeric", "character"))

  coding_soc10 <- data.table::fread(paste0(coding_frames_path,
                                           "/",
                                           "occ10_coding.csv"),
                                  colClasses = c("numeric", "character"))
                                              

  coding_sic07 <- data.table::fread(paste0(coding_frames_path,
                                           "/",
                                           "sic07_coding.csv"),
                                  colClasses = c("numeric", "character")) |>
    unique()
                                          
  coding_sic03 <- data.table::fread(paste0(coding_frames_path,
                                           "/",
                                           "sic03_coding.csv"),
                                  colClasses = c("numeric", "character")) |>
    unique(by = "sic03")

  # coding_sic03[, .N, by = sic03][order(N)]
  # coding_sic07[, .N, by = sic07][order(N)]

  cli::cli_alert_info("Merging coding frame descriptions")
  
  # TODO: Rewrite using dt[dt2, x := i.y] syntax
  # use if_exists as below too maybe??

  if("occ00" %in% names(ashe)) {
  ashe <- merge(ashe, coding_soc00, all.x = TRUE, by.x = "occ00", by.y = "SOC")
  } else {
    ashe[, OCCUPATION00_DESCRIPTION := NA_character_]
  }
  if("occ10" %in% names(ashe)) {
  ashe <- merge(ashe, coding_soc10, all.x = TRUE, by.x = "occ10", by.y = "SOC")
  } else {
    ashe[, OCCUPATION10_DESCRIPTION := NA_character_]
  }

  if("sic03" %in% names(ashe)) {
  ashe <- merge(ashe, coding_sic03, all.x = TRUE, by.x = "sic03", by.y = "sic03")
  } else {
    ashe[, SIC03_DESCRIPTION := NA_character_]
  }

  if("sic07" %in% names(ashe)) {
  ashe <- merge(ashe, coding_sic07, all.x = TRUE, by.x = "sic07", by.y = "sic07")
  } else {
    ashe[, SIC07_DESCRIPTION := NA_character_]
  }

  ashe[, occupation := data.table::fcase(year %in% 1997:2010, OCCUPATION00_DESCRIPTION,
                                         year %in% 2011:2021, OCCUPATION10_DESCRIPTION)]

  ashe[, industry := data.table::fcase(year %in% 1997:2008, SIC03_DESCRIPTION,
                                         year %in% 2009:2021, SIC07_DESCRIPTION)]

  # Remove old occupation descriptions and turn to factor
  ashe[, c("OCCUPATION10_DESCRIPTION",
           "OCCUPATION00_DESCRIPTION",
           "SIC07_DESCRIPTION",
           "SIC03_DESCRIPTION") := NULL]
  
  ashe[, occupation := factor(occupation)]
  ashe[, industry := factor(industry)]
  # TODO: Change to if exists

  convert_to_factor_if_exists(ashe, "file")
  convert_to_factor_if_exists(ashe, "occ00")
  convert_to_factor_if_exists(ashe, "occ10")
  convert_to_factor_if_exists(ashe, "sic07")
  convert_to_factor_if_exists(ashe, "sic03")
  convert_to_factor_if_exists(ashe, "luaref")

  return(ashe)
}

convert_to_factor_if_exists <- function(ashe, column) {

  if (column %in% names(ashe) & ashe[, class(column)] != "factor") {
  ashe[, (column) := as.factor(get(column))]
  }

}


#' Compile to one file
#'
#' @import data.table
#' 
#' @param ashe_folder Folder with the ashe fst files
#' @param select_columns Filter columns which are compiled
#' @param save_to_folder If TRUE, will save the fst file to the folder indicated by the `DATA_DIRECTORY` environment variable. This will enalbe the `ashe_load()` function.
#' 
#' @export
ashe_compile <- function(ashe_folder,
                         select_columns = NULL,
                         save_to_folder = FALSE) {

  cli::cli_h1("Loading in ASHE datasets")

  ashe_files <- paste0(ashe_folder, "/", list.files(ashe_folder))

  ashe <- ashe_load_data(ashe_files)

  # Keep columns from select_columns if they exist
  if (!is.null(select_columns)) {
  select_columns <- select_columns[select_columns %in% names(ashe)]

  select_columns <- c("file", select_columns)

  ashe <- ashe[, ..select_columns]
  }

  ashe <- ashe_add_descriptions(ashe)
  

  # Remove duplicates by important variables

  # TODO: Check if this makes ASHE align to official publications
  duplication_check_variables <- c("piden", "year", "thrs", "age", "ft", "sex",
                                   "gpay", "occ00", "occ10")

  # filter out nonexistent
  duplication_check_variables <- duplication_check_variables[duplication_check_variables %in% names(ashe)]


  # Remove duplicate rows - might not be necessary but the way
  # the files arrive means it's useful
  ashe <- unique(ashe, by = duplication_check_variables)

   # If enabled
  if (save_to_folder == TRUE) {

  cli::cli_alert_info("Saving as fst")
  save_name <- paste0("ashe_data.fst")

  # If DATA_DIRECTORY environment variable is present, save there.
  if (Sys.getenv("DATA_DIRECTORY") != "") {
    fst::write_fst(
      ashe,
      paste0(Sys.getenv("DATA_DIRECTORY"), "/", save_name)
    )

    cli::cli_alert_info("DATA_DIRECTORY environment variable set - saving in folder")
  }

  }

  setcolorder(ashe, c("year", "piden"))

  return(ashe)


}


#' Load ASHE from DATA_DIRECTORY folder
#'
#' To be able to use one file across many projects,
#' set an environment variable called `DATA_DIRECTORY` with a folder path,
#' and use this function to load the data in.
#'
#' @param set_key Set key to YEAR/PIDEN?
#' @param as.data.table Passed to read_fst
#' @param ... Passed to fst::read_fst
#'
#' @export
ashe_load <- function(set_key = 1, as.data.table = TRUE, ...) {

  # Check if DATA_DIRECTORY environment variable is present
  if (Sys.getenv("DATA_DIRECTORY") == "") {
    cli::cli_alert_danger("You don't have the DATA_DIRECTORY environment variable set: see the help for `ashe_compile`.")

    stop()
  }

  read_name <- "ashe_data.fst"

  ashe <- fst::read_fst(paste0(Sys.getenv("DATA_DIRECTORY"), "/", read_name),
    as.data.table = as.data.table,
    ...
  )

  if(set_key == 1){setkey(ashe, year, piden)}

  setcolorder(ashe, c("year", "piden"))

  return(ashe)

}
