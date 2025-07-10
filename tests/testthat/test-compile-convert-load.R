test_that("Converting and compiling runs succesfully", {

  # Use realistic test data
  test_data <- ashe_test_data()

  test_convert_compile <- function() {
    withr::local_envvar(DATA_DIRECTORY = ".")
    ashe_convert(directory = "test", output_directory = "test_rds_data/")
    ashe <- ashe_compile(directory = "test_rds_data", save_to_folder = TRUE)
  }

  withr::with_file(list(fs::dir_create("test"),
    fs::dir_create("test_rds_data"),
    "test/ashe_2011_2017_GB_v12.sav" = haven::write_sav(test_data, "test/ashe_2011_2017_GB_v12.sav"),
    "test/ashe_2018_2019_GB_v12.sav" = haven::write_sav(test_data, "test/ashe_2018_2019_GB_v12.sav"),
    "ashe_data.fst"
  ), {
    expect_error(suppressMessages(test_convert_compile()), NA)
  })


})


test_that("Filter files works", {
  
  test_data <- ashe_test_data()
  
  test_convert_compile <- function() {
    ashe_convert(directory = "test", output_directory = "test_rds_data/")
    ashe <- ashe_compile(directory = "test_rds_data", filter_files = "ashe_2011_2017_GB_v12")
  }

  withr::with_file(
    list(
      fs::dir_create("test"),
      fs::dir_create("test_rds_data"),
      "test/ashe_2011_2017_GB_v12.sav" = haven::write_sav(test_data, "test/ashe_2011_2017_GB_v12.sav"),
      "test/ashe_2018_2019_GB_v12.sav" = haven::write_sav(test_data, "test/ashe_2018_2019_GB_v12.sav")
    ),
    {
      suppressMessages({
        ashe_convert(directory = "test", output_directory = "test_rds_data/")
        ashe <- ashe_compile(directory = "test_rds_data", filter_files = "ashe_2011_2017_GB_v12")
        expect_equal(nrow(ashe), 1)
      })
    }
  )

})

