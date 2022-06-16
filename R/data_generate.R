#' List of Sheets required in the template
template_data_sheets <- c("Hierarchy", "Data", "Indicator", "Apps", "List")

#' Source template Excel file sheets and export to separate RDS files
#'
#' @param template Template resulting data to be exported
#' @param app_path Path to target application directory
export_template_data <- function(template, app_path) {

  for (data in names(template)) {
    rio::export(template[[data]], glue::glue("{app_path}/src/data/{data}.rds"))
  }
}

#' Sheet Column Types
#'
#' Set Excel sheet column types for loading.
#'
#' Set type of columns starting with "Score" and "Cond" to "text".
#'
#' @param sheet template data sheet
#' @examples
#' excel_template_path <- "../TAD STRI/Matrix_Simulator_Template_APEC_scaleback.xlsx"
#' sheet_col_types("Indicator", excel_template_path)
#' sheet_col_types()
sheet_col_types <- function(sheet="Data", excel_template_path) {
    col_types_out <-
        if (sheet == "Indicator") {
            dat <- readxl::read_excel(excel_template_path,
                                      sheet = sheet, na = "",
                                      n_max = 0)
            col_types <-
                names(dat) %>%
                stringr::str_detect("^Score|^Cond") %>%
                ifelse("text", "guess")
            names(col_types) <- names(dat)
            col_types
        } else {
            "guess"
        }
    return(col_types_out)
}

#' Import template Excel file sheets as list
#'
#' @param excel_template_path Path to template excel file.
import_template_data <- function(excel_template_path) {
  template <- list()
  for (sheet in template_data_sheets) {
      template[[sheet]] <-
          readxl::read_excel(
                      excel_template_path, sheet = sheet, na = "",
                      col_types = sheet_col_types(sheet, excel_template_path))
  }
  return(template)
}

#' Import RDS Simulator base files
#'
#' @param path Path to simulator rds files
import_simulator_data <- function(path = "data") {
  template <- list()
  for (sheet in template_data_sheets) {
    template[[sheet]] <- readRDS(glue::glue("{path}/{sheet}.rds"))
  }
  return(template)
}

#' Convert selected columns to upper values
#'
#' @param data Table object
#' @param columns Columns to convert
convert_to_upper <- function(data, columns = colnames(data)[-1]) {
  dplyr::mutate_at(data, columns, toupper)
}

#' Convert empty string to NA
#' 
#' @param x string
convna <- function(x) {
  if (is.character(x)) {
    x <- ifelse(x == "", NA, x)
  }
  return(x)
}

#' Convert empty cell to NA
#'
#' @param data Table object
convert_empty_to_na <- function(data) {
  dplyr::mutate_all(data, convna)
}

#' Convert selected columns to character
#'
#' @param data Table object
#' @param columns Columns to convert
convert_to_character <- function(data, columns = colnames(data)) {
  dplyr::mutate_at(data, columns, as.character)
}

#' Get Apps Value
#'
#' Get value from "Apps" sheet of template.
#'
#' Function to extract metadata item from "Apps" sheet. If not found, return
#' "DEFAULT"
#' @param app_data Application data object created with
#'     \code{import_simulator_data}
#' @param item Character the metadata item(s) of interest, e.g. "Copyright" or
#'     \code{c("About", "Aggregation")}
#' @return Character string
#' @examples
#' app_data <- import_simulator_data(path = file.path(root_dir, "data"))
#' get_apps_value(app_data, items = "Aggregation")
#' get_apps_value(app_data, items = c("Logo", "Aggregation"))

get_apps_value <- function(app_data, items) {
    res <- sapply(items,
                  function(x) {
                      value <- app_data$Apps$Name[app_data$Apps$MetadataName == x]
                      ifelse(length(value)==0, "DEFAULT", value)
                  },
                  USE.NAMES = TRUE)
    return(res)
}

#' Calculate level-based scores
#'
#' The function takes original scores data and levels meta information, and performs
#' aggregation for all weighted levels that were defined in hierarchy Sheet.
#'
#' @param levels Hierarchy levels.
#' @param aggregation_data Data storing scores, and weights for each hierarchy
#'     level.
#' @param levels_meta Named list of hierarchy levels, including "weighted" (e.g.
#'     CH0, CH1, CH2); Note: for PMR aggregation, \code{levels_meta$weighted}
#'     must also contain last value of \code{levels} (e.g. "CH3"); for STRI it
#'     mustn't.
#' @param score_column Column to use for Score values, default: "Score".
#' @param correct_weights Logical set WH columns to zero if score missing (PMR
#'     logic).
#' @param weighted_average Logical divide sum of Score by sum of corrected
#'     weights (PMR logic).
#' @examples
#' e <- rlang::env()
#' load(file = "../example/compute_score_data.rda", envir = e)
#' ## used in ./inst/app/src/lib/all_backend_tables.R
#' Aggregation_Table <- readRDS(file = "../example/Aggregation_Table.rds") # generated in ./vignettes/aggregation.Rmd (based on "./inst/app/src/lib/all_backend_tables.R")
#' levels <- e$levels_meta$all
#' aggregation_levels <- purrr::map((length(levels) - 1):1,~levels[1:(1 + .)])
#' level_score(levels = aggregation_levels[[1]], aggregation_data = Aggregation_Table, levels_meta = e$levels_meta, score_column = "Score")

level_score <- function(levels,
                        aggregation_data,
                        levels_meta,
                        score_column="Score",
                        correct_weights=FALSE,
                        weighted_average=FALSE) {

    aggregation_data_1 <-
        aggregation_data %>%
        dplyr::group_by_at(vars(one_of(levels))) %>%
        dplyr::mutate(!!sym(score_column) := !!sym(score_column) * !!sym(get_level_col(dplyr::last(levels), "W")))

    if(correct_weights) {
        aggregation_data_1 <-
            aggregation_data_1 %>%
            dplyr::mutate_at(
                       vars(get_level_col(levels[-1], "W")),  ~ . * ifelse(is.nan(!!sym(score_column)), 0, 1)
                   )
    }

    aggregation_data_out <-
        if (!dplyr::last(levels) %in% levels_meta$weighted) {
            ## last level (CH3) not in levels_meta$weighted, only higher levels (i.e. CH0, CH1 and CH2)
            aggregation_data_1
        } else {
            ## for columns in levels_meta$weighted, calculate sum and group size
            group_n <- aggregation_data_1 %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n)

            aggregation_data_2 <- aggregation_data_1 %>%
                dplyr::summarise_at(vars(score_column, get_level_col(levels[-1], "W")), ~ sum(., na.rm = TRUE))

            if(weighted_average) {
                ## PMR: divide sum of Score by sum of corrected weights
                aggregation_data_2 %>%
                    dplyr::mutate(!!sym(score_column) := !!sym(score_column) / !!sym(get_level_col(dplyr::last(levels), "W")))
            } else {
                ## STRI: divide weights by group size
                aggregation_data_2$group_n <- group_n
                aggregation_data_2 %>%
                    dplyr::mutate_at(vars(get_level_col(levels[-1], "W")), ~ . / group_n) %>%
                    dplyr::select(- group_n)
            }
        }
    return(dplyr::ungroup(aggregation_data_out))
}

#' Aggregate level-based scores
#' 
#' The function takes original scores data and levels meta information, and performs 
#' aggregation for all weighted levels that were defined in hierarchy Sheet.
#' 
#' @param aggregation_data Data storing scores, and weights for each hierarchy level.
#' @param levels_meta Hierarchy levels meta information object.
#' @param score_column Name of the column that stores scores to be aggregated.
aggregate_scores <- function(aggregation_data, levels_meta,
                             method="DEFAULT",
                             ... # additional parameters like "score_column" get
                                 # passed to level_score function
                             ) {
  levels <- levels_meta$all
  aggregation_levels <- purrr::map(
    (length(levels) - 1):1,
    ~levels[1:(1 + .)]
  )
  ## use provided data at first step of iteration
  aggregations <- aggregation_data
  aggregations_list <- list()
  for (levels in aggregation_levels) {
      aggregations_list[[dplyr::last(levels)]] <-
          if (method == "CORRECTEDWEIGHTS") {
              level_score(levels = levels,
                          aggregation_data = aggregations,
                          levels_meta = levels_meta,
                          correct_weights=TRUE,
                          weighted_average=TRUE,
                          ...
                          )
          } else {
              level_score(levels = levels,
                          aggregation_data = aggregations,
                          levels_meta = levels_meta,
                          ...
                          )
          }
    ## update input data with calculation results for subsequent iteration step
    aggregations <- aggregations_list[[dplyr::last(levels)]]

  }
  aggregations_list
}
