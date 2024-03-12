# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}



#### The next function returns a distribution table and class for a given column in a data frame. We need to define a variable default_data <- "rare"

qt <- function(input_data){   # simple function for listing category variables - wrapper for table function

  s1 <- glue ("df <- {default_data}")
  eval(parse(text = s1))

  if ((input_data %notin% colnames(df)) == TRUE ) {
    print(glue("Column not found in {default_data} dataframe"))

  } else {

    s2 <- glue ("table_x <- table({default_data}$`{input_data}`, useNA = 'always')")
    eval(parse(text = s2))

    s3 <- glue("class_x <- class({default_data}$`{input_data}`)")
    eval(parse(text = s3))

    return_list <- list(distribution = table_x, class = class_x)

    return(return_list)
  }
}


### This function returns a cross table:

qxt <- function(input_data, output_data){ # create simple function to display cross tables

  s1 <- glue ("table_x <- table({default_data}$`{input_data}`, {default_data}$`{output_data}`, useNA = 'always')")
  eval(parse(text = s1))

  return(table_x)

}


# The next function mikedate is a wrapper for the function anydate, which is part of the

# anytime formats - required for the anydate function, which in turn is part of the anytime, function, which will need to be loaded. It is designed to deal with a greater variation in date import formations

# to apply mikedate, you need the following code, given as an example:

#df2$death_date <- lapply(df2$death_date_raw, mikedate) %>% unlist() %>% as.Date(origin="1970-01-01")



mikedate <- function(date_text) {     # this is a robust function for cleaning up variations in date formats.

  removeFormats(c("%m-%d-%Y","%m/%d/%Y %H:%M:%S%f","%m-%e-%Y","%m/%e/%Y %H:%M:%S%f"))
  addFormats(c("%d-%m-%Y","%d/%m/%Y %H:%M:%S%f","%e-%m-%Y","%e/%m/%Y %H:%M:%S%f" ))

  date_text <- str_trim(date_text)
  dash_bar <- if_else(grepl("\\-|\\/|\\s",date_text) == TRUE,1,0)  # these look for any of a -, \ or space: \\b

  if(dash_bar == 1) {
    new_date = anydate(date_text)
  }

  if(dash_bar == 0) {
    i_date = as.integer(date_text)
    new_date = as.Date(i_date, origin="1899-12-30")
  }

  return(new_date)

  #to apply, use code similar to this - df2$death_date <- lapply(df2$death_date_raw, mikedate) %>% unlist() %>% as.Date(origin="1970-01-01")
}




### look for particular text in column names

col_look <- function(partial_text) {

  s1 <- glue("return(grep(partial_text,colnames({default_data}), value = TRUE))")
  eval(parse(text = s1))

}


fmt_pvalue_with_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0(style_pvalue(x), "***"),
    x < 0.01 ~ paste0(style_pvalue(x), "**"),
    x < 0.05 ~ paste0(style_pvalue(x), "*"),
    TRUE ~ style_pvalue(x)
  )
}

# sample comment
