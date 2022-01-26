# Extract PDF tables
# 23-06-2020

library(magrittr)
library(pdftools)

global_sep = ";"
path <- "data/test0/"

pdf_list <- list.files(path = path, pattern = "*.pdf",
                       full.names = TRUE, recursive = TRUE)

trim_page <- function(pg, n1, n2) {
  if (length(n1) > 0) {
    txt <- pg[[1]]
    # Remove header with single "\n" symbol
    txt[n1] <- strsplit(txt[n1], "\n")[[1]][2]
    if (length(n2 > 0)) {
      p_cut <- txt[n1:(n2 - 1)]
    } else {
      p_cut <- txt[n1:length(txt)]
    }
    p_cut <- gsub("\\s{2,}", global_sep, p_cut, perl = TRUE)
    p_cut <- gsub("(?<=[0-9])\\s(?![0-9])", global_sep, p_cut, perl = TRUE)
  } else {
    p_cut <- NULL
  }
  return(as.data.frame(p_cut))
}

extract_table <- function(fn_pdf) {
  dtab <- pdf_data(fn_pdf)
  header <- paste(dtab[[1]]$text[1:4], collapse = " ")
  nr <- nrow(dtab[[1]])
  footer <- paste(dtab[[1]]$text[(nr - 1):nr], collapse = " ")

  text <- pdf_text(fn_pdf)
  # WARNING! Delimiter may change for another OS / pdf !!! Check "\n" or "\r\n"
  pages <- sapply(X = text, FUN = strsplit, split = "\n\n", USE.NAMES = FALSE)
  # Match starting row of data in table
  n_start <- sapply(pages, grep, pattern = "ФИО", USE.NAMES = FALSE)
  # Match ending row of data in table
  n_end <- sapply(pages, grep, pattern = "Итого", USE.NAMES = FALSE)
  pages_cut <- sapply(seq_along(pages), function(np) {
    trim_page(pages[np], n_start[[np]], n_end[[np]])
  })
  fulltable <-
    dplyr::bind_rows(pages_cut) %>%
    dplyr::transmute(date_list = header,
                     month_year = footer,
                     string_data = p_cut)
  return(list(pdf_name = fn_pdf, text = text,
              header = header, footer = footer,
              fulltable = fulltable))
}

x <- lapply(pdf_list, extract_table)

out_file <- paste0(path, "test0.csv", collapse = "/")

write.table(
  data.frame(
    "Список_от",
    "Месяц_год",
    "Номер",
    "Организация",
    "Табельный",
    "Счет",
    "Получатель",
    "Сумма",
    "Плательщик"
  ),
  file = out_file,
  sep = global_sep,
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

lapply(x, function(y) {
  write.table(
    y$fulltable,
    file = out_file,
    append = TRUE,
    quote = FALSE,
    sep = global_sep,
    row.names = FALSE,
    col.names = FALSE
  )
})
