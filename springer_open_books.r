suppressPackageStartupMessages({
  library("magrittr")
  library("dplyr")
  library("rvest")
})

get_url <- function(page = 1L, content_type = "Book", discipline = "Statistics",
                    include_not_open = "false", just_en = TRUE, series = NULL){
  
  base_url <- paste0('http://link.springer.com/search/page/%s?',
                     'facet-content-type="%s"&',
                     'showAll=%s',
                     if (just_en) '&facet-language="En"' else '')
  
  if (!is.null(series)){
    paste0(base_url, '&facet-series="%s"') %>%
      sprintf(page, content_type, include_not_open, series)
  } else{
    paste0(base_url, '&facet-discipline="%s"') %>%
      sprintf(page, content_type, include_not_open, discipline)
  }
}

get_books <- function(page, ...){
  books <- get_url(page = page, ...) %>%
    read_html() %>%
    html_nodes("#results-list li") %>%
    html_nodes("div.text")
  
  data_frame(title = books %>% html_nodes("a.title") %>% html_text(),
             
             subtitle = books %>% lapply(html_nodes, "p.subtitle") %>%
                          lapply(html_text) %>% sapply(paste, collapse = " & "),
             
             authors = books %>% lapply(html_nodes, "span.authors") %>%
                        lapply(html_nodes, "a") %>% lapply(html_text) %>%
                        sapply(paste, collapse = " & "),
             
             year = books %>% lapply(html_nodes, "span.year") %>%
                      lapply(html_text) %>% sapply(extract, 1),
             
             link = books %>% html_nodes("a.title") %>% html_attr("href")
  ) %>%
    mutate(link = paste0("http://link.springer.com", link),
           pdf = gsub("/book/", "/content/pdf/", link, fixed = TRUE),
           pdf = paste0(pdf, ".pdf"),
           full_title =  ifelse(trimws(subtitle) == "",
                                title, paste(title, subtitle, sep = " - "))
    )
}

get_search_pages <- function(...){
  get_url(page = 1, ...) %>%
    read_html() %>%
    html_nodes("span.number-of-pages") %>%
    extract2(1) %>%
    html_text() %>%
    as.integer()
}

get_all_books <- function(...){
  total_pages <- get_search_pages(...)
  
  1L:total_pages %>%
    lapply(get_books, ...) %>%
    bind_rows()
}

make_md <- function(title, df, file, sep = "\n\n"){
  md_format <- "- [%s, %s %s](%s) [[pdf](%s)]"
  
  md <- df %>% 
    with(sprintf(md_format, full_title, authors, year, link, pdf))
  
  c(title, md) %>%
    cat(file = file, sep = sep)
}

## All books
all_stat_books <- get_all_books()

make_md("## ~~Open~~ Statistics Books on Springer (English only)",
        all_stat_books, "all_stat_books.md")

## Lecture Notes in Statistics
lnss_books <- get_all_books(series = 694)

make_md("## ~~Open~~ Books of the _Lecture Notes in Statistics_ series (English only)",
        lnss_books, "lecture_notes_in_statistics.md")

## Springer Texts in Statistics Series
stss_books <- get_all_books(series = 417)

make_md("## ~~Open~~ Books of the _Springer Texts in Statistics_ series (English only)",
        stss_books, "texts_in_statistics.md")


## To download (could take a long time) you could do something like:
#
# stss_books %>%
#   # Clean the title to make a suitable dest_file name
#   mutate(dest_file = gsub("([[:punct:]]|\\s)+", "_", x = title) %>%
#            tolower() %>% 
#            stringi::stri_trans_general("latin-ASCII") %>%
#            paste0(".pdf")
#          ) %$%
#   download.file(url = pdf, destfile = dest_file)
