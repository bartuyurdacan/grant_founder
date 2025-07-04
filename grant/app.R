# ============================================================================
#  Grant Crawler Shiny App (domain-bounded, BFS) w/ Dynamic grant_list
# ============================================================================

library(shiny)
library(shinyWidgets)
library(readxl)
library(openxlsx)
library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(DT)
library(htmlwidgets)
library(htmltools)
library(urltools)
library(glue)

# ───────────────────────────── UI ────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Grant Link Extractor (Domain Crawler)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel (.xlsx) with 'grantforward_url' column", accept = ".xlsx"),
      textAreaInput("manual_urls", "Or enter URLs manually (one per line):",
                    rows = 6, placeholder = "https://example.edu/research"),
      numericInput("depth", "Max crawl depth (clicks):", value = 1, min = 0, max = 5, step = 1),
      actionButton("check", "Start Checking", class = "btn-primary"),
      br(), br(),
      progressBar(id = "pb", value = 0, total = 100, display_pct = TRUE,
                  status = "info", title = "Overall Progress"),
      br(),
      textOutput("progress_text"),
      br(),
      downloadButton("download",      "Download Results (.xlsx)"),
      downloadButton("download_html", "Download Results (.html)"),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("All Results", DTOutput("results_all")),
        tabPanel("Grant INSERTs", DTOutput("results_grant"))
      )
    )
  )
)

# ─────────────────────────── Helper Functions ───────────────────────────────

scan_single_page <- function(url, grant_kw, eligibility_kw, amount_re, timeout_sec = 15) {
  tryCatch({
    resp <- httr::GET(url,
                      httr::user_agent("Mozilla/5.0 (GrantBot/0.2)"),
                      httr::timeout(timeout_sec),
                      httr::config(ssl_verifypeer = FALSE, proxy = ""))
    if (httr::http_error(resp))
      stop(sprintf("HTTP %s", httr::status_code(resp)))
    page <- httr::content(resp, as = "parsed", encoding = "UTF-8")
    text <- rvest::html_text2(page) %>% tolower()
    is_grant  <- any(str_detect(text, grant_kw))
    has_amt   <- str_detect(text, amount_re)
    has_elig  <- any(str_detect(text, eligibility_kw))
    links <- rvest::html_elements(page, "a") %>% rvest::html_attr("href")
    links <- links[!is.na(links)] %>% url_absolute(url)
    list(info = tibble(url = url,
                       Grant_Found           = is_grant,
                       Amount_Found          = has_amt,
                       Eligibility_Mentioned = has_elig),
         links = links)
  }, error = function(e) {
    list(info = tibble(url = url,
                       Grant_Found           = NA,
                       Amount_Found          = NA,
                       Eligibility_Mentioned = NA,
                       error = e$message),
         links = character())
  })
}

scan_domain <- function(start_url, max_depth, grant_kw, eligibility_kw, amount_re) {
  root_domain <- sub("^www\\.", "", urltools::domain(start_url))
  seen   <- character()
  queue  <- tibble(url = start_url, depth = 0)
  results_list <- list()
  while (nrow(queue) > 0) {
    current <- queue[1, ]; queue <- queue[-1, ]
    u   <- current$url; d <- current$depth
    if (u %in% seen) next
    seen <- c(seen, u)
    res <- scan_single_page(u, grant_kw, eligibility_kw, amount_re)
    results_list[[length(results_list) + 1]] <- res$info
    if (d < max_depth && length(res$links) > 0) {
      domain_links <- res$links[grepl(root_domain, res$links, fixed = TRUE)]
      domain_links <- setdiff(domain_links, seen)
      if (length(domain_links) > 0) {
        queue <- bind_rows(queue,
                           tibble(url = unique(domain_links), depth = d + 1))
      }
    }
  }
  bind_rows(results_list)
}

aggregate_domain_results <- function(df) {
  tibble(
    Original_URL          = df$url[1],
    Grant_Found           = any(df$Grant_Found, na.rm = TRUE),
    Amount_Found          = any(df$Amount_Found, na.rm = TRUE),
    Eligibility_Mentioned = any(df$Eligibility_Mentioned, na.rm = TRUE),
    Grant_Pages           = paste(df$url[df$Grant_Found==TRUE], collapse = ", ")
  )
}

# ───────────────────────────── Server ────────────────────────────────────────
server <- function(input, output, session) {
  # regex tanımları
  grant_kw       <- regex("grant|funding opportunity|research grant|fellowship", ignore_case = TRUE)
  eligibility_kw <- regex("eligibility|who can apply|applicant must", ignore_case = TRUE)
  amount_re      <- regex("[\\$€£]\\s?\\d{1,3}(?:[,\\.]\\d{3})*", ignore_case = TRUE)
  
  # 1) crawl
  results_data <- eventReactive(input$check, {
    urls_excel <- character()
    if (!is.null(input$file)) {
      df_in <- read_excel(input$file$datapath)
      validate(need("grantforward_url" %in% names(df_in),
                    "Excel must contain a 'grantforward_url' column."))
      urls_excel <- df_in$grantforward_url
    }
    manual_urls <- str_split(input$manual_urls, "[\r\n]+")[[1]] %>%
      str_trim() %>% discard(~ .x == "")
    seeds <- unique(c(urls_excel, manual_urls))
    validate(need(length(seeds)>0, "Upload file or enter at least one URL."))
    
    out <- map_df(seq_along(seeds), function(i) {
      df_dom <- scan_domain(seeds[i], input$depth, grant_kw, eligibility_kw, amount_re)
      agg <- aggregate_domain_results(df_dom)
      pct <- round(i/length(seeds)*100)
      updateProgressBar(session, "pb", value = pct)
      output$progress_text <- renderText(sprintf("%d / %d processed (%d%%)",
                                                 i, length(seeds), pct))
      agg
    })
    out
  })
  
  # 2) All Results sekmesi
  output$results_all <- renderDT({
    df <- results_data()
    validate(need(nrow(df)>0, "No data to display"))
    datatable(df, escape=FALSE, rownames=FALSE,
              options=list(pageLength=10, autoWidth=TRUE),
              colnames=c("Original URL","Grant Found?","Amount?","Eligibility?","Grant Pages")) %>%
      formatStyle('Grant_Found',
                  backgroundColor = styleEqual(c(TRUE,FALSE), c('#dff0d8','#f2dede')))
  })
  
  # 3) Grant INSERTs sekmesi: yalnızca Grant_Found == TRUE
  output$results_grant <- renderDT({
    df <- results_data() %>%
      filter(Grant_Found==TRUE) %>%
      mutate(
        # Grant_Pages içindeki her URL'den path bölümünü al
        patterns = str_split(Grant_Pages, ",\\s*"),
        grant_list = map_chr(patterns, ~ {
          paths <- map_chr(.x, ~ url_parse(.x)$path)
          paths <- paths[paths != ""]
          paths <- unique(paths)
          glue("ARRAY['{paste(paths, collapse=\"','\")}']")
        }),
        INSERT = glue(
          "INSERT INTO opportunity_process_links VALUES ('{Original_URL}', FALSE, NULL, now(), NULL, FALSE, {grant_list}, NULL);"
        )
      ) %>%
      select(Original_URL, grant_list, INSERT)
    
    validate(need(nrow(df)>0, "No grants found to insert."))
    
    datatable(df, escape=FALSE, rownames=FALSE,
              options=list(pageLength=10, autoWidth=TRUE),
              colnames=c("Original URL","grant_list","SQL INSERT"))
  })
  
  # 4) Download handlers (full results)
  output$download <- downloadHandler(
    filename = function() sprintf("grant_results_%s.xlsx", Sys.Date()),
    content  = function(file) write.xlsx(results_data(), file)
  )
  output$download_html <- downloadHandler(
    filename = function() sprintf("grant_results_%s.html", Sys.Date()),
    content  = function(file) {
      widget <- datatable(results_data(), escape=FALSE, rownames=FALSE,
                          options=list(pageLength=10, autoWidth=TRUE))
      saveWidget(widget, file, selfcontained=TRUE)
    }
  )
}

# ───────────────────────────── Launch App ───────────────────────────────────
shinyApp(ui, server)
