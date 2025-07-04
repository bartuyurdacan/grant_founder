# Grant Crawler Shiny App

**Domain-bounded BFS crawler for extracting grant-related links**

## Description

This Shiny application crawls specified domains to detect and collect pages related to grants, funding opportunities, and eligibility information. It performs a breadth-first search (BFS) up to a user-defined depth, aggregates results, and generates both Excel and HTML reports.

## Features

- Upload a list of seed URLs via Excel (`.xlsx`) or enter URLs manually
- Configure maximum crawl depth (number of clicks)
- Detect pages containing grant keywords, funding amounts, and eligibility phrases
- Aggregate results by domain and highlight grant pages
- Download consolidated results as `.xlsx` or `.html`
- Interactive DataTables for browsing and copying results (including SQL INSERT statements)

## Prerequisites

- R >= 4.0
- RStudio (recommended)
- Internet access (or configured proxy)

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/<your-username>/grant-crawler-shiny.git
   cd grant-crawler-shiny
   ```
2. Install required R packages:
   ```r
   install.packages(c(
     "shiny", "shinyWidgets", "readxl", "openxlsx",
     "httr", "rvest", "xml2", "dplyr", "stringr", "purrr",
     "DT", "htmlwidgets", "htmltools", "urltools", "glue"
   ))
   ```

## Configuration (Proxy)

If you are behind a corporate proxy, set environment variables **before** launching the app:

```r
Sys.setenv(
  http_proxy  = "http://username:password@proxy.company.com:8080",
  https_proxy = "http://username:password@proxy.company.com:8080"
)
```

Or add these lines to your `~/.Renviron` for persistence.

## Usage

1. Run the app from R:
   ```r
   library(shiny)
   runApp()
   ```
2. In the UI:
   - **Upload Excel**: Select your `.xlsx` file containing a `grantforward_url` column
   - **Or enter URLs manually**: One URL per line
   - **Max crawl depth**: Number of link levels to traverse
   - Click **Start Checking** to begin crawling
3. View results in the **All Results** and **Grant INSERTs** tabs
4. Download consolidated output via **Download Results** buttons

## App Structure

```
app.R             # Single-file Shiny app (UI + server)
README.md         # This file
```

## Dependencies

- shiny
- shinyWidgets
- readxl, openxlsx
- httr, rvest, xml2
- dplyr, stringr, purrr, glue
- DT, htmlwidgets, htmltools
- urltools

## License

MIT License. See LICENSE file for details.

## Author

Fikret Bartu Yurdacan\
Statistical & Data Science Consulting

