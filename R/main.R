#' Open and download cheatsheet in the local browser
#'
#' @param entry One row from getListOfCheatsheets()
#'
#' @examples openCheatsheet(getListOfCheatsheets()[0])
openCheatsheet <- function(entry) {
  temp <- paste0(tempdir(), "/", entry$name)
  if (!file.exists(temp)) {
    print(paste("Downloading to:", temp))
    download.file(entry$download_url, temp)
  }
  browseURL(temp)
}

# ==== User Interface (Shiny) ====
library(shiny)
library(miniUI)

#' Download a List of available Cheatsheets
#'
#' @return dataframe of available cheatsheets
#' @export
#'
#' @examples getListOfCheatsheets()
getListOfCheatsheets <- function() {
  files <- jsonlite::fromJSON("https://api.github.com/repos/rstudio/cheatsheets/contents/")
  # filter out everything that is not a pdf file
  pdfs <- files[grep(".pdf$", files$name),]
  # drop 0 - Template.pdf at the start
  pdfs <- pdfs[2:nrow(pdfs),]
  # reset row numbers
  rownames(pdfs) <- 1:nrow(pdfs)

  pdfs
}

#' Start shiny gadget to browse cheatsheets
#'
#' This is the actual addin.
#'
#' @export
#'
#' @examples browseCheatsheets()
browseCheatsheets <- function() {
  cheatsheets <- getListOfCheatsheets()

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Browse Cheatsheets"),
    miniContentPanel(
      tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: #75AADB !important; cursor: pointer;}')),
      DT::dataTableOutput("cheatsheetTable", height = "100%")
    )
  )

  server <- function(input, output, session) {

    output$cheatsheetTable <- DT::renderDataTable(cheatsheets[c("name", "url")], server = FALSE, rownames = FALSE, options = list(
      # pageLength = nrow(cheatsheets)
      paging = FALSE
    ))

    observeEvent(input$cheatsheetTable_row_last_clicked, {
      row_selected <- input$cheatsheetTable_row_last_clicked

      if (length(row_selected) > 0) {
        cheatsheet_selected <- cheatsheets[row_selected,]

        openCheatsheet(cheatsheet_selected)
      }
    })

    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- paneViewer(minHeight = 300)
  runGadget(ui, server, viewer = viewer)

}
