
options(shiny.maxRequestSize = 100*1024^2)

library(shiny)
library(openxlsx)
library(readxl)
library(data.table)
library(DT)

ui <- fluidPage(
  titlePanel("Medical Code List Checker"),
  sidebarLayout(
    sidebarPanel(
      fileInput("codeFile", "Upload Code List Excel File"),
      uiOutput("sheetSelector"),
      fileInput("dictFile", "Upload MedCode Dictionary Text/Excel File"),
      actionButton("run", "Run Comparison"),
      downloadButton("download", "Download Updated Excel")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      DTOutput("previewTable")
    )
  )
)

server <- function(input, output, session) {
  results <- reactiveVal(NULL)

  observeEvent(input$codeFile, {
    req(input$codeFile)
    code_path <- input$codeFile$datapath
    sheets <- excel_sheets(code_path)

    output$sheetSelector <- renderUI({
      checkboxGroupInput(
        "selectedSheets",
        "Select Sheets to Process:",
        choices = sheets,
        selected = sheets
      )
    })
  })

  observeEvent(input$run, {
    req(input$codeFile, input$dictFile, input$selectedSheets)
    withProgress(message = "Processing sheets...", value = 0, {
      code_path <- input$codeFile$datapath
      dict_path <- input$dictFile$datapath

      output$status <- renderText("Processing...")

      if(dict_path %like% "xlsx") medicalData <- read_excel(dict_path,col_types = "text")
      else medicalData <- fread(dict_path,colClasses = "character")
      setDT(medicalData)
      medicalData$Observations <- as.numeric(medicalData$Observations)
      xlsheets <- input$selectedSheets
      total <- length(xlsheets)

      outbook <- createWorkbook()
      newStyle <- createStyle(fgFill = "yellow")

      summaryList <- list()
      for (i in seq_along(xlsheets)) {
        s <- xlsheets[i]
        incProgress(1/total, detail = paste("Processing", s))

        dat <- read_excel(code_path, sheet = s,col_types = "text")
        setDT(dat)

        dat2 <- medicalData[
          (SnomedCTConceptId %in% dat$SnomedCTConceptId) &
          !(MedCodeId %in% dat$MedCodeId) &
          !(Term %in% dat$Term)
        ]
        dat2 <- dat2[order(Observations, decreasing = TRUE)]

        summaryList[[s]] <- nrow(dat2)

        combined <- rbindlist(list(old = dat, new = dat2), idcol = "Source", fill = TRUE)

        addWorksheet(outbook, sheetName = s)
        writeData(outbook, s, combined)
        addStyle(outbook, s, newStyle,
                 rows = (nrow(dat) + 2):(nrow(dat) + nrow(dat2) + 1),
                 cols = 1:10, gridExpand = TRUE)
      }

      tmpfile <- tempfile(fileext = ".xlsx")
      saveWorkbook(outbook, tmpfile, overwrite = TRUE)
      results(tmpfile)

      summaryDT <- data.table(
        `Sheet Name` = names(summaryList),
        `New Rows Found` = unlist(summaryList)
      )
      output$previewTable <- renderDT(summaryDT)
      output$status <- renderText("Done! You can download the results below.")
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("updatedCodes_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(results())
      file.copy(results(), file)
    }
  )
}

shinyApp(ui, server)
