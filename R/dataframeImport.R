dataframeImportInput <- function(id, formats=c('Excel', 'Copy&Paste', 'Example'), rexp="", selected=NULL) {
  ns <- NS(id)
  l <- list(
    'Excel' = conditionalPanel(
      condition = "input.inputFormat == 'Excel'",
      fileInput(inputId = ns("iFile"), label = NULL, accept= c(".xls",".xlsx")),
      checkboxInput(ns("columnNames"), "First row contains column names"),
      ns = ns
    ),
    'Copy&Paste' = conditionalPanel(
      condition = "input.inputFormat == 'Copy&Paste'",
      textAreaInput(ns('iPaste'), "Paste column of values from table sheet here",""),
      splitLayout(
        radioButtons(ns('dec'), 'Character for decimal points', c("Comma"=",", "Point"="."), inline = TRUE),
        numericInput(ns("nCols"), "Number of Columns:", 1)
      ),
      ns = ns
    ),
    'Example' = conditionalPanel(
      condition = "input.inputFormat == 'Example'",
      selectInput(inputId = ns("exampleData"), label = "Example Data Set:",
                  choices = getExampleData(),
                  multiple = FALSE, selectize = TRUE),
      ns = ns
    )
  )

  tagList(
    c(
      list(
        useShinyjs(),
        radioButtons(ns('inputFormat'), 'Input format', formats, inline = TRUE, selected=selected)
      ),
      l[formats]
    )
  )
}

# Module server function
dataframeImport <- function(input, output, session, setSeed=function(){}) {

  ## copy data in GUI
  copyPasteInput <- reactive({
    x <- unlist(strsplit(as.character(input$iPaste), "\\s"))
    if (input$dec==",") {
      x <- gsub("\\.", "", x)
      x <- gsub(",", ".", x)
    } else {
      x <- gsub("\\,", "", x)
      x <- gsub("'", "", x)
    }
    losses <- as.vector(as.numeric(x))
    if (length(losses)==0) return(NULL)
    if (input$nCols>1) {
      losses <- as.data.frame(matrix(losses, ncol = input$nCols, byrow = TRUE))
      if (all(is.na(losses[1,]))) {
        colnames(losses) <- x[1:input$nCols] #x[1+dim(losses)[2]*(0:(input$nCols-1))]
        losses <- losses[-1,]
      }
      return(losses)
    }
    return(data.frame(Losses=losses))
  })

  ## Choose file from disk
  chooseFile <- reactive({
    inFile <- input$iFile
    if (!is.null(inFile)) {
      return(list(path = inFile$datapath))
    } else {
      return(NULL)
    }
  })

  txtLossInput <- reactive({
    txt <- safeRInput()
    if (is.null(txt)) return(NULL)
    #log2file(txt, "rexpression")
    setSeed()
    result <- try(eval(parse(text=paste("c(", txt,")"))), silent=TRUE)
    if ("try-error" %in% class(result) || !is.numeric(result)) {
      return(NULL)
    } else {
      return(result=result)
    }
  })

  txtLossInputError <- reactive({
    txt <- safeRInput()
    if (is.null(txt)) return(NULL)
    setSeed()
    result <- try(eval(parse(text=paste("c(", txt,")"))), silent=TRUE)
    if ("try-error" %in% class(result)) {
      return(result)
    } else if (!is.numeric(result)) {
      return("Result is not numeric!")
    }
    return(NULL)
  })

  exampleLossInput <- reactive({
    dataSet <- input$exampleData
    #log2file(dataSet, "example-data")
    result <- getExampleData(dataSet)
    return(result=result)
  })

  readDataFrame <- reactive({
    #log2file(input$inputFormat, "loading")
    if (is.null(input$inputFormat)) {
      validate( need(FALSE, "Please select a data set") )
      return(NULL)
    }
    if (input$inputFormat == 'Excel') {
      objFile <- chooseFile()
      if (!is.null(objFile)){
        wb <- try(loadWorkbook(objFile$path), silent = FALSE)
        if (!("try-error" %in% class(wb))) {
          datdf <- readWorkbook(wb, sheet = 1, colNames = input$columnNames)
        } else {
          print(wb)
          datdf <- as.data.frame(read_excel(objFile$path, col_names = input$columnNames))
        }
        return(datdf)
      }
    } else if (input$inputFormat == 'Copy&Paste') {
      pasteData <- try(copyPasteInput(), silent=TRUE)
      if (!("try-error" %in% class(pasteData)) && is.data.frame(pasteData) && !is.null(pasteData)) {
        return(pasteData)
      }
    } else if (input$inputFormat == 'R Expression') {
      txtData <- txtLossInput()
      if (!is.null(txtData)) {
        return(as.data.frame(txtData))
      }
    } else if (input$inputFormat == 'Example') {
      exampleData <- exampleLossInput()
      if (!is.null(exampleData)) {
        return(as.data.frame(exampleData))
      }
    }
    validate(
      need(FALSE, "Please select a data set")
    )
    return(NULL)
  })

  return(readDataFrame)
}

