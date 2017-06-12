function(input, output, session) {
  
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  pkgStream <- packageStream(session)
  
  
  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 5
  
  # pkgData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  pkgData <- packageData(pkgStream, maxAgeSecs)
  
  # dlCount dummy function
  dlCount <- function(){10}
  
  # usrCount is a reactive expression dummy
  # usrCount <- userCount(pkgStream)
  usrCount <-function(){"$100,000"}

  
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())
  
  output$npp <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - startTime
    # downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)
    downloadRate <- function(){8.5}
    
    valueBox(
      value = downloadRate(),
      subtitle = "Net Promoter's Score",
      icon = icon("area-chart"),
      # color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
      color = "yellow"
    )
  })
  
  output$cltv <- renderValueBox({
    valueBox(
      value = dlCount(), # function that returns the 
      subtitle = "CLTV",
      icon = icon("users")
    )
  })
  
  output$investment <- renderValueBox({
    valueBox(
      usrCount(),
      "Total Investment",
      icon = icon("dollar")
    )
  })
  
  output$packagePlot <- renderBubbles({
    if (nrow(pkgData()) == 0)
      return()
    
    order <- unique(pkgData()$package)
    df <- pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)
    
    bubbles(df$n, df$package, key = df$package)
  })
  
  output$packageTable <- renderTable({
    pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      mutate(percentage = n / nrow(pkgData()) * 100) %>%
      select("Package name" = package, "% of downloads" = percentage) %>%
      as.data.frame() %>%
      head(15)
  }, digits = 1)
  
  output$downloadCsv <- downloadHandler(
    filename = "cranlog.csv",
    content = function(file) {
      write.csv(pkgData(), file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(pkgData(), input$maxrows))
    options(orig)
  })
}