##docs: https://developers.google.com/analytics/trusted-testing/reporting/
hello_world <- function(viewId,
                        start,
                        end,
                        metrics,
                        dimensions=NULL,
                        filters=NULL,
                        segments=NULL,
                        pivots=NULL,
                        samplingLevel="DEFAULT") {
  
  body <- list(
    reportRequests = list(
      list(
        viewId = paste0("ga:",viewId),
        dateRanges = list(
          list(
            startDate = start,
            endDate = end
          )
        ),
        metrics = list(
          list(
            expression = metrics
          )
        )
      )
    )
  )

  url2 <- "https://analytics.googleapis.com/v4/reports:batchGet"

  f <- googleAuth_fetch_generator(url2,
                                  "POST",
                                  data_parse_function = function(x){
                                    x$reports
                                  })

  f(the_body = body)
  
}