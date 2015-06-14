context("request")

api <- MixPanel()

test_that("website demo requests are formed correctly and returned as expired",
          {
            # Example from https://mixpanel.com/docs/api-documentation/data-export-api
            # which is now expired
            html_response <-
              request(api, 
                      methods = list("events"), 
                      params = list(
                        api_key = "f0aa346688cee071cd85d857285a3464",
                        sig = "02e38a843297d188ee73779ab872cc1e",
                        expire = 1275627103,
                        type = "unique"
                      ),
                      presigned = TRUE
              )
            
            expect_match(html_response, "\"error\": \"Expired request\"")            
          })

test_that("call with no params should result in an error",
          {
            expect_error(
              request(api, methods = list("events")),
              "request parameters"
            )
          })

test_that("call with unnamed params list should result in an error",
          {
            expect_error(
              request(api, methods = list("events"),
                           params = list("test")),
              "named list"
            )
          })
