context("export")

api <- MixPanel()

test_that("call with no event should result in an error",
          {
            expect_error(
              export(api,
                     params = list( from_date = "2015-05-05",
                                    to_date = "2015-05-05")),
              "event"
            )
          })

test_that("call with no params should result in an error",
          {
            expect_error(
              export(api, event = "event"),
              "request params"
            )
          })

test_that("call with unnamed params list should result in an error",
          {
            expect_error(
              export(api, event = "event", 
                          params = list(from_date = "2015-05-05",
                                        to_date = "2015-05-05",
                                        "test")),
              "named list"
            )
          })

test_that("call without to_date should result in an error",
          {
            expect_error(
              export(api, event="event", 
                          params = list(from_date = "2015-05-05")
                    ),
              "to_date"
            )
          })

test_that("call without from_date should result in an error",
          {
            expect_error(
              export(api, event = "event", 
                          params = list(to_date = "2015-05-05")
              ),
              "from_date"
            )
          })

test_that("call with multiple events should result in an error",
          {
            expect_error(
              export(api, event = c("event1","event2"),
                          params = list( from_date = "2015-05-05",
                                         to_date = "2015-05-05")),
              "single event"
            )
          })
