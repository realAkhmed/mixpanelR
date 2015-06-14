#' MixPanel Data Export API Package
#' @docType package
#' @name mixpanelR
NULL

# See http://r-pkgs.had.co.nz/man.html for roxygen tips

#' MixPanel Data Export API class
#' @docType class
#' @import RCurl digest rjson dplyr methods
#' @slot api_key MixPanel API key
#' @slot api_secret MixPanel API secret
#' @slot END_POINT Default API endpoint (each request may specify a different endpoint if needed)
#' @slot VERSION API version
#' @examples \dontrun{
#' library(mixpanelR)
#' 
#' api <- MixPanel(
#'      api_key = "<API_KEY>",
#'      api_secret = "<API_SECRET>"
#' )
#' }
#' @seealso \code{\link{request,MixPanel-method}, \link{export,MixPanel-method}} 
#' @name MixPanel-class
#' @param ... Constructor parameters
#' @exportClass MixPanel
MixPanel <- setClass(
  "MixPanel",
  
  slots = c(
    api_key = "character",
    api_secret = "character",
    END_POINT = "character",
    VERSION = "character"
  ),
  
  prototype = list(
    api_key = "",
    api_secret = "",
    END_POINT = 'http://mixpanel.com/api',
    VERSION = '2.0'
  ),
  
  validity = function (object)
  {
    return (TRUE)
  }
)

setGeneric(name = "hash_args",
           def = function( obj, ... ) standardGeneric("hash_args")
)

setMethod(f = "hash_args",
          signature = c("MixPanel"),
          definition = function( obj, args=NULL, secret=NULL )
          {
            if (is.null(args)) stop("Must specify hash arguments")
            if (!is.list(args)) stop("Hash arguments must be a list")
            
            args <- lapply( args, 
                            function(x) ifelse(is.recursive(x),
                                               rjson::toJSON(x),
                                               toString(x) )
            )
            
            args_joined <- paste(
              lapply(sort(names(args)), 
                     function(x) paste( 
                       enc2utf8(x),
                       "=",
                       enc2utf8(args[[x]]),
                       sep="") 
              ),
              collapse="")
            
            api_secret <- ifelse(is.null(secret),
                                 obj@api_secret,
                                 secret)
            
            hash <- digest::digest( paste(args_joined, api_secret,sep=""),
                            algo="md5", serialize=FALSE)
            
            return ( hash )
          }
)

setGeneric(name = "unicode_urlencode",
           def = function( obj, ... ) standardGeneric("unicode_urlencode")
)

setMethod(f = "unicode_urlencode",
          signature = c("MixPanel"),
          definition = function( obj, params=NULL )
          {
            if (is.null(params)) stop("Please specify request parameters")
            if (!is.list(params)) stop("Parameters must be a list")

            params <- lapply( params, 
                              function(x) ifelse(is.recursive(x),
                                                 rjson::toJSON(x),
                                                 toString(x) )
            )
            
            params_joined <- paste(
              lapply(sort(names(params)), 
                     function(x)
                       paste( 
                         RCurl::curlEscape( enc2utf8(x) ),
                         "=",
                         RCurl::curlEscape( enc2utf8(params[[x]]) ),
                         sep="") 
              ),
              collapse="&")
            
            return ( params_joined )
          }
)

#' S4 generic dispatch method
#' @param obj Object
#' @param ... arguments for the corresponding method
#' @name request
#' @seealso \code{\link{request,MixPanel-method}}
#' @export
setGeneric(name = "request",
           def = function( obj, ... ) standardGeneric("request")
)

#' MixPanel Data Export API request method
#' @param obj MixPanel object
#' @param methods The list of methods for the API call
#' @param params The list of parameters of the API call
#' @param format Response format (default: json)
#' @param endpoint URL of custom API endpoint
#' @param presigned Specifies whether the request is already signed (default: FALSE)
#' @return The raw text of the response in the requested format (default: JSON)
#' @examples \dontrun{
#' library(mixpanelR)
#' library(rjson)
#' 
#' api <- MixPanel(
#'      api_key = "<API_KEY>",
#'      api_secret = "<API_SECRET>"
#' )
#'
#' raw_response <- request(api, 
#'      methods = list("events"), 
#'      params = list(
#'          event = list("project:successful_donation","project:loaded"),
#'          unit = "hour",
#'          interval = 24,
#'          type = "general"
#'      )
#')
#'
#' data <- fromJSON(raw_response)}
#' @export
setMethod(f = "request",
          signature = c("MixPanel"),
          definition = function( obj, methods=NULL, params=NULL, format='json', endpoint=NULL, presigned=FALSE )
          {
            if (is.null(methods)) stop("Please specify request methods")
            if (is.null(params)) stop("Please specify request parameters")
            if (!is.list(params)) stop("Parameters must be a list")
            if (!(format %in% c("json","csv")))
                stop("Format must be either 'json' or 'csv'")
            if (is.null(names(params)) || any(names(params)==""))
                stop("Parameters must be a named list")
            
            if (!presigned)
            {
              params$sig <- NULL # Erase sig
            
              params <- c(
                api_key = obj@api_key,
                expire = as.integer( Sys.time() ) + 600,   # Grant this request 10 minutes.
                format = format,
                params
              )
            
              params$sig <- hash_args(obj, params)
            }
            
            my_endpoint <- ifelse(is.null(endpoint),
                                obj@END_POINT,
                                endpoint)
            request_url <- 
              paste(
                paste(c(my_endpoint, obj@VERSION, methods),
                      collapse="/"),
                "/?",
                unicode_urlencode(obj, params),
                sep="")
            
            request_HTML <- RCurl::getURL( request_url )
            #.opts=list(timeout.ms=120*100))
            return ( request_HTML )
          }
)


#' S4 generic dispatch method
#' @param obj Object
#' @param ... arguments for the corresponding method
#' @name export
#' @seealso \code{\link{export,MixPanel-method}}
#' @export
setGeneric(name = "export", 
           def = function( obj, ... ) standardGeneric("export")
)

#' High-level API request for exporting events
#' 
#' Produces an \code{export} request for the specified single event
#' and accounting for the specified parameters. Note that the parameters
#' \code{from_date} and \code{to_date} are required.
#' 
#' @param obj MixPanel object
#' @param event The single event to be exported
#' @param params The list of parameters of API export request
#' @return The data.frame containing the exported data
#' @examples \dontrun{
#' library(mixpanelR)
#' 
#' api <- MixPanel(
#'      api_key = "<API_KEY>",
#'      api_secret = "<API_SECRET>"
#' )
#'
#' df <-  export(api,
#'      event = "project:successful_donation",
#'      params = list(from_date = "2015-01-01", 
#'                      to_date = "2015-03-05")
#' )
#'
#' summary(df)}
#' @export 
setMethod(f = "export",
          signature = c("MixPanel"),
          definition = function( obj, event=NULL, params=NULL )
          {
            if (is.null(event)) stop("Please specify the event to be exported")
            if (length(event)>1)
              stop("Must specify a single event to be exported since only one 
                   data.frame will be produced as the result.")

            if (is.null(params)) stop("Please specify request params")
            if (!is.list(params)) stop("Params must be a list")
            
            if (is.null(names(params)) || any(names(params)==""))
              stop("Parameters must be a named list")

            if (is.null(params$from_date) || is.null(params$to_date))
              stop("Both the from_date and to_date are required inside params for export.")
            
            # Replace events list in params with one event
            # Only allow one event to be exported per export request
            # for safety
            params$event <- list(event)
            
            json_response <- request(obj, 
                                     methods = list("export"), 
                                     params = params,
                                     endpoint ="http://data.mixpanel.com/api",
                                     format = 'json')
            
            # API returns the set of JSONs separated by \n
            # which is not a valid JSON
            # Therefore, we need to assemble them all into the data.frame
            
            # Split the string into a vector of text
            json_vec <- unlist( strsplit(json_response,"\n"))
            
            # Function for flattening the recursive list
            # into the one-level list. Inspired by:
            # https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion
            flatten2 <- function(x) {
              # Get the list structure and length
              lst_struct <- rapply(x, function(x) 1L)
              len <- sum( lst_struct )
              
              # Init the list
              y <- vector('list', len)
              
              # Populate the list
              i <- 0L
              rapply(x, function( x )
              { 
                i <<- i+1L
                y[[i]] <<- x 
              })
              
              # Assign the names to the list 
              names(y) <- names(lst_struct)
              return(y)
            }
            
            # Parse the JSON in each element into a small df
            df_lst <- lapply(json_vec, 
                             function(x) 
                             {
                               # Flatten the list structure produced from JSON
                               y <- tryCatch( rjson::fromJSON(x),
                                              error = function(c)
                                    {
                                      stop( paste("*** Incorrect JSON reported by MixPanel:\n",
                                                  x, "\n",
                                                  "*** which generated the error:\n",
                                                  c) )
                                    })
                                              
                               flat_list <- flatten2( y )
                               
                               # Feed the flattened list into df
                               data.frame( flat_list,
                                           stringsAsFactors=FALSE) 
                             }
            )
            
            # Combine the list into one data.frame fast (dplyr)
            df <- dplyr::rbind_all(df_lst)
            return ( df )
          }
)

#' MixPanel Constructor 
#' @param ... Constructor parameters
#' @aliases NULL 
#' @usage NULL
#' @name MixPanel-constructor
#' @export
MixPanel <- function(...) new("MixPanel",...)

