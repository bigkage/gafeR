


library(googleAuthR)
library(dplyr)
library(jsonlite)

options("googleAuthR.client_id" = client_id)
options("googleAuthR.client_secret" = client_secret)
options(
  "googleAuthR.scopes.selected" = c(
    "https://www.googleapis.com/auth/classroom.courses",
    "https://www.googleapis.com/auth/admin.directory.user",
    "https://www.googleapis.com/auth/admin.directory.device.chromeos"
  )
)

##### Users #####
# return dataframe with all user accounts in the domain
# https://developers.google.com/admin-sdk/directory/v1/reference/users

get_users <- function(domain) {
  page_token <- NULL # used to track next page
  
  # api returns max of 500 records per call, need to loop through until
  #    all records are returned, join all records into one dataframe
  repeat {
    # construct and process first time api called
    if (is.null(page_token)) {
      f <-
        gar_api_generator(
          "https://www.googleapis.com/admin/directory/v1/users",
          "GET",
          pars_args = list(domain = domain,
                           maxResults = "500")
        )
      
      g_users <- f(the_body = body)
      
      df_users <-
        flatten(as.data.frame(g_users$content$users), recursive = TRUE)
      
    } else {
      # 2nd+ times api called to include pageToken
      f <-
        gar_api_generator(
          "https://www.googleapis.com/admin/directory/v1/users",
          "GET",
          pars_args = list(
            domain = domain,
            maxResults = "500",
            pageToken = page_token
          )
        )
      g_users <- f(the_body = body)
      
      users_temp <-
        flatten(as.data.frame(g_users$content$users), recursive = TRUE)
      
      # add new records to final dataframe
      df_users <- bind_rows(df_users, users_temp)
      
    }
    
    users_temp <- NULL
    
    page_token = g_users$content$nextPageToken
    
    # if there is no more next page tokens, that means were done looping
    if (is.null(page_token)) {
      return(df_users)
    }
  }
}


##### Classroom #####
# may need to add loop to handle multiple pages of returned classes
# https://developers.google.com/classroom/reference/rest/

get_classes <- function() {
  f <-
    gar_api_generator("https://classroom.googleapis.com/v1/courses", "GET")
  
  g_classes <- f(the_body = body)
  
  return(as.data.frame(g_classes$content$courses))
}


##### ChromeOS Devices #####
# https://developers.google.com/admin-sdk/directory/v1/reference/chromeosdevices
# need to create loop, current default is 100 returned records at a time

get_chrome_devices <- function(customerID) {
  page_token <- NULL # used to track next page
  
  # construct API URL to include customerID which is found via user query
  # may want to add a way to automatically find the customerID via another query
  apiURL <-
    paste(
      "https://www.googleapis.com/admin/directory/v1/customer/",
      customerID,
      "/devices/chromeos",
      sep = ""
    )
  
  repeat {
    # construct and process first time api called
    if (is.null(page_token)) {
      f <- gar_api_generator(apiURL, "GET",
                             pars_args = list(maxResults = "500"))
      
      g_devices <- f(the_body = body)
      
      page_token = g_devices$content$nextPageToken
      
      df_devices <-
        as.data.frame(g_devices$content$chromeosdevices)
      
    } else {
      f <- gar_api_generator(apiURL,
                             "GET",
                             pars_args = list(maxResults = "500",
                                              pageToken = page_token))
      
      g_devices <- f(the_body = body)
      
      page_token = g_devices$content$nextPageToken
      
      temp_devices <-
        as.data.frame(g_devices$content$chromeosdevices)
      
      df_devices <-
        bind_rows(df_devices, temp_devices)
    }
    
    # if there is no more next page tokens, that means were done looping
    if (is.null(page_token)) {
      return(df_devices)
      
    }
  }
}