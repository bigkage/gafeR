
# gafeR.R
# Functions to get or post to Google APIs commonly used for
#     Google Apps for Education districts

library(googleAuthR)
library(dplyr)
library(jsonlite)

options("googleAuthR.client_id" = client_id)
options("googleAuthR.client_secret" = client_secret)
options(
  "googleAuthR.scopes.selected" = c(
    "https://www.googleapis.com/auth/classroom.courses",
    "https://www.googleapis.com/auth/admin.directory.user",
    "https://www.googleapis.com/auth/admin.directory.device.chromeos",
    "https://www.googleapis.com/auth/admin.directory.group",
    "https://www.googleapis.com/auth/urlshortener",
    "https://www.googleapis.com/auth/admin.directory.orgunit"
  )
)
options(googleAuthR.rawResponse = FALSE)

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
      
      df_users$creationTime <- as.POSIXct(df_users$creationTime, "%Y-%m-%dT%H:%M:%S", tz="UTC")
      df_users$lastLoginTime <- as.POSIXct(df_users$lastLoginTime, "%Y-%m-%dT%H:%M:%S", tz="UTC")
      
      return(df_users)
    }
  }
}

#https://www.googleapis.com/admin/directory/v1/users
# force password reset on first login
add_user <-
  function(family_name,
           given_name,
           primary_email,
           temp_password) {
    f <-
      gar_api_generator(
        "https://www.googleapis.com/admin/directory/v1",
        "POST",
        path_args = list(users = ""),
        data_parse_function = function(x)
          x$id
      )
    
    body <-
      list(
        name = list(familyName = family_name, givenName = given_name),
        password = temp_password,
        primaryEmail = primary_email,
        changePasswordAtNextLogin = TRUE
      )
    
    # body_json <- toJSON(body)
    
    f(the_body = body)
  }


##### OrgUnits #####

# Get all org units (doesn't appear to be a page max)
# Reference: https://developers.google.com/admin-sdk/directory/v1/reference/orgunits/list#request

get_org_units <- function(customer_ID, org_path) {
  f <-
    gar_api_generator(
      "https://www.googleapis.com/admin/directory/v1",
      "GET",
      path_args = list(customer = "default_customer_ID", orgunits = ""),
      pars_args = list(type = "all")
    )
  
  g_org_units <-
    f(the_body = body,
      path_arguments = list(customer = customer_ID))
  
  return(as.data.frame(g_org_units$content$organizationUnits))
}

# Create a new org unit with the org structure
add_org_unit <- function(customer_ID, org_name, parent_path) {
  #https://www.googleapis.com/admin/directory/v1/customer/{customerId}/orgunits
  
  body = list(name = org_name,
              parentOrgUnitPath = parent_path)
  
  f <-
    gar_api_generator(
      "https://www.googleapis.com/admin/directory/v1",
      "POST",
      path_args = list(customer = "default_customer_ID", orgunits = ""),
      data_parse_function = function(x)
        x$id
    )
  
  f(the_body = body,
    path_arguments = list(customer = customer_ID))
  
}


##### Groups #####
# need to add looping for domains with more than 500 groups
# maybe add optional parameter to return one group based on email address

get_groups <- function(domain) {
  f <-
    gar_api_generator(
      "https://www.googleapis.com/admin/directory/v1/groups",
      "GET",
      pars_args = list(domain = domain,
                       maxResults = "500")
    )
  
  g_groups <- f(the_body = body)
  
  return(as.data.frame(g_groups$content$groups))
  
}


# Get all members of a given group
# Reference: https://developers.google.com/admin-sdk/directory/v1/reference/members/list#request
# API https://www.googleapis.com/admin/directory/v1/groups/groupKey/members
# TODO: need to add looping to accomodate groups with more than 200 members
get_members <- function(group_key) {
  
  page_token <- NULL # used to track next page
  
  repeat {
    # construct and process first time api called
    if (is.null(page_token)) {
      f <-
        gar_api_generator(
          "https://www.googleapis.com/admin/directory/v1",
          "GET",
          pars_args = list(maxResults = "200"),
          path_args = list(groups = "default_group_key",
                           members = "")
        )
      
      g_group_members <-
        f(the_body = body,
          path_arguments = list(groups = group_key))
      
      df_group_members <-
        as.data.frame(g_group_members$content$members)
      
    } else {
      # 2nd+ pages
      f <-
        gar_api_generator(
          "https://www.googleapis.com/admin/directory/v1",
          "GET",
          pars_args = list(maxResults = "200",
                           pageToken = page_token),
          path_args = list(groups = "default_group_key",
                           members = "")
        )
      
      g_group_members <-
        f(the_body = body,
          path_arguments = list(groups = group_key))
      
      group_members_temp <-
        as.data.frame(g_group_members$content$members)
      
      df_group_members <-
        rbind(df_group_members, group_members_temp)
    }
    
    group_members_temp <- NULL
    
    page_token = g_group_members$content$nextPageToken
    
    # done fetching pages
    if (is.null(page_token)) {
      return(df_group_members)
    }
  }
}

add_member <- function(email_address, group_email){
  
  # API https://www.googleapis.com/admin/directory/v1/groups/{groupKey}/members
  
  body = list(
    email = email_address
  )
  
  f <- gar_api_generator("https://www.googleapis.com/admin/directory/v1",
                         "POST",
                         path_args = list(groups = "defaultGroupKey", members = ""),
                         data_parse_function = function(x) x$id)
  
  f(the_body = body, path_arguments = list(groups = group_email))
  
}

##### Classroom #####
# may need to add loop to handle multiple pages of returned classes
# https://developers.google.com/classroom/reference/rest/
# TODO: set columns with date/time to posix data types
# TODO: join with user data to get name of owner in one df

get_classes <- function() {
  f <-
    gar_api_generator("https://classroom.googleapis.com/v1/courses", "GET")
  
  g_classes <- f(the_body = body)
  
  df_classes <- as.data.frame(g_classes$content$courses)
  
  # convert some data types
  df_classes$courseState <- as.factor(df_classes$courseState)
  df_classes$creationTime <- as.POSIXct(df_classes$creationTime, "%Y-%m-%dT%H:%M:%S", tz="UTC")
  df_classes$updateTime <- as.POSIXct(df_classes$updateTime, "%Y-%m-%dT%H:%M:%S", tz="UTC")
  return(df_classes)
}


##### ChromeOS Devices #####
# https://developers.google.com/admin-sdk/directory/v1/reference/chromeosdevices
# fix path arguments

get_chrome_devices <- function(customer_ID) {
  page_token <- NULL # used to track next page
  
  repeat {
    # construct and process first time api called
    if (is.null(page_token)) {
      f <-
        gar_api_generator(
          "https://www.googleapis.com/admin/directory/v1/customer/{customerID}/devices/chromeo",
          "GET",
          pars_args = list(maxResults = "500"),
          path_args = list(customerID = customer_ID)
        )
      
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