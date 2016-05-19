# gafeR
Google Apps for Education API R Interface

Functions to make common API calls to manage users, groups, org units, classroom, and chrome OS devices.

This relies on the googleAuthR package [https://github.com/MarkEdmondson1234/googleAuthR](https://github.com/MarkEdmondson1234/googleAuthR). Thank you to Mark Edmondson!

Hide authentication secrets in configs.R file:
```{r}
# Used for APIs regarding devices and org units
customer_ID <- "mycustomerID"

# Used for Oauth
client_id <- "myClientID"
client_secret <- "myClientSecret"

my_domain <- "myGafeDomain"
```