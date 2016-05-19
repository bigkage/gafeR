# gafeR
Google Apps for Education API R Interface

Functions to make common API calls to manage users, groups, org units, classroom, and chrome OS devices.

Hide authentication secrets in configs.R file:
```{r}
# Used for APIs regarding devices and org units
customer_ID <- "mycustomerID"

# Used for Oauth
client_id <- "myClientID"
client_secret <- "myClientSecret"
```