
# NOT FINISHED
# Add Students from Skyward to GAFE

# Export all students from Skyward who are active and don't have disable acct flag in Skyward set to TRUE

source('gafeR.R')

# get list of groups
groups <- get_groups(my_domain)

# get list of org units
org_units <- get_org_units(customer_ID)

# get list of users in GAFE
users <- get_users(my_domain)

# import students from skyward file
students_skyward <- read.delim("~/R/gafeR/student_export.txt")

# prep data
# only active students
students_skyward <- filter(students_skyward, Student.Status == "A")

# disable flag is NO
students_skyward <- filter(students_skyward, Temp_Disable_AD_Account != "Yes")

# inter join between skyward list and GAFE list
# TODO: Check the new export file from skyward
students_to_add <- anti_join(students_skyward, users, by = c("School.Email.Address" = "primaryEmail"))

# create column that defines groups and org units based on school, gradelevel, teacher (for elem)

# check for existance of group

# check for existance of org unit

# create each user