# Predic(t)ament Solution
### 1. Installation
In order to run the application you need:
- [R](https://cran.r-project.org/mirrors.html)

All relevant R packages will be automatically installed and imported when the application is executed.
It is also recommended to use an IDLE:
- [R studio](https://www.rstudio.com/products/rstudio/download/#download)

 A local MySQL server is also needed with the database in "dump2.sql". To load this dump into SQL, open a terminal and type "mysql -u 'username' -p dbname < dump2.sql". For windows users, MySQL server will have to be added to environment variables.
 - [MySQL](https://www.mysql.com/downloads/)

The code is set up for a database called "predictamentdb" and a table called "train_data", it is also set up for a user called "root" with a password "root". However all four of these variables can be edited.

---
### 2. Use

If all the above have been installed, download/clone this repository, and in Rstudio open the package "PredictSolution", open "app.R" from the "/R" directory in the package and "Run App". 

The application can also be found at http://35.246.16.145. 

If you would like to see a description of any of the methods, enter into the RStudio console "devtools::document()" and then "?functionName" for more information on each function.

---
### 3. FAQ
This section is currently in development.
