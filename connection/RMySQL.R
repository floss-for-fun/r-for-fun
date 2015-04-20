# load library
library(RMySQL)

# connect to database 'test' in localhost, username 'root',
# with no password
mydb <- dbConnect(MySQL(), user='root', password='',
        dbname='test', host='localhost')

# show all tables in database
dbListTables(mydb)

# show all column in table
dbListFields(mydb, 'nama_table')

# get data from table to resultset
rs <- dbSendQuery(mydb, 'SELECT * FROM nama_table')

# fetch all data in resultset to data frame
data <- fetch(rs, n=-1)

# summarize
summary(data)
