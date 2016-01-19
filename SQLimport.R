library(RODBC)
conn < - odbcDriverConnect("Driver=SQL Server; Server=xxx.xx.xxx.xx; Database=test_DB; Uid=''; Pwd='';")

#sqlTables(channel)
queryResult <- sqlQuery(conn, "SELECT * FROM TableName")
odbcClose(conn)
dim(queryResult)

