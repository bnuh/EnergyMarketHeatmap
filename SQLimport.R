library(RODBC)
conn < - odbcDriverConnect("Driver=SQL Server; Server=xxx.xx.xxx.xx; Database=test_DB; Uid=''; Pwd='';")

#sqlTables(channel)
queryResult <- sqlQuery(conn, "SELECT * FROM TableName")
odbcClose(conn)
dim(queryResult)




library(RODBC)
conn < - odbcConnect(dsn="Energy Market", uid="", pwd="",'sun.jdbc.odbc.JdbcOdbcDriver','jdbc:odbc:Driver={SQL Server};Server=NSQL2;Database=EnergyMarket;Trusted_Connection=YES')
#sqlTables(channel)
queryResult <- sqlQuery(conn, "SELECT * FROM TableName")
odbcClose(conn)
dim(queryResult)

