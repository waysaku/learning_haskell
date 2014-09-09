import Control.Monad 
import Database.HDBC 
import Database.HDBC.MySQL 

main = do 
  conn <- connectMySQL defaultMySQLConnectInfo 
                                      {  mysqlHost     = "127.0.0.1", 
                                         mysqlPort     = 3306,
                                         mysqlDatabase = "conv_mng",
                                         mysqlUser     = "root", 
                                         mysqlPassword = "rootpasswd" 
                                      }
  rows <- quickQuery' conn "SELECT 1 + 1 from mst_template" []
  forM_ rows $ \row -> putStrLn $ show row
