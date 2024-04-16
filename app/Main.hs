import AppServer (runServer)
import MyPrelude

main :: IO ()
main = runSimpleApp $ liftIO runServer
