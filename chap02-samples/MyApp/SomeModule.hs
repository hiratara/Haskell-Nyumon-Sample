module MyApp.SomeModule (helloMyApp, byeMyApp) where

helloMyApp :: String -> IO ()
helloMyApp name = putStrLn (helloMessage name)

byeMyApp :: String -> IO ()
byeMyApp name = putStrLn (byeMessage name)

helloMessage :: String -> String
helloMessage name = name ++ ", hello!"

byeMessage :: String -> String
byeMessage name = name ++ ", bye!"
