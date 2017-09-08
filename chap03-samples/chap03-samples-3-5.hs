module Main (main) where

data Employee = NewEmployee { employeeAge :: Integer
                            , employeeIsManager :: Bool
                            , employeeName :: String
                            } deriving (Show)

employee :: Employee
employee = NewEmployee { employeeName      = "Subhash Khot"
                       , employeeAge       = 39
                       , employeeIsManager = False
                       }

employee' :: Employee
employee' = employee { employeeIsManager = True
                     , employeeAge = employeeAge employee + 1
                     }

main :: IO ()
main = do
  print $ employeeAge employee
  print $ employeeIsManager employee
  print $ employeeName employee

  print $ employeeAge employee'
  print $ employeeIsManager employee'
  print $ employeeName employee'
