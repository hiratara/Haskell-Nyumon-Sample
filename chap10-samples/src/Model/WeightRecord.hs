{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, DataKinds, DeriveGeneric #-}

module Model.WeightRecord
     ( NewWRecord(NewWRecord, nwrUserId, nwrTime, nwrWeight)
     , nwrUserId'
     , nwrTime'
     , nwrWeight'
     , insertNewWRecord
     , selectWRecord
     )
  where

import           Control.Exception         (catch)
import           Data.Functor.ProductIsomorphic as DFP
import qualified Data.Time.LocalTime       as TM
import           Database.HDBC
    ( IConnection
    , SqlError
    , withTransaction
    )
import           Database.HDBC.Query.TH    (makeRelationalRecord)
import qualified Database.HDBC.Record      as DHR
import qualified Database.Relational       as HRR
import qualified Entity.WeightRecord       as WRecord
import           GHC.Generics              (Generic)
import           System.IO                 (hPrint, stderr)

data NewWRecord = NewWRecord
    { nwrUserId :: !Int
    , nwrTime   :: !TM.LocalTime
    , nwrWeight :: !Double
    } deriving (Generic)

makeRelationalRecord ''NewWRecord

insertNewWRecord
  :: IConnection c
  => NewWRecord -> c -> IO Integer
insertNewWRecord wr conn = do
  let ins = HRR.typedInsert WRecord.tableOfWeightRecord piNewWRecord
  withTransaction conn $
    \conn' ->
       DHR.runInsert conn' ins wr `catch`
       \e ->
          do hPrint stderr (e :: SqlError)
             return 0

piNewWRecord :: HRR.Pi WRecord.WeightRecord NewWRecord
piNewWRecord =
  NewWRecord DFP.|$| WRecord.userId' DFP.|*| WRecord.time' DFP.|*|
  WRecord.weight'

selectWRecord
  :: IConnection c
  => Int -> c -> IO [WRecord.WeightRecord]
selectWRecord uid conn = DHR.runQuery conn q uid
  where
    q :: HRR.Query Int WRecord.WeightRecord
    q =
      HRR.relationalQuery . HRR.relation' . HRR.placeholder $
      \ph ->
        do a <- HRR.query WRecord.weightRecord
           HRR.wheres $ a HRR.! WRecord.userId' HRR..=. ph
           HRR.desc $ a HRR.! WRecord.time'
           return a
