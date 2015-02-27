{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Aeson
import GHC.Generics (Generic)
import Data.Time.Clock
import qualified Data.Map as M
import System.IO.Unsafe
import Network.Wreq
import Web.Scotty

type Patient = Int

data HealthData = HealthData {
		dataType :: Int, value :: Double,
		time :: UTCTime, patient :: Patient
	} deriving (Show, Generic)


instance FromJSON HealthData
instance ToJSON HealthData
instance Eq HealthData where
	(HealthData t1 _ tm1 _ ) == (HealthData t2 _ tm2 _) = t1 == t2 && tm1 == tm2

instance Ord HealthData where
	(HealthData _ _ time1 _ ) `compare` (HealthData _ _ time2 _ ) = time1 `compare` time2

type PatientData = [HealthData]
type HealthDB = M.Map Patient PatientData

data Trigger = Trigger {
		triggerType :: Int, targetPatient :: Patient,
		timestamp :: UTCTime
	} deriving (Show, Generic)

type EventTrigger = HealthData -> Trigger
data TriggerThreshold = TriggerThreshold {
		dataTypeId :: Int, thMin :: Double, thMax :: Double,
		patientId :: Maybe Int, triggerId :: Int
	} deriving (Show, Generic)

instance FromJSON TriggerThreshold
instance ToJSON TriggerThreshold

getUniquedataTypes :: PatientData -> PatientData
getUniquedataTypes xs = remove $ sort xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | dataType x1 == dataType x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)


patientState :: Patient -> HealthDB -> Maybe PatientData
patientState p hdb =
	case M.lookup p hdb of
		Nothing -> Nothing
		Just a -> Just $ getUniquedataTypes a

isThresholdSurpassed :: Patient -> Double -> TriggerThreshold -> Bool
isThresholdSurpassed p value threshold =
	(value < thMin threshold || value > thMax threshold) && patientMatches (patientId threshold) p
	where
		patientMatches Nothing _ = True
		patientMatches (Just a) b = a == b

checkThreshold :: HealthData -> TriggerThreshold -> Bool
checkThreshold d th = dataTypeId th == dataType d && (isThresholdSurpassed (patient d) (value d) th)

tryFireEvent :: HealthData -> TriggerThreshold -> Maybe Trigger
tryFireEvent d th =
	if checkThreshold d th
	then Just (Trigger (triggerId th) (patient d) (unsafePerformIO getCurrentTime))
	else Nothing


getEventsForData :: [TriggerThreshold] -> HealthData -> [Trigger]
getEventsForData ths d = [ fromJust x | x <- map (tryFireEvent d) ths, isJust x ]

getEvents ths pData = map (getEventsForData ths) pData

main = scotty 3000 $ do
	get "/:word" $ do
		beam <- param "word"
		html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
