{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Aeson (FromJSON, ToJSON, encode, decode, toJSON)
import GHC.Generics (Generic)
import Data.Time.Clock
import qualified Data.Map as M
import System.IO.Unsafe
import qualified Network.Wreq as R
import Web.Scotty
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent.MVar
import Data.Text.Lazy (toStrict)
import Control.Monad
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Applicative     ((<$>))
import Web.Scotty.Internal.Types
import Data.Text.Internal.Lazy
import Network.Wai.Middleware.RequestLogger


data HealthData = HealthData {
		dataType :: Int, value :: Double,
		time :: UTCTime, patient :: Int
	} deriving (Show, Generic)

instance Eq HealthData where
	(HealthData t1 _ tm1 _ ) == (HealthData t2 _ tm2 _) = t1 == t2 && tm1 == tm2

instance Ord HealthData where
	(HealthData _ _ time1 _ ) `compare` (HealthData _ _ time2 _ ) = time1 `compare` time2

instance FromJSON HealthData
instance ToJSON HealthData

type PatientData = [HealthData]
data PatientJsonData = PatientJsonData { patientEvents :: [HealthData] }


type HealthDB = M.Map Int [HealthData]

data Trigger = Trigger {
		triggerType :: Int, targetPatient :: Int,
		timestamp :: UTCTime
	} deriving (Show, Generic)

instance FromJSON Trigger
instance ToJSON Trigger

type EventTrigger = HealthData -> Trigger
data TriggerThreshold = TriggerThreshold {
		dataTypeId :: Int, thMin :: Double, thMax :: Double,
		patientId :: Maybe Int, triggerId :: Int
	} deriving (Show, Generic)

instance FromJSON TriggerThreshold
instance ToJSON TriggerThreshold

isThresholdSurpassed :: Int -> Double -> TriggerThreshold -> Bool
isThresholdSurpassed p val threshold =
	(val < thMin threshold || val > thMax threshold) && patientMatches (patientId threshold) p
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


getFiredEventsForData :: [TriggerThreshold] -> HealthData -> [Trigger]
getFiredEventsForData ths d = catMaybes $ map (tryFireEvent d) ths

getFiredEvents :: [TriggerThreshold] -> PatientData -> [Trigger]
getFiredEvents ths pData = concat $ map (getFiredEventsForData ths) pData

getEventsDB :: Int -> HealthDB -> [HealthData]
getEventsDB pId hdb =
	case M.lookup pId hdb of
		Just e -> e
		Nothing -> []

data EventsGetResponse = EventsGetResponse { success :: Bool, fired :: Int }

main :: IO ()
main = scotty 3000 $ do
	m <- liftIO $ newMVar (M.empty :: HealthDB, [ ] :: [TriggerThreshold])
	middleware logStdoutDev
	post "/thresholds/" $ do
		newTh <- jsonData
		liftIO $ modifyMVar_ m $ \(h, tl) -> return (h, tl ++ newTh)
		(_, thl) <- liftIO $ readMVar m
		json thl
	post "/events/:id" $ do
		pId <- param "id"
		newEvents <- jsonData
		(hdb, thl) <- liftIO $ readMVar m
		let existingEvents = getEventsDB pId hdb
		let updEvents = existingEvents ++ newEvents
		let firedEvents = getFiredEvents thl newEvents
		liftIO $ modifyMVar_ m $ \(h, tl) -> return (M.insert pId updEvents h, tl)
		liftIO $ R.post "http://localhost:3001/notify" (toJSON firedEvents)

		json firedEvents

	get "/thdelete" $ do
		liftIO $ modifyMVar_ m $ \(h, tl) -> return (h, [])
		text "Deleted!"

	get "/date" $ do
		d <- liftIO getCurrentTime
		json $ d

