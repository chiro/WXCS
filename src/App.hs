{-# LANGUAGE TemplateHaskell, GADTs, OverloadedStrings #-}
module App (
  app
  ) where

import Control.Monad (when, liftM)
import Control.Monad.IO.Class

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text())
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.List

import qualified Database.Persist.Sqlite as Sq

import Network.HTTP.Types.Status (status401, status500)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (FileInfo(..))

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty hiding (source, status)
import qualified Web.Scotty as WS

import Model
import ModelTypes
import OnlineJudge
import Utils

cssClass :: JudgeStatus -> String
cssClass Accepted = "AC"
cssClass WrongAnswer = "WA"
cssClass RuntimeError = "RE"
cssClass TimeLimitExceeded = "TLE"
cssClass MemoryLimitExceeded = "MLE"
cssClass OutputLimitExceeded = "OLE"
cssClass CompileError = "CE"
cssClass SubmissionError = "CE"
cssClass Pending = "CE"
cssClass Running = "CE"

getUsers :: [Submit] -> [String]
getUsers = nub . map submitUserId

getACTime :: [Submit] -> String -> String -> Int
getACTime statuses user pid =
  if length st == 0 then 0 else 1
  where st = filter (\status -> eqUser status && eqProblem status && submitJudge status == Accepted) statuses
        eqUser s = submitUserId s == user
        eqProblem s = submitProblemId s == filter ('\r'/=) pid

getWA :: [Submit] -> String -> String -> Int
getWA statuses user pid = length st
  where st = filter (\status -> eqUser status && eqProblem status && submitJudge status /= Accepted) statuses
        eqUser s = submitUserId s == user
        eqProblem s = submitProblemId s == filter ('\r'/=) pid

user_status :: [Submit] -> [String] -> String -> (String, [(Int, Int)], Int, Int)
user_status status problem_list user =
  (user, zip wa ac, length $ filter (>0) ac, sum ac)
  where ac = map (getACTime status user) problem_list
        wa = map (getWA status user) problem_list

rank_standings :: [(String, [(Int, Int)], Int, Int)]
                  -> [(Int, String, [(Int, Int)], Int, Int)]
rank_standings l =
  zip5 [1..] name state ac wa
  where (name, state, ac, wa) = unzip4 l

getByIntId :: (Integral i, Sq.PersistEntity val, Sq.PersistStore m,
               Sq.PersistEntityBackend val ~ Sq.PersistMonadBackend m)
              => i -> m (Maybe val)
getByIntId i = Sq.get $ Sq.Key $ Sq.PersistInt64 (fromIntegral i)

getId :: Sq.Entity a -> Text
getId ent = let Right key = Sq.fromPersistValue . Sq.unKey $ Sq.entityKey ent in key

mkContestTuple :: Sq.Entity Contest -> (Text, String, JudgeType, ZonedTime, ZonedTime, String)
mkContestTuple entity =
  let contest = Sq.entityVal entity in
  (getId entity, contestName contest, contestJudgeType contest,
   contestStart contest, contestEnd contest, contestSetter contest)

entityToTuple :: Sq.Entity a -> (Text, a)
entityToTuple ent = (getId ent, Sq.entityVal ent)

forwardedUserKey :: TL.Text
forwardedUserKey = "X-Forwarded-User"

-- Handler for exceptions.
handleEx :: TL.Text -> ActionM ()
handleEx "Unauthorized" = do
  WS.status status401
  html $ "<h1>You are not logined.</h1>"
handleEx message = do
  WS.status status500
  text message

-- Get remote user.
getUser :: ActionM String
getUser = do
 user' <- reqHeader forwardedUserKey
 when (isNothing user') $ raise "Unauthorized"
 return . TL.unpack $ fromJust user'

app :: Text -> ScottyM ()
app db_file = do
  middleware logStdoutDev
  middleware $ staticPolicy $ addBase "static"
    >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")
  defaultHandler handleEx

  get "/" $ do
    user_id <- getUser
    current_time <- liftIO getLocalTime
    contests <- liftIO (Sq.runSqlite db_file (Sq.selectList [] []))
                :: ActionM [Sq.Entity Contest]
    let contest_list = map mkContestTuple contests
    html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

  get "/contest/:contest_id" $ do
    user_id <- getUser
    contest_id_ <- param "contest_id" :: ActionM String
    let contest_id = read contest_id_ :: Int
    current_time <- liftIO getLocalTime
    contest' <- liftIO (Sq.runSqlite db_file (getByIntId contest_id)) :: ActionM (Maybe Contest)
    case contest' of
      Nothing -> redirect "/" -- contest not found!
      Just contest -> do
        status_db <- liftIO (Sq.runSqlite db_file (Sq.selectList [] []))
                     :: ActionM [Sq.Entity Submit]
        let contest_type = contestJudgeType contest
        let problem_list = contestProblems contest

        let status_list_ = map Sq.entityVal status_db
        let status_list = filter (\s -> submitContestnumber s == contest_id
                                        && submitJudgeType s == contest_type) status_list_
        let status_ac = map (getACTime status_list user_id) problem_list
        let status_wa = map (getWA status_list user_id) problem_list
        let problems = zip4 problem_list (map (getDescriptionURL contest_type) problem_list)
                       status_ac status_wa

        let users = getUsers status_list
        let standings = map (user_status status_list problem_list) users
        let contest_status = rank_standings standings

        html $ renderHtml $ $(hamletFile "./template/contest.hamlet") undefined

  post "/submit" $ do
    user_id <- getUser
    currentTime <- liftIO getZonedTime
    judgeType <- liftM read $ param "type" :: ActionM JudgeType
    problemId <- liftM (filter $ not . isSpace) $ param "problem" :: ActionM String
    lang <- param "language" :: ActionM String
    contestId <- param "contest" :: ActionM Int
    code' <- param "code" :: ActionM String
    codefiles <- files
    let code = foldl (\acc (_,file) -> acc ++ unpack (fileContent file)) code' codefiles
    let size = show $ length code
    _ <- liftIO $ Sq.runSqlite db_file $ do
      Sq.insert $ Submit currentTime user_id judgeType contestId
        problemId Pending "" "" size lang code
    redirect "status"

  get "/setcontest" $ do
    user_id <- getUser
    current_time <- liftIO getLocalTime
    html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined

  post "/setcontest" $ do
    setter <- getUser
    contest_name <- param "name" :: ActionM String
    contest_type <- liftM read $ param "type" :: ActionM JudgeType
    start_time_ <- param "starttime" :: ActionM String
    start_time <- liftIO $ toZonedTime start_time_
    end_time_ <- param "endtime" :: ActionM String
    end_time <- liftIO $ toZonedTime end_time_
    problem <- param "problem" :: ActionM String
    _ <- liftIO $ Sq.runSqlite db_file $ do
      Sq.insert $ Contest contest_name contest_type start_time end_time setter (lines problem)
    redirect "./"

  get "/status" $ do
    user_id <- getUser
    current_time <- liftIO getLocalTime
    status_db <- liftIO (Sq.runSqlite db_file (Sq.selectList [] []))
                 :: ActionM [Sq.Entity Submit]
    let status_list = map entityToTuple status_db
    html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

  get "/findcontest" $ do
    user_id <- getUser
    current_time <- liftIO getLocalTime
    status_db <- liftIO (Sq.runSqlite db_file (Sq.selectList [] []))
                 :: ActionM [Sq.Entity Submit]
    let status_l = map entityToTuple status_db
    contest_id <- param "contest" :: ActionM Int
    let status_list = filter (\(_,s) -> submitContestnumber s == contest_id) status_l
    html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

  get "/user" $ do
    user_id <- getUser
    current_time <- liftIO getLocalTime
    status_db <- liftIO (Sq.runSqlite db_file (Sq.selectList [] []))
                 :: ActionM [Sq.Entity Submit]
    let status_l = map entityToTuple status_db
    name <- param "name" :: ActionM String
    let status_list = filter (\(_,s) -> submitUserId s == name) status_l
    html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

  get "/statistics" $ do
    user_id <- getUser
    current_time <- liftIO getLocalTime
    status_db <- liftIO (Sq.runSqlite db_file (Sq.selectList [] []))
                 :: ActionM [Sq.Entity Submit]
    let status_l = map entityToTuple status_db
    jtype <- liftM read $ param "type" :: ActionM JudgeType
    pid <- param "pid" :: ActionM String
    let status_list = filter (\(_,s) -> submitJudgeType s == jtype && submitProblemId s == pid) status_l
    html $ renderHtml $ $(hamletFile "./template/status.hamlet") undefined

  get "/source/:source_id" $ do
    user_id <- getUser
    source_id <- param "source_id" :: ActionM Int
    current_time <- liftIO getLocalTime
    source' <- liftIO (Sq.runSqlite db_file (getByIntId source_id)) :: ActionM (Maybe Submit)
    case source' of
      Nothing -> redirect "../status" -- source code not found!
      Just source -> do
        let problem_id = submitProblemId source
        let submit_user_id = submitUserId source
        html $ renderHtml $ $(hamletFile "./template/source.hamlet") undefined

  get "/rejudge/:submit_id" $ do
    submit_id <- param "submit_id" :: ActionM Int
    submit' <- liftIO $ Sq.runSqlite db_file (getByIntId submit_id)
    case submit' of
      Nothing -> redirect "../status"
      Just submit -> do
        liftIO $ updateSubmit db_file $ submit { submitJudge = Pending }
        redirect "../status"
