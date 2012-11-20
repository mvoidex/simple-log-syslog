{-# LANGUAGE OverloadedStrings #-}

-- | Module provides syslog logger for simple-log
--
-- >yourFunction :: MonadLog m => m ()
-- >yourFunction = scope "your" $ log Trace "Hello"
-- >
-- >run :: IO ()
-- >run = do
-- >    l <- newLog (fileCfg "log.cfg" 60) [syslog "name"]
-- >    withLog l yourFunction
--
module System.Log.Syslog (
    SyslogMessage(..),
    syslogMsg,
    toSyslog, toSyslog_,
    syslog, syslog_,

    Option(..), Facility(..)
    ) where

import System.Posix.Syslog (Option(..), Facility(..))
import qualified System.Posix.Syslog as Syslog
import qualified Data.Text as T
import System.Log.Base

data SyslogMessage = SyslogMessage {
    syslogPriority :: Syslog.Priority,
    syslogMessage :: String }
        deriving (Eq, Show)

syslogMsg :: Converter SyslogMessage
syslogMsg (Message _ lvl path msg) = SyslogMessage (priority lvl) $ T.unpack $ T.concat [T.intercalate "/" path, "> ", msg] where
    priority Trace = Syslog.Debug
    priority Debug = Syslog.Debug
    priority Info = Syslog.Info
    priority Warning = Syslog.Warning
    priority Error = Syslog.Error
    priority Fatal = Syslog.Critical

toSyslog :: String -> [Option] -> Facility -> Consumer SyslogMessage
toSyslog name ops facility = Consumer withSys where
    withSys f = Syslog.withSyslog name ops facility $ f $ \(SyslogMessage p str) -> Syslog.syslog p str

toSyslog_ :: String -> Consumer SyslogMessage
toSyslog_ name = Consumer withSys where
    withSys f = Syslog.useSyslog name $ f $ \(SyslogMessage p str) -> Syslog.syslog p str

syslog :: String -> [Option] -> Facility -> Logger
syslog name ops facility = logger syslogMsg (toSyslog name ops facility)

syslog_ :: String -> Logger
syslog_ name = logger syslogMsg (toSyslog_ name)
