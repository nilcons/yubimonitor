-- Copyright 2014 Mihaly Barasz & Gergely Risko
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay, forkFinally)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import qualified Data.Array.MArray as MA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.Word (Word32)
import qualified Graphics.UI.Gtk as Gtk
import HFlags
import Foreign.C.Types (CTime(..))
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import System.Environment
import System.IO.Error
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Time
import Text.Regex.Base.RegexLike
import Text.Regex.Posix.ByteString as RE

defineFlag "log_socket" ("$XDG_RUNTIME_DIR/gnupg/S.scdaemon.log" :: String) "Unix socket path used by scdaemon to log debug messages."
-- End of flags.
return []

--------------------------------------------------------------------------------
-- State

data State
  = State
    { _sActive :: TVar Bool
    , _sConnected :: TVar Bool
    , _sLastActive :: TVar CTime
    , _sIcon :: PixbufIcon
    }

--------------------------------------------------------------------------------
-- scdaemon log processing

logPrefix :: ByteString
logPrefix = "scdaemon\\[[0-9]+\\]: "

matchStart :: ByteString -> Bool
matchStart = matchTest re
  where
    re :: RE.Regex
    re = makeRegex $ logPrefix <> "DBG: apdu_open_reader: "

matchEnd :: ByteString -> Bool
matchEnd = matchTest re
  where
    re :: RE.Regex
    re = makeRegex $ logPrefix <> "operation [a-z]+ result"

newConnection :: State -> IO ()
newConnection (_sIcon -> icon) =
  do putStrLn "\nNew connection..."
     updateIcon icon cConnected

endConnection :: State -> IO ()
endConnection (_sIcon -> icon) =
  do updateIcon icon cDisconnected
     putStrLn "Disconnected...\n"

processLog :: State -> ByteString -> IO ()
processLog state msg = do
  putStr " -> " >> print msg
  case () of
    () | matchStart msg -> do _ <- forkIO $ startBlinking state
                              putStrLn "Start blink"
    () | matchEnd msg -> do putStrLn "Stop blink"
                            stopBlinking state
    () -> return ()


socketPath :: IO String
socketPath | runDirPrefix `isPrefixOf` flags_log_socket =
  do runDir <- getEnv runDirVar
     return $ runDir <> drop (length runDirPrefix - 1) flags_log_socket
           | otherwise = return flags_log_socket
  where
    runDirVar = "XDG_RUNTIME_DIR"
    runDirPrefix = "$" <> runDirVar <> "/"

runLogSocket :: State -> IO ()
runLogSocket state = do path <- socketPath
                        withSocketsDo $ E.bracket (open path) (end path) loop
  where
    unlink path = removeLink path `catchIOError` (\_ -> return ())
    open path = do
      unlink path
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock (SockAddrUnix path)
      listen sock 10
      return sock
    loop sock = forever $ do
      (conn, _) <- accept sock
      newConnection state
      void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
      msg <- recv conn 4096
      if (BS.null msg)
        then endConnection state
        else do processLog state msg
                talk conn
    end path sock = do
      close sock
      unlink path

--------------------------------------------------------------------------------
-- Random helpers

replaceTMVar :: TMVar a -> a -> STM ()
replaceTMVar var v = tryTakeTMVar var >> putTMVar var v

getTime :: IO CTime
getTime = epochTime

--------------------------------------------------------------------------------
-- Status Icon handling

data PixbufIcon
  = PI { _miIcon :: Gtk.StatusIcon
       , _miPixbuf :: TMVar Gtk.Pixbuf
       }

-- Note: Colors are in ABGR (RGBA little-endian)
type Pixel = Word32

createIcon :: IO PixbufIcon
createIcon = do
  icon <- Gtk.statusIconNew
  pixbufVar <- newEmptyTMVarIO
  _ <- Gtk.on icon Gtk.statusIconSizeChanged $ \size -> do
    pixbuf <- Gtk.pixbufNew Gtk.ColorspaceRgb True 8 size size
    atomically $ replaceTMVar pixbufVar pixbuf
    return True

  return $ PI icon pixbufVar

updateIcon :: PixbufIcon -> Pixel -> IO ()
updateIcon (PI icon pixbufVar) color = do
  pixbuf <- atomically $ readTMVar pixbufVar
  drawPixbuf pixbuf color
  Gtk.postGUIAsync $ Gtk.statusIconSetFromPixbuf icon pixbuf

drawPixbuf :: Gtk.Pixbuf -> Pixel -> IO ()
drawPixbuf pb color = do
  -- Clear the pixbuf first
  Gtk.pixbufFill pb 0 0 0 255
  h <- Gtk.pixbufGetHeight pb
  w <- Gtk.pixbufGetWidth pb
  rs <- (`div` 4) <$> Gtk.pixbufGetRowstride pb
  pixels <- Gtk.pixbufGetPixels pb :: IO (Gtk.PixbufData Int Pixel)
  let
    setP (x,y) = MA.writeArray pixels ((h-1-y)*rs + x) color
  mapM_ setP [(x,y) | x <- [2..w-3], y <- [2..h-3],
              -- Art by Kata
              let f = 2*x - y, let b = 2*x + y, f <= 16,
              f >= 11 || (b <= 29 && b >= 24)
             ]

cDisconnected, cConnected, cBlinkHi, cBlinkLo :: Pixel
cDisconnected = 0xff333333
cConnected = 0xff777777
cBlinkHi = 0xff00ff00
cBlinkLo = 0xff005500

startBlinking :: State -> IO ()
startBlinking (State {_sActive = vActive, _sIcon = icon}) =
  do atomically $ writeTVar vActive True
     go 20 True
  where
    go :: Int -> Bool -> IO ()
    go 0 _ = return ()
    go n on = do
      updateIcon icon $ if on then cBlinkHi else cBlinkLo
      threadDelay 500000
      active <- atomically $ readTVar vActive
      when active $ go (n-1) (not on)

stopBlinking :: State -> IO ()
stopBlinking (State {_sActive = vActive, _sIcon = icon}) =
  do atomically $ writeTVar vActive False
     updateIcon icon cConnected


--------------------------------------------------------------------------------

main :: IO ()
main = do
  nonGtkArgs <- Gtk.unsafeInitGUIForThreadedRTS
  [] <- withArgs nonGtkArgs $ $initHFlags "Monitor YubiKey activity with tray icon"

  now <- getTime
  icon <- createIcon
  state <- State <$> newTVarIO False <*> newTVarIO False <*> newTVarIO now <*> pure icon
  _ <- forkIO $ updateIcon icon cDisconnected

  _ <- forkIO $ runLogSocket state

  -- Properly handle Ctrl-C:
  _ <- installHandler sigINT (Catch $ Gtk.postGUIAsync Gtk.mainQuit) Nothing
  Gtk.mainGUI
