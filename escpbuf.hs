{-
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Main where

import qualified Data.ByteString.Char8 as BS
import System.IO
import Data.Word

flushBuf = "\ESCJ\0"
reset = "\ESC@"
unidir = "\ESCU\x01"

initCode = reset ++ unidir ++ marginCode 80

{- Used to do this to roll paper.  Now we'll try to move horizontally.
postBuf = "\ESCJ\x46" -- forward roll x/216 in (108/216 in)
preBuf = "\ESCj\x46"  -- reverse roll

-}

prePause = marginCode 90 ++ replicate 10 ' ' ++ flushBuf
postPause = replicate 10 '\b' ++ marginCode 80

main = 
    do hSetBuffering stdin NoBuffering
       hSetBuffering stdout (BlockBuffering Nothing)
       BS.hPut stdout (BS.pack initCode)
       masterReadLoop ""

-- Code to set the right margin to n chargs
-- from the left; n ranges from 1 to 255
marginCode :: Word8 -> String
marginCode chars =
    "\ESCQ" ++ [toEnum . fromIntegral $ chars]

masterReadLoop :: String -> IO ()
masterReadLoop writeBefore =
    do content <- BS.hGet stdin 1
       BS.hPut stdout (BS.pack writeBefore)
       BS.hPut stdout content
       hFlush stdout
       delayLoop 250 prePause (masterReadLoop postPause)

delayLoop :: Int                -- ^ The delay after which pause code kicks in
          -> String             -- ^ Code to send when pause starts
          -> IO ()              -- ^ Function to call after pause has started
          -> IO ()              -- ^ Return value
delayLoop delay prePauseCode pauseFunc =
    do hasInput <- hWaitForInput stdin delay
       if hasInput
          then do content <- BS.hGetNonBlocking stdin 4096
                  if BS.null content
                     then return ()
                     else do BS.hPut stdout content
                             hFlush stdout
                             delayLoop delay prePauseCode pauseFunc
          else do BS.hPut stdout (BS.pack prePause)
                  hFlush stdout
                  pauseFunc
