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

printDelay = 250
moveDelay = 1250

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
       delayLoop

delayLoop :: IO ()
delayLoop =
    do -- wait for a short time for input to become available.
       hasInput <- hWaitForInput stdin printDelay
       if hasInput
          then -- If available, then we read it, send it off, and start again
               do content <- BS.hGetNonBlocking stdin 4096
                  if BS.null content
                     then return ()
                     else do BS.hPut stdout content
                             hFlush stdout
                             delayLoop
          else -- Otherwise, start a pause.  Flush buffer.
               do BS.hPut stdout (BS.pack flushBuf)
                  hFlush stdout
                  -- Wait a longer time, and move printhead out of way if
                  -- there's a longer pause.  If we get input in this time,
                  -- recurse to process it.
                  hasInput <- hWaitForInput stdin moveDelay
                  if hasInput
                     then delayLoop
                     else do BS.hPut stdout (BS.pack prePause)
                             hFlush stdout
                             masterReadLoop postPause
