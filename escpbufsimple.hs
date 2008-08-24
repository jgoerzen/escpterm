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

delay = 250
reset = "\ESC@"
unidir = "\ESCU\x01"
immediate = "\ESCi1"

main = 
    do hSetBuffering stdin NoBuffering
       hSetBuffering stdout (BlockBuffering Nothing)
       BS.hPut stdout (BS.pack (reset ++ immediate))
       masterReadLoop

{- In this loop, read only 1 byte in blocking mode to ensure that we
never block waiting for more data.  Write it out. -}
masterReadLoop :: IO ()
masterReadLoop =
    do content <- BS.hGet stdin 1
       BS.hPut stdout content
       hFlush stdout
       innerLoop
       masterReadLoop

{- In this loop, we read up to 4k in non-blocking mode.  Keep doing so while
we keep getting data.  The moment there is no data waiting, back to other
loop. -}
innerLoop :: IO ()
innerLoop = 
    do content <- BS.hGetNonBlocking stdin 4096
       if BS.null content
          then return ()
          else do BS.hPut stdout content
                  hFlush stdout
                  innerLoop
