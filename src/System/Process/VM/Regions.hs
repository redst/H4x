module System.Process.VM.Regions
  ( Region (..), parseRegion, getRegions, anon
  ) where

import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Char8 as B
import Data.Word

import Numeric

import Prelude hiding (readFile)

import Text.Printf 
import System.Posix.Types

import Text.Parsec

data Region = Region
  { r_start :: Word
  , r_end   :: Word
  , r_name  :: ByteString
  , r_read  :: Bool
  , r_write :: Bool
  , r_shared:: Bool
  } deriving (Eq)

instance Show Region where
    show r = printf "%s %c%c: 0x%x-0x%x (0x%o)" 
                name rea wri start end (end - start) 
      where
        name = if B.null (r_name r) then "<anon>" else B.unpack (r_name r)
        start = r_start r
        end = r_end r
        rea = if r_read r then 'r' else '-'
        wri = if r_write r then 'w' else '-'

anon :: Region -> Bool
anon = B.null . r_name

parseRegion :: Parsec ByteString () Region
parseRegion = do
    st <- (fst . head . readHex) <$> manyTill hexDigit (char '-')
    end <- (fst . head . readHex) <$> manyTill hexDigit (char ' ')
    r <- option False (char 'r' >> return True)
    w <- option False (char 'w' >> return True)
    anyChar
    s <- option False (char 's' >> return True)
    count 4 $ manyTill anyChar (char ' ')
    name <- option "" $ do
        many (char ' ')
        manyTill anyChar (char '\n')
    return (Region st end (B.pack name) r w s)

getRegions :: CPid -> IO [Region]
getRegions pid = do
    cont <- readFile ("/proc/" ++ show pid ++ "/maps")
    return $ either (pure []) id $ parse (manyTill parseRegion eof) "getRegions" cont
        
