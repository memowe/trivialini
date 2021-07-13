-- | Ultra light weight ini file parser
module Trivialini ( readIniFile ) where

import Trivialini.Ini ( Ini(..), showIni )
import Trivialini.Parser ( readIni )

-- | Convenience file system access:
--   Return the complete ini data from the given filename
readIniFile :: String -> IO Ini
readIniFile file = readIni <$> readFile file
