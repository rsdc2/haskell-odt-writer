{-# LANGUAGE OverloadedStrings #-}

module QuerySpec where


import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Text.XML as X
import Text.ODT hiding (italicPara)

import ConstantsSpec (loadArchive, testText, italicPara)


lastParaHappy :: IO Bool
lastParaHappy = do
  archive <- loadArchive "example"
  let orig = contentDoc archive
  let new = appendODT italicPara orig
  let lastParaText = getText . getLastPara 
  return $ lastParaText orig /= testText &&
            lastParaText new == testText