{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module OverTheFinishLine.Dashboard.Enumerations where

import Database.Persist
import Database.Persist.TH

data ThirdPartyService = GitHub
  deriving (Eq, Read, Show)

derivePersistField "ThirdPartyService"
