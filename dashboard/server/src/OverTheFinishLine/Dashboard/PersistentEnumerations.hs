{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module OverTheFinishLine.Dashboard.PersistentEnumerations where

import Database.Persist
import Database.Persist.TH

import OverTheFinishLine.Dashboard.Model

derivePersistField "ThirdPartyService"
