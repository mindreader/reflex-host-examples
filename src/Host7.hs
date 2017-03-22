{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Host7 (
    go7
  , Input(..)
  , Output(..)
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless, void)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

data Input t = Input {
    ieOpen :: Event t ()
  , ieRead :: Event t String
  }

data Output t = Output {
    oeWrite :: Event t String
  , oeQuit  :: Event t ()
  }


spider :: SpiderHost Global ()
spider = do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    (Output eWrite eQuit) <- runHostFrame $ guest $ Input eOpen eRead
    ((), FireCommand fireIO) <- hostPerformEventT $ guestIO eWrite

    hQuit  <- subscribeEvent eQuit

    let

      -- readPhase :: _rp Global (Maybe ())
      readPhase = readEvent hQuit >>= sequence

      loop :: SpiderHost Global ()
      loop = do
        input <- liftIO getLine


        mQuit <- liftIO (readIORef eReadTriggerRef) >>= \case
          Nothing -> return []
          Just eTrigger -> do
            fireIO [eTrigger :=> Identity input] readPhase

        let quit = any isJust mQuit

        unless quit
          loop

    void $ liftIO (readIORef eOpenTriggerRef) >>= \case
      Nothing -> return []
      Just eTrigger -> do
       fireIO [eTrigger :=> Identity ()] (return Nothing)

    loop


guest :: (MonadFix m, MonadHold t m, Reflex t) => Input t -> m (Output t)
guest (Input eOpen eRead) = do
  let
    eQuit    = () <$ ffilter (== "/quit") eRead
    eMessage =       ffilter (/= "/quit") eRead
    eWrite   = leftmost [
        "Hi"  <$ eOpen
      ,          eMessage
      , "Bye" <$ eQuit
      ]
  return $ Output eWrite eQuit



guest' :: (Monad m, Reflex t) => Input t -> m (Output t)
guest' (Input eOpen eRead) = do
  let
    eMessage =       ffilter (/= "/quit") eRead
    eQuit    = () <$ ffilter (== "/quit") eRead
    eWrite   = leftmost [
                eMessage
      ,  "Hi"  <$ eOpen
      , "Bye" <$ eQuit
      ]
  return $ Output eWrite eQuit

guestIO :: (MonadIO (Performable m), PerformEvent t m) => Event t String -> m ()
guestIO eWrite =  performEvent_ $ ( liftIO . putStrLn . ("> " ++)) <$> eWrite

go7 :: IO ()
go7 = do
  hSetBuffering stdin LineBuffering
  runSpiderHost spider
