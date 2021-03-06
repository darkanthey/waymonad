{-
waymonad A wayland compositor in the spirit of xmonad
Copyright (C) 2017  Markus Ongyerth

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

Reach us at https://github.com/ongy/waymonad
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Waymonad.View
    ( ShellSurface (..)
    , View (..)
    , getViewSize
    , getViewBox
    , createView
    , moveView
    , resizeView
    , getViewSurface
    , activateView
    , renderViewAdditional
    , getViewEventSurface
    , setViewBox
    , closeView
    , getViewClient
    , getViewInner
    , getViewTitle
    , getViewAppId
    , setViewLocal
    , viewGetScale
    , viewGetLocal
    , getViewID

    , addViewDestroyListener
    , rmViewDestroyListener
    , triggerViewDestroy

    , addViewResizeListener
    , rmViewResizeListener
    , triggerViewResize

    , viewSetClean
    , viewIsDirty
    , setViewFocus
    , unsetViewFocus
    , doFocusView
    , setViewRemove
    , unsetViewRemove
    , doRemoveView
    , viewHasCSD
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.IORef (IORef, readIORef, writeIORef, newIORef, modifyIORef)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Foreign.Ptr (Ptr)

import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.Signal

import Graphics.Wayland.Resource (resourceGetClient)
import Graphics.Wayland.Server (Client)

import Graphics.Wayland.WlRoots.Surface (WlrSurface, getSurfaceResource, getWlrSurfaceEvents, WlrSurfaceEvents (..))
import Graphics.Wayland.WlRoots.Box (WlrBox(..), toOrigin, centerBox)

import Waymonad.Utility.Base (doJust)
import Waymonad.Types.Core (View (..), ShellSurface (..), Seat)

import qualified Data.IntMap as IM

getViewSize :: MonadIO m => View -> m (Double, Double)
getViewSize View {viewSurface=surf} = getSize surf

getViewBox :: MonadIO m => View -> m WlrBox
getViewBox = liftIO . readIORef . viewBox

setViewBox :: MonadIO m => View -> WlrBox -> m ()
setViewBox v box = do
    moveView v (fromIntegral $ boxX box) (fromIntegral $ boxY box)
    resizeView v (fromIntegral $ boxWidth box) (fromIntegral $ boxHeight box)
    liftIO $ writeIORef (viewBox v) box

viewCounter :: IORef Int
{-# NOINLINE viewCounter #-}
viewCounter = unsafePerformIO (newIORef 0)

removeListeners :: MonadIO m => View -> m ()
removeListeners View {viewTokens = toks} =
    liftIO $ mapM_ removeListener toks

handleCommit :: MonadIO m => View -> IORef (Double, Double) -> m ()
handleCommit view ref = liftIO $ do
    writeIORef (viewDirty view) True
    (width, height) <- getViewSize view
    (oldWidth, oldHeight) <- readIORef ref
    when (oldWidth /= width || oldHeight /= height) $ do
        setViewLocal view $ WlrBox 0 0 (floor width) (floor height)
        writeIORef ref (width, height)
        triggerViewResize view

createView :: (ShellSurface a, MonadIO m) => a -> m View
createView surf = liftIO $ do
    (width, height) <- getSize surf
    let box = WlrBox 0 0 (floor width) (floor height)
    global <- newIORef box
    local <- newIORef box
    scale <- newIORef 1.0
    destroyCBs <- newIORef mempty
    resizeCBs <- newIORef mempty
    idVal <- readIORef viewCounter
    modifyIORef viewCounter (+1)
    viewRef <- newIORef undefined


    mainSurf <- getSurface surf
    tokens <- case mainSurf of
        Nothing -> pure []
        Just wlrSurf -> do
            let events = getWlrSurfaceEvents wlrSurf
            destroyHandler <- addListener
                (WlListener $ const $ removeListeners $ unsafePerformIO $ readIORef viewRef)
                (wlrSurfaceEvtDestroy events)
            sizeRef <- newIORef (0, 0)
            commitHandler <- addListener
                (WlListener $ const $ handleCommit (unsafePerformIO $ readIORef viewRef) sizeRef)
                (wlrSurfaceEvtCommit events)

            pure [destroyHandler, commitHandler]

    dirty <- newIORef True
    focus <- newIORef Nothing
    remove <- newIORef Nothing
    let ret = View
            { viewSurface = surf
            , viewBox = global
            , viewPosition = local
            , viewScaling = scale
            , viewDestroy = destroyCBs
            , viewResize = resizeCBs
            , viewID = idVal
            , viewTokens = tokens
            , viewDirty = dirty
            , viewFocus = focus
            , viewRemove = remove
            }
    writeIORef viewRef ret
    pure ret

setViewRemove :: MonadIO m => View -> (View -> IO ()) -> m ()
setViewRemove v fun = liftIO $ writeIORef (viewRemove v) (Just fun)

unsetViewRemove :: MonadIO m => View -> m ()
unsetViewRemove v = liftIO $ writeIORef (viewRemove v) Nothing

doRemoveView :: MonadIO m => View -> m ()
doRemoveView view = liftIO $ do
    fun <- readIORef (viewRemove view)
    case fun of
        Just act -> act view
        Nothing -> pure ()
    writeIORef (viewRemove view) Nothing

setViewFocus :: MonadIO m => View -> (Seat -> View -> IO ()) -> m ()
setViewFocus v fun = liftIO $ writeIORef (viewFocus v) (Just fun)

unsetViewFocus :: MonadIO m => View -> m ()
unsetViewFocus v = liftIO $ writeIORef (viewFocus v) Nothing

doFocusView :: MonadIO m => View -> Seat -> m ()
doFocusView view seat = liftIO $ do
    fun <- readIORef (viewFocus view)
    case fun of
        Just act -> act seat view
        Nothing -> pure ()

closeView :: MonadIO m => View -> m ()
closeView View {viewSurface=surf} = close surf

moveView :: MonadIO m => View -> Double -> Double -> m ()
moveView View {viewSurface = surf, viewBox = ref} x y = do
    old <- liftIO $ readIORef ref
    let new = old { boxX = floor x, boxY = floor y}
    liftIO $ writeIORef ref new
    setPosition surf x y


resizeView :: MonadIO m => View -> Double -> Double -> m ()
resizeView v@View {viewSurface = surf, viewBox = ref} width height = do
    old <- liftIO $ readIORef ref
    let new = old { boxWidth = floor width, boxHeight = floor height}
    liftIO $ writeIORef ref new
    (oldWidth, oldHeight) <- getSize surf
    resize surf (floor width) (floor height)

    setViewLocal v $ WlrBox 0 0 (floor oldWidth) (floor oldHeight)

getViewSurface :: MonadIO m => View -> m (Maybe (Ptr WlrSurface))
getViewSurface View {viewSurface = surf} = getSurface surf


activateView :: MonadIO m => View -> Bool -> m ()
activateView View {viewSurface = surf} = activate surf


renderViewAdditional :: MonadIO m => (Ptr WlrSurface -> WlrBox -> m ()) -> View -> m ()
renderViewAdditional fun View {viewSurface = surf} =
    renderAdditional fun surf


getViewEventSurface :: MonadIO m => View -> Double -> Double -> m (Maybe (Ptr WlrSurface, Double, Double))
getViewEventSurface View {viewSurface = surf, viewPosition = local, viewScaling = scale} x y = liftIO $ do
    scaleFactor <- readIORef scale
    posBox <- readIORef local
    getEventSurface surf
        ((x - fromIntegral (boxX posBox)) / realToFrac scaleFactor)
        ((y - fromIntegral (boxY posBox)) / realToFrac scaleFactor)

getViewClient :: MonadIO m => View -> m (Maybe Client)
getViewClient View {viewSurface = surf} =
    doJust (getSurface surf) $ \wlrSurf -> liftIO $ do
        res <- getSurfaceResource wlrSurf
        Just <$> resourceGetClient res

getViewInner :: Typeable a => View -> Maybe a
getViewInner View {viewSurface = surf} = cast surf

getViewTitle :: MonadIO m => View -> m (Maybe Text)
getViewTitle View {viewSurface = surf} = getTitle surf

getViewAppId :: MonadIO m => View -> m (Maybe Text)
getViewAppId View {viewSurface = surf} = getAppId surf

getLocalBox :: WlrBox -> WlrBox -> (WlrBox, Float)
getLocalBox inner outer =
    if boxWidth inner <= boxWidth outer && boxHeight inner <= boxHeight outer
        then (inner, 1.0)
        else
            let scale:: Float = min (fromIntegral (boxWidth  outer) / fromIntegral (boxWidth  inner))
                                    (fromIntegral (boxHeight outer) / fromIntegral (boxHeight inner))

             in (WlrBox 0 0 (floor $ scale * fromIntegral (boxWidth inner)) (floor $ scale * fromIntegral (boxHeight inner)), scale)

-- | This should be called whenever the contained surface is resized. It will
-- determine whether we should try and downscale it to fit the area, or
-- position it somewhere inside the configured box, because it is *smaller*
-- than the intended area
setViewLocal :: MonadIO m => View -> WlrBox -> m ()
setViewLocal View {viewBox = global, viewPosition = local, viewScaling = scaleRef} box = liftIO $ do
    outerBox <- readIORef global
    if toOrigin outerBox == box
        then do
            writeIORef local box
            writeIORef scaleRef 1
        else do
            let (inner, scale) = getLocalBox box outerBox
            writeIORef local (centerBox inner $ toOrigin outerBox)
            writeIORef scaleRef scale

viewGetScale :: MonadIO m => View -> m Float
viewGetScale View {viewScaling = scale} = liftIO $ readIORef scale

viewGetLocal :: MonadIO m => View -> m WlrBox
viewGetLocal View {viewPosition = local} = liftIO $ readIORef local

getViewID :: View -> Int
--getViewID (View {viewSurface = surf}) = getID surf
getViewID = viewID

addViewDestroyListener :: MonadIO m => Int -> (View -> IO ()) -> View -> m ()
addViewDestroyListener key cb View {viewDestroy = ref} = liftIO $ modifyIORef ref (IM.insert key cb)

rmViewDestroyListener :: MonadIO m => Int -> View -> m ()
rmViewDestroyListener key View {viewDestroy = ref} = liftIO $ modifyIORef ref (IM.delete key)

triggerViewDestroy :: MonadIO m => View -> m ()
triggerViewDestroy v@View {viewDestroy = ref} = liftIO $ do
    cbs <- readIORef ref
    mapM_ ($ v) cbs


addViewResizeListener :: MonadIO m => Int -> (View -> IO ()) -> View -> m ()
addViewResizeListener key cb View {viewResize = ref} = liftIO $ modifyIORef ref (IM.insert key cb)

rmViewResizeListener :: MonadIO m => Int -> View -> m ()
rmViewResizeListener key View {viewResize = ref} = liftIO $ modifyIORef ref (IM.delete key)

triggerViewResize :: MonadIO m => View -> m ()
triggerViewResize v@View {viewResize = ref} = liftIO $ do
    cbs <- readIORef ref
    mapM_ ($ v) cbs

viewIsDirty :: MonadIO m => View -> m Bool
viewIsDirty = liftIO . readIORef . viewDirty

viewSetClean :: MonadIO m => View -> m ()
viewSetClean = liftIO . flip writeIORef False . viewDirty

viewHasCSD :: MonadIO m => View -> m Bool
viewHasCSD View {viewSurface=surf} = hasCSD surf
