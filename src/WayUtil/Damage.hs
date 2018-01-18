module WayUtil.Damage
    ( damageFun
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.IORef (readIORef)

import Graphics.Wayland.WlRoots.Output (setOutputNeedsSwap)


import Output (Output (..), outputFromWlr, frameHandler)
import Utility (intToPtr)
import View (View)
import ViewSet (WSTag)
import Waymonad (makeCallback, getState)
import Waymonad.Types

import qualified Data.IntMap as IM

actualFun :: WSTag ws => View -> Way vs ws ()
actualFun view = do
    state <- getState
    cache <- liftIO . readIORef $ wayBindingCache state
    let nums = map fst $ filter (any ((==) view . fst) . snd) $ IM.toList cache
    outs <- catMaybes <$> mapM (outputFromWlr . intToPtr) nums
    liftIO $ mapM_ (flip setOutputNeedsSwap True . outputRoots) outs
    mapM_ (\out -> do
        skipped <- liftIO . readIORef $ outputSkipped out
        when skipped $ do
            frameHandler 0 out
          ) outs

damageFun :: WSTag ws => Way vs ws (View -> IO ())
damageFun = makeCallback actualFun
