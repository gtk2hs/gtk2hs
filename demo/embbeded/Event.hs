module Event where

import Data.List

import Key

import qualified Graphics.UI.Gtk.Gdk.Events as E

-- | The advanced event.
data Event = Event Key [Modifier] deriving (Eq)

-- | Output event describe.
eventGetName :: Event -> String
eventGetName (Event key mods) = concatMap ((++ "-") . keyModifier) mods ++ keyDescribe key
  where 
    -- Key describe
    keyDescribe (KFun i) = 'F' : show i -- Function key (F1, F2, F3... etc.)
    keyDescribe (KASCII c) = [c]        -- Character key ('A', 'B', 'C'... etc.)
    keyDescribe k = tail $ show k       -- Control key (Ctrl, Alt, Shift... etc.)
    -- Key modifier
    keyModifier m = [show m !! 1]

-- | Transform basic event to advanced event.    
eventTransform :: E.Event -> Maybe Event
eventTransform (E.Key {E.eventKeyName         = keyName, 
                       E.eventKeyChar         = keyChar,
                       E.eventModifier        = keyModifier})
    = fmap (\k -> Event k 
                 (nub $ sort $ (if isShift 
                                then filter (/= MShift) 
                                else id)                          -- key
                 $ concatMap eventTransformModifier keyModifier)) -- modifier
      key
      where
        (key, isShift) =
            case keyChar of
              Just c -> (Just $ KASCII c, True)     -- character key
              Nothing -> (keyLookup keyName, False) -- other key
eventTransform _ = Nothing

-- | Transform event modifier.
eventTransformModifier E.Control = [MCtrl]
eventTransformModifier E.Alt     = [MMeta]
eventTransformModifier E.Shift   = [MShift]
eventTransformModifier E.Super   = [MSuper]
eventTransformModifier _         = [] -- Use underscore so we don't depend on the differences between gtk2hs versions
