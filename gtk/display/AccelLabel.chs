-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget AccelLabel
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This widget is a special version of @Label. It displays an accelerator
--   key next to the Label. The key name is not explicitly set but taken from
--   the key that is associated with the activation of another widget.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module AccelLabel(
  AccelLabel,
  AccelLabelClass,
  castToAccelLabel,
  accelLabelNew,
  accelLabelSetAccelWidget
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new label with an accelerator key. (EXPORTED)
--
accelLabelNew :: String -> IO AccelLabel
accelLabelNew str = withCString str $ \strPtr -> makeNewObject mkAccelLabel $ 
  liftM castPtr $ {#call unsafe accel_label_new#} strPtr

-- Set the key name from the activation signal of another widget. (EXPORTED)
--
accelLabelSetAccelWidget :: (AccelLabelClass acl, WidgetClass w) =>
  w -> acl -> IO ()
accelLabelSetAccelWidget w acl = {#call accel_label_set_accel_widget#}
  (toAccelLabel acl) (toWidget w)

