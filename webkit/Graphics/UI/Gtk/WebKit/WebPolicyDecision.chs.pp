-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebPolicyDecision
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Access to the WebKit PolicyDecision
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebPolicyDecision (
-- * Types
  WebPolicyDecision,

-- * Methods  
  webPolicyDecisionDownload,
  webPolicyDecisionIgnore,
  webPolicyDecisionUse,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

-- * Methods

-- | Will send the DOWNLOAD decision to the policy implementer.
webPolicyDecisionDownload :: 
   WebPolicyDecisionClass self => self
 -> IO ()   
webPolicyDecisionDownload pd =
  {#call web_policy_decision_download#} (toWebPolicyDecision pd)

-- | Will send the IGNORE decision to the policy implementer.
webPolicyDecisionIgnore ::
   WebPolicyDecisionClass self => self  
 -> IO ()
webPolicyDecisionIgnore pd =
  {#call web_policy_decision_ignore#} (toWebPolicyDecision pd)

-- | Will send the USE decision to the policy implementer.
webPolicyDecisionUse ::
   WebPolicyDecisionClass self => self
 -> IO ()
webPolicyDecisionUse pd =
  {#call web_policy_decision_use#} (toWebPolicyDecision pd)
