{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 30-Apirl-2010
--
--  Copyright (c) 2010 Andy Stewart
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--
--  GIO, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GIO documentation.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.File.MountOperation (
-- * Details
--
-- | 'MountOperation' provides a mechanism for interacting with the user. It can be used for
-- authenticating mountable operations, such as loop mounting files, hard drive partitions or server
-- locations. It can also be used to ask the user questions or show a list of applications preventing
-- unmount or eject operations from completing.
--
-- Note that 'Mount'Operation is used for more than just 'Mount' objects – for example it is also used in
-- 'driveStart'.
--
-- Users should instantiate a subclass of this that implements all the various callbacks to show the
-- required dialogs, such as 'MountOperation'. If no user interaction is desired (for example when
-- automounting filesystems at login time), usually 'Nothing' can be passed, see each method taking a
-- 'MountOperation' for details.

-- * Types
   MountOperation(..),
   MountOperationClass,

-- * Enums
   MountOperationResult(..),
   AskPasswordFlags(..),
   PasswordSave(..),

-- * Methods
    mountOperationNew,
    mountOperationReply,

-- * Attributes
    mountOperationAnonymous,
    mountOperationChoice,
    mountOperationDomain,
    mountOperationPassword,
    mountOperationPasswordSave,
    mountOperationUsername,

-- * Signals
#if GLIB_CHECK_VERSION(2,20,0)
    mountOperationAborted,
    mountOperationAskPassword,
    -- askQuestion,
    mountOperationReplySignal,
    -- showProcesses,
#endif
    ) where

#include <gio/gio.h>

import Control.Monad
import System.GIO.Enums
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.Properties
import System.Glib.Signals
import System.Glib.UTFString
{#import System.GIO.Signals#}
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

--------------------
-- Methods

-- | Creates a new mount operation.
mountOperationNew :: IO MountOperation
mountOperationNew =
    wrapNewGObject mkMountOperation $
    {# call g_mount_operation_new #}

-- | Emits the "reply" signal.
mountOperationReply :: MountOperationClass op => op -> MountOperationResult -> IO ()
mountOperationReply op result =
  {#call g_mount_operation_reply#} (toMountOperation op) ((fromIntegral . fromEnum) result)

--------------------
-- Attributes
-- | Whether to use an anonymous user when authenticating.
--
-- Default value: 'False'
mountOperationAnonymous :: MountOperationClass op => Attr op Bool
mountOperationAnonymous = newAttrFromBoolProperty "anonymous"

-- | The index of the user's choice when a question is asked during the mount operation. See the
-- 'askQuestion' signal.
--
-- Allowed values: >= 0
--
-- Default value: 0
mountOperationChoice :: MountOperationClass op => Attr op Int
mountOperationChoice = newAttrFromIntProperty "choice"

-- | The domain to use for the mount operation.
--
-- Default value: \"\"
mountOperationDomain :: (MountOperationClass op, GlibString string) => Attr op string
mountOperationDomain = newAttrFromStringProperty "domain"

-- | The password that is used for authentication when carrying out the mount operation.
--
-- Default value: \"\"
mountOperationPassword :: (MountOperationClass op, GlibString string) => Attr op string
mountOperationPassword = newAttrFromStringProperty "password"

-- | Determines if and how the password information should be saved.
--
-- Default value: 'PasswordSaveNever'
mountOperationPasswordSave :: MountOperationClass op => Attr op PasswordSave
mountOperationPasswordSave = newAttrFromEnumProperty "password-save"
               {#call pure unsafe g_password_save_get_type #}

-- | The user name that is used for authentication when carrying out the mount operation.
--
-- Default value: \"\"
mountOperationUsername :: (MountOperationClass op, GlibString string) => Attr op string
mountOperationUsername = newAttrFromStringProperty "username"

--------------------
-- Signals
#if GLIB_CHECK_VERSION(2,20,0)
-- | Emitted by the backend when e.g. a device becomes unavailable while a mount operation is in
-- progress.
--
-- Implementations of 'MountOperation' should handle this signal by dismissing open password dialogs.
--
-- Since 2.20
mountOperationAborted :: MountOperationClass op => Signal op (IO ())
mountOperationAborted = Signal (connect_NONE__NONE "aborted")
#endif

-- | Emitted when a mount operation asks the user for a password.
--
-- If the message contains a line break, the first line should be presented as a heading. For example,
-- it may be used as the primary text in a 'MessageDialog'.
mountOperationAskPassword :: (MountOperationClass op, GlibString string) => Signal op (string -> string -> string -> AskPasswordFlags -> IO ())
mountOperationAskPassword = Signal (connect_GLIBSTRING_GLIBSTRING_GLIBSTRING_ENUM__NONE "ask-password")

-- | Emitted when asking the user a question and gives a list of choices for the user to choose from.
--
-- If the message contains a line break, the first line should be presented as a heading. For example,
-- it may be used as the primary text in a 'MessageDialog'.
-- askQuestion :: MountOperationClass op => Signal op (String -> [String] -> IO ())
-- askQuestion Signal (\after obj user -> connect_GLIBSTRING_BOXED__NONE "ask-question" after obj
--                                       (\message choicesPtr -> do
--                                          choices <- peekUTFString choicesPtr
--                                          user str choices))

-- | Emitted when the user has replied to the mount operation.
mountOperationReplySignal :: MountOperationClass op => Signal op (MountOperationResult -> IO ())
mountOperationReplySignal = Signal (connect_ENUM__NONE "reply")

