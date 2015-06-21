{-# LANGUAGE CPP, ScopedTypeVariables, DeriveDataTypeable #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) GError API
--
--  Author : Duncan Coutts
--
--  Created: 2 July 2004
--
--  Copyright (C) 2004 Duncan Coutts
--  parts derived from Structs.hsc Copyright (c) 1999..2002 Axel Simon
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
-- Error Reporting, glib's system for reporting errors.
--
-- 'GError's are used by glib to report recoverable runtime errors.
--
-- This module provides functions for checking glib\/gtk functions that report
-- 'GError's. It also provides functions for throwing and catching 'GError's as
-- Haskell exceptions.
--
module System.Glib.GError (

  -- * Data types
  --
  GError(..),
  GErrorDomain,
  GErrorCode,
  GErrorMessage,

  -- * Catching GError exceptions
  -- | To catch GError exceptions thrown by Gtk2Hs functions use the
  -- catchGError* or handleGError* functions. They work in a similar way to
  -- the standard 'Control.Exception.catch' and 'Control.Exception.handle'
  -- functions.
  --
  -- 'catchGError' \/ 'handleGError' catches all GError exceptions, you provide
  -- a handler function that gets given the GError if an exception was thrown.
  -- This is the most general but is probably not what you want most of the
  -- time. It just gives you the raw error code rather than a Haskell
  -- enumeration of the error codes. Most of the time you will only want to
  -- catch a specific error or any error from a specific error domain. To
  -- catch just a single specific error use
  -- 'catchGErrorJust' \/ 'handleGErrorJust'. To catch any error in a
  -- particular error domain use 'catchGErrorJustDomain' \/
  -- 'handleGErrorJustDomain'
  --
  catchGErrorJust,
  catchGErrorJustDomain,

  handleGErrorJust,
  handleGErrorJustDomain,

  -- ** Deprecated
  catchGError,
  handleGError,
  failOnGError,
  throwGError,

  -- * Checking for GErrors returned by glib\/gtk functions
  -- | * Note, these functions are only useful to implementors
  --
  -- If you are wrapping a new API that reports 'GError's you should probably
  -- use 'propagateGError' to convert the GError into an exception. You should
  -- also note in the documentation for the function that it throws GError
  -- exceptions and the Haskell enumeration for the expected glib GError
  -- domain(s), so that users know what exceptions they might want to catch.
  --
  -- If you think it is more appropriate to use an alternate return value (eg
  -- Either\/Maybe) then you should use 'checkGError'.

  GErrorClass(..),
  propagateGError,
  checkGError
  ) where

import Foreign
import Foreign.C
import System.Glib.UTFString
import Control.Exception
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Prelude hiding (catch)

-- | A GError consists of a domain, code and a human readable message.
data GError = GError !GErrorDomain !GErrorCode !GErrorMessage
  deriving Typeable

instance Show GError where
  show (GError _ _ msg) = T.unpack msg

instance Exception GError


type GQuark = {#type GQuark #}

-- | A code used to identify the \'namespace\' of the error. Within each error
--   domain all the error codes are defined in an enumeration. Each gtk\/gnome
--   module that uses GErrors has its own error domain. The rationale behind
--   using error domains is so that each module can organise its own error codes
--   without having to coordinate on a global error code list.
type GErrorDomain  = GQuark

-- | A code to identify a specific error within a given 'GErrorDomain'. Most of
--   time you will not need to deal with this raw code since there is an
--   enumeration type for each error domain. Of course which enumeraton to use
--   depends on the error domain, but if you use 'catchGErrorJustDomain' or
--   'handleGErrorJustDomain', this is worked out for you automatically.
type GErrorCode = Int

-- | A human readable error message.
type GErrorMessage = Text

instance Storable GError where
  sizeOf _ = {#sizeof GError #}
  alignment _ = alignment (undefined:: GQuark)
  peek ptr = do
    (domain  :: GQuark)         <- {#get GError->domain  #} ptr
    (code    :: {#type gint #}) <- {#get GError->code    #} ptr
    (msgPtr  :: CString)        <- {#get GError->message #} ptr
    msg <- peekUTFString msgPtr
    return $ GError (fromIntegral domain) (fromIntegral code) msg
  poke _ = error "GError::poke: not implemented"

-- | Each error domain's error enumeration type should be an instance of this
--   class. This class helps to hide the raw error and domain codes from the
--   user. This interface should be implemented by calling the approrpiate
--   @{error_domain}_error_quark@. It is safe to use a pure FFI call for this.
--
-- Example for 'Graphics.UI.Gtk.Gdk.Pixbuf.PixbufError':
--
-- > instance GErrorClass PixbufError where
-- >   gerrorDomain _ = {#call pure unsafe pixbuf_error_quark#}
--
class Enum err => GErrorClass err where
  gerrorDomain :: err -> GErrorDomain -- ^ This must not use the value of its
                                      -- parameter so that it is safe to pass
                                      -- 'undefined'.

-- | Glib functions which report 'GError's take as a parameter a @GError
-- **error@. Use this function to supply such a parameter. It checks if an
-- error was reported and if so throws it as a Haskell exception.
--
-- Example of use:
--
-- > propagateGError $ \gerrorPtr ->
-- > {# call g_some_function_that_might_return_an_error #} a b gerrorPtr
--
propagateGError :: (Ptr (Ptr ()) -> IO a) -> IO a
propagateGError action = checkGError action throwGError

-- | Like 'propagateGError' but instead of throwing the GError as an exception
--   handles the error immediately using the supplied error handler.
--
-- Example of use:
--
-- > checkGError
-- >   (\gerrorPtr -> {# call g_some_function_that_might_return_an_error #} a b gerrorPtr)
-- >   (\(GError domain code msg) -> ...)
--
checkGError :: (Ptr (Ptr ()) -> IO a) -> (GError -> IO a) -> IO a
checkGError action handler =
  alloca $ \(errPtrPtr  :: Ptr (Ptr GError)) -> do
  poke errPtrPtr nullPtr
  result <- action (castPtr errPtrPtr)
  errPtr <- peek errPtrPtr
  if errPtr == nullPtr
    then return result
    else do gerror <- peek errPtr
            {# call unsafe g_error_free #} (castPtr errPtr)
            handler gerror

-- | Use this if you need to explicitly throw a GError or re-throw an existing
--   GError that you do not wish to handle.
throwGError :: GError -> IO a
throwGError = throw
{-# DEPRECATED throwGError "Use ordinary Control.Exception.throw" #-}

-- | This will catch any GError exception. The handler function will receive the
--   raw GError. This is probably only useful when you want to take some action
--   that does not depend on which GError exception has occured, otherwise it
--   would be better to use either 'catchGErrorJust' or 'catchGErrorJustDomain'.
--   For example:
--
-- > catchGError
-- >   (do ...
-- >       ...)
-- >   (\(GError dom code msg) -> fail msg)
--
catchGError :: IO a            -- ^ The computation to run
            -> (GError -> IO a) -- ^ Handler to invoke if an exception is raised
            -> IO a
catchGError = catch
{-# DEPRECATED catchGError "Use ordinary Control.Exception.catch" #-}

-- | This will catch just a specific GError exception. If you need to catch a
--   range of related errors, 'catchGErrorJustDomain' is probably more
--   appropriate. Example:
--
-- > do image <- catchGErrorJust PixbufErrorCorruptImage
-- >               loadImage
-- >               (\errorMessage -> do log errorMessage
-- >                                    return mssingImagePlaceholder)
--
catchGErrorJust :: GErrorClass err => err  -- ^ The error to catch
                -> IO a                    -- ^ The computation to run
                -> (GErrorMessage -> IO a) -- ^ Handler to invoke if an exception is raised
                -> IO a
catchGErrorJust code action handler = catch action handler'
  where handler' gerror@(GError domain code' msg)
          | fromIntegral domain == gerrorDomain code
           && code' == fromEnum code   = handler msg
          | otherwise                  = throw gerror

-- | Catch all GErrors from a particular error domain. The handler function
--   should just deal with one error enumeration type. If you need to catch
--   errors from more than one error domain, use this function twice with an
--   appropriate handler functions for each.
--
-- > catchGErrorJustDomain
-- >   loadImage
-- >   (\err message -> case err of
-- >       PixbufErrorCorruptImage -> ...
-- >       PixbufErrorInsufficientMemory -> ...
-- >       PixbufErrorUnknownType -> ...
-- >       _ -> ...)
--
catchGErrorJustDomain :: GErrorClass err => IO a        -- ^ The computation to run
                      -> (err -> GErrorMessage -> IO a) -- ^ Handler to invoke if an exception is raised
                      -> IO a
catchGErrorJustDomain action (handler :: err -> GErrorMessage -> IO a) =
    catch action handler'
  where handler' gerror@(GError domain code msg)
          | fromIntegral domain == gerrorDomain (undefined::err) = handler (toEnum code) msg
          | otherwise                                            = throwGError gerror

-- | A verson of 'catchGError' with the arguments swapped around.
--
-- > handleGError (\(GError dom code msg) -> ...) $
-- >   ...
--
handleGError :: (GError -> IO a) -> IO a -> IO a
handleGError = handle
{-# DEPRECATED handleGError "Use ordinary Control.Exception.handle" #-}

-- | A verson of 'handleGErrorJust' with the arguments swapped around.
handleGErrorJust :: GErrorClass err => err -> (GErrorMessage -> IO a) -> IO a -> IO a
handleGErrorJust code = flip (catchGErrorJust code)

-- | A verson of 'catchGErrorJustDomain' with the arguments swapped around.
handleGErrorJustDomain :: GErrorClass err => (err -> GErrorMessage -> IO a) -> IO a -> IO a
handleGErrorJustDomain = flip catchGErrorJustDomain

-- | Catch all GError exceptions and convert them into a general failure.
failOnGError :: IO a -> IO a
failOnGError action = catchGError action (\(GError dom code msg) -> fail (T.unpack msg))
