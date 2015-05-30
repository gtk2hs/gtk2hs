#include <gtk/gtk.h>
/* these are necessary on Win32 to circumvent the strcuture size check */
#undef gtk_init_check
#undef gtk_init

#if GTK_MAJOR_VERSION >= 3
#include <gtk/gtkx.h>
#endif

#undef Bool
#undef True
#undef False
#undef Button1 
#undef Button2
#undef Button3
#undef Button4
#undef Button5
#undef Button1Mask 
#undef Button2Mask
#undef Button3Mask
#undef Button4Mask
#undef Button5Mask

#undef ShiftMask
#undef LockMask
#undef ControlMask
#undef Mod1Mask
#undef Mod2Mask
#undef Mod3Mask
#undef Mod4Mask
#undef Mod5Mask

#undef None
#undef ParentRelative
#undef CopyFromParent
#undef PointerWindow
#undef InputFocus
#undef PointerRoot
#undef AnyPropertyType
#undef AnyKey
#undef AnyButton
#undef AllTemporary
#undef CurrentTime
#undef NoSymbol

#undef NoEventMask
#undef KeyPressMask
#undef KeyReleaseMask
#undef ButtonPressMask
#undef ButtonReleaseMask
#undef EnterWindowMask
#undef LeaveWindowMask
#undef PointerMotionMask
#undef PointerMotionHintMask
#undef Button1MotionMask
#undef Button2MotionMask
#undef Button3MotionMask
#undef Button4MotionMask
#undef Button5MotionMask
#undef ButtonMotionMask
#undef KeymapStateMask
#undef ExposureMask
#undef VisibilityChangeMask
#undef StructureNotifyMask
#undef ResizeRedirectMask
#undef SubstructureNotifyMask
#undef SubstructureRedirectMask
#undef FocusChangeMask
#undef PropertyChangeMask
#undef ColormapChangeMask
#undef OwnerGrabButtonMask

#undef Status
#undef Expose
#undef Below
#undef GrabSuccess
#undef GrabAlreadyGrabbed
#undef GrabInvalidTime
#undef GrabNotViewable
#undef GrabFrozen
#undef OwnerChangeNewOwner
#undef OwnerChangeDestroy
#undef OwnerChangeClose
#undef NULL
