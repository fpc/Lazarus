{%MainUnit gtk2proc.pp}{%MainUnit gtkint.pp}
{ $Id$ }

{******************************************************************************
  Centralized includefile for some common gtk defines
 ******************************************************************************}


// TODO: Test on all platforms
{$IFNDEF DisableAsyncProcess}
  {$IFDEF Linux}
    {$IFDEF CPUI386}
      {off $DEFINE UseAsyncProcess}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{off $DEFINE GDK_ERROR_TRAP_FLUSH}

{$DEFINE REPORT_GDK_ERRORS}

{off $DEFINE VerboseAccelerator}

{off $define VerboseModifiermap}

{$define HideKeyTableWarnings}

{off $DEFINE VerboseTimer}

{off $DEFINE VerboseMouseBugfix}

{off $DEFINE RaiseExceptionOnNilPointers}

{off $Define DisableCriticalSections}

{off $Define Disable_GC_SysColors}

{$define RawimageConsistencyChecks}

(*
 keep track of keystates instead of using OS
 This is the old mode and might be removed
*)
{$DEFINE Use_KeyStateList}

(*
 Since some platforms (Darwin) can have X but also have a native gtk
 implementation, the following defines are used

 HasX -> X can be used for accessing functions not implented in gtk/gdk
         (this does not mean that gdkx functions can be used)
 HasGdk2X -> gdk2x can be used (gdk2x is defined for fpc 2.1+)
 UseX -> Use the X version of gtk instead of a native version
*)

{off $define UseX}

{$ifdef Unix}
  // on darwin we try to use native gtk
  {$ifdef Darwin}
    {$ifdef UseX} // it can be overridden
      {$info Compiling with gtk2 for X}
      {$define HasX}
    {$else}
      {$IFDEF VerboseGtkToDos}{$note On darwin the native gtk2 widgetset is used. }{$ENDIF}
      {$IFDEF VerboseGtkToDos}{$note If you want to use gtk2 for X, define UseX }{$ENDIF}
    {$endif}
  {$else}
    {$define HasX}
  {$endif}
  {$if defined(HasX)}
    {$define HasGdk2X}
  {$endif}

{$else}
  {$define GTK_2_10}
  {$define USE_GTK_MAIN_OLD_ITERATION} // in other case it hangs
{$endif}

{$ifdef HasX}
  (*
   GTK or GNOME has problems reporting ssAlt and ssAltGr when different
   keyboardlayouts are used. With UseOwnShiftState we keep track of the shiftate
   ourself
  *)
  {$ifndef DisableUseOwnShiftState}
    {$define UseOwnShiftState}
  {$endif}
{$endif}

// see http://bugs.freepascal.org/view.php?id=12145.
// Turn it off if something happens.
{$DEFINE EnabledGtkThreading}
{$define GTK_2_8}

// See http://bugs.freepascal.org/view.php?id=18966
{$define Gtk2CallMouseDownBeforeContext}

{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
{$ENDIF}

