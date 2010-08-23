
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Include file               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

{$mode objfpc}

{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }

{$DEFINE HASVARIANT}

//{$DEFINE FREEREP2217READ}

//-------------------------Additional components----------------------------

//------------------- JPEG images -----------------------------------
// JPEG images - only available in Delphi 3 and above.
// If you want to show jpeg images in "Picture" object, uncomment
// the following line:
//{$DEFINE JPEG}

//------------------- Interbase Express (IBX) components ------------
// Allows using Interbase DB's in data manager with no BDE required.
//{$DEFINE IBX}

//------------------- LazReport Options
//
// Enabling following define allow initialization of global localization
// variables using libc. This is not supported in all platforms, see readme.txt
//{$DEFINE UseLibC}
//
// Controls the modal behaviour of Report Designer, modal by default.
{$DEFINE MODALDESIGNER}
//
// Enable following define to get Object Inspector in one external window
//{$DEFINE EXTOI}
//
// In Some widgetsets Report Designer can't be modal if Object Inspector is in
// external window, the current default is embedded Object Inspector
{$IFDEF EXTOI}
  {$IFNDEF MSWINDOWS}
    {$UNDEFINE MODALDESIGNER}
  {$ENDIF}
{$ENDIF}
{$DEFINE Delphi5}
