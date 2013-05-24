{
 *****************************************************************************
 *                          CustomDrawnWSDialogs.pp                          *
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CustomDrawnWSDialogs;

{$mode objfpc}{$H+}
{$I customdrawndefines.inc}

interface

uses
  // RTL
  SysUtils, Classes, Types,
//  {$ifdef CD_Windows}Windows, customdrawn_WinProc,{$endif}
//  {$ifdef CD_Cocoa}MacOSAll, CocoaAll, customdrawn_cocoaproc, CocoaGDIObjects,{$endif}
//  {$ifdef CD_X11}X, XLib, XUtil, BaseUnix, customdrawn_x11proc,{$ifdef CD_UseNativeText}xft, fontconfig,{$endif}{$endif}
//  {$ifdef CD_Android}customdrawn_androidproc, jni, bitmap, log, keycodes,{$endif}
  // LCL
  // RTL + LCL
  LCLType, LCLProc, Dialogs, Controls, Forms, Graphics,
  // Widgetset
  WSDialogs, WSLCLClasses,
  customdrawncontrols, customdrawnwscontrols, customdrawnproc;

type

  { TCDWSCommonDialog }

  TCDWSCommonDialog = class(TWSCommonDialog)
  published
{    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;}
  end;

  { TCDWSFileDialog }

  TCDWSFileDialog = class(TWSFileDialog)
  published
{    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;}
  end;

  { TCDWSOpenDialog }

  TCDWSOpenDialog = class(TWSOpenDialog)
  published
  end;

  { TCDWSSaveDialog }

  TCDWSSaveDialog = class(TWSSaveDialog)
  published
  end;

  { TCDWSSelectDirectoryDialog }

  TCDWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  published
{    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;}
  end;

  { TCDWSColorDialog }

  TCDWSColorDialog = class(TWSColorDialog)
  published
{    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;}
  end;

  { TCDWSColorButton }

  TCDWSColorButton = class(TWSColorButton)
  published
  end;

  { TCDWSFontDialog }

  TCDWSFontDialog = class(TWSFontDialog)
  published
{    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;}
  end;


implementation

end.
