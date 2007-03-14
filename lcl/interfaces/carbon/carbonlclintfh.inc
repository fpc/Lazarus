{%MainUnit carbonint.pas}

{******************************************************************************
  All Carbon interface communication implementations.
  This are the overrides of the Carbon Interface for the methods defined in the
  lcl/include/lclintfh.inc

  !! Keep alphabetical !!

 ******************************************************************************
 Interface
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

function CreateStandardCursor(ACursor: SmallInt): hCursor; override;
function DrawSplitter(DC: HDC; const ARect: TRect; Horizontal: boolean): boolean; override;

function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
  Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;

function FontCanUTF8(Font: HFont): boolean; override;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetLCLOwnerObject(Handle: HWnd): TObject; override;
function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; override;
function GetListBoxItemRect(ListBox: TComponent; Index: integer; var ARect: TRect): boolean; override;

function IntfSendsUTF8KeyPress: boolean; override;

function PromptUser(const DialogCaption : string;
                    const DialogMessage : string;
                          DialogType    : LongInt;
                          Buttons       : PLongInt;
                          ButtonCount   : LongInt;
                          DefaultIndex  : LongInt;
                          EscapeResult  : LongInt) : LongInt; override;

function ReplaceBitmapMask(var Image, Mask: HBitmap; NewMask: HBitmap): boolean; override;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

// included by carbonint.pas
