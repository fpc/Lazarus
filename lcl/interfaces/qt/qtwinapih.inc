{%MainUnit qtint.pas}

{******************************************************************************
  All QT Winapi related stuff goes here.
  This are the overrides of the QT Interface for the methods defined in the
  lcl/include/winapih.inc

  !! Keep this alphabetical !!

 ******************************************************************************
 Interface
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc; override;
function CreateBitmapFromRawImage(const RawImage: TRawImage; var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean; Override;
//function CreateCompatibleDC(DC: HDC): HDC;

function EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer; override;
function GetBitmapRawImageDescription(Bitmap: HBITMAP; Desc: PRawImageDescription): Boolean; override;
Function GetClientBounds(handle : HWND; var ARect : TRect) : Boolean; override;
function GetDeviceRawImageDescription(DC: HDC; Desc: PRawImageDescription): boolean; override;
function GetDC(hWnd: HWND): HDC; override;
function GetCursorPos(var lpPoint: TPoint ): Boolean; override;
function GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; override;
function GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; Override;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
function ReleaseDC(hWnd: HWND; DC: HDC): Integer; override;
function SetCursorPos(X, Y: Integer): Boolean; override;
function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean; override;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line


