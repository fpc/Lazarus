{ $Id$}
{
 *****************************************************************************
 *                               WSControls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit WSControls;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls,
////////////////////////////////////////////////////
  WSLCLClasses, WSImgList;

type
  { TWSDragImageList }

  TWSDragImageList = class(TWSCustomImageList)
  end;

  { TWSControl }

  TWSControl = class(TWSLCLComponent)
    class procedure SetCursor(const AControl: TControl; const ACursor: TCursor); virtual;
  end;

  TWSControlClass = class of TWSControl;

  { TWSWinControl }

  TWSWinControl = class(TWSControl)
    class function  HasText(const AWinControl: TWinControl): Boolean; virtual;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; virtual;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; virtual;
    class procedure SetCursor(const AControl: TControl; const ACursor: TCursor); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); virtual;
  end;       
  TWSWinControlClass = class of TWSWinControl;

  { TWSGraphicControl }

  TWSGraphicControl = class(TWSControl)
  end;

  { TWSCustomControl }

  TWSCustomControl = class(TWSWinControl)
  end;

  { TWSImageList }

  TWSImageList = class(TWSDragImageList)
  end;


implementation

uses
  LMessages;

{ TWSControl }

procedure TWSControl.SetCursor(const AControl: TControl; const ACursor: TCursor);
begin
end;

{ TWSWinControl }

function TWSWinControl.HasText(const AWinControl: TWinControl): Boolean;
begin
  Result := true;
end;

function TWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean; 
begin
  Result := CNSendMessage(LM_GETTEXT, AWinControl, @AText) <> 0;
end;
  
function TWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; 
var
  S: String;
begin
  Result := GetText(AWinControl, S);
  if Result
  then ALength := Length(S);
end;
  
procedure TWSWinControl.SetCursor(const AControl: TControl; const ACursor: TCursor);
begin
  //TODO: add default
end;

procedure TWSWinControl.SetText(const AWinControl: TWinControl; const AText: String); 
begin
  CNSendMessage(LM_SetLabel, AWinControl, PChar(AText));
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TWSDragImageList);
  RegisterWSComponent(TControl, TWSControl);
  RegisterWSComponent(TWinControl, TWSWinControl);
//  RegisterWSComponent(TGraphicControl, TWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TWSCustomControl);
//  RegisterWSComponent(TImageList, TWSImageList);
////////////////////////////////////////////////////
end.
