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

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
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
  private
  protected
  public
  end;

  { TWSControl }

  TWSControl = class(TWSLCLComponent)
  private
  protected
  public
    class procedure SetCursor(const AControl: TControl; const ACursor: TCursor); virtual;
  end;

  TWSControlClass = class of TWSControl;

  { TWSWinControl }

  TWSWinControlClass = class of TWSWinControl;
  TWSWinControl = class(TWSControl)
  private
  protected
  public    
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; virtual;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; virtual;
    class procedure SetCursor(const AControl: TControl; const ACursor: TCursor); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); virtual;
  end;       

  { TWSGraphicControl }

  TWSGraphicControl = class(TWSControl)
  private
  protected
  public
  end;

  { TWSCustomControl }

  TWSCustomControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSImageList }

  TWSImageList = class(TWSDragImageList)
  private
  protected
  public
  end;


implementation

uses
  LMessages;

{ TWSControl }

procedure TWSControl.SetCursor(const AControl: TControl; const ACursor: TCursor);
begin
end;

{ TWSWinControl }

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
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TWSDragImageList);
  RegisterWSComponent(TControl, TWSControl);
  RegisterWSComponent(TWinControl, TWSWinControl);
//  RegisterWSComponent(TWinControl, TWSWinControl);
//  RegisterWSComponent(TGraphicControl, TWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TWSCustomControl);
//  RegisterWSComponent(TImageList, TWSImageList);
////////////////////////////////////////////////////
end.
