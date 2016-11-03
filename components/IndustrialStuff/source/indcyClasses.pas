{  Unit indcyClasses from cyClasses

    Description:
    Unit with sub-properties for components.


    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * No contributors for now ...
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    
unit indcyClasses;

{$mode objfpc}{$H+}

// {$I cyCompilerDefines.inc}

interface

uses
   LCLIntf, Classes, Graphics, Controls, SysUtils,
   indcyTypes, indcyGraphics;

type
  tcyBevel = class(TCollectionItem)
  private
    FHighlightColor: TColor;
    FShadowColor: TColor;
    FWidth: Word;
    FStyle: TcyBevelCut;
    FDrawRight: Boolean;
    FDrawLeft: Boolean;
    FDrawTop: Boolean;
    FDrawBottom: Boolean;
    FNeedOwnerRealign: Boolean;
    procedure SetHighlightColor(const Value: TColor);
    procedure SetShadowColor(const Value: TColor);
    procedure SetWidth(const Value: Word);
    procedure SetStyle(const Value: TcyBevelCut);
    procedure SetDrawBottom(const Value: Boolean);
    procedure SetDrawLeft(const Value: Boolean);
    procedure SetDrawRight(const Value: Boolean);
    procedure SetDrawTop(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property DrawLeft: Boolean read FDrawLeft write SetDrawLeft default True;
    property DrawTop: Boolean read FDrawTop write SetDrawTop default True;
    property DrawRight: Boolean read FDrawRight write SetDrawRight default True;
    property DrawBottom: Boolean read FDrawBottom write SetDrawBottom default True;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default clBtnHighlight;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property Style: TcyBevelCut read FStyle write SetStyle default bcRaised;
    property Width: Word read FWidth write SetWidth default 1;
  end;

  TcyBevelClass = class of tcyBevel;

  tcyBevels = Class(TCollection)
  private
    FControl: TControl;
    FOnChange: TNotifyEvent;
    FNeedOwnerRealign: Boolean;
    function GetBevel(Index: Integer): TcyBevel;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aControl: TControl; BevelClass: TcyBevelClass);
    function Add: TcyBevel;
    procedure Delete(Index: Integer);
    procedure DrawBevels(aCanvas: TCanvas; var BoundsRect: TRect; RoundRect: Boolean);
    function xBevelsWidth: Integer;
    function BevelsWidth: Integer;
    property Items[Index: Integer]: TcyBevel read GetBevel; default;
    property NeedOwnerRealign: Boolean read FNeedOwnerRealign;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ tcyBevel }
procedure tcyBevel.Assign(Source: TPersistent);
begin
  if Source is tcyBevel then
  begin
    FHighlightColor := tcyBevel(Source).FHighlightColor;
    FShadowColor := tcyBevel(Source).FShadowColor;
    FWidth := tcyBevel(Source).FWidth;
    FStyle  := tcyBevel(Source).FStyle;
    FDrawRight  := tcyBevel(Source).FDrawRight;
    FDrawLeft    := tcyBevel(Source).FDrawLeft;
    FDrawTop := tcyBevel(Source).FDrawTop;
    FDrawBottom := tcyBevel(Source).FDrawBottom;
  end;
//  inherited Assign(Source);
end;

constructor tcyBevel.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FHighlightColor := clBtnHighlight;
  FShadowColor := clBtnShadow;
  FWidth := 1;
  FStyle := bcRaised;
  FDrawLeft := true;
  FDrawTop := true;
  FDrawRight := true;
  FDrawBottom := true;
  FNeedOwnerRealign := true;
end;

function tcyBevel.GetDisplayName: string;
begin
  case FStyle of
    bcLowered: Result := 'Lowered';
    bcRaised: Result := 'Raised';
    bcNone: Result := 'None';
    bcTransparent: Result := 'Transparent';
  end;

  Result := Result + ' Bevel';
  Result := Result + ' Width = ' + intToStr(FWidth);
end;

procedure tcyBevel.SetDrawBottom(const Value: Boolean);
begin
  FDrawBottom := Value;
  Changed(false);            // It will call TcyBevels.Update !
end;

procedure tcyBevel.SetDrawLeft(const Value: Boolean);
begin
  FDrawLeft := Value;
  Changed(false);
end;

procedure tcyBevel.SetDrawRight(const Value: Boolean);
begin
  FDrawRight := Value;
  Changed(false);
end;

procedure tcyBevel.SetDrawTop(const Value: Boolean);
begin
  FDrawTop := Value;
  Changed(false);
end;

procedure tcyBevel.SetHighlightColor(const Value: TColor);
begin
  FHighlightColor := Value;
  Changed(false);
end;

procedure tcyBevel.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Changed(false);
end;

procedure tcyBevel.SetStyle(const Value: TcyBevelCut);
begin
  if FStyle = Value then EXIT;

  if (FStyle = bcNone) or (Value = bcNone)
  then FNeedOwnerRealign := true;

  FStyle := Value;
  Changed(false);
end;

procedure tcyBevel.SetWidth(const Value: Word);
begin
  if FWidth = Value then EXIT;

  FWidth := Value;
  FNeedOwnerRealign := true;
  Changed(false);
end;

{TcyBevels}
constructor TcyBevels.Create(aControl: TControl; BevelClass: TcyBevelClass);
begin
  inherited Create(BevelClass);
  FControl := aControl;
  FNeedOwnerRealign := false;
end;

function TcyBevels.GetBevel(Index: Integer): TcyBevel;
begin
  Result := TcyBevel(inherited Items[Index]);
end;

function TcyBevels.GetOwner: TPersistent;
begin
  Result := FControl;
end;

// Event Called by setting properties/events of TcyBevel :
procedure TcyBevels.Update(Item: TCollectionItem);
begin
  Inherited;
  if Assigned(FOnChange)
  then begin
    if Item <> nil
    then
      if TcyBevel(Item).FNeedOwnerRealign
      then begin
        FNeedOwnerRealign := true;
        TcyBevel(Item).FNeedOwnerRealign := false;
      end;

    FOnChange(Self);
    FNeedOwnerRealign := false;
  end
  else
    FControl.Invalidate;
end;

function TcyBevels.Add: TcyBevel;
begin
  Result := TcyBevel(inherited Add);
  Result.Changed(false);      // It will call TcyBevels.Update only at run-time!
end;

procedure TcyBevels.Delete(Index: Integer);
begin
  Inherited;
  FNeedOwnerRealign := true;
  Update(Nil);
end;

procedure TcyBevels.DrawBevels(aCanvas: TCanvas; var BoundsRect: TRect; RoundRect: Boolean);
var i: Integer;
begin
  for i := 0 to Count-1 do
    case Items[i].FStyle of
      bcRaised:
        begin
          cyFrame3D(aCanvas, BoundsRect, Items[i].FHighlightColor, Items[i].FShadowColor, Items[i].FWidth,
              Items[i].FDrawLeft, Items[i].FDrawTop, Items[i].FDrawRight, Items[i].FDrawBottom, RoundRect);
          RoundRect := false;
        end;

      bcLowered:
        begin
          cyFrame3D(aCanvas, BoundsRect, Items[i].FShadowColor, Items[i].FHighlightColor, Items[i].FWidth,
              Items[i].FDrawLeft, Items[i].FDrawTop, Items[i].FDrawRight, Items[i].FDrawBottom, RoundRect);
          RoundRect := false;
        end;

      bcTransparent:   // Just Inflate Rect
        begin
          InflateRect(BoundsRect, (-1) * Items[i].FWidth, (-1) * Items[i].FWidth);
          RoundRect := false;
        end;

      bcNone: ;
    end;
end;

function TcyBevels.xBevelsWidth: Integer;
begin
  RESULT := 0;
end;

// 9999 for All other units like TcySimpleGauge
function TcyBevels.BevelsWidth: Integer;
var i: Integer;
begin
  RESULT := 0;
  for i := 0 to Count-1 do
    if Items[i].FStyle <> bcNone
    then Inc(RESULT, Items[i].FWidth);
end;

end.
