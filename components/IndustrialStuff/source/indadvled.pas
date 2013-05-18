{   Component(s):
    TindAdvLed  ---> old cindy name tcyAdvLed

    Description:
    An advanced led with Group feature

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
    
{**********************************************************************
 Package pl_Cindy.pkg
 for CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************
Modified by Jurassic Pork 2013 for package Industrial of Lazarus}

unit IndAdvLed;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Classes, Types, Controls, Graphics, cyBaseLed, cyGraphics;

type
  TcyCustomAdvLed = class(TcyBaseLed)  
    FPictureOn: TPicture;
    FPictureOff: TPicture;
    FPictureDisabled: TPicture;
  private
    FTransparent: boolean;
    procedure SetPictureOn(Value: TPicture);
    procedure SetPictureOff(Value: TPicture);
    procedure SetPictureDisabled(Value: TPicture);
    procedure SetTransparent(const Value: boolean);
  protected
    function TransparentColorAtPos(Point: TPoint): boolean; override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); override;
    property Height default 25;
    property Width default 25;
    property PictureOn: TPicture read FPictureOn write SetPictureOn;
    property PictureOff: TPicture read FPictureOff write SetPictureOff;
    property PictureDisabled: TPicture read FPictureDisabled write SetPictureDisabled;
    property Transparent: boolean read FTransparent write SetTransparent default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustSize; override;
  published
  end;

  TindAdvLed = class(TcyCustomAdvLed)
  private
  protected
  public
  published
    property Align;
    property Autosize;
    property Anchors;
    property Color;
    property Constraints;
    property Enabled;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // Herited from TcyBaseLed :
    property AllowAllOff;
    property GroupIndex;
    property LedValue;
    property ReadOnly;
    // Herited from TcyCustomAdvLed :
    property PictureOn;
    property PictureOff;
    property PictureDisabled;
    property Transparent;
  end;

procedure Register;

implementation
procedure Register;
begin
  RegisterComponents('Industrial',[TindAdvLed]);
end;

constructor TcyCustomAdvLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPictureOn := TPicture.Create;
  FPictureOff := TPicture.Create;
  FPictureDisabled := TPicture.Create;
  FTransparent := false;
  Height := 25;
  Width := 25;
  //Autosize := true;  // Don' t work
end;

destructor TcyCustomAdvLed.Destroy;
begin
  FPictureOn.Free;
  FPictureOff.Free;
  FPictureDisabled.Free;
  inherited Destroy;
end;

procedure TcyCustomAdvLed.Paint;
var curPicture: TPicture;
begin
  if csDesigning in ComponentState
  then 
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  case GetLedStatus of
    lsOn: curPicture := FPictureOn;
    lsOff: curPicture := FPictureOff;
    lsDisabled: curPicture := FPictureDisabled;
  end;

  if ValidGraphic(curPicture.Graphic)
  then begin
    if curPicture.Graphic.Transparent <> FTransparent
    then curPicture.Graphic.Transparent := FTransparent;

    if not AutoSize
    then begin
     { if curPicture.Graphic is TIcon
      then DrawIconEx(Canvas.Handle, 0, 0, curPicture.Icon.Handle, Width, Height, 0, 0, DI_Normal)  // Stretch draw doesn't work for icons!
      else }Canvas.StretchDraw(ClientRect, curPicture.Graphic);
    end
    else
      Canvas.Draw(0, 0, curPicture.Graphic);
  end;
end;

procedure TcyCustomAdvLed.AdjustSize;
var curPicture: TPicture;
begin
  if Autosize
  then begin
    case GetLedStatus of
      lsOn: curPicture := FPictureOn;
      lsOff: curPicture := FPictureOff;
      lsDisabled: curPicture := FPictureDisabled;
    end;

    if ValidGraphic(curPicture.Graphic)
    then SetBounds(Left, Top, curPicture.Graphic.Width, curPicture.Graphic.Height);
  end;
end;

procedure TcyCustomAdvLed.SetAutoSize(Value: Boolean);
begin
  Inherited;
  Invalidate;
end;

function TcyCustomAdvLed.TransparentColorAtPos(Point: TPoint): boolean;
var
  curPicture: TPicture;
begin
  RESULT := false; 

  if Transparent
  then begin
    case GetLedStatus of
      lsOn: curPicture := FPictureOn;
      lsOff: curPicture := FPictureOff;
      lsDisabled: curPicture := FPictureDisabled;
    end;

    if ValidGraphic(curPicture.Graphic)
    then begin
      if not AutoSize       // Convert point coordinates to pixel coordinates ...
      then begin
        Point.x := (Point.x * curPicture.Graphic.Width) div Width;
        Point.y := (Point.y * curPicture.Graphic.Height) div Height;
      end;

      RESULT := PictureIsTransparentAtPos(curPicture, Point);
    end;
  end;
end;

procedure TcyCustomAdvLed.SetPictureOn(Value: TPicture);
begin
  try
    FPictureOn.Assign(Value);
    if GetLedStatus = lsOn
    then begin
      AdjustSize;
      Invalidate;
    end;
  except
  end;
end;

procedure TcyCustomAdvLed.SetPictureOff(Value: TPicture);
begin
  try
    FPictureOff.Assign(Value);
    if GetLedStatus = lsOff
    then begin
      AdjustSize;
      Invalidate;
    end;
  except
  end;
end;

procedure TcyCustomAdvLed.SetPictureDisabled(Value: TPicture);
begin
  try
    FPictureDisabled.Assign(Value);
    if GetLedStatus = lsDisabled
    then begin
      AdjustSize;
      Invalidate;
    end;
  except
  end;
end;

procedure TcyCustomAdvLed.SetTransparent(const Value: boolean);
begin
  if value <> FTransparent
  then begin
    FTransparent := Value;
    Invalidate;
  end;
end;

end.
