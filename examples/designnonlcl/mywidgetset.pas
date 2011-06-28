{ Example widgetset.
  It does not have any useful implementation, it only provides the classes
  and published properties to define a child-parent relationship and some
  coordinates. The Lazarus designer will do the rest:
  Opening, closing, editing forms of this example widgetset.
  At designtime the TMyWidgetMediator will paint.


  Copyright (C) 2009 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit MyWidgetSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, types;

type
  IMyWidgetDesigner = interface(IUnknown)
    procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
  end;

  { TMyWidget }

  TMyWidget = class(TComponent)
  private
    FAcceptChildsAtDesignTime: boolean;
    FBorderBottom: integer;
    FBorderLeft: integer;
    FBorderRight: integer;
    FBorderTop: integer;
    FCaption: string;
    FChilds: TFPList; // list of TMyWidget
    FHeight: integer;
    FLeft: integer;
    FParent: TMyWidget;
    FTop: integer;
    FVisible: boolean;
    FWidth: integer;
    function GetChilds(Index: integer): TMyWidget;
    procedure SetBorderBottom(const AValue: integer);
    procedure SetBorderLeft(const AValue: integer);
    procedure SetBorderRight(const AValue: integer);
    procedure SetBorderTop(const AValue: integer);
    procedure SetCaption(const AValue: string);
    procedure SetHeight(const AValue: integer);
    procedure SetLeft(const AValue: integer);
    procedure SetParent(const AValue: TMyWidget);
    procedure SetTop(const AValue: integer);
    procedure SetVisible(const AValue: boolean);
    procedure SetWidth(const AValue: integer);
  protected
    procedure InternalInvalidateRect(ARect: TRect; Erase: boolean); virtual;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Parent: TMyWidget read FParent write SetParent;
    function ChildCount: integer;
    property Children[Index: integer]: TMyWidget read GetChilds;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); virtual;
    procedure InvalidateRect(ARect: TRect; Erase: boolean);
    procedure Invalidate;
    property AcceptChildsAtDesignTime: boolean read FAcceptChildsAtDesignTime;
  published
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property Visible: boolean read FVisible write SetVisible;
    property BorderLeft: integer read FBorderLeft write SetBorderLeft default 5;
    property BorderRight: integer read FBorderRight write SetBorderRight default 5;
    property BorderTop: integer read FBorderTop write SetBorderTop default 20;
    property BorderBottom: integer read FBorderBottom write SetBorderBottom default 5;
    property Caption: string read FCaption write SetCaption;
  end;
  TMyWidgetClass = class of TMyWidget;

  { TMyForm }

  TMyForm = class(TMyWidget)
  private
    FDesigner: IMyWidgetDesigner;
  protected
    procedure InternalInvalidateRect(ARect: TRect; Erase: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Designer: IMyWidgetDesigner read FDesigner write FDesigner;
  end;

  { TMyButton
    A widget that does not allow children at design time }

  TMyButton = class(TMyWidget)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TMyGroupBox
    A widget that does allow children at design time }

  TMyGroupBox = class(TMyWidget)
  end;

implementation

{ TMyWidget }

function TMyWidget.GetChilds(Index: integer): TMyWidget;
begin
  Result:=TMyWidget(FChilds[Index]);
end;

procedure TMyWidget.SetBorderBottom(const AValue: integer);
begin
  if FBorderBottom=AValue then exit;
  FBorderBottom:=AValue;
  Invalidate;
end;

procedure TMyWidget.SetBorderLeft(const AValue: integer);
begin
  if FBorderLeft=AValue then exit;
  FBorderLeft:=AValue;
  Invalidate;
end;

procedure TMyWidget.SetBorderRight(const AValue: integer);
begin
  if FBorderRight=AValue then exit;
  FBorderRight:=AValue;
  Invalidate;
end;

procedure TMyWidget.SetBorderTop(const AValue: integer);
begin
  if FBorderTop=AValue then exit;
  FBorderTop:=AValue;
  Invalidate;
end;

procedure TMyWidget.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  Invalidate;
end;

procedure TMyWidget.SetHeight(const AValue: integer);
begin
  SetBounds(Left,Top,Width,AValue);
end;

procedure TMyWidget.SetLeft(const AValue: integer);
begin
  SetBounds(AValue,Top,Width,Height);
end;

procedure TMyWidget.SetParent(const AValue: TMyWidget);
begin
  if FParent=AValue then exit;
  if FParent<>nil then begin
    Invalidate;
    FParent.FChilds.Remove(Self);
  end;
  FParent:=AValue;
  if FParent<>nil then begin
    FParent.FChilds.Add(Self);
  end;
  Invalidate;
end;

procedure TMyWidget.SetTop(const AValue: integer);
begin
  SetBounds(Left,AValue,Width,Height);
end;

procedure TMyWidget.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  Invalidate;
end;

procedure TMyWidget.SetWidth(const AValue: integer);
begin
  SetBounds(Left,Top,AValue,Height);
end;

procedure TMyWidget.InternalInvalidateRect(ARect: TRect; Erase: boolean);
begin
  // see TMyForm
end;

procedure TMyWidget.SetName(const NewName: TComponentName);
begin
  if Name=Caption then Caption:=NewName;
  inherited SetName(NewName);
end;

procedure TMyWidget.SetParentComponent(Value: TComponent);
begin
  if Value is TMyWidget then
    Parent:=TMyWidget(Value);
end;

function TMyWidget.HasParent: Boolean;
begin
  Result:=Parent<>nil;
end;

function TMyWidget.GetParentComponent: TComponent;
begin
  Result:=Parent;
end;

procedure TMyWidget.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i:=0 to ChildCount-1 do
    if Children[i].Owner=Root then
      Proc(Children[i]);
end;

constructor TMyWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChilds:=TFPList.Create;
  FBorderLeft:=5;
  FBorderRight:=5;
  FBorderBottom:=5;
  FBorderTop:=20;
  FAcceptChildsAtDesignTime:=true;
end;

destructor TMyWidget.Destroy;
begin
  Parent:=nil;
  while ChildCount>0 do Children[ChildCount-1].Free;
  FreeAndNil(FChilds);
  inherited Destroy;
end;

function TMyWidget.ChildCount: integer;
begin
  Result:=FChilds.Count;
end;

procedure TMyWidget.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  if (Left=NewLeft) and (Top=NewTop) and (Width=NewWidth) and (Height=NewHeight) then
    exit;
  Invalidate;
  FLeft:=NewLeft;
  FTop:=NewTop;
  FWidth:=NewWidth;
  FHeight:=NewHeight;
  Invalidate;
end;

procedure TMyWidget.InvalidateRect(ARect: TRect; Erase: boolean);
begin
  //writeln('TMyWidget.InvalidateRect ',Name,' ',ARect.Left,',',ARect.Top);
  ARect.Left:=Max(0,ARect.Left);
  ARect.Top:=Max(0,ARect.Top);
  ARect.Right:=Min(Width,ARect.Right);
  ARect.Bottom:=Max(Height,ARect.Bottom);
  if Parent<>nil then begin
    OffsetRect(ARect,Left+Parent.BorderLeft,Top+Parent.BorderTop);
    Parent.InvalidateRect(ARect,Erase);
  end else begin
    InternalInvalidateRect(ARect,Erase);
  end;
end;

procedure TMyWidget.Invalidate;
begin
  InvalidateRect(Rect(0,0,Width,Height),false);
end;

{ TMyForm }

procedure TMyForm.InternalInvalidateRect(ARect: TRect; Erase: boolean);
begin
  if (Parent=nil) and (Designer<>nil) then
    Designer.InvalidateRect(Self,ARect,Erase);
end;

constructor TMyForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

{ TMyButton }

constructor TMyButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

end.

