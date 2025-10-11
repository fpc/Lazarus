{
 /***************************************************************************
                               chatcontrol.pp
                             -------------------
                      basic 2-user-chat displaying control
 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ChatControl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Graphics, Controls, ExtCtrls, StdCtrls, Forms, TypingIndicator;

Const
  DefaultBackground : TColor = $00CDEBD0;
  DefaultRightBackground : TColor = $00B6F5A2;
  DefaultLeftBackground : TColor = $00F0F0F0;
  DefaultItemSpacing = 8;
  DefaultItemPadding = 8;
  DefaultItemMargin  = 4;

Type
  TTextSide = (tsLeft,tsRight);

  { TChatItem }

  TChatItem = Class(TObject)
  private
    FText: String;
    FSide: TTextSide;
  Public
    constructor Create(aText : String; aSide : TTextSide);
    Property Text : String Read FText Write FText;
    Property Side : TTextSide Read FSide Write FSide;
  end;
  TChatItemArray = Array of TChatItem;

  TItemClickEvent = procedure(Sender : TObject; aItem : TChatItem) of object;

  { TChatControl }

  TChatControl = class(TScrollingWinControl)
  Protected
    Type
      TDisplayChatItem = Class;
  Private
    FChatList : TFPObjectList;
    FCtrlSelects: Boolean;
    FItemMargin: Integer;
    FItemPadding: Integer;
    FItemSpacing: Integer;
    FLeftBackground: TColor;
    FLeftTextColor: TColor;
    FOnItemClick: TItemClickEvent;
    FRightBackground: TColor;
    FRightTextColor: TColor;
    FTyping : Array[TTextSide] of TTypingIndicator;
    function GetChatCount: Integer;
    function GetDisplayItem(aIndex: Integer): TDisplayChatItem;
    function GetIndicatorSettings(AIndex: Integer): TTypingDotIndicatorSettings;
    function GetIsTyping(aSide : TTextSide): Boolean;
    function GetItem(aIndex : integer): TChatItem;
    function GetLeftTyping: Boolean;
    function GetRightTyping: Boolean;
    function GetTyping(aIndex : TTextSide): TTypingIndicator;
    function LayoutTypingItem(aSide: TTextSide; aTop: Integer): Integer;
    procedure SetIndicatorSettings(AIndex: Integer; AValue: TTypingDotIndicatorSettings);
    procedure SetIsTyping(aSide : TTextSide; AValue: Boolean);
    procedure SetItemMargin(AValue: Integer);
    procedure SetItemPadding(AValue: Integer);
    procedure SetItemSpacing(AValue: Integer);
    procedure SetLeftBackground(AValue: TColor);
    procedure SetLeftTextColor(AValue: TColor);
    procedure SetLeftTyping(AValue: Boolean);
    procedure SetRightBackground(AValue: TColor);
    procedure SetRightTextColor(AValue: TColor);
    procedure SetRightTyping(AValue: Boolean);
  Protected
    Type

      { TDisplayChatItem }

      TDisplayChatItem = class(TChatItem)
      Private
        Type
          TColorKind = (ckBackground,ckBackgroundPen,ckBackgroundBrush,ckLabelFont,ckLabelBack);
      Private
        FBackground: TShape;
        FLabel: TLabel;
        FOnClick: TNotifyEvent;
        FDownShift: TShiftState;
        FSelected: Boolean;
        FSavedColors: Array[TColorKind] of TColor;
        procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        function GetBottom: Integer;
        procedure SetSelected(AValue: Boolean);
        procedure SetColors;
      protected
        procedure SetBackground(AValue: TShape); virtual;
        procedure SetLabel(AValue: TLabel); virtual;
        Procedure LayoutAt(aTop,aPadding,aMargin : Integer); virtual;
        procedure DoClick(Sender : TObject); virtual;
        procedure Invalidate; virtual;
      Public
        Destructor Destroy; override;
        Property OnClick : TNotifyEvent Read FOnClick Write FOnClick;
        Property Bottom : Integer Read GetBottom;
        Property TextLabel : TLabel Read FLabel Write SetLabel;
        Property TextBackGround : TShape Read FBackground Write SetBackground;
        Property Selected : Boolean read FSelected Write SetSelected;
      end;

    procedure DoItemClick(Sender: TObject);
    procedure SelectItem(aItem : TDisplayChatItem; IsSelect,aAddToSelection : Boolean);
    function CreateChatLabel(aParent: TWinControl): TLabel; virtual;
    function CreateTypingIndicator(aSide: TTextSide): TTypingIndicator; virtual;
    function DoCreateItem(aText: String; aSide: TTextSide): TDisplayChatItem; virtual;
    function CreateItem(aText: String; aSide: TTextSide; aParent: TWinControl): TDisplayChatItem;
    procedure DoOnResize; override;
    function LayoutItem(aIndex: Integer; aTop: Integer): Integer;
    procedure LayoutItems;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function LayoutTyping(aTop: Integer): Integer;
    Property Typing [aIndex : TTextSide] : TTypingIndicator Read GetTyping;
    Property DisplayItems[aIndex: Integer] : TDisplayChatItem Read GetDisplayItem;
  Public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure CopySelectionToClipBoard;
    Procedure AddText(const aText : String; aSide : TTextSide);
    Procedure SelectItem(aIndex : Integer; aAddToSelection : Boolean);
    Procedure DeleteItem(aItem : TChatItem);
    Procedure DeleteItem(aIndex : Integer);
    Function GetItemAt(X,Y : Integer) : TChatItem;
    Function GetSelectedItems : TChatItemArray;
    Property ChatItems[aIndex : integer] : TChatItem Read GetItem;
    Property ChatCount : Integer Read GetChatCount;
    Property IsTyping[aSide : TTextSide] : Boolean Read GetIsTyping Write SetIsTyping;
  Published
    Property CtrlSelects : Boolean Read FCtrlSelects Write FCtrlSelects;
    Property OnItemClick : TItemClickEvent Read FOnItemClick Write FOnItemClick;
    Property LeftTyping : Boolean Read GetLeftTyping Write SetLeftTyping;
    Property RightTyping : Boolean Read GetRightTyping Write SetRightTyping;
    Property LeftBackground : TColor Read FLeftBackground Write SetLeftBackground;
    Property RightBackground : TColor Read FRightBackground Write SetRightBackground;
    Property LeftTextColor : TColor Read FLeftTextColor Write SetLeftTextColor;
    Property RightTextColor : TColor Read FRightTextColor Write SetRightTextColor;
    Property ItemSpacing : Integer Read FItemSpacing Write SetItemSpacing;
    Property ItemPadding : Integer Read FItemPadding Write SetItemPadding;
    Property ItemMargin : Integer Read FItemMargin Write SetItemMargin;
    Property LeftTypingIndicator : TTypingDotIndicatorSettings Index Ord(tsLeft) Read GetIndicatorSettings Write SetIndicatorSettings;
    Property RightTypingIndicator : TTypingDotIndicatorSettings Index Ord(tsRight) Read GetIndicatorSettings Write SetIndicatorSettings;
  Published
    property Align;
    property Anchors;
    property AutoSize;
    property AutoScroll default True;
    property BorderSpacing;
    property BiDiMode;
    property BorderStyle default bsSingle;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Font;
    property ParentBackground default True;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    //property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnPaint;
  end;

implementation

uses
  LCLType, Clipbrd;

{ TChatItem }

constructor TChatItem.Create(aText: String; aSide: TTextSide);
begin
  FText:=aText;
  FSide:=aSide;
end;

{ TChatControl }

procedure TChatControl.SetItemSpacing(AValue: Integer);
begin
  if FItemSpacing=AValue then Exit;
  FItemSpacing:=AValue;
  LayoutItems;
end;

function TChatControl.LayoutItem(aIndex: Integer; aTop: Integer): Integer;

var
  NewMax : Integer;
  Recalc : Boolean;
  W,H : Integer;

begin
  With TDisplayChatItem(FChatList[aIndex]) do
    begin
    NewMax:=Round(Width*0.75);
    // Work around a strange behaviour in Laz:
    // When resizing and we have a bigger size, the label does not resize to take more space.
    // After much experimenting the following workaround was found:
    // By switching off wordwrap and switching back on wordwrap, it works.
    Recalc:=NewMax>TextLabel.Constraints.MaxWidth;
    TextLabel.Constraints.MaxWidth:=NewMax;
    if Recalc then
      begin
      TextLabel.WordWrap:=False;
      TextLabel.WordWrap:=True;
      end;
    LayoutAt(aTop,FItemPadding,FItemMargin);
    Result:=Bottom+ItemSpacing;
    end;
end;

procedure TChatControl.LayoutItems;

var
  I : Integer;
  lTop : Integer;

begin
  lTop:=ItemSpacing;
  For I:=0 to FChatList.Count-1 do
    lTop:=LayoutItem(I,lTop)+ItemSpacing;
  LayoutTyping(lTop);
  VertScrollbar.Position:=VertScrollbar.Range-1;
end;

procedure TChatControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key=Ord('c')) or (Key=ord('C')) and (Shift=[ssCtrl]) then
    CopySelectionToClipBoard;
end;

function TChatControl.LayoutTypingItem(aSide : TTextSide; aTop : Integer) : Integer;

var
  Ind : TTypingIndicator;
  lPos : TPoint;

begin
  Ind:=FTyping[aSide];
  lPos.Y:=aTop;
  if aSide=tsLeft then
    lPos.X:=ItemMargin
  else
    lPos.X:=Width-Ind.Width-ItemMargin;
  Ind.SetBounds(lPos.X,lPos.Y,80,40);
  Result:=aTop+Ind.Height;
end;

procedure TChatControl.SetIndicatorSettings(AIndex: Integer; AValue: TTypingDotIndicatorSettings);
begin
  FTyping[TTextSide(aIndex)].DotSettings:=aValue;
end;

procedure TChatControl.SetIsTyping(aSide : TTextSide; AValue: Boolean);
begin
  FTyping[aSide].Visible:=aValue;
  FTyping[aSide].Active:=aValue;
end;

function TChatControl.LayoutTyping(aTop: Integer): Integer;

var
  aSide : TTextSide;

begin
  if ChatCount=0 then
    aSide:=tsLeft
  else
    aSide:=ChatItems[ChatCount-1].Side;
  aTop:=LayoutTypingItem(aSide,aTop)+ItemSpacing;
  if aSide=tsLeft then
    aSide:=tsRight
  else
    aSide:=tsLeft;
  Result:=LayoutTypingItem(aSide,aTop);
end;

procedure TChatControl.SetItemMargin(AValue: Integer);
begin
  if FItemMargin=AValue then Exit;
  FItemMargin:=AValue;
  LayoutItems;
end;

procedure TChatControl.SetItemPadding(AValue: Integer);
begin
  if FItemPadding=AValue then Exit;
  FItemPadding:=AValue;
  LayoutItems;
end;

function TChatControl.GetItem(aIndex : integer): TChatItem;
begin
  Result:=TChatItem(FChatList[aIndex])
end;

function TChatControl.GetLeftTyping: Boolean;
begin
  Result:=FTyping[tsLeft].Visible;
end;

function TChatControl.GetRightTyping: Boolean;
begin
  Result:=FTyping[tsRight].Visible;
end;

function TChatControl.GetTyping(aIndex : TTextSide): TTypingIndicator;
begin
  Result:=FTyping[aIndex]
end;

function TChatControl.GetChatCount: Integer;
begin
  Result:=FChatList.Count;
end;

function TChatControl.GetDisplayItem(aIndex: Integer): TDisplayChatItem;
begin
  Result:=TDisplayChatItem(FChatList[aIndex]);
end;

procedure TChatControl.DoItemClick(Sender: TObject);

var
  lItm : TDisplayChatItem absolute sender;
  doSelect : Boolean;
  doAdd : Boolean;

begin
  if not (Sender is TDisplayChatItem) then
    exit;
  doSelect:=(Not CtrlSelects) or (ssCtrl in lItm.FDownShift);
  if DoSelect then
    begin
    if CtrlSelects then
      doAdd:=ssShift in lItm.FDownShift
    else
      doAdd:=ssCtrl in lItm.FDownShift;
    SelectItem(lItm,not lItm.Selected,doAdd);
    end;
  If Assigned(FOnItemClick)  then
    FOnItemClick(Self,lItm);
end;

function TChatControl.GetIndicatorSettings(AIndex: Integer): TTypingDotIndicatorSettings;
begin
  Result:=FTyping[TTextSide(aIndex)].DotSettings;
end;

function TChatControl.GetIsTyping(aSide : TTextSide): Boolean;
begin
  Result:=FTyping[aSide].Visible;
end;

procedure TChatControl.SetLeftBackground(AValue: TColor);
begin
  if FLeftBackground=AValue then Exit;
  FLeftBackground:=AValue;
end;

procedure TChatControl.SetLeftTextColor(AValue: TColor);
begin
  if FLeftTextColor=AValue then Exit;
  FLeftTextColor:=AValue;
end;

procedure TChatControl.SetLeftTyping(AValue: Boolean);
begin
  FTyping[tsLeft].Visible:=aValue;
end;

procedure TChatControl.SetRightBackground(AValue: TColor);
begin
  if FRightBackground=AValue then Exit;
  FRightBackground:=AValue;
end;

procedure TChatControl.SetRightTextColor(AValue: TColor);
begin
  if FRightTextColor=AValue then Exit;
  FRightTextColor:=AValue;
end;

procedure TChatControl.SetRightTyping(AValue: Boolean);
begin
  FTyping[tsRight].Visible:=aValue;
end;

procedure TChatControl.SelectItem(aItem: TDisplayChatItem; IsSelect, aAddToSelection: Boolean);
var
  I : Integer;
  lItem : TDisplayChatItem;

begin
  if IsSelect and not aAddToSelection then
    For I:=0 to ChatCount-1 do
      begin
      lItem:=DisplayItems[i];
      if (lItem<>aItem) and lItem.Selected then
        lItem.Selected:=False;
      end;
   aItem.Selected:=IsSelect;
end;

function TChatControl.CreateChatLabel(aParent : TWinControl): TLabel;
begin
  Result:=Tlabel.Create(Self);
  Result.Parent:=aParent;
end;

function TChatControl.DoCreateItem(aText: String; aSide: TTextSide): TDisplayChatItem;
begin
  Result:=TDisplayChatItem.Create(aText,aSide);
end;

function TChatControl.CreateItem(aText: String; aSide: TTextSide; aParent: TWinControl): TDisplayChatItem;
var
  L : TLabel;
  S : TShape;
begin
  Result:=DoCreateItem(aText,aSide);
  L:=CreateChatLabel(Self);
  L.AutoSize:=True;
  L.Constraints.MaxWidth:=Round(ClientWidth*0.75);
  L.WordWrap:=True;
  L.Caption:=Result.Text;
  S:=TShape.Create(Self);
  S.Parent:=Self;
  S.Shape:=stRoundRect;
  if aSide=tsLeft then
    begin
    L.Font.Color:=LeftTextColor;
    S.Brush.Color:=LeftBackground;
    S.Pen.Color:=LeftBackground;
    end
  else
    begin
    L.Font.Color:=RightTextColor;
    S.Brush.Color:=RightBackground;
    S.Pen.Color:=RightBackground;
    end;
  Result.TextLabel:=L;
  Result.TextBackground:=S;
  Result.OnClick:=@DoItemClick;
end;

procedure TChatControl.DoOnResize;
begin
  inherited DoOnResize;
  LayoutItems;
end;

function TChatControl.CreateTypingIndicator(aSide: TTextSide): TTypingIndicator;

begin
  Result:=TTypingIndicator.Create(Self);
  Result.Parent:=Self;
  Result.Visible:=False;
  Result.Active:=False;
end;

constructor TChatControl.Create(aOwner: TComponent);
begin
  Inherited;
  fCompStyle:= csScrollBox;
  ControlStyle := ControlStyle + [csCaptureMouse]
    - [csOpaque] + [csParentBackground]; // we need the default background
  AutoScroll := True;
  BorderStyle := bsSingle;
  FTyping[tsLeft]:=CreateTypingIndicator(tsLeft);
  FTyping[tsRight]:=CreateTypingIndicator(tsRight);
  FChatList:=TFPObjectList.Create(True);
  Color:=DefaultBackground;
  RightTextColor:=clBlack;
  LeftTextColor:=clBlack;
  RightBackground:=DefaultRightBackGround;
  LeftBackground:=DefaultLeftBackGround;
  ItemSpacing:=DefaultItemSpacing;
  ItemPadding:=DefaultItemPadding;
  ItemMargin:=DefaultItemMargin;
end;


destructor TChatControl.Destroy;
begin
  FreeAndNil(FTyping[tsLeft]);
  FreeAndNil(FTyping[tsRight]);
  FreeAndNil(FChatList);
  inherited destroy;
end;

procedure TChatControl.Clear;
begin
  FChatList.Clear;
  Invalidate;
end;

procedure TChatControl.CopySelectionToClipBoard;
var
  I,lCount : Integer;
  S : String;

begin
  lCount:=0;
  For I:=0 to ChatCount-1 do
    if DisplayItems[i].selected then
      inc(lCount);
  if lCount=0 then
    exit;
  S:='';
  For I:=0 to ChatCount-1 do
    if DisplayItems[i].selected then
      begin
      if S<>'' then
        S:=S+sLineBreak+sLineBreak;
      S:=S+DisplayItems[i].Text;
      end;
  ClipBoard.AsText:=S;
end;

procedure TChatControl.AddText(const aText: String; aSide: TTextSide);

var
  aItem : TDisplayChatItem;
  Idx,ltop : Integer;
begin
  aItem:=CreateItem(aText,aSide,Self);
  Idx:=FChatList.Add(aItem);
  lTop:=ItemSpacing;
  if Idx>0 then
    lTop:=lTop+TDisplayChatItem(FChatList[Idx-1]).Bottom;
  lTop:=LayoutItem(Idx,lTop)+ItemSpacing;
  LayoutTyping(lTop);
  VertScrollbar.Position:=VertScrollbar.Range-1;
end;

procedure TChatControl.SelectItem(aIndex: Integer; aAddToSelection: Boolean);
begin
  SelectItem(ChatItems[aIndex] as TDisplayChatItem,True,aAddToSelection);
end;

procedure TChatControl.DeleteItem(aItem: TChatItem);
begin
  FChatList.Remove(aItem);
  Invalidate;
end;

procedure TChatControl.DeleteItem(aIndex: Integer);
begin
  FChatList.Delete(aIndex);
  Invalidate;
end;

function TChatControl.GetItemAt(X, Y: Integer): TChatItem;

var
  lPt : TPoint;
  Itm : TDisplayChatItem;
  I : Integer;

begin
  Result:=Nil;
  lpt:=GetClientScrollOffset;
  lpt.Offset(X,y);
  for I:=0 to FChatList.Count-1 do
    begin
    Itm:=TDisplayChatItem(GetItem(I));
    if Itm.FBackground.BoundsRect.Contains(lpt) then
      Exit(Itm);
    end;
end;

function TChatControl.GetSelectedItems: TChatItemArray;

var
  i,lCount : Integer;
  lItem : TDisplayChatItem;
begin
  lCount:=0;
  For I:=0 to FChatList.Count-1 do
    if TDisplayChatItem(FChatList[i]).Selected then
      inc(lCount);
  SetLength(Result,lCount);
  if lCount=0 then exit;
  lCount:=0;
  For I:=0 to FChatList.Count-1 do
    begin
    lItem:=TDisplayChatItem(FChatList[i]);
    if lItem.Selected then
      begin
      Result[lCount]:=lItem;
      inc(lCount);
      end;
    end;
end;

{ TChatControl.TChatItem }

function TChatControl.TDisplayChatItem.GetBottom: Integer;
begin
  Result:=FBackground.Top+FBackground.Height;
end;

procedure TChatControl.TDisplayChatItem.SetColors;

begin
  if Not Selected then
    begin
    if Assigned(FBackground) then
      begin
      FBackground.Pen.Color:=FSavedColors[ckBackgroundPen];
      FBackground.Brush.Color:=FSavedColors[ckBackgroundBrush];
      FBackground.Color:=FSavedColors[ckBackground];
      end;
    if Assigned(FLabel) then
      begin
      FLabel.Font.Color:=FSavedColors[ckLabelFont];
      FLabel.Color:=FSavedColors[ckLabelBack];
      end;
    end
  else
    begin
    if Assigned(FBackground) then
      begin
      FBackground.Pen.Color:=clHighlight;
      FBackground.Brush.Color:=clHighlight;
      FBackground.Color:=clHighlight;
      end;
    if Assigned(FLabel) then
      begin
      FLabel.Font.Color:=clHighlightText;
      FLabel.Color:=clHighlight;
      end;
    end;
end;

procedure TChatControl.TDisplayChatItem.SetSelected(AValue: Boolean);
begin
  if FSelected=AValue then Exit;
  FSelected:=AValue;
  SetColors;
end;

procedure TChatControl.TDisplayChatItem.Invalidate;

begin
  If assigned(FBackground) then
    FBackground.Invalidate;
  If assigned(FLabel) then
    FLabel.Invalidate;
end;

procedure TChatControl.TDisplayChatItem.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  FDownShift:=Shift;
end;

procedure TChatControl.TDisplayChatItem.SetBackground(AValue: TShape);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
  if Assigned(FBackground) then
    begin
    FSavedColors[ckBackground]:=FBackGround.Color;
    FSavedColors[ckBackgroundPen]:=FBackGround.Pen.Color;
    FSavedColors[ckBackgroundBrush]:=FBackGround.Brush.Color;
    FBackGround.OnClick:=@DoClick;
    FBackGround.OnMouseDown:=@DoMouseDown;
    end;
end;

procedure TChatControl.TDisplayChatItem.SetLabel(AValue: TLabel);
begin
  if FLabel=AValue then Exit;
  FLabel:=AValue;
  if Assigned(FLabel) then
    begin
    FSavedColors[ckLabelFont]:=FLabel.Font.Color;
    FSavedColors[ckLabelBack]:=FLabel.Color;
    FLabel.OnClick:=@DoClick;
    FLabel.OnMouseDown:=@DoMouseDown;
    end;
end;

destructor TChatControl.TDisplayChatItem.Destroy;
begin
  FreeAndNil(FLabel);
  FreeAndNil(FBackground);
  inherited Destroy;
end;

procedure TChatControl.TDisplayChatItem.LayoutAt(aTop, aPadding, aMargin: Integer);

var
  lPos : TPoint;
  lSize : TPoint;

begin
  lPos.Y:=aTop;
  lSize.X:=TextLabel.Width;
  lSize.Y:=TextLabel.Height;
  Case Side of
   tsLeft : lPos.X:=4;
   tsRight : lPos.X:=TextLabel.Parent.Width-lSize.X-2*aPadding-aMargin;
  end;
  TextBackground.SetBounds(lPos.X,lpos.Y,lSize.X+2*aPadding,lSize.Y+2*aPadding);
  TextLabel.Left:=lPos.X+aPadding;
  TextLabel.Top:=lPos.Y+aPadding;
  TextLabel.BringToFront;
end;

procedure TChatControl.TDisplayChatItem.DoClick(Sender: TObject);
begin
  If Assigned(FOnClick) then
    FOnClick(Self);
end;

end.

