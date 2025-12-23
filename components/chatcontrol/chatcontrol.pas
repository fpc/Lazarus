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
  Classes, SysUtils, Contnrs, Graphics, Controls, ExtCtrls, StdCtrls, Forms, TypingIndicator,
  markdown.canvasrender, markdown.control;

Const
  DefaultBackground : TColor = $00CDEBD0;
  DefaultRightBackground : TColor = $00B6F5A2;
  DefaultLeftBackground : TColor = $00F0F0F0;
  DefaultItemSpacing = 8;
  DefaultItemPadding = 8;
  DefaultItemMargin  = 4;

Const
  // Integer
  IdxBaseFontSize     = 0;
  IdxBlockQuoteIndent = 1;
  IdxParagraphSpacing = 2;
  IdxExtraIndent      = 3;
  IdxImageMargin      = 4;
  MaxIntegerIdx       = IdxImageMargin;

  // Strings
  IdxFontName     = 0;
  IdxMonoFontName = 1;
  IdxBulletChar1  = 2;
  IdxBulletChar2  = 3;
  IdxBulletChar3  = 4;
  MaxStringIdx    = IdxBulletChar3;

  // Color
  IdxFontColor      = 0;
  IdxFontCodeColor  = 1;
  IdxFontQuoteColor = 2;
  IdxHyperLinkColor = 3;
  IdxBGCodeColor    = 4;
  MaxColorIdx       = IdxBGCodeColor;

Type
  TTextSide = (tsLeft,tsRight);

  { TChatItem }

  TChatItem = Class(TObject)
  private
    FMarkDown: Boolean;
    FText: String;
    FSide: TTextSide;
  Public
    constructor Create(aText : String; aSide : TTextSide);
    Property Text : String Read FText Write FText;
    Property Side : TTextSide Read FSide Write FSide;
    property MarkDown : Boolean Read FMarkDown Write FMarkDown;
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
    FOnGetImage: TMarkdownImageEvent;
    FOnItemClick: TItemClickEvent;
    FOnOpenURL: TOpenURLEvent;
    FRightBackground: TColor;
    FRightTextColor: TColor;
    FTyping : Array[TTextSide] of TTypingIndicator;
    FMarkDownColors : Array [0..MaxColorIdx] of TColor;
    FMarkDownSizes: Array[0..MaxIntegerIdx] of Integer;
    FMarkdownStrings : Array[0..MaxStringIdx] of string;
    procedure DoGetOnImage(Sender: TObject; const aURL: string; var aImage: TPicture);
    procedure DoOpenURL(Sender: TObject; aURL: String);
    function GetChatCount: Integer;
    function GetMarkDownColor(AIndex: Integer): TColor;
    function GetDisplayItem(aIndex: Integer): TDisplayChatItem;
    function GetIndicatorSettings(AIndex: Integer): TTypingDotIndicatorSettings;
    function GetMarkDownInteger(AIndex: Integer): Integer;
    function GetIsTyping(aSide : TTextSide): Boolean;
    function GetItem(aIndex : integer): TChatItem;
    function GetLeftTyping: Boolean;
    function GetRightTyping: Boolean;
    function GetMarkdownString(AIndex: Integer): string;
    function GetTyping(aIndex : TTextSide): TTypingIndicator;
    function LayoutTypingItem(aSide: TTextSide; aTop: Integer): Integer;
    procedure SetIndicatorSettings(AIndex: Integer; AValue: TTypingDotIndicatorSettings);
    procedure SetMarkDownColorsOnItems(aIndex: Integer);
    procedure SetMarkdownInteger(AIndex: Integer; const aValue: Integer);
    procedure SetIsTyping(aSide : TTextSide; AValue: Boolean);
    procedure SetItemMargin(AValue: Integer);
    procedure SetItemPadding(AValue: Integer);
    procedure SetItemSpacing(AValue: Integer);
    procedure SetLeftBackground(AValue: TColor);
    procedure SetLeftTextColor(AValue: TColor);
    procedure SetLeftTyping(AValue: Boolean);
    procedure SetMarkDownIntegersOnItems(aIndex: Integer);
    procedure SetMarkDownStringsOnItems(aIndex: Integer);
    procedure SetMarkdownColor(AIndex: Integer; const aValue: TColor);
    procedure SetRightBackground(AValue: TColor);
    procedure SetRightTextColor(AValue: TColor);
    procedure SetRightTyping(AValue: Boolean);
    procedure SetMarkDownString(AIndex: Integer; const aValue: string);
  Protected
    Type

      { TDisplayChatItem }

      TDisplayChatItem = class(TChatItem)
      Private
        Type
          TColorKind = (ckBackground,ckBackgroundPen,ckBackgroundBrush,ckLabelFont,ckLabelBack);
      Private
        FBackground: TShape;
        FOnClick: TNotifyEvent;
        FDownShift: TShiftState;
        FSelected: Boolean;
        FSavedColors: Array[TColorKind] of TColor;
        procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        function GetBottom: Integer;
        procedure SetSelected(AValue: Boolean);
      protected
        procedure SetColors; virtual;
        procedure SetBackground(AValue: TShape); virtual;
        procedure SetMaxWidth(aMaxWidth : Integer); virtual;
        Procedure LayoutAt(aTop,aPadding,aMargin : Integer); virtual; abstract;
        procedure DoClick(Sender : TObject); virtual;
        procedure Invalidate; virtual;
      Public
        Destructor Destroy; override;
        Property OnClick : TNotifyEvent Read FOnClick Write FOnClick;
        Property Bottom : Integer Read GetBottom;
        Property TextBackGround : TShape Read FBackground Write SetBackground;
        Property Selected : Boolean read FSelected Write SetSelected;
      end;

      { TPlainDisplayChatItem }

      TPlainDisplayChatItem = class(TDisplayChatItem)
      private
        FLabel: TLabel;
        procedure SetLabel(const aValue: TLabel);
      Protected
        procedure Invalidate; override;
        procedure SetColors; override;
        procedure SetMaxWidth(aMaxWidth: Integer); override;
        procedure LayoutAt(aTop,aPadding,aMargin : Integer); override;
      public
        destructor destroy; override;
        Property TextLabel : TLabel Read FLabel Write SetLabel;
      end;

      { TMarkdownDisplayChatItem }

      TMarkdownDisplayChatItem = class(TDisplayChatItem)
      private
        FMaxWidth: Integer;
        FControl : TMarkDownControl;
      Protected
        Procedure SetMarkdownProperty(aIndex : integer; aValue : string); virtual; overload;
        Procedure SetMarkdownProperty(aIndex : integer; aValue : Integer); virtual; overload;
        Procedure SetMarkdownColorProperty(aIndex : integer; aValue : TColor); virtual; overload;
        procedure Invalidate; override;
        procedure SetColors; override;
        procedure SetMaxWidth(aMaxWidth: Integer); override;
        procedure LayoutAt(aTop,aPadding,aMargin : Integer); override;
        property MaxWidth : Integer Read FMaxWidth Write SetMaxWidth;
      public
        destructor destroy; override;
      end;
    procedure FontChanged(Sender: TObject); override;
    procedure DoItemClick(Sender: TObject); virtual;
    procedure InitMarkdownProperties; virtual;
    procedure SetMarkDownProperties(aItem: TMarkDownDisplayChatItem); virtual;
    procedure SelectItem(aItem : TDisplayChatItem; IsSelect,aAddToSelection : Boolean);
    function CreateChatLabel(aParent: TWinControl): TLabel; virtual;
    function CreateTypingIndicator(aSide: TTextSide): TTypingIndicator; virtual;
    function DoCreatePlainItem(aText: String; aSide: TTextSide): TPlainDisplayChatItem; virtual;
    function CreatePlainItem(aText: String; aSide: TTextSide; aParent: TWinControl): TDisplayChatItem;
    function DoCreateMarkdownItem(aText: String; aSide: TTextSide): TMarkdownDisplayChatItem;
    function CreateMarkdownItem(aText: String; aSide: TTextSide): TDisplayChatItem;
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
    procedure AddText(const aText: String; aSide: TTextSide; aMarkdown: Boolean = False);
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
    property BaseFontSize: Integer index idxBaseFontSize read GetMarkDownInteger Write SetMarkDownInteger;
    property MonoFontName: string index idxMonoFontName read GetMarkDownString write SetMarkDownString;
    property FontCodeColor: TColor index idxFontCodeColor read GetMarkdownColor write SetMarkDownColor;
    property FontQuoteColor: TColor index idxFontQuoteColor read GetMarkdownColor write SetMarkDownColor;
    property HyperLinkColor: TColor index idxHyperLinkColor read GetMarkdownColor write SetMarkDownColor;
    property BGCodeColor: TColor index IdxBGCodeColor read GetMarkdownColor write SetMarkDownColor;
    property BulletChar1 : string index IdxBulletChar1 read GetMarkDownString write SetMarkDownString;
    property BulletChar2 : string index IdxBulletChar2 read GetMarkDownString write SetMarkDownString;
    property BulletChar3 : string index IdxBulletChar3 read GetMarkDownString write SetMarkDownString;
    Property BlockQuoteIndent : Integer index IdxBlockQuoteIndent read GetMarkDownInteger Write SetMarkDownInteger;
    Property ExtraIndent : Integer index IdxExtraIndent read GetMarkDownInteger Write SetMarkDownInteger;
    Property ParagraphSpacing : Integer index IdxParagraphSpacing read GetMarkDownInteger Write SetMarkDownInteger;
    Property ImageMargin : integer index IdxImageMargin read GetMarkDownInteger Write SetMarkDownInteger;
    property OnGetImage : TMarkdownImageEvent read FOnGetImage Write FOnGetImage;
    property OnOpenURL : TOpenURLEvent Read FOnOpenURL Write FOnOpenURL;
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
    SetMaxwidth(NewMax);
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

procedure TChatControl.SetMarkdownInteger(AIndex: Integer; const aValue: Integer);
begin
  FMarkDownSizes[aIndex]:=aValue;
  SetMarkDownIntegersOnItems(aIndex);
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

function TChatControl.GetMarkdownString(AIndex: Integer): string;
begin
  Result:=FMarkdownStrings[aIndex];
end;

function TChatControl.GetTyping(aIndex : TTextSide): TTypingIndicator;
begin
  Result:=FTyping[aIndex]
end;

function TChatControl.GetChatCount: Integer;
begin
  Result:=FChatList.Count;
end;

procedure TChatControl.DoGetOnImage(Sender: TObject; const aURL: string; var aImage: TPicture);
begin
  if assigned(FOnGetImage) then
    FOnGetImage(Self,aURL,aImage)
  else
    aImage:=nil;
end;

procedure TChatControl.DoOpenURL(Sender: TObject; aURL: String);
begin
  if assigned(FOnOpenURL) then
    FOnOpenURL(Self,aURL);
end;

function TChatControl.GetMarkDownColor(AIndex: Integer): TColor;
begin
  Result:=FMarkDownColors[aIndex];
end;

function TChatControl.GetMarkDownInteger(AIndex: Integer): Integer;
begin
  Result:=FMarkDownSizes[aIndex];
end;

procedure TChatControl.SetMarkdownColor(AIndex: Integer; const aValue: TColor);
begin
  FMarkDownColors[aIndex]:=aValue;
  SetMarkDownColorsOnItems(aIndex);
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

procedure TChatControl.InitMarkdownProperties;
begin
  FMarkDownSizes[IdxImageMargin]:=2;
  FMarkDownSizes[IdxBaseFontSize]:=10;
  FMarkDownSizes[IdxParagraphSpacing]:=12;
  FMarkDownSizes[IdxExtraIndent]:=0;
  FMarkDownSizes[IdxBlockQuoteIndent]:=40;

  FMarkdownStrings[IdxBulletChar1]:='•';
  FMarkdownStrings[IdxBulletChar2]:='◦';
  FMarkdownStrings[IdxBulletChar3]:='▪';

  FMarkDownStrings[IdxFontName]:='Sans Serif';
  FMarkDownStrings[IdxMonoFontName]:='Monospace';

  FMarkDownColors[IdxBGCodeColor]:=clInfoBk;
  FMarkDownColors[IdxFontCodeColor]:=clInfoText;
  FMarkDownColors[IdxFontColor]:=clWindowText;
  fMarkDownColors[IdxHyperLinkColor]:=RGBToColor(17,85,204);
  fMarkDownColors[IdxFontQuoteColor]:=clWindowFrame;

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

procedure TChatControl.SetMarkDownStringsOnItems(aIndex: Integer);
var
  i : Integer;
begin
  for I:=0 to FChatList.Count-1 do
    if FChatList[i] is TMarkdownDisplayChatItem then
      TMarkdownDisplayChatItem(FChatList[i]).SetMarkdownProperty(aIndex,FMarkdownStrings[aIndex]);
end;

procedure TChatControl.SetMarkDownIntegersOnItems(aIndex : Integer);
var
  i : Integer;
begin
  for I:=0 to FChatList.Count-1 do
    if FChatList[i] is TMarkdownDisplayChatItem then
      TMarkdownDisplayChatItem(FChatList[i]).SetMarkdownProperty(aIndex,FMarkdownSizes[aIndex]);
end;

procedure TChatControl.SetMarkDownColorsOnItems(aIndex : Integer);
var
  i : Integer;
begin
  for I:=0 to FChatList.Count-1 do
    if FChatList[i] is TMarkdownDisplayChatItem then
      TMarkdownDisplayChatItem(FChatList[i]).SetMarkdownColorProperty(aIndex,FMarkdownSizes[aIndex]);
end;


procedure TChatControl.SetMarkDownString(AIndex: Integer; const aValue: string);
begin
  FMarkdownStrings[aIndex]:=aValue;
  SetMarkDownStringsOnItems(aIndex);
end;

procedure TChatControl.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  SetMarkDownString(IdxFontName,Font.Name);
  SetMarkDownInteger(IdxBaseFontSize,Font.Size);
  SetMarkDownColor(IdxFontColor,Font.Color);
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

function TChatControl.DoCreatePlainItem(aText: String; aSide: TTextSide): TPlainDisplayChatItem;
begin
  Result:=TPlainDisplayChatItem.Create(aText,aSide);
end;

function TChatControl.DoCreateMarkdownItem(aText: String; aSide: TTextSide): TMarkdownDisplayChatItem;
begin
  Result:=TMarkdownDisplayChatItem.Create(aText,aSide);
end;

procedure TChatControl.SetMarkDownProperties(aItem : TMarkDownDisplayChatItem);
var
  Idx : Integer;
begin
  for Idx:=0 to MaxColorIdx do
    aItem.SetMarkdownProperty(Idx,FMarkDownColors[Idx]);
  for Idx:=0 to MaxStringIdx do
    aItem.SetMarkdownProperty(Idx,FMarkDownStrings[Idx]);
  for Idx:=0 to MaxIntegerIdx do
    aItem.SetMarkdownProperty(Idx,FMarkDownSizes[Idx]);
  aItem.FControl.OnGetImage:=@DoGetOnImage;
  aItem.FControl.OnOpenURL:=@DoOpenURL;
  if aItem.Side=tsLeft then
    begin
    aItem.FControl.BGCodeColor:=LeftBackground;
    aItem.FControl.FontColor:=LeftTextColor;
    end
  else
    begin
    aItem.FControl.BGCodeColor:=RightBackground;
    aItem.FControl.FontColor:=RightTextColor;
    end
end;

function TChatControl.CreateMarkdownItem(aText: String; aSide: TTextSide): TDisplayChatItem;
var
  C : TMarkDownControl;
  S : TShape;
  P : TMarkDownDisplayChatItem;
begin
  P:=DoCreateMarkdownItem(aText,aSide);
  C:=TMarkDownControl.Create(Self);
  C.Parent:=Self;
  P.FControl:=C;
  SetMarkDownProperties(P);
  C.MarkDown.Text:=aText;
  P.MaxWidth:=Round(ClientWidth*0.75);
  S:=TShape.Create(Self);
  S.Parent:=Self;
  S.Shape:=stRoundRect;
  if aSide=tsLeft then
    begin
    C.FontColor:=LeftTextColor;
    C.Color:=LeftBackground;
    S.Brush.Color:=LeftBackground;
    S.Pen.Color:=LeftBackground;
    end
  else
    begin
    C.FontColor:=RightTextColor;
    C.Color:=RightBackground;
    S.Brush.Color:=RightBackground;
    S.Pen.Color:=RightBackground;
    end;
  Result:=P;
  Result.TextBackground:=S;
  Result.OnClick:=@DoItemClick;
end;


function TChatControl.CreatePlainItem(aText: String; aSide: TTextSide; aParent: TWinControl): TDisplayChatItem;
var
  L : TLabel;
  S : TShape;
  P : TPlainDisplayChatItem;
begin
  P:=DoCreatePlainItem(aText,aSide);
  Result:=P;
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
  P.TextLabel:=L;
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
  InitMarkdownProperties;
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

procedure TChatControl.AddText(const aText: String; aSide: TTextSide; aMarkdown: Boolean);

var
  aItem : TDisplayChatItem;
  Idx,ltop : Integer;
begin
  if aMarkDown then
    aItem:=CreateMarkdownItem(aText,aSide)
  else
    aItem:=CreatePlainItem(aText,aSide,Self);

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
    end
  else
    begin
    if Assigned(FBackground) then
      begin
      FBackground.Pen.Color:=clHighlight;
      FBackground.Brush.Color:=clHighlight;
      FBackground.Color:=clHighlight;
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

procedure TChatControl.TDisplayChatItem.SetMaxWidth(aMaxWidth: Integer);
begin
  // Do nothing
end;


destructor TChatControl.TDisplayChatItem.Destroy;
begin
  FreeAndNil(FBackground);
  inherited Destroy;
end;


procedure TChatControl.TDisplayChatItem.DoClick(Sender: TObject);
begin
  If Assigned(FOnClick) then
    FOnClick(Self);
end;

{ TChatControl.TPlainDisplayChatItem }

procedure TChatControl.TPlainDisplayChatItem.LayoutAt(aTop, aPadding, aMargin: Integer);

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
  TextBackground.RoundRectHorizontalRadius:=aPadding;
  TextBackground.RoundRectVerticalRadius:=aPadding;
  TextBackground.SetBounds(lPos.X,lpos.Y,lSize.X+2*aPadding,lSize.Y+2*aPadding);
  TextLabel.Left:=lPos.X+aPadding;
  TextLabel.Top:=lPos.Y+aPadding;
  TextLabel.BringToFront;
end;

procedure TChatControl.TPlainDisplayChatItem.SetLabel(const aValue: TLabel);
begin
  if FLabel=aValue then Exit;
  FLabel:=AValue;
  if Assigned(FLabel) then
    begin
    FSavedColors[ckLabelFont]:=FLabel.Font.Color;
    FSavedColors[ckLabelBack]:=FLabel.Color;
    FLabel.OnClick:=@DoClick;
    FLabel.OnMouseDown:=@DoMouseDown;
    end;
end;

procedure TChatControl.TPlainDisplayChatItem.Invalidate;
begin
  inherited Invalidate;
  If assigned(FLabel) then
    FLabel.Invalidate;
end;

procedure TChatControl.TPlainDisplayChatItem.SetColors;
begin
  if Not Assigned(FLabel) then
    exit;
  if FSelected then
    begin
    FLabel.Font.Color:=clHighlightText;
    FLabel.Color:=clHighlight;
    end
  else
    begin
    FLabel.Font.Color:=FSavedColors[ckLabelFont];
    FLabel.Color:=FSavedColors[ckLabelBack];
    end

end;

procedure TChatControl.TPlainDisplayChatItem.SetMaxWidth(aMaxWidth: Integer);
var
  recalc : boolean;
begin
  Recalc:=aMaxWidth>TextLabel.Constraints.MaxWidth;
  TextLabel.Constraints.MaxWidth:=aMaxWidth;
  if Recalc then
    begin
    TextLabel.WordWrap:=False;
    TextLabel.WordWrap:=True;
    end;
end;

destructor TChatControl.TPlainDisplayChatItem.destroy;
begin
  FreeAndNil(FLabel);
  inherited destroy;
end;

{ TChatControl.TMarkdownDisplayChatItem }

procedure TChatControl.TMarkdownDisplayChatItem.SetMarkdownProperty(aIndex: integer; aValue: string);
begin
  Case aIndex of
    IdxFontName     : FControl.FontName:=aValue;
    IdxMonoFontName : FControl.MonoFontName:=aValue;
    IdxBulletChar1  : FControl.BulletChar1:=aValue;
    IdxBulletChar2  : FControl.BulletChar2:=aValue;
    IdxBulletChar3  : FControl.BulletChar3:=aValue;
  end;
end;

procedure TChatControl.TMarkdownDisplayChatItem.SetMarkdownProperty(aIndex: integer; aValue: Integer);
begin
  case aIndex of
    IdxBaseFontSize     : FControl.BaseFontSize:=aValue;
    IdxBlockQuoteIndent : FControl.BlockQuoteIndent:=aValue;
    IdxImageMargin      : FControl.ImageMargin:=aValue;
    IdxParagraphSpacing : FControl.ParagraphSpacing:=aValue;
    IdxExtraIndent      : FControl.ExtraIndent:=aValue;
  end;
end;

procedure TChatControl.TMarkdownDisplayChatItem.SetMarkdownColorProperty(aIndex: integer; aValue: TColor);
begin
  case aIndex of
    IdxFontColor      : FControl.FontColor:=aValue;
    IdxFontCodeColor  : FControl.FontCodeColor:=aValue;
    IdxFontQuoteColor : FControl.FontQuoteColor:=aValue;
    IdxHyperLinkColor : FControl.FontQuoteColor:=aValue;
    IdxBGCodeColor    : FControl.BGCodeColor:=aValue;
  end;
end;

procedure TChatControl.TMarkdownDisplayChatItem.Invalidate;
begin
  // Do nothing
end;

procedure TChatControl.TMarkdownDisplayChatItem.SetColors;
begin
  // Do nothing
end;

procedure TChatControl.TMarkdownDisplayChatItem.SetMaxWidth(aMaxWidth: Integer);
begin
  FControl.Width:=aMaxWidth;
  FControl.CalcLayout;
end;

procedure TChatControl.TMarkdownDisplayChatItem.LayoutAt(aTop, aPadding, aMargin: Integer);
var
  lPos : TPoint;
  lSize : TPoint;

begin
  lPos.Y:=aTop;
  lSize.X:=FControl.Width;
  lSize.Y:=FControl.Height;
  Case Side of
   tsLeft : lPos.X:=4;
   tsRight : lPos.X:=FControl.Parent.Width-lSize.X-2*aPadding-aMargin;
  end;
  TextBackground.RoundRectHorizontalRadius:=aPadding;
  TextBackground.RoundRectVerticalRadius:=aPadding;
  TextBackground.SetBounds(lPos.X,lpos.Y,lSize.X+2*aPadding,lSize.Y+2*aPadding);
  FControl.Left:=lPos.X+aPadding;
  FControl.Top:=lPos.Y+aPadding;
  FControl.BringToFront;
end;

destructor TChatControl.TMarkdownDisplayChatItem.destroy;
begin
  inherited destroy;
end;

end.

