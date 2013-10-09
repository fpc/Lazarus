{ LazReport dialogs control

  Copyright (C) 2012-2013 alexs alexs75.at.hotbox.ru

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

unit LRDialogControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Graphics, DB, LR_Class, Controls, StdCtrls,
  LMessages, LCLType, LCLIntf, Buttons, EditBtn, Themes, ButtonPanel;

type
  TLRDialogControls = class(TComponent)
  end;

  { TlrVisualControl }

  TlrVisualControl = class(TfrControl)
  private
    function GetAutoSize: Boolean;
    function GetCaption: string;
    function GetColor: TColor;
    function GetEnabled: boolean;
    function GetFont: TFont;
    function GetOnClick: TfrScriptStrings;
    procedure SetAutoSize(AValue: Boolean);
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TColor);
    procedure SetEnabled(AValue: boolean);
    procedure SetFont(AValue: TFont);
    procedure OnClickHandle(Sender: TObject);
    procedure SetOnClick(AValue: TfrScriptStrings);
  protected
    FControl: TControl;
    procedure SetVisible(AValue: Boolean);override;
    procedure PaintDesignControl; override;
    procedure SetName(const AValue: string); override;
    procedure AfterCreate;override;
    function CreateControl:TControl;virtual;abstract;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure UpdateControlPosition; override;
    procedure AttachToParent; override;

    property Control: TControl read FControl write FControl;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize;
    property Color:TColor read GetColor write SetColor;
    property Caption:string read GetCaption write SetCaption;
    property Text:string read GetCaption write SetCaption;
    property Font:TFont read GetFont write SetFont;
    property OnClick:TfrScriptStrings read GetOnClick write SetOnClick;
  published
    property Enabled:boolean read GetEnabled write SetEnabled;
  end;
  TlrVisualControlClass = class of TlrVisualControl;

  { TlrLabel }

  TlrLabel = class(TlrVisualControl)
  private
    function GetAlignment: TAlignment;
    function GetWordWrap: boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetWordWrap(AValue: boolean);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property WordWrap:boolean read GetWordWrap write SetWordWrap;
    property AutoSize;
    property Color;
    property Caption;
    property Font;
    property OnClick;
  end;

  { TlrEdit }

  TlrEdit = class(TlrVisualControl)
  private
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Text;
    property OnClick;
  end;

  { TlrMemo }

  TlrMemo = class(TlrVisualControl)
  private
    procedure MemoChange(Sender: TObject);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Memo;
    property OnClick;
  end;

  { TlrButton }

  TlrButton = class(TlrVisualControl)
  private
    function GetKind: TBitBtnKind;
    procedure SetKind(AValue: TBitBtnKind);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property AutoSize;
    property Color;
    property Enabled;
    property Caption;
    property Kind: TBitBtnKind read GetKind write SetKind;
    property OnClick;
  end;

  { TlrCheckBox }

  TlrCheckBox = class(TlrVisualControl)
  private
    function GetChecked: boolean;
    procedure SetChecked(AValue: boolean);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    function GetCheckStyle(ACheck:boolean):TThemedButton;virtual;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property AutoSize;
    property Color;
    property Enabled;
    property Caption;
    property Checked:boolean read GetChecked write SetChecked;
    property OnClick;
  end;

  { TlrRadioButton }

  TlrRadioButton = class(TlrCheckBox)
  protected
    function CreateControl:TControl;override;
    function GetCheckStyle(ACheck:boolean):TThemedButton;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
  end;

  { TlrListBox }

  TlrListBox = class(TlrVisualControl)
  private
    function GetItemIndex: integer;
    function GetItems: TStrings;
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Items:TStrings read GetItems write SetItems;
    property ItemIndex:integer read GetItemIndex write SetItemIndex;
    property OnClick;
  end;

  { TlrComboBox }

  TlrComboBox = class(TlrVisualControl)
  private
    function GetDropDownCount: integer;
    function GetItemIndex: integer;
    function GetItems: TStrings;
    function GetStyle: TComboBoxStyle;
    procedure SetDropDownCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
    procedure SetStyle(AValue: TComboBoxStyle);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Text;
    property AutoSize;
    property Style:TComboBoxStyle read GetStyle write SetStyle;
    property Items:TStrings read GetItems write SetItems;
    property ItemIndex:integer read GetItemIndex write SetItemIndex;
    property DropDownCount:integer read GetDropDownCount write SetDropDownCount;
    property OnClick;
  end;

  { TlrDateEdit }

  TlrDateEdit = class(TlrVisualControl)
  private
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Date:TDateTime read GetDate write SetDate;
    property OnClick;
  end;

  { TlrButtonPanel }

  TlrButtonPanel = class(TlrVisualControl)
  private
    function GetButtonOrder: TButtonOrder;
    function GetShowButtons: TPanelButtons;
    procedure SetButtonOrder(AValue: TButtonOrder);
    procedure SetShowButtons(AValue: TPanelButtons);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    procedure AfterCreate;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure UpdateControlPosition; override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property ButtonOrder: TButtonOrder read GetButtonOrder write SetButtonOrder default boDefault;
    property ShowButtons: TPanelButtons read GetShowButtons write SetShowButtons default DefShowButtons;
    property Color;
    property Enabled;
    property Text;
    property OnClick;
  end;

procedure Register;

procedure DoRegsiterControl(var cmpBMP:TBitmap; lrClass:TlrVisualControlClass);
implementation
uses typinfo, types,
   lrDBDialogControls;

procedure Register;
begin
  RegisterComponents('LazReport',[TLRDialogControls]);
end;

var
  lrBMP_LRLabel:TBitmap = nil;
  lrBMP_LREdit:TBitmap = nil;
  lrBMP_LRButton:TBitmap = nil;
  lrBMP_LRCheckBox:TBitmap = nil;
  lrBMP_LRComboBox:TBitmap = nil;
  lrBMP_LRRadioButton:TBitmap = nil;
  lrBMP_LRMemo:TBitmap = nil;
  lrBMP_LRListBox:TBitmap = nil;
  lrBMP_LRDateEdit:TBitmap = nil;
  lrBMP_LRButtonPanel:TBitmap = nil;

procedure DoRegsiterControl(var cmpBMP:TBitmap; lrClass:TlrVisualControlClass);
begin
  if not assigned(cmpBMP) then
  begin
    cmpBMP := TBitmap.Create;
    cmpBMP.LoadFromLazarusResource(lrClass.ClassName);
    frRegisterObject(lrClass, cmpBMP, lrClass.ClassName, nil, otlUIControl, nil);
  end;
end;

procedure InitLRComp;
begin
  DoRegsiterControl(lrBMP_LRLabel, TlrLabel);
  DoRegsiterControl(lrBMP_LREdit, TlrEdit);
  DoRegsiterControl(lrBMP_LRMemo, TlrMemo);
  DoRegsiterControl(lrBMP_LRButton, TlrButton);
  DoRegsiterControl(lrBMP_LRCheckBox, TlrCheckBox);
  DoRegsiterControl(lrBMP_LRComboBox, TlrComboBox);
  DoRegsiterControl(lrBMP_LRRadioButton, TlrRadioButton);
  DoRegsiterControl(lrBMP_LRListBox, TlrListBox);
  DoRegsiterControl(lrBMP_LRDateEdit, TlrDateEdit);
  DoRegsiterControl(lrBMP_LRButtonPanel, TlrButtonPanel);

end;

{ TlrButtonPanel }

function TlrButtonPanel.GetButtonOrder: TButtonOrder;
begin
  Result:= TButtonPanel(FControl).ButtonOrder;
end;

function TlrButtonPanel.GetShowButtons: TPanelButtons;
begin
  Result:= TButtonPanel(FControl).ShowButtons;
end;

procedure TlrButtonPanel.SetButtonOrder(AValue: TButtonOrder);
begin
  TButtonPanel(FControl).ButtonOrder:=AValue;
end;

procedure TlrButtonPanel.SetShowButtons(AValue: TPanelButtons);
begin
  TButtonPanel(FControl).ShowButtons:=AValue;
end;

procedure TlrButtonPanel.PaintDesignControl;
var
  AY, AX, aH, aW:integer;
  R1:TRect;
  i:TPanelButton;

  B:TPanelBitBtn;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);

  R1:=DRect;
  R1.Right:=R1.Right - 6;
  R1.Top:=R1.Top + 6;
  R1.Bottom:=R1.Bottom - 6;

  for i:=Low(TPanelButton) to High(TPanelButton) do
  begin
    if i in ShowButtons then
    begin

      case i of
        pbOK:B:=TButtonPanel(FControl).OKButton;
        pbCancel:B:=TButtonPanel(FControl).CancelButton;
        pbClose:B:=TButtonPanel(FControl).CloseButton;
        pbHelp:B:=TButtonPanel(FControl).HelpButton;
      else
        b:=nil;
      end;
      if Assigned(B) then
      begin
        R1.Left:=R1.Right - B.Width;
        DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);

        AX:=(R1.Left +  R1.Right) div 2;
        AY:=(R1.Top +  R1.Bottom) div 2;
        aW:=Canvas.TextWidth(B.Caption);
        aH:=Canvas.TextHeight(B.Caption) div 2;

        if aW>B.Width then
          Canvas.TextRect(R1, 0, AY - aH, B.Caption)
        else
          Canvas.TextRect(R1, AX - (aW div 2), AY - aH, B.Caption)

      end;
      R1.Right:=R1.Left - 6;
    end;
  end;
end;

function TlrButtonPanel.CreateControl: TControl;
begin
  Result:=TButtonPanel.Create(nil);
end;

procedure TlrButtonPanel.AfterCreate;
begin
  inherited AfterCreate;
  FControl.OnClick:=nil;
  TButtonPanel(FControl).OKButton.OnClick:=@OnClickHandle;
end;

constructor TlrButtonPanel.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrButtonPanel';
  AutoSize:=true;
  UpdateControlPosition;
end;

procedure TlrButtonPanel.UpdateControlPosition;
begin
  X:=2;
  Y:=OwnerPage.Height - FControl.Height - 4;
  Dx:=OwnerPage.Width-8;
  Dy:=FControl.Height;
end;

procedure TlrButtonPanel.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  RestoreProperty('ButtonOrder',XML.GetValue(Path+'ButtonOrder', 'boDefault'));
  RestoreProperty('ShowButtons',XML.GetValue(Path+'ShowButtons', 'pbOK, pbCancel, pbClose, pbHelp'));
  UpdateControlPosition;
end;

procedure TlrButtonPanel.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'ButtonOrder', GetSaveProperty('ButtonOrder'));
  XML.SetValue(Path+'ShowButtons', GetSaveProperty('ShowButtons'));
end;

{ TlrDateEdit }

function TlrDateEdit.GetDate: TDateTime;
begin
  Result:=TDateEdit(FControl).Date;
end;

procedure TlrDateEdit.SetDate(AValue: TDateTime);
begin
  TDateEdit(FControl).Date:=AValue;
end;

procedure TlrDateEdit.PaintDesignControl;
var
  AY, aH:integer;
  R1:TRect;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text);

  R1:=DRect;
  R1.Left:=R1.Right - 16;
  DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
end;

function TlrDateEdit.CreateControl: TControl;
begin
  Result:=TDateEdit.Create(nil);
end;

constructor TlrDateEdit.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrDateEdit';
  AutoSize:=true;
end;

procedure TlrDateEdit.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
end;

procedure TlrDateEdit.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
end;

{ TlrListBox }

function TlrListBox.GetItemIndex: integer;
begin
  Result:=TListBox(FControl).ItemIndex;
end;

function TlrListBox.GetItems: TStrings;
begin
  Result:=TListBox(FControl).Items;
end;

procedure TlrListBox.SetItemIndex(AValue: integer);
begin
  TListBox(FControl).ItemIndex:=AValue;
end;

procedure TlrListBox.SetItems(AValue: TStrings);
begin
  TListBox(FControl).Items:=AValue;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TlrListBox.PaintDesignControl;
var
  AY, aH, i:integer;
  S:string;
begin
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  AY:=DRect.Top + 1;
  i:=0;
  while (AY < DRect.Bottom) and (i<TListBox(FControl).Items.Count) do
  begin
    S:=TListBox(FControl).Items[i];
    Canvas.TextRect(DRect, DRect.Left + 3, AY, S);
    inc(AY, aH + 3);
    inc(i);
  end;
end;

function TlrListBox.CreateControl: TControl;
begin
  Result:=TListBox.Create(nil);
end;

constructor TlrListBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrListBox';
  AutoSize:=true;
end;

procedure TlrListBox.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Items.Text:=XML.GetValue(Path+'Items/Text/Value'{%H-}, '');
  ItemIndex:=XML.GetValue(Path+'ItemIndex/Value'{%H-}, -1);
end;

procedure TlrListBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Items/Text/Value'{%H-}, Items.Text);
  XML.SetValue(Path+'ItemIndex/Value'{%H-}, ItemIndex);
end;

{ TlrMemo }

procedure TlrMemo.MemoChange(Sender: TObject);
begin
  if Assigned(FControl) then
    TMemo(FControl).Lines.Assign(Memo);
end;

procedure TlrMemo.PaintDesignControl;
var
  AY, aH, i:integer;
  S:string;
begin
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  AY:=DRect.Top + 1;
  i:=0;
  while (AY < DRect.Bottom) and (i<TMemo(FControl).Lines.Count) do
  begin
    S:=TMemo(FControl).Lines[i];
    Canvas.TextRect(DRect, DRect.Left + 3, AY, S);
    inc(AY, aH + 3);
    inc(i);
  end;
end;

function TlrMemo.CreateControl: TControl;
begin
  Result:=TMemo.Create(nil);
end;

constructor TlrMemo.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrMemo';
  Memo.OnChange:=@MemoChange;
end;

procedure TlrMemo.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
end;

procedure TlrMemo.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
end;

{ TlrRadioButton }

function TlrRadioButton.CreateControl: TControl;
begin
  Result:=TRadioButton.Create(nil);
end;

function TlrRadioButton.GetCheckStyle(ACheck: boolean): TThemedButton;
begin
  if ACheck then
    Result:=tbRadioButtonCheckedNormal
  else
    Result:=tbRadioButtonUncheckedNormal
end;

constructor TlrRadioButton.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrRadioButton';
end;


{ TlrComboBox }

function TlrComboBox.GetDropDownCount: integer;
begin
  Result:=TComboBox(FControl).DropDownCount;
end;

function TlrComboBox.GetItemIndex: integer;
begin
  Result:=TComboBox(FControl).ItemIndex;
end;

function TlrComboBox.GetItems: TStrings;
begin
  Result:=TComboBox(FControl).Items;
end;

function TlrComboBox.GetStyle: TComboBoxStyle;
begin
  Result:=TComboBox(FControl).Style;
end;

procedure TlrComboBox.SetDropDownCount(AValue: integer);
begin
  TComboBox(FControl).DropDownCount:=AValue;
end;

procedure TlrComboBox.SetItemIndex(AValue: integer);
begin
  TComboBox(FControl).ItemIndex:=AValue;
end;

procedure TlrComboBox.SetItems(AValue: TStrings);
begin
  TComboBox(FControl).Items:=AValue;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TlrComboBox.SetStyle(AValue: TComboBoxStyle);
begin
  TComboBox(FControl).Style:=AValue;
end;

procedure TlrComboBox.PaintDesignControl;
var
  AY, aH:integer;
  R1:TRect;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text);

  R1:=DRect;
  R1.Left:=R1.Right - 16;
  DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
end;

function TlrComboBox.CreateControl: TControl;
begin
  Result:=TComboBox.Create(nil);
end;

constructor TlrComboBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrComboBox';
  AutoSize:=true;
end;

procedure TlrComboBox.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Items.Text:=XML.GetValue(Path+'Items/Text/Value'{%H-}, '');
  RestoreProperty('Style',XML.GetValue(Path+'Style/Value','csDropDown'));
  ItemIndex:=XML.GetValue(Path+'ItemIndex/Value'{%H-}, -1);
  DropDownCount:=XML.GetValue(Path+'DropDownCount/Value'{%H-}, DropDownCount);
end;

procedure TlrComboBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Items/Text/Value'{%H-}, Items.Text);
  XML.SetValue(Path+'Style/Value', GetSaveProperty('Style'));
  XML.SetValue(Path+'ItemIndex/Value'{%H-}, ItemIndex);
  XML.SetValue(Path+'DropDownCount/Value'{%H-}, DropDownCount);
end;

{ TlrCheckBox }

type
  THackChekBox = class(TCustomCheckBox);

function TlrCheckBox.GetChecked: boolean;
begin
  Result:=THackChekBox(FControl).Checked;
end;

procedure TlrCheckBox.SetChecked(AValue: boolean);
begin
  THackChekBox(FControl).Checked:=AValue;
end;

procedure TlrCheckBox.PaintDesignControl;
var
  AX, AY, aW, aH:integer;
  CSize: TSize;
  PaintRect:TRect;
  details: TThemedElementDetails;
begin
  Details := ThemeServices.GetElementDetails(GetCheckStyle(Checked));
  CSize := ThemeServices.GetDetailSize(Details);
  PaintRect.Left := DRect.Left;
  PaintRect.Top  := (DRect.Top + DRect.Bottom - CSize.cy) div 2;
  PaintRect := Bounds(PaintRect.Left, PaintRect.Top, CSize.cx, CSize.cy);
  ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect, nil);

  AX:=DRect.Left + CSize.cx + 2;
  AY:=(DRect.Top +  DRect.Bottom) div 2;
//  aW:=Canvas.TextWidth(Caption);
  aH:=Canvas.TextHeight(Caption) div 2;
  Canvas.TextRect(DRect, AX, AY - aH, Caption)
end;

function TlrCheckBox.CreateControl: TControl;
begin
  Result:=TCheckBox.Create(nil);
end;

function TlrCheckBox.GetCheckStyle(ACheck: boolean): TThemedButton;
begin
  if ACheck then
    Result:=tbCheckBoxCheckedNormal
  else
    Result:=tbCheckBoxUncheckedNormal
end;

constructor TlrCheckBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName:='lrCheckBox';
  AutoSize:=true;
end;

procedure TlrCheckBox.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Checked:=XML.GetValue(Path+'Checked/Value'{%H-}, false);
end;

procedure TlrCheckBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Checked/Value'{%H-}, Checked);
end;

{ TlrButton }

function TlrButton.GetKind: TBitBtnKind;
begin
  Result:=TBitBtn(FControl).Kind;
end;

procedure TlrButton.SetKind(AValue: TBitBtnKind);
begin
  TBitBtn(FControl).Kind:=AValue;
end;

procedure TlrButton.PaintDesignControl;
var
  AX, AY, aW, aH:integer;
begin
  DrawFrameControl(Canvas.Handle, DRect, DFC_BUTTON, DFCS_BUTTONPUSH);
  AX:=(DRect.Left +  DRect.Right) div 2;
  AY:=(DRect.Top +  DRect.Bottom) div 2;
  aW:=Canvas.TextWidth(Caption);
  aH:=Canvas.TextHeight(Caption) div 2;
  if aW>Width then
    Canvas.TextRect(DRect, 0, AY - aH, Caption)
  else
    Canvas.TextRect(DRect, AX - (aW div 2), AY - aH, Caption)
end;

function TlrButton.CreateControl: TControl;
begin
  Result:=TBitBtn.Create(nil);
  Result.AutoSize:=true;
end;

constructor TlrButton.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrButton';
end;

procedure TlrButton.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  RestoreProperty('Kind',XML.GetValue(Path+'Kind/Value','bkCustom'));
end;

procedure TlrButton.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Kind/Value', GetSaveProperty('Kind'));
end;

{ TlrEdit }

procedure TlrEdit.PaintDesignControl;
var
  AY, aH:integer;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text)
end;

function TlrEdit.CreateControl: TControl;
begin
  Result:=TEdit.Create(nil);
end;

constructor TlrEdit.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrEdit';
end;

procedure TlrEdit.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Text:=XML.GetValue(Path+'Text/Value'{%H-}, '');
end;

procedure TlrEdit.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Text/Value'{%H-}, Text);
end;

{ TlrLabel }

function TlrLabel.GetAlignment: TAlignment;
begin
  Result:=TLabel(FControl).Alignment;
end;

function TlrLabel.GetWordWrap: boolean;
begin
  Result:=TLabel(FControl).WordWrap;
end;

procedure TlrLabel.SetAlignment(AValue: TAlignment);
begin
  TLabel(FControl).Alignment:=AValue;
end;

procedure TlrLabel.SetWordWrap(AValue: boolean);
begin
  TLabel(FControl).WordWrap:=AValue;
end;

procedure TlrLabel.PaintDesignControl;
var
  AY, aH:integer;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text)
end;

function TlrLabel.CreateControl: TControl;
begin
  Result:=TLabel.Create(nil);
end;


constructor TlrLabel.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrLabel';
end;

procedure TlrLabel.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  WordWrap:=XML.GetValue(Path+'WordWrap/Value'{%H-}, false);
  RestoreProperty('Alignment',XML.GetValue(Path+'Alignment/Value',''));
end;

procedure TlrLabel.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'WordWrap/Value'{%H-}, WordWrap);
  XML.SetValue(Path+'Alignment/Value', GetSaveProperty('Alignment'));
end;


{ TlrVisualControl }

procedure TlrVisualControl.SetColor(AValue: TColor);
begin
  FControl.Color:=AValue;
  Invalidate;
end;

function TlrVisualControl.GetAutoSize: Boolean;
begin
  Result:=FControl.AutoSize;
end;

function TlrVisualControl.GetCaption: string;
begin
  Result:=FControl.Caption;
end;

function TlrVisualControl.GetColor: TColor;
begin
  Result:=FControl.Color;
end;

function TlrVisualControl.GetEnabled: boolean;
begin
  Result:=FControl.Enabled;
end;

function TlrVisualControl.GetFont: TFont;
begin
  Result:=FControl.Font;
end;

function TlrVisualControl.GetOnClick: TfrScriptStrings;
begin
  Result:=Script;
end;

procedure TlrVisualControl.SetAutoSize(AValue: Boolean);
begin
  FControl.AutoSize:=AValue;
end;

procedure TlrVisualControl.SetCaption(AValue: string);
begin
  FControl.Caption:=AValue;
{  DX:=FControl.Width;
  DY:=FControl.Height;}
  Invalidate;
end;

procedure TlrVisualControl.SetEnabled(AValue: boolean);
begin
  FControl.Enabled:=AValue;
  Invalidate;
end;

procedure TlrVisualControl.SetFont(AValue: TFont);
begin
  FControl.Font:=AValue;
  Invalidate;
end;

procedure TlrVisualControl.OnClickHandle(Sender: TObject);
var
  FSaveView:TfrView;
  FSavePage:TfrPage;
  CmdList, ErrorList:TStringList;
begin
  if (DocMode = dmPrinting) and (Script.Count>0) and (Trim(Script.Text)<>'') and (Assigned(CurReport))then
  begin
    FSaveView:=CurView;
    FSavePage:=CurPage;
    CmdList:=TStringList.Create;
    ErrorList:=TStringList.Create;
    try
      CurView := Self;
      CurPage:=OwnerPage;
      frInterpretator.PrepareScript(Script, CmdList, ErrorList);
      frInterpretator.DoScript(CmdList);
    finally
      CurPage:=FSavePage;
      CurView := FSaveView;
      FreeAndNil(CmdList);
      FreeAndNil(ErrorList);
    end;
  end;
end;

procedure TlrVisualControl.SetOnClick(AValue: TfrScriptStrings);
begin
  Script:=AValue;
end;

procedure TlrVisualControl.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
  FControl.Visible:=AValue;
end;

procedure TlrVisualControl.PaintDesignControl;
var
  AY, aH:integer;
  S:string;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Brush.Color := clRed;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Name)
end;

procedure TlrVisualControl.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FControl.Name:=Name;
end;

procedure TlrVisualControl.UpdateControlPosition;
begin
  FControl.Left:=round(Left);
  FControl.Top:=round(Top) - 20; //Header width
  FControl.Width:=round(Width);
  FControl.Height:=round(Height);
end;

procedure TlrVisualControl.AttachToParent;
begin
  FControl.Parent := OwnerForm;
end;

procedure TlrVisualControl.AfterCreate;
begin
  inherited AfterCreate;
  if Assigned(FControl) then
    FControl.OnClick:=@OnClickHandle;
end;

constructor TlrVisualControl.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FControl:=CreateControl;
  if Assigned(FControl) then
  begin
    x:=FControl.Left;
    Y:=FControl.Top;
    DX:=FControl.Width;
    DY:=FControl.Height;
  end;
end;

destructor TlrVisualControl.Destroy;
begin
  FControl.Free;
  FControl := nil;
  inherited Destroy;
end;

procedure TlrVisualControl.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Enabled:=XML.GetValue(Path+'Enabled/Value'{%H-}, true);
  Font.Name := XML.GetValue(Path+'Font/Name/Value', 'Arial'); // todo chk
  Font.Size := XML.GetValue(Path+'Font/Size/Value'{%H-}, 10); // todo chk
  RestoreProperty('CharSet',XML.GetValue(Path+'Font/Charset/Value',''),Font);
  RestoreProperty('Style',XML.GetValue(Path+'Font/Style/Value',''),Font);
  Font.Color := StringToColor(XML.GetValue(Path+'Font/Color/Value','clBlack')); // todo chk
  Caption:=XML.GetValue(Path+'Caption/Value'{%H-}, '');
  AutoSize:=XML.GetValue(Path+'AutoSize/Value'{%H-}, true);
  Color:= StringToColor(XML.GetValue(Path+'Color/Value', 'clNone'));
  if StreamMode = smDesigning then
  begin
    OnClick.Text:=XML.GetValue(Path+'Event/OnClick/Value', '');
  end;
end;

procedure TlrVisualControl.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Enabled/Value'{%H-}, Enabled);
  XML.SetValue(Path+'Font/Name/Value', Font.Name);
  XML.SetValue(Path+'Font/Size/Value'{%H-}, Font.Size);
  XML.SetValue(Path+'Font/Color/Value', ColorToString(Font.Color));
  XML.SetValue(Path+'Font/Charset/Value', GetSaveProperty('CharSet',Font));
  XML.SetValue(Path+'Font/Style/Value', GetSaveProperty('Style',Font));
  XML.SetValue(Path+'Caption/Value'{%H-}, Caption);
  XML.SetValue(Path+'AutoSize/Value'{%H-}, AutoSize);
  XML.SetValue(Path+'Color/Value', ColorToString(Color));

  if StreamMode = smDesigning then
  begin
    if IsPublishedProp(self,'OnClick') then
      XML.SetValue(Path+'Event/OnClick/Value', OnClick.Text);
  end;
end;


initialization
  {$I lrdialogcontrols_img.inc}
  InitLRComp;

finalization
  if Assigned(lrBMP_LRLabel) then
    FreeAndNil(lrBMP_LRLabel);
  if Assigned(lrBMP_LREdit) then
    FreeAndNil(lrBMP_LREdit);
  if Assigned(lrBMP_LRButton) then
    FreeAndNil(lrBMP_LRButton);
  if Assigned(lrBMP_LRCheckBox) then
    FreeAndNil(lrBMP_LRCheckBox);
  if Assigned(lrBMP_LRComboBox) then
    FreeAndNil(lrBMP_LRComboBox);
  if Assigned(lrBMP_LRRadioButton) then
    FreeAndNil(lrBMP_LRRadioButton);
  if Assigned(lrBMP_LRMemo) then
    FreeAndNil(lrBMP_LRMemo);
  if Assigned(lrBMP_LRListBox) then
    FreeAndNil(lrBMP_LRListBox);
  if Assigned(lrBMP_LRDateEdit) then
    FreeAndNil(lrBMP_LRDateEdit);
  if Assigned(lrBMP_LRButtonPanel) then
    FreeAndNil(lrBMP_LRButtonPanel);
end.


