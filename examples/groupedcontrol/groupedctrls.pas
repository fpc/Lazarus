unit GroupedCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, GraphType, Buttons, MaskEdit, LCLType, StdCtrls,
  Controls, Graphics;

type

  { TCustomTransparentPanel }

  TCustomTransparentPanel = class(TCustomPanel)
  public
    constructor Create(TheOwner: TComponent); override;
    property BevelInner default bvNone;
    property BevelOuter default bvNone;
  end;

  TTransparentPanel = class(TCustomTransparentPanel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  { TCustomGroupedEditButton }

  TCustomGroupedEditButton = class(TCustomTransparentPanel)
  private
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput: Boolean;
    FEdit: TMaskEdit;
    FOnButtonClick: TNotifyEvent;
    function GetAutoSelect: Boolean;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetCharCase: TEditCharCase;
    function GetEditDragCursor: TCursor;
    function GetEditDragMode: TDragMode;
    function GetEchoMode: TEchoMode;
    function GetEditColor: TColor;
    function GetEditText: string;
    function GetFlat: Boolean;
    function GetGlyph: TBitmap;
    function GetMaxLength: Integer;
    function GetNumGlyphs: Integer;
    function GetOnChange: TNotifyEvent;
    function GetPasswordChar: Char;
    function GetReadOnly: boolean;
    function IsCustomGlyph: Boolean;
    procedure SetAutoSelect(AValue: Boolean);
    procedure SetButtonHint(AValue: TTranslateString);
    procedure SetButtonNeedsFocus(AValue: Boolean);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetDirectInput(AValue: Boolean);
    procedure SetEditDragCursor(AValue: TCursor);
    procedure SetEchoMode(AValue: TEchoMode);
    procedure SetEditColor(AValue: TColor);
    procedure SetEditDragMode(AValue: TDragMode);
    procedure SetEditText(AValue: string);
    procedure SetFlat(AValue: Boolean);
    procedure SetGlyph(AValue: TBitmap);
    procedure SetMaxLength(AValue: Integer);
    procedure SetNumGlyphs(AValue: Integer);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetPasswordChar(AValue: Char);
    procedure SetReadOnly(AValue: boolean);
  protected
    function CalcButtonVisible: boolean; virtual;
    procedure CheckButtonVisible; virtual;
    procedure AnchorEditAndButton; virtual;
    procedure DoButtonClick(Sender: TObject); virtual;
    function GetDefaultGlyph: TBitmap; virtual;
    function GetDefaultGlyphName: String; virtual;
    procedure Loaded; override;
    property DirectInput: Boolean read FDirectInput write SetDirectInput default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    procedure SetBiDiMode(AValue: TBiDiMode); override;
  protected
    // edit
    property Edit: TMaskEdit read FEdit;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property EchoMode: TEchoMode read GetEchoMode write SetEchoMode default emNormal;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    property DragCursor: TCursor read GetEditDragCursor write SetEditDragCursor;
    property DragMode: TDragMode read GetEditDragMode write SetEditDragMode;
    property Color: TColor read GetEditColor write SetEditColor default {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    property Text: string read GetEditText write SetEditText;
  protected
    // button
    property Button: TSpeedButton read FButton;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
    property ButtonOnlyWhenFocused: Boolean read FButtonNeedsFocus write SetButtonNeedsFocus default False;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property AutoSize default true;
  end;

  TGroupedEditButton = class(TCustomGroupedEditButton)
  public
    property Button;
    property Edit;
  published
    property AutoSize;
    property AutoSelect;
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property ButtonHint;
    property CharCase;
    property Color;
    property DirectInput; // ToDo
    property DragCursor;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property MaxLength;
    property NumGlyphs;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

implementation

{ TCustomTransparentPanel }

constructor TCustomTransparentPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BevelOuter := bvNone;
  BevelInner := bvNone;
end;

{ TCustomGroupedEditButton }

procedure TCustomGroupedEditButton.DoButtonClick(Sender: TObject);
begin
  if (not ReadOnly) and Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

function TCustomGroupedEditButton.GetButtonHint: TTranslateString;
begin
  Result:=Button.Hint;
end;

function TCustomGroupedEditButton.GetAutoSelect: Boolean;
begin
  Result:=Edit.AutoSelect;
end;

function TCustomGroupedEditButton.GetButtonWidth: Integer;
begin
  Result:=Button.Width;
end;

function TCustomGroupedEditButton.GetCharCase: TEditCharCase;
begin
  Result:=Edit.CharCase;
end;

function TCustomGroupedEditButton.GetEditDragCursor: TCursor;
begin
  Result:=Edit.DragCursor;
end;

function TCustomGroupedEditButton.GetEditDragMode: TDragMode;
begin
  Result:=Edit.DragMode;
end;

function TCustomGroupedEditButton.GetEchoMode: TEchoMode;
begin
  Result:=Edit.EchoMode;
end;

function TCustomGroupedEditButton.GetEditColor: TColor;
begin
  Result:=Edit.Color;
end;

function TCustomGroupedEditButton.GetEditText: string;
begin
  Result:=Edit.Text;
end;

function TCustomGroupedEditButton.GetFlat: Boolean;
begin
  Result:=Button.Flat;
end;

function TCustomGroupedEditButton.GetGlyph: TBitmap;
begin
  Result:=Button.Glyph;
end;

function TCustomGroupedEditButton.GetMaxLength: Integer;
begin
  Result:=Edit.MaxLength;
end;

function TCustomGroupedEditButton.GetNumGlyphs: Integer;
begin
  Result:=Button.NumGlyphs;
end;

function TCustomGroupedEditButton.GetOnChange: TNotifyEvent;
begin
  Result:=Edit.OnChange;
end;

function TCustomGroupedEditButton.GetPasswordChar: Char;
begin
  Result:=Edit.PasswordChar;
end;

function TCustomGroupedEditButton.GetReadOnly: boolean;
begin
  Result:=Edit.ReadOnly;
end;

function TCustomGroupedEditButton.IsCustomGlyph: Boolean;

  function _LoadRes: TBitmap;
  var
    ResName: String;
    C : TCustomBitmap;
  begin
    ResName := GetDefaultGlyphName;
    if ResName = '' then
      Exit(nil);
    Result := TBitmap.Create;
    try
      try
        C := CreateBitmapFromLazarusResource(ResName);
        Result.Assign(C); // the "Equals" did not work with ClassType different
        // maybe it should compare the "RawImage" because it is independent of ClassType
      finally
        C.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  end;

var
  B, GlypRes, GlypActual: TBitmap;
begin
  GlypActual := nil;
  GlypRes := nil;
  try
    B := GetDefaultGlyph;
    if B = nil then                // if Default Glyph is nil, use the resource
    begin
      GlypRes := _LoadRes;
      B := GlypRes;
    end;
    if B = nil then
      Result := Glyph <> nil
    else if Glyph = nil then
      Result := True
    else
    begin
      GlypActual := TBitmap.Create; // the "Equals" did not work with ClassType different.
      GlypActual.Assign(Glyph);
      Result := not GlypActual.Equals(B);
    end;
  finally
    GlypRes.Free;
    GlypActual.Free;
  end;
end;

procedure TCustomGroupedEditButton.SetAutoSelect(AValue: Boolean);
begin
  Edit.AutoSelect:=AValue;
end;

procedure TCustomGroupedEditButton.SetButtonHint(AValue: TTranslateString);
begin
  Button.Hint:=AValue;
end;

procedure TCustomGroupedEditButton.SetButtonNeedsFocus(AValue: Boolean);
begin
  if FButtonNeedsFocus=AValue then Exit;
  FButtonNeedsFocus:=AValue;
  CheckButtonVisible;
end;

procedure TCustomGroupedEditButton.SetButtonWidth(AValue: Integer);
begin
  Button.Width:=AValue;
end;

procedure TCustomGroupedEditButton.SetCharCase(AValue: TEditCharCase);
begin
  Edit.CharCase:=AValue;
end;

procedure TCustomGroupedEditButton.SetDirectInput(AValue: Boolean);
begin
  // ToDo
  FDirectInput := AValue;
  //Edit.ReadOnly:=((not FDirectInput) or (FIsReadOnly))
end;

procedure TCustomGroupedEditButton.SetEditDragCursor(AValue: TCursor);
begin
  Edit.DragCursor:=AValue;
end;

procedure TCustomGroupedEditButton.SetBiDiMode(AValue: TBiDiMode);
begin
  if BiDiMode=AValue then exit;
  DisableAutoSizing;
  try
    inherited SetBiDiMode(AValue);
    AnchorEditAndButton;
  finally
    EnableAutoSizing;
  end;
end;

procedure TCustomGroupedEditButton.SetEchoMode(AValue: TEchoMode);
begin
  Edit.EchoMode:=AValue;
end;

procedure TCustomGroupedEditButton.SetEditColor(AValue: TColor);
begin
  Edit.Color:=AValue;
end;

procedure TCustomGroupedEditButton.SetEditDragMode(AValue: TDragMode);
begin
  Edit.DragMode:=AValue;
end;

procedure TCustomGroupedEditButton.SetEditText(AValue: string);
begin
  Edit.Text:=AValue;
end;

procedure TCustomGroupedEditButton.SetFlat(AValue: Boolean);
begin
  Button.Flat:=AValue;
end;

procedure TCustomGroupedEditButton.SetGlyph(AValue: TBitmap);
begin
  Button.Glyph:=AValue;
end;

procedure TCustomGroupedEditButton.SetMaxLength(AValue: Integer);
begin
  Edit.MaxLength:=AValue;
end;

procedure TCustomGroupedEditButton.SetNumGlyphs(AValue: Integer);
begin
  Button.NumGlyphs:=AValue;
end;

procedure TCustomGroupedEditButton.SetOnChange(const AValue: TNotifyEvent);
begin
  Edit.OnChange:=AValue;
end;

procedure TCustomGroupedEditButton.SetPasswordChar(AValue: Char);
begin
  Edit.PasswordChar:=AValue;
end;

procedure TCustomGroupedEditButton.SetReadOnly(AValue: boolean);
begin
  Edit.ReadOnly:=AValue;
  Button.Enabled:=not AValue;
end;

function TCustomGroupedEditButton.CalcButtonVisible: boolean;
begin
  Result := (csdesigning in ComponentState) or
            (IsVisible and (Focused or not FButtonNeedsFocus));
end;

procedure TCustomGroupedEditButton.CheckButtonVisible;
begin
  if Assigned(Button) then
    Button.Visible:=CalcButtonVisible;
end;

procedure TCustomGroupedEditButton.AnchorEditAndButton;
begin
  DisableAutoSizing;
  try
    if IsRightToLeft then begin
      // button + edit
      Button.Anchors:=[akLeft,akTop];
      Button.AnchorParallel(akLeft,0,Self);
      Button.AnchorVerticalCenterTo(Edit);
      Edit.AnchorAsAlign(alRight,0);
      Edit.AnchorToNeighbour(akLeft,0,Button);
    end else begin
      // edit + button
      Button.Anchors:=[akRight,akTop];
      Button.AnchorParallel(akRight,0,Self);
      Button.AnchorVerticalCenterTo(Edit);
      Edit.AnchorAsAlign(alLeft,0);
      Edit.AnchorToNeighbour(akRight,0,Button);
    end;
  finally
    EnableAutoSizing;
  end;
end;

function TCustomGroupedEditButton.GetDefaultGlyph: TBitmap;
begin
  Result:=nil;
end;

function TCustomGroupedEditButton.GetDefaultGlyphName: String;
begin
  Result:='';
end;

procedure TCustomGroupedEditButton.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
end;

constructor TCustomGroupedEditButton.Create(TheOwner: TComponent);
var
  aGlyph: TBitmap;
begin
  inherited Create(TheOwner);
  UseDockManager := False;
  Caption:='';

  FDirectInput := True;

  FEdit:=TMaskEdit.Create(Self);
  Edit.ControlStyle := FEdit.ControlStyle + [csNoDesignSelectable] - [csSetCaption];
  Edit.Parent:=Self;

  FButton:=TSpeedButton.Create(Self);
  Button.OnClick:=@DoButtonClick;
  Button.Cursor := crArrow;
  Button.ControlStyle := Button.ControlStyle + [csNoDesignSelectable];
  aGlyph := GetDefaultGlyph;
  if aGlyph = nil then
    Button.LoadGlyphFromLazarusResource(GetDefaultGlyphName)
  else
    Button.Glyph := aGlyph;
  Button.Parent:=Self;

  SetInitialBounds(0,0,Edit.Width+Button.Width,Edit.Height);

  AnchorEditAndButton;
end;

destructor TCustomGroupedEditButton.Destroy;
begin
  FreeAndNil(FButton);
  FreeAndNil(FEdit);
  inherited Destroy;
end;


end.

