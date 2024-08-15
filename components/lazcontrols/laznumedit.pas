unit LazNumEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, StdCtrls, LCLType, Controls, Clipbrd, StrUtils;

(* Since generics can not override anythning, the specialized class must do this
   and redirect the calls
*)

type

  TLazIntegerEditBaseChangeEvent = procedure(ASender: TObject; ACurrentBase: Integer; var ANewBase: integer; var APrefix) of object;

  { TLazIntegerEditGen }

  generic TLazIntegerEditGen<T: TCustomEdit> = class(T)
  protected const
    Min_Limit = low(Int64);
    Max_Limit = High(Int64);
  private
    FAllowMinus: Boolean;
    FAllowPlus: Boolean;
    FOnBaseChange: TLazIntegerEditBaseChangeEvent;
    FDisplayBase: integer;
    FBinIndicator: String;
    FHexIndicator: String;
    FOctIndicator: String;
    FSetBinKeys: String;
    FSetDecimalKeys: String;
    FSetHexKeys: String;
    FSetOctKeys: String;
    FToggleBinKeys: String;
    FToggleHexKeys: String;
    FToggleOctKeys: String;
    FValue: Int64;
    FMinValue, FMaxValue: Int64;
    FCurrentBasePrefix: string;
    FCurrentPrefix: string;
    FLastDecodeWasEmpty: Boolean;

    function GetCurrentValue: Int64;
    function GetValid: Boolean;
    procedure SetBinIndicator(AValue: String);
    procedure SetDisplayBase(AValue: integer);
    procedure SetHexIndicator(AValue: String);
    procedure SetMaxValue(AValue: Int64);
    procedure SetMinValue(AValue: Int64);
    procedure SetOctIndicator(AValue: String);
    procedure SetSetDecimalKeys(AValue: String);
    procedure SetValue(AValue: Int64);

    procedure UpdateText(ANewText: string; AnAdjustPos: Integer = 0; AnAdjustOffs: Integer = 0; AWasEmpty: boolean = False);
    function  ReEncodeText(ACheckLimit: boolean; ANewBase: integer=-1; ANewPrefix: String=''): Int64;
    function  DecodeText(out APrefix: String; out AVal: Int64; ACheckLimit: boolean): Boolean;
    function  EncodeText(APrefix: String; AVal: Int64; APrefixOnly: Boolean = False): String;
  protected
    procedure _KeyDown(var Key: Word; Shift: TShiftState); //override;
    procedure _KeyPress(var Key: Char); //override;
    procedure _Utf8KeyPress(var UTF8Key: TUTF8Char); //override;

    procedure _InitializeWnd; //override;
    function _RealGetText: TCaption; //override;
    procedure _FinalizeWnd; //override;
    procedure _DoExit; //override;
    procedure _EditingDone; //override;

    procedure _Init; //create; override;
  public
    property Value: Int64 read FValue write SetValue;
    property CurrentValue: Int64 read GetCurrentValue; // while editing // before EditingDone or focus shift
    property Valid: Boolean read GetValid;             // CurrentValue is valid
    property MinValue: Int64 read FMinValue write SetMinValue;
    property MaxValue: Int64 read FMaxValue write SetMaxValue;

    property DisplayBase: integer read FDisplayBase write SetDisplayBase default 10;
    property SetDecimalKeys: String read FSetDecimalKeys write SetSetDecimalKeys;

    property HexIndicator: String read FHexIndicator write SetHexIndicator;
    property SetHexKeys: String read FSetHexKeys write FSetHexKeys;
    property ToggleHexKeys: String read FToggleHexKeys write FToggleHexKeys; // between hex/dec

    property OctIndicator: String read FOctIndicator write SetOctIndicator;
    property SetOctKeys: String read FSetOctKeys write FSetOctKeys;
    property ToggleOctKeys: String read FToggleOctKeys write FToggleOctKeys;

    property BinIndicator: String read FBinIndicator write SetBinIndicator;
    property SetBinKeys: String read FSetBinKeys write FSetBinKeys;
    property ToggleBinKeys: String read FToggleBinKeys write FToggleBinKeys;

    property OnBaseChange: TLazIntegerEditBaseChangeEvent read FOnBaseChange write FOnBaseChange;

    property AllowMinus: Boolean read FAllowMinus write FAllowMinus default True;
    property AllowPlus: Boolean read FAllowPlus write FAllowPlus default True;
    //property DisplayQWord: boolean;
  end;


  { TLazIntegerEdit }

  TLazIntegerEdit = class(specialize TLazIntegerEditGen<TCustomEdit>)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Utf8KeyPress(var UTF8Key: TUTF8Char); override;

    function RealGetText: TCaption; override;
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure DoExit; override;
    procedure EditingDone; override;
    //procedure DoEnter; override;

  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value;
    property MinValue;
    property MaxValue;
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property NumbersOnly;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property TextHint;
    property Visible;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

function Str2QWord(S: string; Base: Byte): QWord;
function QWord2Str(N: QWord; Base: Byte): string;

implementation

// from StrUtils.Dec2Numb / Numb2Dec
function QWord2Str(N: QWord; Base: Byte): string;
var
  C: Integer;
  Number: QWord;
begin
  if N=0 then
    Result:='0'
  else
    begin
    Number:=N;
    Result:='';
    while Number>0 do
      begin
      C:=Number mod Base;
      if C>9 then
        C:=C+55
      else
        C:=C+48;
      Result:=Chr(C)+Result;
      Number:=Number div Base;
      end;
    end;
end;

{$PUSH}{$R+}{$Q+}
function Str2QWord(S: string; Base: Byte): QWord;
var
  i, P: sizeint;
begin
  i:=Length(S);
  Result:=0;
  S:=UpperCase(S);
  P:=1;
  while (i>=1) do
    begin
    if (S[i]>'@') then
      Result:=Result+(Ord(S[i])-55)*P
    else
      Result:=Result+(Ord(S[i])-48)*P;
    Dec(i);
    P:=P*Base;
    end;
end;
{$POP}

{ TLazIntegerEditGen }

function TLazIntegerEditGen.GetCurrentValue: Int64;
var
  p: String;
begin
  if HandleAllocated then begin
    DecodeText(p, Result, False);
  end
  else
    Result := FValue;
end;

function TLazIntegerEditGen.GetValid: Boolean;
var
  p: String;
  v: Int64;
begin
  Result := DecodeText(p, v, False);
end;

procedure TLazIntegerEditGen.SetBinIndicator(AValue: String);
begin
  if FBinIndicator = AValue then Exit;
  FBinIndicator := AValue;

  if FDisplayBase = 2 then
    ReEncodeText(False, FDisplayBase, AValue);
end;

procedure TLazIntegerEditGen.SetDisplayBase(AValue: integer);
var
  np: String;
begin
  if FDisplayBase = AValue then
    exit;
  if (AValue < 2) then AValue := 2;
  if (AValue > 35) then AValue := 35;

  case AValue of
     2:  np := FBinIndicator;
     8:  np := FOctIndicator;
    16:  np := FHexIndicator;
    else np := '';
  end;

  if FOnBaseChange <> nil then
    FOnBaseChange(Self, FDisplayBase, AValue, np);

  if (AValue = FDisplayBase) and (np = FCurrentBasePrefix) then
    exit;

  ReEncodeText(False, AValue, np);
end;

procedure TLazIntegerEditGen.SetHexIndicator(AValue: String);
begin
  if FHexIndicator = AValue then Exit;
  FHexIndicator := AValue;

  if FDisplayBase = 16 then
    ReEncodeText(False, FDisplayBase, AValue);
end;

procedure TLazIntegerEditGen.SetMaxValue(AValue: Int64);
begin
  if FMaxValue = AValue then
    exit;

  FMaxValue := AValue;
  FValue := ReEncodeText(True);
end;

procedure TLazIntegerEditGen.SetMinValue(AValue: Int64);
begin
  if FMinValue = AValue then
    exit;

  FMinValue := AValue;
  FValue := ReEncodeText(True);
end;

procedure TLazIntegerEditGen.SetOctIndicator(AValue: String);
begin
  if FOctIndicator = AValue then Exit;
  FOctIndicator := AValue;

  if FDisplayBase = 86 then
    ReEncodeText(False, FDisplayBase, AValue);
end;

procedure TLazIntegerEditGen.SetSetDecimalKeys(AValue: String);
begin
  if FSetDecimalKeys = AValue then Exit;
  FSetDecimalKeys := AValue;
end;

procedure TLazIntegerEditGen.SetValue(AValue: Int64);
begin
  if FValue = AValue then
    exit;

  FValue := AValue;
  Text := EncodeText(FCurrentBasePrefix, FValue);
end;

procedure TLazIntegerEditGen.UpdateText(ANewText: string; AnAdjustPos: Integer;
  AnAdjustOffs: Integer; AWasEmpty: boolean);
var
  sb, se, m: Integer;
begin
  sb := SelStart;
  se := sb+SelLength;
  Text := ANewText;

  if AnAdjustOffs <> 0 then begin
    m := AnAdjustPos;
    if AnAdjustOffs < 0 then m := m + AnAdjustOffs;
    if sb >= AnAdjustPos then sb := Max(m, sb + AnAdjustOffs);
    if se >= AnAdjustPos then se := Max(m, se + AnAdjustOffs);
  end;
  if AWasEmpty then
    se := Length(ANewText);

  SelStart := sb;
  SelLength := se-sb;
end;

function TLazIntegerEditGen.ReEncodeText(ACheckLimit: boolean; ANewBase: integer;
  ANewPrefix: String): Int64;
var
  p: String;
  x, o: Integer;
begin
  DecodeText(p, Result, ACheckLimit);
  x := 0;
  o := 0;
  if ANewBase > 1 then begin
    x := Length(FCurrentBasePrefix);
    o := Length(ANewPrefix) - x;
    if x < 0 then inc(x);

    FDisplayBase := ANewBase;
    FCurrentBasePrefix := ANewPrefix;
    p := ANewPrefix;
  end;
  UpdateText(EncodeText(p, Result), x, o, FLastDecodeWasEmpty);
end;

function TLazIntegerEditGen.DecodeText(out APrefix: String; out AVal: Int64; ACheckLimit: boolean
  ): Boolean;
var
  s: String;
  n: Boolean;
begin
  APrefix := '';
  AVal := 0;
  s := RealGetText;
  n := (s<>'') and (s[1]='-');
  if (s<>'') and (s[1] in ['-', '+']) then
    delete(s, 1, 1);
  if (Length(s) >= Length(FCurrentBasePrefix)) and
     (strlicomp(PChar(s), PChar(FCurrentBasePrefix), Length(FCurrentBasePrefix)) = 0)
  then begin
    APrefix := copy(s, 1, Length(FCurrentBasePrefix));
    delete(s, 1, Length(FCurrentBasePrefix));
  end;
  FLastDecodeWasEmpty := s = '';

  try
    {$PUSH}{$R+}{$Q+}
    if s <> '' then
      AVal := Str2QWord(s, FDisplayBase);
    if n then
      AVal := -AVal;
    {$POP}
  except
    if n then
      AVal := Min_Limit
    else
      AVal := Max_Limit;
  end;
  if ACheckLimit then begin
    if AVal < FMinValue then
      AVal := FMinValue
    else
    if AVal > FMaxValue then
      AVal := FMaxValue;
  end;
end;

function TLazIntegerEditGen.EncodeText(APrefix: String; AVal: Int64; APrefixOnly: Boolean): String;
begin
  Result := APrefix + QWord2Str(abs(AVal), FDisplayBase);
  if APrefixOnly then
    exit;

  if AVal < 0 then
    Result := '-' + Result;
end;

procedure TLazIntegerEditGen._KeyDown(var Key: Word; Shift: TShiftState);
var
  s: TCaption;
  PreB, PreE, SelB, SelE, l: Integer;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_BACK: begin
      s := Text;
      PreB := 0;
      if (s <> '') and (s[1] = '-') then PreB := 1;
      PreE := PreB + Length(FCurrentBasePrefix);
      SelB := SelStart;
      l := SelLength;
      SelE := SelB+l;

      if ((l = 0) and (SelB <= PreE)) or
         (SelE <= PreE)
      then begin
        if (SelB <= PreE) then
          Key := 0;
      end
      else begin
        if (SelB <= PreE) then begin
          SelStart := PreE;
          SelLength := SelE-PreE;
        end;
      end;
    end;
    VK_DELETE: begin
      s := Text;
      PreB := 0;
      if (s <> '') and (s[1] = '-') then PreB := 1;
      PreE := PreB + Length(FCurrentBasePrefix);
      SelB := SelStart;
      l := SelLength;
      SelE := SelB+l;

      if ((l = 0) and (SelB < PreE)) or
         ((l > 0) and (SelE <= PreE))
      then begin
        Key := 0;
      end
      else begin
        if (SelB <= PreE) then begin
          SelStart := PreE;
          SelLength := SelE-PreE;
        end;
      end;
    end;
  end;
end;

procedure TLazIntegerEditGen._KeyPress(var Key: Char);
  function CheckChar(c: Char): boolean;
  begin
    Result :=
     (c >= '0') and
     ( (c <= chr(ord('0')+Min(10, FDisplayBase-1))) or
       ( (FDisplayBase > 10) and
         ( ((ord(c) and $DF) >= 65) and
           ((ord(c) and $DF) < 55 + FDisplayBase)
     ) ) );
  end;
const
  AllowedControlChars = [#8,#9,^C,^Z];
var
  Utf8Key: TUtf8Char;
  p: String;
  v: Int64;
  e: integer;
  s: TCaption;
  sgn, SelS, SelE: Integer;
  k: Char;
begin
  inherited KeyPress(Key);

  if ReadOnly then exit;
  if key in AllowedControlChars then
    exit;

  s := Text;
  if (s = '') or (s = '-') then begin
    s := s + FCurrentBasePrefix;
    if s <> '' then begin
      Text := s;
      SelStart := Length(s);
    end;
  end;
  sgn := 0;
  if (s <> '') and (s[1] = '-') then sgn := 1;
  SelS := SelStart;
  SelE := SelS + SelLength;

  if (key = ^X) then begin
    Clipboard.AsText := SelText;
    if SelS < sgn + Length(FCurrentBasePrefix) then begin
      SelStart := 0;
      SelLength := SelE;
    end;
    SelText := '';
  end;

  if (key = ^V) then begin
    if (SelS = 0) and (SelE = Length(s)) then begin
      val(Clipboard.AsText, v, e);
      if e = 0 then begin
        FValue := v;
        Text := EncodeText(FCurrentBasePrefix, FValue);
      end;
    end
    else
    if (SelS >= sgn + Length(FCurrentBasePrefix))then begin
      for k in Clipboard.AsText do
        if not CheckChar(k) then begin
          Key := #0;
          exit;
        end;
      exit; // handle key
    end;
    Key := #0;
    exit;
  end;

  if ( (SelS = 0) and (SelE = Length(s)) and CheckChar(Key) ) or
     ( (SelS = sgn) and (SelE = Length(s)-sgn) and CheckChar(Key) )
  then begin
    FDisplayBase := 10;
    FCurrentBasePrefix := '';
    exit;
  end;

  if (SelS >= sgn + Length(FCurrentBasePrefix)) and CheckChar(Key) then
    exit;

  if pos(Key, FToggleBinKeys)  > 0 then begin
    if DisplayBase = 2
    then DisplayBase := 10
    else DisplayBase := 2;
  end
  else
  if pos(Key, FToggleOctKeys) > 0 then begin
    if DisplayBase = 8
    then DisplayBase := 10
    else DisplayBase := 8;
  end
  else
  if pos(Key, FToggleHexKeys) > 0 then begin
    if DisplayBase = 16
    then DisplayBase := 10
    else DisplayBase := 16;
  end
  else if pos(Key, FSetDecimalKeys) > 0 then DisplayBase := 10
  else if pos(Key, FSetBinKeys)     > 0 then DisplayBase := 2
  else if pos(Key, FSetOctKeys)    > 0 then DisplayBase := 8
  else if pos(Key, FSetHexKeys)    > 0 then DisplayBase := 16

  else
  if FAllowMinus and (key = '-') then begin
    DecodeText(p, v, False);
    UpdateText(EncodeText(p, -v, FLastDecodeWasEmpty));
  end
  else
  if FAllowPlus and (key = '+') then begin
    DecodeText(p, v, False);
    if v < 0 then
      UpdateText(EncodeText(p, -v, FLastDecodeWasEmpty));
  end;

  Key := #0;
end;

procedure TLazIntegerEditGen._Utf8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited Utf8KeyPress(UTF8Key);
  if (Length(Utf8Key) > 1) then
    UTF8Key := '';
end;

procedure TLazIntegerEditGen._InitializeWnd;
begin
  inherited InitializeWnd;
  Text := EncodeText(FCurrentBasePrefix, FValue);
end;

function TLazIntegerEditGen._RealGetText: TCaption;
begin
  if HandleAllocated then
    Result := inherited RealGetText
  else
    Result := EncodeText(FCurrentBasePrefix, FValue);
end;

procedure TLazIntegerEditGen._FinalizeWnd;
var
  p: String;
begin
  DecodeText(p, FValue, True);
  inherited FinalizeWnd;
end;

procedure TLazIntegerEditGen._DoExit;
begin
  FValue := ReEncodeText(True);
  inherited DoExit;
end;

procedure TLazIntegerEditGen._EditingDone;
begin
  FValue := ReEncodeText(True);
  inherited EditingDone;
end;

procedure TLazIntegerEditGen._Init;
begin
  FMinValue       := Min_Limit;
  FMaxValue       := Max_Limit;
  FDisplayBase    := 10;
  FBinIndicator   := '%';
  FHexIndicator   := '$';
  FOctIndicator   := '&';
  FSetDecimalKeys := '#';
  FToggleBinKeys  := '%';
  FToggleHexKeys  := '$x';
  FToggleOctKeys  := '&';
  FAllowMinus     := True;
  FAllowPlus      := True;
end;

{ TLazIntegerEdit }

procedure TLazIntegerEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  _KeyDown(Key, Shift);
end;

procedure TLazIntegerEdit.KeyPress(var Key: Char);
begin
  _KeyPress(Key);
end;

procedure TLazIntegerEdit.Utf8KeyPress(var UTF8Key: TUTF8Char);
begin
  _Utf8KeyPress(UTF8Key);
end;

function TLazIntegerEdit.RealGetText: TCaption;
begin
  Result := _RealGetText;
end;

procedure TLazIntegerEdit.InitializeWnd;
begin
  _InitializeWnd;
end;

procedure TLazIntegerEdit.FinalizeWnd;
begin
  _FinalizeWnd;
end;

procedure TLazIntegerEdit.DoExit;
begin
  _DoExit;
end;

procedure TLazIntegerEdit.EditingDone;
begin
  _EditingDone;
end;

constructor TLazIntegerEdit.Create(AOwner: TComponent);
begin
  _Init;
  inherited Create(AOwner);
end;

end.

