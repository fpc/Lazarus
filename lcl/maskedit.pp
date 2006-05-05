{
 /***************************************************************************
                                 maskedit.pp
                                 -----------
                           Component Library Code
                           
        Does not yet support charsets that use multiple bytes per char

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit MaskEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, LMessages, LCLType, Graphics;
  
const
  DefaultBlank: Char = '_';
  MaskFieldSeparator: Char = ';';
  MaskNoSave: Char = '0';

  mDirReverse = '!';         { removes leading blanks if true, else trailing blanks}
  mDirUpperCase = '>';       { all chars that follow to upper case }
  mDirLowerCase = '<';       { all chars that follow to lower case }
                             { '<>' means remove casing directive }
  mDirLiteral = '\';         { char that immediately follows is a literal }

  mMskAlpha = 'L';           { in US = A-Z,a-z }
  mMskAlphaOpt = 'l';
  mMskAlphaNum = 'A';        { in US = A-Z,a-z,0-9 }
  mMskAlphaNumOpt  = 'a';
  mMskAscii = 'C';           { any character}
  mMskAsciiOpt = 'c';
  mMskNumeric = '0';         { 0-9, no plus or minus }
  mMskNumericOpt = '9';
  mMskNumSymOpt = '#';       { 0-9, plus and minus }

  mMskTimeSeparator = ':';
  mMskDateSeparator = '/';

type

  EDBEditError = class(Exception);

  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);
  

  { TCustomMaskEdit }

  TMaskCharType = (mcNone, mcLiteral, mcIntlLiteral, mcDirective, mcMask,
    mcMaskOpt, mcFieldSeparator, mcField);
  TMaskDirectives = set of (mdReverseDir, mdUpperCase, mdLowerCase,
    mdLiteralChar);
  TMaskedState = set of (msMasked, msReEnter, msDBSetText);
  
  TCustomMaskEdit = class(TCustomEdit)
  private
    FEditMask: string;
    FMaskBlank: Char;
    FMaxChars: Integer;
    FMaskSave: Boolean;
    FMaskState: TMaskedState;
    FCaretPos: Integer;
    FBtnDownX: Integer;
    FOldValue: string;
    function IsCharAlpha(AChar: Char): Boolean;
    function IsCharAlphaNumeric(AChar: Char): Boolean;
    function DoInputChar(var NewChar: char; MaskOffset: Integer): Boolean;
    function InputChar(var NewChar: char; Offset: Integer): Boolean;
    function DeleteSelection(var Value: string; Offset: Integer;
      Len: Integer): Boolean;
    function InputString(var Value: string; const NewValue: string;
      Offset: Integer): Integer;
    function AddEditFormat(const Value: string; Active: Boolean): string;
    function RemoveEditFormat(const Value: string): string;
    function FindLiteralChar (MaskOffset: Integer; InChar: Char): Integer;
    function GetMasked: Boolean;
    function GetMaskText: string;
    function GetMaxLength: Integer;
    function CharKeys(var CharCode: char): Boolean;
    procedure SetEditText(const Value: string);
    procedure SetEditMask(const Value: string);
    procedure SetMaxLength(Value: Integer);
    procedure SetMaskText(const Value: string);
    procedure DeleteKeys(CharCode: Word);
    procedure HomeEndKeys(CharCode: Word; Shift: TShiftState);
    procedure CursorInc(CursorPos: Integer; Incr: Integer);
    procedure CursorDec(CursorPos: Integer);
    procedure ArrowKeys(CharCode: Word; Shift: TShiftState);
    procedure TextChanged; override;
  protected
    procedure ReformatText(const NewMask: string);
    procedure SetCursor(Pos: Integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    function EditCanModify: Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Reset; virtual;
    function GetEditText: string; virtual;
    function GetFirstEditChar: Integer;
    function GetLastEditChar: Integer;
    function GetNextEditChar(Offset: Integer): Integer;
    function GetPriorEditChar(Offset: Integer): Integer;
    function GetMaxChars: Integer;
    function Validate(const Value: string; var Pos: Integer): Boolean; virtual;
    procedure ValidateError; virtual;
    procedure CheckCursor;
    property EditMask: string read FEditMask write SetEditMask;
    property MaskState: TMaskedState read FMaskState write FMaskState;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default -1;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateEdit; virtual;
    procedure Clear;
    property IsMasked: Boolean read GetMasked;
    property EditText: string read GetEditText write SetEditText;
    property Text: string read GetMaskText write SetMaskText;
  end;
  
  
  { TMaskEdit }

  TMaskEdit = class(TCustomMaskEdit)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
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
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
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
  end;
  
function FormatMaskText(const EditMask: string; const Value: string): string;
function MaskGetMaskSave(const EditMask: string): Boolean;
function MaskGetMaskBlank(const EditMask: string): Char;
function MaskGetFldSeparator(const EditMask: string): Integer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional',[TMaskEdit]);
end;

function ByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result := mbSingleByte;
  { ToDo:
    if SysLocale.FarEast then
      Result := ByteTypeTest(PChar(S), Index-1);
  }
end;

function MaskGetCharType(const EditMask: string; MaskOffset: Integer): TMaskCharType;
var
  MaskChar: Char;
begin
  Result := mcLiteral;
  MaskChar := #0;
  if MaskOffset <= Length(EditMask) then
    MaskChar := EditMask[MaskOffset];
  if MaskOffset > Length(EditMask) then
    Result := mcNone

  else if ByteType(EditMask, MaskOffset) <> mbSingleByte then
    Result := mcLiteral

  else if (MaskOffset > 1) and (EditMask[MaskOffset - 1] = mDirLiteral) and
      (ByteType(EditMask, MaskOffset - 1) = mbSingleByte) and
      not ((MaskOffset > 2) and (EditMask[MaskOffset - 2] = mDirLiteral) and
      (ByteType(EditMask, MaskOffset - 2) = mbSingleByte)) then
    Result := mcLiteral

  else if (MaskChar = MaskFieldSeparator) and
         (Length(EditMask) >= 4) and
         (MaskOffset > Length(EditMask) - 4) then
    Result := mcFieldSeparator

  else if (Length(EditMask) >= 4) and
         (MaskOffset > (Length(EditMask) - 4)) and
         (EditMask[MaskOffset - 1] = MaskFieldSeparator) and
         not ((MaskOffset > 2) and (EditMask[MaskOffset - 2] = mDirLiteral) and
         (ByteType(EditMask, MaskOffset - 2) <> mbTrailByte)) then
    Result := mcField

  else if MaskChar in [mMskTimeSeparator, mMskDateSeparator] then
    Result := mcIntlLiteral

  else if MaskChar in [mDirReverse, mDirUpperCase, mDirLowerCase,
      mDirLiteral] then
    Result := mcDirective

  else if MaskChar in [mMskAlphaOpt, mMskAlphaNumOpt, mMskAsciiOpt,
      mMskNumSymOpt, mMskNumericOpt] then
    Result := mcMaskOpt

  else if MaskChar in [mMskAlpha, mMskAlphaNum, mMskAscii, mMskNumeric] then
    Result := mcMask;
end;

function MaskGetCurrentDirectives(const EditMask: string;
  MaskOffset: Integer): TMaskDirectives;
var
  I: Integer;
  MaskChar: Char;
begin
  Result := [];
  for I := 1 to Length(EditMask) do
  begin
    MaskChar := EditMask[I];
    if (MaskChar = mDirReverse) then
      Include(Result, mdReverseDir)
    else if (MaskChar = mDirUpperCase) and (I < MaskOffset) then
    begin
      Exclude(Result, mdLowerCase);
      if not ((I > 1) and (EditMask[I-1] = mDirLowerCase)) then
        Include(Result, mdUpperCase);
    end
    else if (MaskChar = mDirLowerCase) and (I < MaskOffset) then
    begin
      Exclude(Result, mdUpperCase);
      Include(Result, mdLowerCase);
    end;
  end;
  if MaskGetCharType(EditMask, MaskOffset) = mcLiteral then
    Include(Result, mdLiteralChar);
end;

function MaskIntlLiteralToChar(IChar: Char): Char;
begin
  Result := IChar;
  case IChar of
    mMskTimeSeparator: Result := TimeSeparator;
  end;
end;

function MaskDoFormatText(const EditMask: string; const Value: string;
  Blank: Char): string;
var
  I: Integer;
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
  Dir: TMaskDirectives;
begin
  Result := Value;
  Dir := MaskGetCurrentDirectives(EditMask, 1);
  if not (mdReverseDir in Dir) then
  begin
    Offset := 1;
    for MaskOffset := 1 to Length(EditMask) do
    begin
      CType := MaskGetCharType(EditMask, MaskOffset);
      if CType in [mcLiteral, mcIntlLiteral] then
      begin
        Result := Copy(Result, 1, Offset-1) +
              MaskIntlLiteralToChar(EditMask[MaskOffset]) +
              Copy(Result, Offset, Length(Result));
        Inc(Offset);
      end
      else if CType in [mcMask, mcMaskOpt] then
      begin
        if Offset > Length(Result) then
          Result := Result + Blank;
        Inc(Offset);
      end;
    end;
  end
  else
  begin
    Offset := Length(Result);
    for I := 0 to Length(EditMask)-1 do
    begin
      MaskOffset := Length(EditMask) - I;
      CType := MaskGetCharType(EditMask, MaskOffset);
      if CType in [mcLiteral, mcIntlLiteral] then
      begin
        Result := Copy(Result, 1, Offset) +
               MaskIntlLiteralToChar(EditMask[MaskOffset]) +
               Copy(Result, Offset + 1, Length(Result) - Offset);
      end
      else if CType in [mcMask, mcMaskOpt] then
      begin
        if Offset < 1 then
          Result := Blank + Result
        else
          Dec(Offset);
      end;
    end;
  end;
end;

function MaskGetMaskSave(const EditMask: string): Boolean;
var
  I: Integer;
  Sep1, Sep2: Integer;
begin
  Result := True;
  if Length(EditMask) >= 4 then
  begin
    Sep1 := 0;
    Sep2 := 0;
    I := Length(EditMask);
    while Sep2 < 1 do
    begin
      if (MaskGetCharType(EditMask, I) =  mcFieldSeparator) then
      begin
        if Sep1 < 1 then begin
          Sep1 := I+1;
        end else begin
          Sep2 := I+1;
        end;
      end;
      Dec(I);
      if (I <= 1) or(I < Length(EditMask) - 3) then
        Break;
    end;
    if Sep2 < 1 then
      Sep2 := Sep1;
    if Sep2 <> Length(EditMask)+1 then
      begin
        try
          Result := not (EditMask [Sep2] = MaskNoSave);
        except
          Result := False;
        end;
      end;
  end;
end;

function MaskGetMaskBlank(const EditMask: string): Char;
begin
  Result := DefaultBlank;
  if Length(EditMask) >= 4 then
  begin
    if (MaskGetCharType(EditMask, Length(EditMask) - 1) =
                                                  mcFieldSeparator) then
    begin
      if (MaskGetCharType(EditMask, Length(EditMask) - 2) =
                                                  mcFieldSeparator) or
        (MaskGetCharType(EditMask, Length(EditMask) - 3) =
                                                  mcFieldSeparator) then
      begin
        Result := EditMask [Length(EditMask)];
      end;
    end;
  end;
end;

function MaskGetFldSeparator(const EditMask: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Length(EditMask) >= 4 then
  begin
    for I := (Length(EditMask) - 4) to Length(EditMask) do
    begin
      if (MaskGetCharType(EditMask, I) = mcFieldSeparator) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
end;

function MaskOffsetToOffset(const EditMask: String; MaskOffset: Integer): Integer;
var
  I: Integer;
  CType: TMaskCharType;
begin
  Result := 0;
  for I := 1 to MaskOffset do
  begin
    CType := MaskGetCharType(EditMask, I);
    if not (CType in [mcDirective, mcField, mcFieldSeparator]) then
      Inc(Result);
  end;
end;

function OffsetToMaskOffset(const EditMask: string; Offset: Integer): Integer;
var
  I: Integer;
  Count: Integer;
  MaxChars: Integer;
begin
  MaxChars  := MaskOffsetToOffset(EditMask, Length(EditMask));
  if Offset > MaxChars then
  begin
    Result := -1;
    Exit;
  end;

  Result := 0;
  Count := Offset;
  for I := 1 to Length(EditMask) do
  begin
    Inc(Result);
    if not (mcDirective = MaskGetCharType(EditMask, I)) then
    begin
      Dec(Count);
      if Count < 0 then begin
        Exit;
      end;
    end;
  end;
end;

function IsLiteralChar(const EditMask: string; Offset: Integer): Boolean;
var
  MaskOffset: Integer;
  CType: TMaskCharType;
begin
  Result := False;
  MaskOffset := OffsetToMaskOffset(EditMask, Offset);
  if MaskOffset > 0 then
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    Result := CType in [mcLiteral, mcIntlLiteral];
  end;
end;

function PadSubField(const EditMask: String; const Value: string;
  StartFld, StopFld, Len: Integer; Blank: Char): string;
var
  Dir: TMaskDirectives;
  StartPad: Integer;
  K: Integer;
begin
  if (StopFld - StartFld) < Len then
  begin
    Dir := MaskGetCurrentDirectives(EditMask, 1);
    StartPad := StopFld - 1;
    if mdReverseDir in Dir then
      StartPad := StartFld - 1;
    Result := Copy(Value, 1, StartPad);
    for K := 1 to (Len - (StopFld - StartFld)) do
      Result := Result + Blank;
    Result := Result + Copy(Value, StartPad + 1, Length(Value));
  end
  else if (StopFld - StartFld) > Len then
  begin
    Dir := MaskGetCurrentDirectives(EditMask, 1);
    if mdReverseDir in Dir then
      Result := Copy(Value, 1, StartFld - 1) +
        Copy(Value, StopFld - Len, Length(Value))
    else
      Result := Copy(Value, 1, StartFld + Len - 1) +
        Copy(Value, StopFld, Length(Value));
  end
  else
    Result := Value;
end;

function PadInputLiterals(const EditMask: String; const Value: string;
  Blank: Char): string;
var
  J: Integer;
  LastLiteral, EndSubFld: Integer;
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
  MaxChars: Integer;
begin
  LastLiteral := 0;

  Result := Value;
  for MaskOffset := 1 to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral] then
    begin
      Offset := MaskOffsetToOffset(EditMask, MaskOffset);
      EndSubFld := Length(Result) + 1;
      for J := LastLiteral + 1 to Length(Result) do
      begin
        if Result[J] = MaskIntlLiteralToChar(EditMask[MaskOffset]) then
        begin
          EndSubFld := J;
          Break;
        end;
      end;
      if EndSubFld > Length(Result) then
        Result := Result + MaskIntlLiteralToChar(EditMask[MaskOffset]);
      Result := PadSubField(EditMask, Result, LastLiteral + 1, EndSubFld,
        Offset - (LastLiteral + 1), Blank);
      LastLiteral := Offset;
    end;
  end;

  MaxChars  := MaskOffsetToOffset(EditMask, Length(EditMask));
  if Length (Result) <> MaxChars then
    Result := PadSubField(EditMask, Result, LastLiteral + 1, Length (Result) + 1,
      MaxChars - LastLiteral, Blank);

  for Offset := 1 to Length (Result) do
  begin
    if Result[Offset] = ' ' then
    begin
      if not IsLiteralChar(EditMask, Offset - 1) then
        Result[Offset] := Blank;
    end;
  end;
end;

function FormatMaskText(const EditMask: string; const Value: string ): string;
begin
  if MaskGetMaskSave(EditMask) then
    Result := PadInputLiterals(EditMask, Value, ' ')
  else
    Result := MaskDoFormatText(EditMask, Value, ' ');
end;

{ TCustomMaskEdit }

constructor TCustomMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaskState := [];
  FMaskBlank := DefaultBlank;
end;

procedure TCustomMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if IsMasked and (Key <> 0) and not (ssAlt in Shift) then
  begin
    if (Key = VK_LEFT) or(Key = VK_RIGHT) then
    begin
      ArrowKeys(Key, Shift);
      if not ((ssShift in Shift) or (ssCtrl in Shift)) then
        Key := 0;
      Exit;
    end
    else if (Key = VK_UP) or(Key = VK_DOWN) then
    begin
      Key := 0;
      Exit;
    end
    else if (Key = VK_HOME) or(Key = VK_END) then
    begin
      HomeEndKeys(Key, Shift);
      Key := 0;
      Exit;
    end
    else if ((Key = VK_DELETE) and ([ssShift, ssCtrl] * Shift = [])) or
      (Key = VK_BACK) then
    begin
      if EditCanModify then
        DeleteKeys(Key);
      Key := 0;
      Exit;
    end;
    CheckCursor;
  end;
end;

procedure TCustomMaskEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if IsMasked and (Key <> 0) then
  begin
    if ((Key = VK_LEFT) or(Key = VK_RIGHT)) and (ssCtrl in Shift) then
      CheckCursor;
  end;
end;

procedure TCustomMaskEdit.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  // TODO UTF8
  if IsMasked and ((Key=#8) or (Key=#9) or (Key > #31)) then begin
    CharKeys(Key);
    Key := #0;
  end;
end;

procedure TCustomMaskEdit.SetEditText(const Value: string);
begin
  if GetEditText <> Value then begin
    if HandleAllocated then
      SetTextBuf(PChar(Value))
    else
      inherited Text:=Value;
    CheckCursor;
  end;
end;

function TCustomMaskEdit.GetEditText: string;
begin
  Result := inherited Text;
end;

function TCustomMaskEdit.GetMaskText: string;
begin
  if not IsMasked then
    Result := inherited Text
  else
  begin
    Result := RemoveEditFormat(EditText);
    if FMaskSave then begin
      Result := AddEditFormat(Result, False);
    end;
  end;
end;

procedure TCustomMaskEdit.SetMaskText(const Value: string);
var
  OldText: string;
  Pos: Integer;
begin
  if not IsMasked then
    inherited Text := Value
  else
  begin
    OldText := Value;
    if FMaskSave then begin
      OldText := PadInputLiterals(EditMask, OldText, FMaskBlank);
    end else begin
      OldText := AddEditFormat(OldText, True);
    end;
    if not (msDBSetText in FMaskState) and
      (csDesigning in ComponentState) and
      not (csLoading in ComponentState) and
      not Validate(OldText, Pos) then
      raise EDBEditError.Create('Mask Error');
    EditText := OldText;
  end;
end;

function TCustomMaskEdit.GetMasked: Boolean;
begin
  Result := EditMask <> '';
end;

function TCustomMaskEdit.GetMaxChars: Integer;
begin
  if IsMasked then
    Result := FMaxChars
  else
    Result := 0;
end;

procedure TCustomMaskEdit.ReformatText(const NewMask: string);
var
  OldText: string;
begin
  OldText := RemoveEditFormat(EditText);
  FEditMask := NewMask;
  FMaxChars  := MaskOffsetToOffset(EditMask, Length(NewMask));
  FMaskSave  := MaskGetMaskSave(NewMask);
  FMaskBlank := MaskGetMaskBlank(NewMask);
  OldText := AddEditFormat(OldText, True);
  EditText := OldText;
end;

procedure TCustomMaskEdit.SetEditMask(const Value: string);
begin
  if Value <> EditMask then
  begin
    if (csDesigning in ComponentState) and (Value <> '') and
      not (csLoading in ComponentState) then
      EditText := '';
    ReformatText(Value);
    Exclude(FMaskState, msMasked);
    if EditMask <> '' then Include(FMaskState, msMasked);
    inherited MaxLength := 0;
    if IsMasked and (FMaxChars > 0) then
      inherited MaxLength := FMaxChars;
    if not (csDesigning in ComponentState) then
      SetCursor(SelStart);
  end;
end;

function TCustomMaskEdit.GetMaxLength: Integer;
begin
  Result := inherited MaxLength;
end;

procedure TCustomMaskEdit.SetMaxLength(Value: Integer);
begin
  if not IsMasked then
    inherited MaxLength := Value
  else
    inherited MaxLength := FMaxChars;
end;

procedure TCustomMaskEdit.SetCursor(Pos: Integer);
begin
  if (Pos >= 1) and (ByteType(EditText, Pos) = mbLeadByte) then Dec(Pos);
  SelStart := Pos;
  if (IsMasked) then
  begin
    if SelStart < 0 then
      SelStart := 0;
      SelLength := 1;
    {if (Length(EditText) > SelStart+1) and (EditText[SelStart+1] in LeadBytes) then
      Inc(SelStart+1);    }
    if SelStart >= FMaxChars then
    begin
      SelStart := FMaxChars;
      SelLength  := 0;
    end;

    FCaretPos := SelStart;
  end
  else
  begin
    if SelStart < 0 then
      SelStart := 0;
    if SelStart >= Length(EditText) then
      SelStart := Length(EditText);
  end;
end;

procedure TCustomMaskEdit.CheckCursor;
begin
  if not HandleAllocated then  Exit;
  if (IsMasked) then
  begin
    if SelLength = 0 then
      SetCursor(SelStart);
  end;
end;

procedure TCustomMaskEdit.Clear;
begin
  Text := '';
end;

function TCustomMaskEdit.EditCanModify: Boolean;
begin
  Result := True;
end;

procedure TCustomMaskEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button=mbLeft) then
    FBtnDownX := X;
end;

procedure TCustomMaskEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if IsMasked and (Button=mbLeft) then begin
    FCaretPos := SelStart;
    if (SelLength < 1) and (X > FBtnDownX) then
      FCaretPos := SelStart;
    CheckCursor;
  end;
end;

procedure TCustomMaskEdit.DoEnter;
begin
  inherited DoEnter;
  if IsMasked and not (csDesigning in ComponentState) then
  begin
    if not (msReEnter in FMaskState) then
    begin
      FOldValue := EditText;
    end;
    Exclude(FMaskState, msReEnter);
    CheckCursor;
  end;
end;

procedure TCustomMaskEdit.DoExit;
begin
  inherited DoExit;
  if IsMasked and not (csDesigning in ComponentState) then
  begin
    ValidateEdit;
    CheckCursor;
  end;
end;

procedure TCustomMaskEdit.Reset;
begin
  if Modified then
  begin
    EditText := FOldValue;
    Modified := False;
  end;
end;

function TCustomMaskEdit.CharKeys(var CharCode: char): Boolean;
var
  Txt: string;
  OldPos: Integer;
begin
  Result := False;
  if Word(CharCode) = VK_ESCAPE then
  begin
    Reset;
    Exit;
  end;
  if not EditCanModify or ReadOnly then Exit;
  if (Word(CharCode) = VK_BACK) then Exit;
  if (Word(CharCode) = VK_RETURN) then
  begin
    ValidateEdit;
    Exit;
  end;

  if (SelLength) > 1 then
  begin
    DeleteKeys(VK_DELETE);
    SelStart := GetNextEditChar(SelStart);
    SetCursor(SelStart);
  end;

  {if (CharCode in LeadBytes) then
    if PeekMessage(CharMsg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE) then
      if CharMsg.Message = WM_Quit then
        PostQuitMessage(CharMsg.wparam); }
  Result := InputChar(CharCode, SelStart);
  
  if Result then
  begin
    {if (CharCode in LeadBytes) then
    begin
      Txt := CharCode + Char(CharMsg.wParam);
      SelLength := 2;
    end
    else   }
    
    Txt := CharCode;
      
    OldPos := SelStart;
    EditText := copy(EditText,1,SelStart) + Txt + copy(EditText,SelStart+SelLength+1, length(EditText));
    CursorInc(OldPos, 1);
  end;
end;

procedure TCustomMaskEdit.ArrowKeys(CharCode: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then Exit;
  if (ssShift in Shift) then
  begin
    if (CharCode = VK_RIGHT) then
    begin
      Inc(FCaretPos);
      if (SelLength = 1) then
      begin
        Inc(FCaretPos);
      end;
      if FCaretPos > FMaxChars then FCaretPos := FMaxChars;
    end
    else
    begin
      Dec(FCaretPos);
      if (SelLength = 2) and
        (FCaretPos > SelStart) then
      begin
        SelStart := SelStart+1;
        SelLength := 0;
        Dec(FCaretPos);
      end;
      if FCaretPos < 0 then FCaretPos := 0;
    end;
  end
  else
  begin
    if (SelLength) > 1 then
    begin
      {if ((SelLength) = 2) and (EditText[SelStart+1] in LeadBytes) then
      begin
        if (CharCode = VK_LEFT) then
          CursorDec(SelStart)
        else
          CursorInc(SelStart, 2);
        Exit;
      end; }
      if selstart+SelLength = FCaretPos then
        Dec(FCaretPos);
      SetCursor(FCaretPos);
    end
    else if (CharCode = VK_LEFT) then
      CursorDec(SelStart)
    else
    begin
      if SelLength = 0 then
        SetCursor(SelStart)
      else
        {if EditText[SelStart+1] in LeadBytes then
          CursorInc(SelStart, 2)
        else     }
          CursorInc(SelStart, 1);
    end;
  end;
end;

procedure TCustomMaskEdit.CursorInc(CursorPos: Integer; Incr: Integer);
var
  NuPos: Integer;
begin
  NuPos := CursorPos + Incr;
  NuPos := GetNextEditChar(NuPos);
  if IsLiteralChar(EditMask, nuPos) then
    NuPos := CursorPos;
  SetCursor(NuPos);
end;


procedure TCustomMaskEdit.CursorDec(CursorPos: Integer);
var
  nuPos: Integer;
begin
  nuPos := CursorPos;
  Dec(nuPos);
  nuPos := GetPriorEditChar(nuPos);
  SetCursor(NuPos);
end;

function TCustomMaskEdit.GetFirstEditChar: Integer;
begin
  Result := 0;
  if IsMasked then
    Result := GetNextEditChar(0);
end;

function TCustomMaskEdit.GetLastEditChar: Integer;
begin
  Result := GetMaxChars;
  if IsMasked then
    Result := GetPriorEditChar(Result - 1);
end;

function TCustomMaskEdit.GetNextEditChar(Offset: Integer): Integer;
begin

  Result := Offset;
  while(Result < FMaxChars) and (IsLiteralChar(EditMask, Result)) do
    Inc(Result);
end;

function TCustomMaskEdit.GetPriorEditChar(Offset: Integer): Integer;
begin
  Result := Offset;
  while(Result >= 0) and (IsLiteralChar(EditMask, Result)) do
    Dec(Result);
  if Result < 0 then
    Result := GetNextEditChar(Result);
end;

procedure TCustomMaskEdit.HomeEndKeys(CharCode: Word; Shift: TShiftState);
begin
  if (CharCode = VK_HOME) then
  begin
    if (ssShift in Shift) then
    begin
      if (SelStart <> FCaretPos) and (SelLength <> 1) then
        SelLength := 1;
      CheckCursor;
    end
    else
      SetCursor(0);
    FCaretPos := 0;
  end
  else
  begin
    if (ssShift in Shift) then
    begin
      if (selstart+SelLength <> FCaretPos) and (SelLength <> 1) then
        SelLength := SelLength - 1;
      CheckCursor;
    end
    else
      SetCursor(FMaxChars);
    FCaretPos := FMaxChars;
  end;
end;

procedure TCustomMaskEdit.DeleteKeys(CharCode: Word);
var
  NuSelStart: Integer;
  CType: TMaskCharType;
  Str: string;
begin
  if ReadOnly then Exit;
  
  if ((SelLength) < 1) and (CharCode = VK_BACK) then
  begin
    NuSelStart := SelStart;
    CursorDec(SelStart);

    Str := EditText;
    DeleteSelection(Str, SelStart+1, 1);

    NuSelStart := SelStart;
    EditText := Str;
    SetCursor(NuSelStart);
    exit;
  end;

  if (SelLength) < 1 then Exit;
  if (selstart) < 1 then exit;

  Str := EditText;

  CType := MaskGetCharType(EditMask, SelStart);
  if CType in [mcLiteral, mcIntlLiteral] then begin
     SelStart := SelStart - 1;
     SelLength := 1;
  end;

  DeleteSelection(Str, SelStart, SelLength);
  NuSelStart := SelStart-1;
  EditText := Str;
  SelStart := NuSelStart;
  if (SelLength) <> 1 then
  begin
    SelStart := GetNextEditChar(SelStart);
    SetCursor(SelStart);
  end
  else begin
    SelStart := GetNextEditChar(SelStart - 1);
    SetCursor(SelStart - 2);
  end;
end;

procedure TCustomMaskEdit.TextChanged;
var
  Temp: Integer;
begin
  inherited;
  FOldValue := EditText;
  if HandleAllocated then
  begin
    Temp := GetNextEditChar(SelStart);
    if Temp <> SelStart then
      SetCursor(Temp);
  end;
end;


procedure TCustomMaskEdit.ValidateEdit;
var
  Str: string;
  Pos: Integer;
begin
  Str := EditText;
  if IsMasked and Modified then
  begin
    if not Validate(Str, Pos) then
    begin
      if not (csDesigning in ComponentState) then
      begin
        Include(FMaskState, msReEnter);
        SetFocus;
      end;
      SetCursor(Pos);
      ValidateError;
    end;
  end;
end;

procedure TCustomMaskEdit.ValidateError;
var
  Str: string;
begin
  //MessageBeep(0);
  Str := 'Mask error:'+EditMask;
  raise EDBEditError.Create(Str);
end;

function TCustomMaskEdit.AddEditFormat(const Value: string; Active: Boolean): string;
begin
  if not Active then
    Result := MaskDoFormatText(EditMask, Value, ' ')
  else
    Result := MaskDoFormatText(EditMask, Value, FMaskBlank);
end;

function TCustomMaskEdit.RemoveEditFormat(const Value: string): string;
var
  I: Integer;
  OldLen: Integer;
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
  Dir: TMaskDirectives;
begin
  Offset := 1;
  Result := Value;
  for MaskOffset := 1 to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);

    if CType in [mcLiteral, mcIntlLiteral] then
      Result := Copy(Result, 1, Offset - 1) +
        Copy(Result, Offset + 1, Length(Result) - Offset);
    if CType in [mcMask, mcMaskOpt] then Inc(Offset);
  end;

  Dir := MaskGetCurrentDirectives(EditMask, 1);
  if mdReverseDir in Dir then
  begin
    Offset := 1;
    for I := 1 to Length(Result) do
    begin
      if Result[I] = FMaskBlank then
        Inc(Offset)
      else
        break;
    end;
    if Offset <> 1 then
      Result := Copy(Result, Offset, Length(Result) - Offset + 1);
  end
  else begin
    OldLen := Length(Result);
    for I := 1 to OldLen do
    begin
      if Result[OldLen - I + 1] = FMaskBlank then
        SetLength(Result, Length(Result) - 1)
      else Break;
    end;
  end;
  if FMaskBlank <> ' ' then
  begin
    OldLen := Length(Result);
    for I := 1 to OldLen do
    begin
      if Result[I] = FMaskBlank then
        Result[I] := ' ';
      if I > OldLen then Break;
    end;
  end;
end;

function TCustomMaskEdit.InputChar(var NewChar: char; Offset: Integer): Boolean;
var
  MaskOffset: Integer;
  CType: TMaskCharType;
  InChar: Char;
begin
  Result := True;
  if EditMask <> '' then
  begin
    Result := False;
    MaskOffset := OffsetToMaskOffset(EditMask, Offset);
    if MaskOffset >= 0 then
    begin
      CType := MaskGetCharType(EditMask, MaskOffset);
      InChar := NewChar;
      Result := DoInputChar(NewChar, MaskOffset);
      if not Result and (CType in [mcMask, mcMaskOpt]) then
      begin
        MaskOffset := FindLiteralChar (MaskOffset, InChar);
        if MaskOffset > 0 then
        begin
          MaskOffset := MaskOffsetToOffset(EditMask, MaskOffset);
          SetCursor (MaskOffset);
          Exit;
        end;
      end;
    end;
  end;
  {if not Result then
    MessageBeep(0)  }
end;

function TCustomMaskEdit.IsCharAlpha(AChar: Char): Boolean;
var
  MyCharSet: Set of Char;
begin
  MyCharSet := ['a'..'z','A'..'Z'];
  Result := False;
  if AChar in MyCharSet then result := True;
end;

function TCustomMaskEdit.IsCharAlphaNumeric(AChar: Char): Boolean;
var
  MyCharSet: set of char;
begin
  MyCharSet := ['a'..'z','A'..'Z','0'..'9'];
  Result := False;
  if AChar in MyCharSet then result := True;
end;

function TCustomMaskEdit.DoInputChar(var NewChar: char; MaskOffset: Integer): Boolean;
var
  Dir: TMaskDirectives;
  Str: string;
  CType: TMaskCharType;

  function TestChar(NewChar: Char): Boolean;
  var
    Offset: Integer;
  begin
    Offset := MaskOffsetToOffset(EditMask, MaskOffset);
    Result := not ((MaskOffset < Length(EditMask)) and
               (UpCase(EditMask[MaskOffset]) = UpCase(EditMask[MaskOffset+1]))) or
               (ByteType(EditText, Offset) = mbTrailByte) or
               (ByteType(EditText, Offset+1) = mbLeadByte);
  end;

begin
  Result := True;
  CType := MaskGetCharType(EditMask, MaskOffset);
  if CType in [mcLiteral, mcIntlLiteral] then
    NewChar := MaskIntlLiteralToChar(EditMask[MaskOffset])
  else
  begin
    Dir := MaskGetCurrentDirectives(EditMask, MaskOffset);
    case EditMask[MaskOffset] of
      mMskNumeric, mMskNumericOpt:
        begin
          if not ((NewChar >= '0') and (NewChar <= '9')) then
            Result := False;
        end;
      mMskNumSymOpt:
        begin
          if not (((NewChar >= '0') and (NewChar <= '9')) or
                 (NewChar = ' ') or(NewChar = '+') or(NewChar = '-')) then
            Result := False;
        end;
      mMskAscii, mMskAsciiOpt:
        begin
          {if (NewChar in LeadBytes) and TestChar(NewChar) then
          begin
            Result := False;
            Exit;
          end;  }
          if IsCharAlpha(NewChar) then
          begin
            Str := ' ';
            Str[1] := NewChar;
            if (mdUpperCase in Dir)  then
              Str := AnsiUpperCase(Str)
            else if mdLowerCase in Dir then
              Str := AnsiLowerCase(Str);
            NewChar := Str[1];
          end;
        end;
      mMskAlpha, mMskAlphaOpt, mMskAlphaNum, mMskAlphaNumOpt:
        begin
          {if (NewChar in LeadBytes) then
          begin
            if TestChar(NewChar) then
              Result := False;
            Exit;
          end; }
          Str := ' ';
          Str[1] := NewChar;
          
          if not IsCharAlpha(NewChar) then
          begin
            Result := False;
            if ((EditMask[MaskOffset] = mMskAlphaNum) or
                (EditMask[MaskOffset] = mMskAlphaNumOpt)) and
                (IsCharAlphaNumeric(NewChar)) then
              Result := True;
          end
          else if mdUpperCase in Dir then
            Str := AnsiUpperCase(Str)
          else if mdLowerCase in Dir then
            Str := AnsiLowerCase(Str);
          NewChar := Str[1];
        end;
    end;
  end;
end;

function TCustomMaskEdit.Validate(const Value: string; var Pos: Integer): Boolean;
var
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
begin
  Result := True;
  Offset := 1;
  for MaskOffset := 1 to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);

    if CType in [mcLiteral, mcIntlLiteral, mcMaskOpt] then
      Inc(Offset)
    else if (CType = mcMask) and (Value <> '') then
    begin
      if (Value [Offset] = FMaskBlank) or
        ((Value [Offset] = ' ') and (EditMask[MaskOffset] <> mMskAscii)) then
      begin
        Result := False;
        Pos := Offset - 1;
        Exit;
      end;
      Inc(Offset);
    end;
  end;
end;

function TCustomMaskEdit.DeleteSelection(var Value: string; Offset: Integer;
  Len: Integer): Boolean;
var
  EndDel: Integer;
  StrOffset, MaskOffset, Temp: Integer;
  CType: TMaskCharType;
begin
  Result := True;
  if Len = 0 then Exit;

  StrOffset := Offset;// + 1;
  EndDel := StrOffset + Len;
  Temp := OffsetToMaskOffset(EditMask, Offset)-1;
  if Temp < 0 then  Exit;
  
  for MaskOffset := Temp to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral] then begin
      Inc(StrOffset);
    end else if CType in [mcMask, mcMaskOpt] then
    begin
      Value[StrOffset] := FMaskBlank;
      Inc(StrOffset);
    end;
    if StrOffset >= EndDel then Break;
  end;
end;

function TCustomMaskEdit.InputString(var Value: string; const NewValue: string;
  Offset: Integer): Integer;
var
  NewOffset, MaskOffset, Temp: Integer;
  CType: TMaskCharType;
  NewVal: string;
  NewChar: char;
begin
  Result := Offset;
  if NewValue = '' then Exit;
  NewOffset := 1;
  NewVal := NewValue;
  Temp := OffsetToMaskOffset(EditMask, Offset);
  if Temp < 0 then  Exit;
  MaskOffset := Temp;
  While MaskOffset <= Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral, mcMask, mcMaskOpt] then
    begin
      NewChar := NewVal[NewOffset];
      if not (DoInputChar(NewChar, MaskOffset)) then
      begin
        {if (NewChar in LeadBytes) then
          NewVal[NewOffset + 1] := FMaskBlank;  }
        NewChar := FMaskBlank;
      end;
        { if pasted text does not contain a literal in the right place,
          insert one }
      if not ((CType in [mcLiteral, mcIntlLiteral]) and
        (NewChar <> NewVal[NewOffset])) then
      begin
        NewVal[NewOffset] := NewChar;
        {if (NewChar in LeadBytes) then
        begin
          Inc(NewOffset);
          Inc(MaskOffset);
        end;     }
      end
      else
        NewVal := Copy(NewVal, 1, NewOffset-1) + NewChar +
          Copy(NewVal, NewOffset, Length (NewVal));
      Inc(NewOffset);
    end;
    if (NewOffset + Offset) > FMaxChars then Break;
    if (NewOffset) > Length(NewVal) then Break;
    Inc(MaskOffset);
  end;

  if (Offset + Length(NewVal)) < FMaxChars then
  begin
    if ByteType(Value, OffSet + Length(NewVal) + 1) = mbTrailByte then
    begin
      NewVal := NewVal + FMaskBlank;
      Inc(NewOffset);
    end;
    Value := Copy(Value, 1, Offset) + NewVal +
      Copy(Value, OffSet + Length(NewVal) + 1,
        FMaxChars -(Offset + Length(NewVal)));
  end
  else
  begin
    Temp := Offset;
    if (ByteType(NewVal, FMaxChars - Offset) = mbLeadByte) then
      Inc(Temp);
    Value := Copy(Value, 1, Offset) +
             Copy(NewVal, 1, FMaxChars - Temp);
  end;
  Result := NewOffset + Offset - 1;
end;

function TCustomMaskEdit.FindLiteralChar(MaskOffset: Integer; InChar: Char): Integer;
var
  CType: TMaskCharType;
  LitChar: Char;
begin
  Result := -1;
  while MaskOffset < Length(EditMask) do
  begin
    Inc(MaskOffset);
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral] then
    begin
      LitChar := EditMask[MaskOffset];
      if CType = mcIntlLiteral then
        LitChar := MaskIntlLiteralToChar(LitChar);
      if LitChar = InChar then
        Result := MaskOffset;
      Exit;
    end;
  end;
end;

{***************************************************************************
Edited 4/16/2003 to correct many bugs related to storing or not storing mask
and the backspace / delete keys - Tony
}

end.
