{
 /***************************************************************************
                                 maskedit.pp
                                 -----------
                           Component Library Code

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LMessages, Clipbrd, LCLType;

const
  { Mask Type }
  cMask_SpecialChar   = '\'; // after this you can set an arbitrary char
  cMask_UpperCase     = '>'; // after this the chars is in upper case
  cMask_LowerCase     = '<'; // after this the chars is in lower case
  cMask_Letter        = 'L'; // only a letter but not necessary
  cMask_LetterFixed   = 'l'; // only a letter
  cMask_AllChars      = 'A'; // a char from space and #122 but not necessary
  cMask_AllCharsFixed = 'a'; // a char from space and #122
  cMask_Number        = '0'; // only a number but not necessary
  cMask_NumberFixed   = '9'; // only a number
  cMask_HourSeparator = ':'; // automatically put the hour separator char
  cMask_DateSeparator = '/'; // automatically but the date separator char
  cMask_SpaceOnly     = '_'; // automatically put a space

type
  { Type for mask (internal) }
  tMaskedType = (Char_Start,
                 Char_Number,
                 Char_NumberFixed,
                 Char_Letter,
                 Char_LetterFixed,
                 Char_LetterUpCase,
                 Char_LetterDownCase,
                 Char_LetterFixedUpCase,
                 Char_LetterFixedDownCase,
                 Char_All,
                 Char_AllFixed,
                 Char_AllUpCase,
                 Char_AllDownCase,
                 Char_AllFixedUpCase,
                 Char_AllFixedDownCase,
                 Char_Space,
                 Char_HourSeparator,
                 Char_DateSeparator,
                 Char_Stop);

  { Exception class }

  EDBEditError = class(Exception);

  { TCustomMaskEdit }
  
  TCustomMaskEdit = Class(TCustomEdit)
  private
    FPosition   : Integer;      // Current position
    FRealMask   : String;       // Real mask inserted
    FMask       : ShortString;  // Acrtual internal mask
    FMaxChars   : Integer;      // Max writeable chars
    FSpaceChar  : Char;         // Char for space (default '_')
    CurrentText : String;       // Current text

    procedure SetMask(Value : String);
    function  ClearChar(Position : Integer) : Char;
    procedure SetCursorPos;
    function  GetIsMasked : Boolean;
    procedure InsertChar(Ch : Char);
    procedure DeleteSelected(AlsoOnePosition : Boolean);
    procedure SetCharToPos;
    procedure DeleteChars(NextChar : Boolean);
    Function  GetText : String;
    Procedure SetText(Value : String);
    Function  CanInsertChar(Position : Integer; Var Ch : Char) : Boolean;
    procedure SetSpaceChar(Value : Char);
    Function  CharToMask(Ch : Char) : tMaskedType;
    Function  MaskToChar(Value : tMaskedType) : Char;
    Function  IsMaskChar(Ch : Char) : Boolean;
    Function  SearchDeletedText : Boolean;
    procedure SetEditText(const AValue: string);
  protected
    // messages
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure CMEnter(var Message: TLMessage); message CM_ENTER;
    procedure LMMButtonUp(var Message: TLMMButtonDown); message LM_MBUTTONUP;
    procedure LMPasteFromClip(var Message: TLMessage); message LM_PASTE;
    procedure LMCutToClip(var Message: TLMessage); message LM_CUT;
    procedure LMClearSel(var Message : TLMessage); message LM_CLEAR;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function EditCanModify: Boolean; virtual;
    function GetEditText: string; virtual;
    procedure Reset; virtual;
  public
    procedure   Clear;
    constructor Create(Aowner : TComponent); override;
    procedure   GetSel(var _SelStart: Integer; var _SelStop: Integer);
    procedure   SetSel(_SelStart: Integer; _SelStop: Integer);
    { Required methods }
    procedure   ValidateEdit; virtual;

    property EditMask  : string  read FRealMask   write SetMask;
    property isMasked  : Boolean read GetIsMasked;
    property Text      : string  read GetText     write SetText;
    property EditText  : string  read GetEditText write SetEditText;
    property SpaceChar : Char    read FSpaceChar  write SetSpaceChar;
  end;

  { TMaskEdit }
  
  TMaskEdit = class(TCustomMaskEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property EditMask;
    property isMasked;
    property Text;
    property SpaceChar;
  end;

procedure Register;

implementation

{ Component registration procedure }
procedure Register;
begin
  RegisterComponents('Additional',[TMaskEdit]);
end;

// Respond to Clear message
procedure TCustomMaskEdit.LMClearSel(var Message: TLMessage);
begin
  if Not SearchDeletedText then inherited;
end;

// Respond to Cut message
procedure TCustomMaskEdit.LMCutToClip(var Message: TLMessage);
begin
  if Not SearchDeletedText then inherited;
end;

// Search and init the deleted text
function TCustomMaskEdit.SearchDeletedText : Boolean;
var
  S             : String;
  NLose, I      : Integer;
  StartPosition : Integer;
  Ok            : Boolean;
begin
  // Number of char deleted
  NLose := Length(CurrentText)-Length(inherited Text);

  // Search part deleted
  Ok            := False;
  StartPosition := 1;
  While (Not Ok) And (StartPosition+NLose-1 <= Length(CurrentText)) Do
  begin
    S := CurrentText;
    Delete(S, StartPosition, NLose);
    Ok := (S = (Inherited Text));
    if Not Ok then Inc(StartPosition);
  end;

  // Found! Reinsert chars
  if Ok then
  begin
    for I := StartPosition to StartPosition+NLose-1 do
    begin
      CurrentText[I] := ClearChar(I);
    end;
    Inherited Text := CurrentText;
    SetCursorPos;
    Result := True;
  end
    else
      Result := False;
end;

procedure TCustomMaskEdit.SetEditText(const AValue: string);
begin
  Text := AValue;
end;

function TCustomMaskEdit.EditCanModify: Boolean;
begin
  Result := True;
end;

function TCustomMaskEdit.GetEditText: string;
begin
  Result := Text;
end;

procedure TCustomMaskEdit.Reset;
begin

end;

// Respond to Paste message
procedure TCustomMaskEdit.LMPasteFromClip(var Message: TLMessage);
Var
  NewText : String;
begin
  if (not IsMasked) or (ReadOnly) then
  begin
    inHerited;
    Exit;
  end;

  // Save the current text
  NewText := InHerited Text;

  // Put the stored text into the control
  InHerited Text := CurrentText;

  // Try to set the new text
  Text := NewText;
end;

Function TCustomMaskEdit.CanInsertChar(Position : Integer; Var Ch : Char) : Boolean;
Var
  Current : tMaskedType;
Begin
  Current := CharToMask(FMask[Position]);
  Result  := False;

  // If in UpCase convert the input char
  if (Current = Char_LetterUpCase     ) Or
     (Current = Char_LetterFixedUpCase) Or
     (Current = Char_AllUpCase        ) Or
     (Current = Char_AllFixedUpCase   )
     then
       Ch := UpCase(Ch);

  // If in LowerCase convert the input char
  if (Current = Char_LetterDownCase     ) Or
     (Current = Char_LetterFixedDownCase) Or
     (Current = Char_AllDownCase        ) Or
     (Current = Char_AllFixedDownCase   )
     then
       Ch := LowerCase(Ch);

  // Check the input (check the valid range)
  case Current Of
       Char_Number              : Result := Ch In ['0'..'9'];
       Char_NumberFixed         : Result := Ch In ['0'..'9'];
       Char_Letter              : Result := Ch In ['a'..'z', 'A'..'Z'];
       Char_LetterFixed         : Result := Ch In ['a'..'z', 'A'..'Z'];
       Char_LetterUpCase        : Result := Ch In ['A'..'Z'];
       Char_LetterDownCase      : Result := Ch In ['a'..'z'];
       Char_LetterFixedUpCase   : Result := Ch In ['A'..'Z'];
       Char_LetterFixedDownCase : Result := Ch In ['a'..'z'];
       Char_All                 : Result := True;
       Char_AllFixed            : Result := True;
       Char_AllUpCase           : Result := True;
       Char_AllDownCase         : Result := True;
       Char_AllFixedUpCase      : Result := True;
       Char_AllFixedDownCase    : Result := True;
       Char_Space               : Result := Ch in [' ', '_'];
       Char_HourSeparator       : Result := Ch in [TimeSeparator];
       Char_DateSeparator       : Result := Ch in [DateSeparator];
  end;
end;

// Insert a single char in position Fposition
procedure TCustomMaskEdit.InsertChar(Ch : Char);
Var
  Ok   : Boolean;
  I, C : Integer;
  S    : ShortString;
begin
  // Search the correct char writeable
  Ok := False;
  I  := 1;
  C  := 0;
  while (Not Ok) And (I <= Length(FMask)) do
  begin
    if IsMaskChar(FMask[I]) then
    begin
      Inc(C);
      Ok := (C = FPosition);
    end;

    if Not Ok then Inc(I);
  end;

  // found!
  if Ok then
    // Char is valid. Put the char into Text
    if CanInsertChar(I, Ch) then
       begin
         S    := Text;
         S[I] := Ch;
         Inherited Text := S;
         CurrentText := S;
         Inc(FPosition);
         SetCursorPos;
       end;
end;

// Respond to Mouse Button Up message
procedure TCustomMaskEdit.LMMButtonUp(var Message: TLMMButtonDown);
begin
  InHerited;

  if isMasked then
  begin
    FPosition := 0;
    SetCharToPos;
    SetCursorPos;
  end;
end;

// Respond to Enter message
procedure TCustomMaskEdit.CMEnter(var Message: TLMessage);
begin
  InHerited;

  if isMasked then
  begin
    FPosition := 0;
    SetCharToPos;
    SetCursorPos;
  end;
end;

// Create object
constructor TCustomMaskEdit.Create(Aowner : TComponent);
begin
  Inherited Create(Aowner);
  FPosition   := 1;
  FRealMask   := '';
  FMask       := '';
  FMaxChars   := 0;
  FSpaceChar  := '_';
  CurrentText := Inherited Text;
end;

// Set the current Space Char
procedure TCustomMaskEdit.SetSpaceChar(Value : Char);
Var
  S      : ShortString;
  I      : Integer;
Begin
  if (Value <> FSpaceChar) And (Not IsMaskChar(Value)) then
  begin
    FSpaceChar := Value;

    if isMasked then
    begin
      S := Text;
      for I := 1 to Length(S) do
          if (CharToMask(FMask[I]) = Char_Space)
          then
            S[I] := FSpaceChar;
      Inherited Text := S;
      CurrentText := S;
    end;
  end;
End;

// Get the current selection
procedure TCustomMaskEdit.GetSel(var _SelStart: Integer; var _SelStop: Integer);
begin
  _SelStart:= GetSelStart();
  _SelStop:= _SelStart + GetSelLength();
end;

// Set the current selection
procedure TCustomMaskEdit.SetSel(_SelStart: Integer; _SelStop: Integer);
begin
  SetSelStart(_SelStart);
  SetSelLength(_SelStop - _SelStart);
end;

procedure TCustomMaskEdit.ValidateEdit;
begin

end;

// Respond to Text Changed message
procedure TCustomMaskEdit.CMTextChanged(var Message: TLMessage);
begin
  InHerited;

  if Text = '' then Clear;
end;

// Single key down procedure
procedure TCustomMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // Not masked or shift pressed -> old procedure
  if (Not isMasked) Or (ReadOnly) Or (ssCTRL in Shift) then
  begin
    InHerited KeyDown(Key, Shift);
    Exit;
  end;

  // Cursor movement
  if (Key = VK_LEFT) or (Key = VK_RIGHT) then
  begin
    if Not (ssShift in Shift) then
    begin
      SetCharToPos;
      if Key = VK_LEFT then Dec(FPosition)
                       else Inc(FPosition);
      Key := 0;
      SetCursorPos;
    end;
    Exit;
  end;

  // Cursor position to end or to begin
  if (Key = VK_HOME) or (Key = VK_END) then
  begin
    if Key = VK_HOME then FPosition := 1
                     else FPosition := FMaxChars;
    SetCursorPos;
    Key := 0;
    Exit;
  end;

  // Cursor Up/Down -> not valid
  if (Key = VK_UP) or (Key = VK_DOWN) then
  begin
    Key := 0;
    Exit;
  end;

  // Delete a single char
  if (Key = VK_BACK) or (Key = VK_DELETE) then
  begin
    DeleteChars(Key = VK_DELETE);
    Key := 0;
    Exit;
  end;

  // Insert a char
  if (Key In [32..122]) then
  begin
    DeleteSelected(False);
    SetCharToPos;
    InsertChar(Char(Lo(Key)));
    Key:= 0;
  end;
  
end;

// Delete a single char from position
procedure TCustomMaskEdit.DeleteChars(NextChar : Boolean);
Var
  SelectionStart, SelectionStop : Integer;
begin
  GetSel(SelectionStart, SelectionStop);

  // Must delete the next char from actual position
  if Not NextChar then
  begin
    Dec(FPosition);
    SetSel(SelectionStart-1, SelectionStop-1);
    SetCursorPos;
  end;

  DeleteSelected(True);
end;

// Return if mask is selected
function TCustomMaskEdit.GetIsMasked : Boolean;
begin
  Result := (FMask <> '') And (FMaxChars > 0);
end;

// Set the cursor position
procedure TCustomMaskEdit.SetCursorPos;
Var
  Ok   : Boolean;
  I, C : Integer;
begin
  if FPosition < 1         then FPosition := 1;
  if FPosition > FMaxChars then FPosition := FMaxChars;

  Ok := False;
  I  := 1;
  C  := 0;
  while (Not Ok) And (I <= Length(FMask)) do
  begin
    if IsMaskChar(FMask[I]) then
    begin
      Inc(C);
      Ok := (C = FPosition);
    end;

    if Not Ok then Inc(I);
  end;

  if Ok then SetSel(I-1, I-1);
end;

// Clear (virtually) a single char in position Position
function TCustomMaskEdit.ClearChar(Position : Integer) : Char;
begin
  Result := FMask[Position];

  case CharToMask(FMask[Position]) Of
       Char_Number              : Result := FSpaceChar;
       Char_NumberFixed         : Result := '0';
       Char_Letter              : Result := FSpaceChar;
       Char_LetterFixed         : Result := 'a';
       Char_LetterUpCase        : Result := FSpaceChar;
       Char_LetterDownCase      : Result := FSpaceChar;
       Char_LetterFixedUpCase   : Result := 'A';
       Char_LetterFixedDownCase : Result := 'a';
       Char_All                 : Result := FSpaceChar;
       Char_AllFixed            : Result := '0';
       Char_AllUpCase           : Result := FSpaceChar;
       Char_AllDownCase         : Result := FSpaceChar;
       Char_AllFixedUpCase      : Result := '0';
       Char_AllFixedDownCase    : Result := '0';
       Char_Space               : Result := FSpaceChar;
       Char_HourSeparator       : Result := DecimalSeparator;
       Char_DateSeparator       : Result := DateSeparator;
  end;
end;

// Clear all string
procedure TCustomMaskEdit.Clear;
Var
  S : ShortString;
  I : Integer;
begin
  Inherited Clear;

  if isMasked then
  begin
    S  := '';
    for I := 1 To Length(FMask) do S := S + ClearChar(I);
    inherited Text := S;
    CurrentText := S;
    FPosition := 0;
    SetCursorPos;
  end;
end;

// Prepare the real Mask
procedure TCustomMaskEdit.SetMask(Value : String);
Var
  S            : ShortString;
  I            : Integer;
  InUp, InDown : Boolean;
  Special      : Boolean;
begin
  if FRealMask <> Value then
  begin
    // init
    FMaxChars := 0;
    FRealMask := Value;
    FMask     := '';

    // Init: No UpCase, No LowerCase, No Special Char
    InUp      := False;
    InDown    := False;
    Special   := False;

    // Get Actual Mask
    S         := FRealMask;
    for I := 1 To Length(S) do
    begin
      // Must insert a special char
      if Special then
      begin
        FMask   := FMask + S[I];
        Special := False;
      end
        else
      begin
        Inc(FMaxChars);

        // Check the char to insert
        case S[I] Of
             cMask_SpecialChar: Special := True;

             cMask_UpperCase: begin
               InUp    := True;
               if InDown then
               begin
                 InUp   := False;
                 InDown := False;
               end;
             end;

             cMask_LowerCase: begin
                InDown  := True;
                if InUp then
                begin
                  InUp   := False;
                  InDown := False;
                end;
             end;

             cMask_Letter: begin
                if InUp
                then
                  FMask := FMask + MaskToChar(Char_LetterUpCase)
                else
                  if InDown
                  then
                    FMask := FMask + MaskToChar(Char_LetterDownCase)
                  else
                    FMask := FMask + MaskToChar(Char_Letter)
             end;

             cMask_LetterFixed: begin
                if InUp
                then
                  FMask := FMask + MaskToChar(Char_LetterFixedUpCase)
                else
                  if InDown
                  then
                    FMask := FMask + MaskToChar(Char_LetterFixedDownCase)
                  else
                    FMask := FMask + MaskToChar(Char_LetterFixed)
             end;

             cMask_AllChars: begin
                if InUp
                then
                  FMask := FMask + MaskToChar(Char_AllUpCase)
                else
                  if InDown
                  then
                    FMask := FMask + MaskToChar(Char_AllDownCase)
                  else
                    FMask := FMask + MaskToChar(Char_All)
             end;

             cMask_AllCharsFixed: begin
                if InUp
                then
                  FMask := FMask + MaskToChar(Char_AllFixedUpCase)
                else
                  if InDown
                  then
                    FMask := FMask + MaskToChar(Char_AllFixedDownCase)
                  else
                    FMask := FMask + MaskToChar(Char_AllFixed)
             end;

             cMask_Number: FMask := FMask + MaskToChar(Char_Number);

             cMask_NumberFixed: FMask := FMask + MaskToChar(Char_NumberFixed);

             cMask_HourSeparator: FMask := FMask + MaskToChar(Char_HourSeparator);

             cMask_DateSeparator: FMask := FMask + MaskToChar(Char_DateSeparator);

             cMask_SpaceOnly: FMask := FMask + MaskToChar(Char_Space);

             else begin
               FMask := FMask + S[I];
               Dec(FMaxChars);
             end;
        end;
      end;
    end;

    Clear;
  end;
end;

// Trasform a position from Text in a real position
procedure TCustomMaskEdit.SetCharToPos;
Var
  SelectionStart, SelectionStop : Integer;
  I, Last, A, B                 : Integer;
begin
  // Search for the position of insertion valid nearest the cursor
  GetSel(SelectionStart, SelectionStop);

  if IsMaskChar(FMask[SelectionStart+1]) then
  begin
    Last := SelectionStart+1;
  end
    else
  begin
    I := SelectionStart+1;
    A := -999;
    while (A < 0) And (I <= Length(FMask)) do
    begin
      if IsMaskChar(FMask[I]) then A := I;
      Inc(I);
    end;

    I := SelectionStart-1;
    B := -999;
    while (B < 0) And (I >= 1) do
    begin
      if IsMaskChar(FMask[I]) then B := I;
      Dec(I);
    end;

    Last := B;
    if (A-SelectionStart) <= (SelectionStart-B) then Last := A;
  end;

  A := 0;
  for I := 1 To Last do
      if IsMaskChar(FMask[I]) then Inc(A);

  FPosition := A;
  SetCursorPos;
end;

// Delete selected chars
procedure TCustomMaskEdit.DeleteSelected(AlsoOnePosition : Boolean);
Var
  SelectionStart, SelectionStop, I : Integer;
  S                                : ShortString;
begin
  GetSel(SelectionStart, SelectionStop);
  if (SelectionStop > SelectionStart                      ) Or
     ((SelectionStop = SelectionStart) And AlsoOnePosition) then
  begin
    S := Text;
    for I := SelectionStart+1 To SelectionStop+1 do S[I] := ClearChar(I);
    inherited Text := S;
    CurrentText := S;
  end;
  SetSel(SelectionStart, SelectionStart);
end;

// Get the actual Text
Function TCustomMaskEdit.GetText : String;
Begin
  Result := InHerited Text;
End;

// Set the actual Text
Procedure TCustomMaskEdit.SetText(Value : String);
Var
  Old, S, S1 : ShortString;
  I          : Integer;
  Ok         : Boolean;
Begin
  if isMasked Then
  begin
    if Value <> '' then
    begin
      Old := Text;
      S   := Value;
      S1  := '';
      I   := 1;
      Ok  := (Length(S) = Length(FMask));
      while (I <= Length(S)) and (Ok) do
      begin
        if Not IsMaskChar(FMask[I])
        then
          Ok := (S[I] = FMask[I])
        else
          Ok := CanInsertChar(I, S[I]);

        if OK then S1 := S1 + S[I];

        Inc(I);
      end;

      if not Ok then
      begin
        inherited Text := Old;
        CurrentText := Old;
        raise EDBEditError.Create('Error setting text...!');
      end
        else
      begin
        inherited Text := S1;
        CurrentText := S1;
      end;
    end
      else
        Clear;
  end
    else
  begin
    inherited Text := Value;
    CurrentText := Value;
  end;
End;

// Trasform a single char in a MaskType
Function TCustomMaskEdit.CharToMask(Ch : Char) : tMaskedType;
Begin
  Result := Char_Start;
  if (Ord(Ch) > Ord(Char_Start)) and
     (Ord(Ch) < Ord(Char_Stop) )
     then
       Result := tMaskedType(Ord(Ch));
End;

// Trasform a single MaskType into a char
Function TCustomMaskEdit.MaskToChar(Value : tMaskedType) : Char;
Begin
  Result := Char(Ord(Value));
End;

// Return if the char passed is a valid MaskType char
Function TCustomMaskEdit.IsMaskChar(Ch : Char) : Boolean;
Begin
  Result := (CharToMask(Ch) <> Char_Start);
End;

end.

