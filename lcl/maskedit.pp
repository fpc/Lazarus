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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}




{
ToDo List:
- Make the EDBEditError errormessage (SMaskEditNoMatch) a Resource string in LCLStrconsts.pas
- Better handling of cut/clear/paste messages

Bugs:
- If you place a TMaskEdit on a form and at designtime set the mask and leave
  the text in the control "invalid" (as in: will not validate) and the TMaskEdit
  is the ActiveControl of the form, then before the form is displayed an exception will
  be raised, because somehow DoExit is executed (which calls ValidateEdit)
  A bugreport on this behaviour is in Mantis: #0012877
- The Delphi helpt text says that a '_' in EditMask will insert a blank in the text.
  However all versions of Delphi up to D2010 treat it as a literal '_' (unless
  specified in the 3rd field of a multifield EditMask), so I rewrote parts to make it behave like
  that also.
  If, in the future, Delphi actually treats '_' as a blank, we'll re-implement it, for that
  purpose I did not remove the concerning code, but commented it out
- UTF8 support for maskcharacters C and c, probably needs major rewrite!!
  For now we disallow any UTF8 characters:
  - in KeyPress only Lower ASCII is handled
  - in SetText, SetEditText, PasteFromClipboard all UTF8 characters in the given string
    are replaced with '?' in the UTF8ToAscii() function
  The reason for this is that an UTF8Char is > 1 byte in lenght, and it will seriously
  screw up the aritmatic of cursor placing, where to put mask-literals etc.
  (Alowing it will result in floating point erros when you type/delete in the control)


Different behaviour than Delphi, but by design (October 2009, BB)
 - In SetText in Delphi, when MasNoSave is in EditMask, it is possible to set text longer then the mask
   allowes for. I disallowed that, because it corrupts internal cursor placement etc.
 - SetEditText is not Delphi compatible. Delphi allows setting any text in the control, leaving the control
   in an unrecoverable state, where it is impossible to leave the control because the text can never be validated
   (too short, too long, overwritten maskliterals). The app wil crash as a result of this.
   I have decided to disallow this:
   - EditText is truncated, or padded with ClearChar if necessary so that Length(EditText) = Length(FMask)
   - Restore all MaskLiterals in the text
}

unit MaskEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LMessages, Clipbrd, LCLType, LCLProc;

const
  { Mask Type }
  cMask_SpecialChar   = '\'; // after this you can set an arbitrary char
  cMask_UpperCase     = '>'; // after this the chars is in upper case
  cMask_LowerCase     = '<'; // after this the chars is in lower case
  cMask_Letter        = 'l'; // only a letter but not necessary
  cMask_LetterFixed   = 'L'; // only a letter
  cMask_AlphaNum      = 'a'; // a char from space and #122 but not necessary
  cMask_AlphaNumFixed = 'A'; // a char from space and #122
  cMask_AllChars      = 'c'; // any char #32 - #255 but not necessary (needs fixing for UTF8 characters!!)
  cMask_AllCharsFixed = 'C'; // any char #32 - #255 (needs fixing for UTF8 characters!!)
  cMask_Number        = '9'; // only a number but not necessary
  cMask_NumberFixed   = '0'; // only a number
  cMask_NumberPlusMin = '#'; // only a number or + or -, but not necessary
  cMask_HourSeparator = ':'; // automatically put the hour separator char
  cMask_DateSeparator = '/'; // automatically but the date separator char
{ cMask_SpaceOnly     = '_'; // automatically put a space          //not Delphi compatible        }
  cMask_NoLeadingBlanks = '!'; //Trim leading blanks, otherwise trim trailing blanks from the data

  {Delphi compatibility: user can change these at runtime}
  DefaultBlank: Char = '_';
  MaskFieldSeparator: Char = ';';
  MaskNoSave: Char = '0';

type
  { Type for mask (internal) }
  tMaskedType = (Char_Start,
                 Char_Number,
                 Char_NumberFixed,
                 Char_NumberPlusMin,
                 Char_Letter,
                 Char_LetterFixed,
                 Char_LetterUpCase,
                 Char_LetterDownCase,
                 Char_LetterFixedUpCase,
                 Char_LetterFixedDownCase,
                 Char_AlphaNum,
                 Char_AlphaNumFixed,
                 Char_AlphaNumUpCase,
                 Char_AlphaNumDownCase,
                 Char_AlphaNumFixedUpCase,
                 Char_AlphaNumFixedDownCase,
                 Char_All,
                 Char_AllFixed,
                 Char_AllUpCase,
                 Char_AllDownCase,
                 Char_AllFixedUpCase,
                 Char_AllFixedDownCase,
                {Char_Space,                 //not Delphi compatible, see notes above  }
                 Char_HourSeparator,
                 Char_DateSeparator,
                 Char_Stop);


  { Exception class }
type
  EDBEditError = class(Exception);

const
  SMaskEditNoMatch = 'The current text does not match the specified mask.';

type
  TMaskeditTrimType = (metTrimLeft, metTrimRight);
  { TCustomMaskEdit }



{ ***********************************************************************************************

 Please leave in this note until it no longer applies!

 FOR ANYONE WHO CARES TO FIX/ENHANCE THIS CODE:

 Since we want total control over anything that is done to the text in the control
 we have to take into consideration the fact that currently we cannot prevent
 cutting/pasting/clearing or dragging selected text in the control, these are handled by the OS
 and text is changed before we can prevent it.
 Not all widgetsets currently handle the messages for cut/paste/clear. Actually we would
 like to have a LM_BEFORE_PASTE (etc.) message...
 If we allow the OS to cut/clear/paste etc. a situation can occur where mask-literals in the
 control are changed with random chars (and cannot be undone) or text is shorter or larger than
 the editmask calls for, which again cannot be undone.


 So, as a horrible hack I decided  to only allow changing of the text if we coded
 this change ourself. This is done by setting the FChangeAllowed field to True before any
 write action (in SetInherited Text() ).
 We try to intercept the messages for cut/paste/copy/clear and perform the appropriate
 actions instead.
 If this fails, then in TextChanged we check and will see that FChangeAllowed = False
 and we will undo the changes made.

 To make this undo possible it is necessary to set FCurrentText every time you set
 the text in the control!
 This is achieved in SetInheritedText() only, so please note:
 !! It is unsafe to make changes to inherited Text unless done so via SetInheritedText() !!!

 (Bart Broersma, januari 2009)

 ************************************************************************************************ }


  TCustomMaskEdit = Class(TCustomEdit)
  private
    FRealMask     : String;            // Real mask inserted
    FMask         : ShortString;       // Acrtual internal mask
    FMaskSave     : Boolean;           // Save mask as part of the data
    FTrimType     : TMaskEditTrimType; // Trim leading or trailing spaces in GetText
    FSpaceChar    : Char;              // Char for space (default '_')
    FCurrentText  : String;            // FCurrentText is our backup. See notes above!
    FTextOnEnter  : String;            // Text when user enters the control, used for Reset()
    FCursorPos    : Integer;           // Current caret position
    FChangeAllowed: Boolean;           // We do not allow text changes by the OS (cut/clear via context menu)
    FInitialText  : String;            // Text set in the formdesigner (must not be handled by SetText)
    FInitialMask  : String;            // EditMask set in the formdesigner
    FValidationFailed: Boolean;        // Flag used in DoEnter
    FMaskIsPushed : Boolean;
    FPushedMask   : ShortString;

    procedure SetMask(Value : String);
    function  GetIsMasked : Boolean;
    procedure SetSpaceChar(Value : Char);

    procedure SetCursorPos;
    procedure SelectNextChar;
    procedure SelectPrevChar;
    procedure SelectFirstChar;
    procedure GotoEnd;
    procedure JumpToNextDot(Dot: Char);
    function  HasSelection: Boolean;
    function  HasExtSelection: Boolean;
    procedure GetSel(out _SelStart: Integer; out _SelStop: Integer);
    procedure SetSel(const _SelStart: Integer; _SelStop: Integer);

    Function  CharToMask(Ch : Char) : tMaskedType;
    Function  MaskToChar(Value : tMaskedType) : Char;
    Function  IsMaskChar(Ch : Char) : Boolean;
    Function  IsLiteral(Ch: Char): Boolean;
    function  TextIsValid(Value: String): Boolean;
    function  CharMatchesMask(const Ch: Char; const Position: Integer): Boolean;
    function  ClearChar(Position : Integer) : Char;

    procedure SetInheritedText(const Value: String); //See notes above!
    procedure InsertChar(Ch : Char);
    Function  CanInsertChar(Position : Integer; Var Ch : Char) : Boolean;
    procedure DeleteSelected;
    procedure DeleteChars(NextChar : Boolean);
  protected
    function DisableMask(const NewText: String): Boolean;
    function RestoreMask(const NewText: String): Boolean;

    Function  GetText : String;
    Procedure SetText(Value : String);
    function  GetEditText: string; virtual;
    procedure SetEditText(const AValue: string);
    procedure TextChanged; override;
    procedure SetCharCase(Value: TEditCharCase);
    function GetCharCase: TEditCharCase;
    procedure SetMaxLength(Value: Integer);
    function GetMaxLength: Integer;

    procedure Loaded; override;

    procedure LMPasteFromClip(var Message: TLMessage); message LM_PASTE;
    procedure LMCutToClip(var Message: TLMessage); message LM_CUT;
    procedure LMClearSel(var Message: TLMessage); message LM_CLEAR;

    function  EditCanModify: Boolean; virtual;
    procedure Reset; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure CheckCursor;
    property EditText: string read GetEditText write SetEditText;
    property IsMasked: Boolean read GetIsMasked;
    property SpaceChar: Char read FSpaceChar write SetSpaceChar;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
  public
    procedure CutToClipBoard; override;
    procedure PasteFromClipBoard; override;
    { Required methods }
    constructor Create(Aowner : TComponent); override;
    procedure Clear;
    procedure ValidateEdit; virtual;
    property EditMask: string read FRealMask write SetMask;
    property Text: string read GetText write SetText;
  end;

  { TMaskEdit }

  TMaskEdit = class(TCustomMaskEdit)
  public
    property IsMasked;
    property EditText;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
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
    property OnEditingDone;
    property OnEndDock;
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
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property EditMask;
    property Text;
    property SpaceChar;
  end;

procedure Register;

implementation


//Define this to prevent validation when the control looses focus
{ $DEFINE MASKEDIT_NOVALIDATEONEXIT}


{
// For debugging purposes only
const
  MaskCharToChar: array[tMaskedType] of Char = (#0, cMask_Number, cMask_NumberFixed, cMask_NumberPlusMin,
     cMask_Letter, cMask_LetterFixed, cMask_Letter, cMask_Letter, cMask_LetterFixed, cMask_LetterFixed,
     cMask_AlphaNum, cMask_AlphaNumFixed, cMask_AlphaNum, cMask_AlphaNum, cMask_AlphaNumFixed, cMask_AlphaNumFixed,
     cMask_AllChars, cMask_AllCharsFixed, cMask_AllChars, cMask_AllChars, cMask_AllCharsFixed, cMask_AllCharsFixed,
     (*cMask_SpaceOnly,*) cMask_HourSeparator, cMask_DateSeparator, #0);
}


const
  Period = '.';
  Comma = ',';


function UTF8ToASCII(Value: String): String;
//Replace all UTF8 chars (>#127) with '?'
//rules based on: http://en.wikipedia.org/wiki/UTF8
//In the case statement I differentiated between legal and illegal UTF8 byte sequences.
//Both are skipped, but in later versions we might do different actions on legal
//sequences, like replace "e with accent egu" with a plain e instead of a '?'
var
  b: byte;
  i,len: Integer;
begin
  Result := '';
  len := Length(Value);
  if len = 0 then exit;
  i := 1;
  while (i <= len) do
  begin
    b := Byte(Value[i]);
    if (b < $80) then
    begin
      Result := Result + Value[i];
    end
    else
    begin
      //UTF8Char, 2 or more bytes
      //replace with '?'
      Result := Result + '?';
      case b of
        $80..$BF: {just skip and continue after this byte}; //invalid first UTF8Char, only 2nd, 3rd, 4th can be in this range
        $C0, $C1:
        begin
          //illegal 2 byte sequence, just skip and continue after this sequence
          Inc(i);
          if (i > len) then exit; //invalid UTF8Char, and out of chars
        end;
        $C2..$DF:
        begin
          //legal 2 byte sequence, just skip and continue after this sequence
          Inc(i);
          if (i > len) then exit; //invalid UTF8Char, and out of chars
        end;
        $E0..$EF:
        begin
          //legal 3 byte sequence, just skip and continue after this sequence
          inc(i,2);
          if (i > len) then Exit; //invalid UTF8 char, and end of string
        end;
        $F0..$F4:
        begin
          //legal 4 byte sequence, just skip and continue after this sequence
          Inc(i,3);
          if (i > len) then Exit; //invalid UTF8 char, and end of string
        end;
        $F5..$F7:
        begin
          //illegal 4 byte sequence, just skip and continue after this sequence
          Inc(i,3);
          if (i > len) then Exit; //invalid UTF8 char, and end of string
        end;
        $F8..$FB:
        begin
          //illegal 5 byte sequence, just skip and continue after this sequence
          Inc(i,4);
          if (i > len) then Exit; //invalid UTF8 char, and end of string
        end;
        $FC..$FD:
        begin
          //illegal 6 byte sequence, just skip and continue after this sequence
          Inc(i,5);
          if (i > len) then Exit; //invalid UTF8 char, and end of string
        end;
        $FE..$FF:
        begin
          //illegal sequence: not defined by UTF8-specification
          Exit; //Absolutely illegal, stop processing the string.
        end;
      end; //case
    end;  //UTF8Char 2 or more bytes
    Inc(i);
  end; //while (i <= len)
end;




{ Component registration procedure }
procedure Register;
begin
  RegisterComponents('Additional',[TMaskEdit]);
end;


// Create object
constructor TCustomMaskEdit.Create(Aowner : TComponent);
begin
  Inherited Create(Aowner);
  FRealMask      := '';
  FMask          := '';
  FSpaceChar     := '_';
  FMaskSave      := True;
  FChangeAllowed := False;
  FTrimType      := metTrimRight;
  FCurrentText   := Inherited Text;
  FTextOnEnter   := Inherited Text;
  FInitialText   := '';
  FInitialMask   := '';
  FValidationFailed := False;
  FMaskIsPushed := False;
  FPushedMask := '';
end;


// Prepare the real internal Mask
procedure TCustomMaskEdit.SetMask(Value : String);
Var
  S            : ShortString;
  I            : Integer;
  InUp, InDown : Boolean;
  Special      : Boolean;
begin
  //Setting Mask while loading has unexpected and unwanted side-effects
  if (csLoading in ComponentState) then
  begin
    FInitialMask := Value;
    Exit;
  end;
  if FRealMask <> Value then
  begin
    FRealMask := Value;
    //Assume no FSpaceChar is defined in new mask, so first set it to DefaultBlank
    FSpaceChar := DefaultBlank;
    FValidationFailed := False;
    FMaskIsPushed := False;
    FPushedMask := '';
    {
      First see if Mask is multifield and if we can extract a value for
      FMaskSave and/or FSpaceChar
      If so, extract and remove from Value (so we know the remaining part of
      Value _IS_ the mask to be set)

      A value for FSpaceChar is only valid if also a value for FMaskSave is specified
      (as by Delphi specifications), so Mask must be at least 4 characters
      These must be the last 2 or 4 characters of EditMask (and there must not be
      an escape character in front!)
    }
    if (Length(Value) >= 4) and (Value[Length(Value)-1] = MaskFieldSeparator) and
       (Value[Length(Value)-3] = MaskFieldSeparator) and
       (Value[Length(Value)-2] <> cMask_SpecialChar) and
       //Length = 4 is OK (Value = ";1;_" for example), but if Length > 4 there must be no escape charater in front
       ((Length(Value) = 4) or ((Length(Value) > 4) and (Value[Length(Value)-4] <> cMask_SpecialChar))) then
    begin
      FSpaceChar := Value[Length(Value)];
      FMaskSave := (Value[Length(Value)-2] <> MaskNosave);
      System.Delete(Value,Length(Value)-3,4);
    end
    //If not both FMaskSave and FSPaceChar are specified, then see if only FMaskSave is specified
    else if (Length(Value) >= 2) and (Value[Length(Value)-1] = MaskFieldSeparator) and
            //Length = 2 is OK, but if Length > 2 there must be no escape charater in front
            ((Length(Value) = 2) or ((Length(Value) > 2) and (Value[Length(Value)-2] <> cMask_SpecialChar))) then
    begin
      FMaskSave := (Value[Length(Value)] <> MaskNoSave);
      //Remove this bit from Mask
      System.Delete(Value,Length(Value)-1,2);
    end;
    // Construct Actual Internal Mask
    // init
    FMask     := '';
    FTrimType := metTrimRight;
    // Init: No UpCase, No LowerCase, No Special Char
    InUp      := False;
    InDown    := False;
    Special   := False;
    S         := Value;
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
        // Check the char to insert
        case S[I] Of
             cMask_SpecialChar: Special := True;
             cMask_UpperCase: begin
               if (I > 1) and (S[I-1] = cMask_LowerCase) then
               begin// encountered <>, so no case checking after this
                 InUp := False;
                 InDown := False
               end else
               begin
                 InUp    := True;
                 InDown := False;
               end;
             end;

             cMask_LowerCase: begin
                InDown  := True;
                InUp := False;
                // <> is catched by next cMask_Uppercase
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

             cMask_AlphaNum: begin
                 if InUp
                 then
                   FMask := FMask + MaskToChar(Char_AlphaNumUpcase)
                 else
                   if InDown
                   then
                     FMask := FMask + MaskToChar(Char_AlphaNumDownCase)
                   else
                     FMask := FMask + MaskToChar(Char_AlphaNum)
             end;

             cMask_AlphaNumFixed: begin
                 if InUp
                 then
                   FMask := FMask + MaskToChar(Char_AlphaNumFixedUpcase)
                 else
                   if InDown
                   then
                     FMask := FMAsk + MaskToChar(Char_AlphaNumFixedDownCase)
                   else
                     FMask := FMask + MaskToChar(Char_AlphaNumFixed)
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

             cMask_NumberPlusMin: FMask := FMask + MaskToChar(Char_NumberPlusMin);

             cMask_HourSeparator: FMask := FMask + MaskToChar(Char_HourSeparator);

             cMask_DateSeparator: FMask := FMask + MaskToChar(Char_DateSeparator);

            {cMask_SpaceOnly: FMask := FMask + MaskToChar(Char_Space); //not Delphi compatible, see remarks above}

             cMask_NoLeadingBlanks:
             begin
               FTrimType := metTrimLeft;
             end;

             else begin
               FMask := FMask + S[I];
             end;
        end;
      end;
    end;
    if (Length(FMask) > 0) then SetCharCase(ecNormal);
    //SetMaxLegth must be before Clear, otherwise Clear uses old MaxLength value!
    SetMaxLength(Length(FMask));
    Clear;
    FTextOnEnter := inherited Text;
  end;
end;


// Return if mask is selected
function TCustomMaskEdit.GetIsMasked : Boolean;
begin
  Result := (FMask <> '');
end;


// Set the current Space Char
procedure TCustomMaskEdit.SetSpaceChar(Value : Char);
Var
  S      : ShortString;
  I      : Integer;
  OldValue: Char;
Begin
  if (Value <> FSpaceChar) And
  ((Not IsMaskChar(Value)) {or (CharToMask(Value) = Char_Space)}) then
  begin
    OldValue := FSpaceChar;
    FSpaceChar := Value;
    if isMasked then
    begin
      S := Inherited Text;
      for I := 1 to Length(S) do
      begin
        if (S[i] = OldValue) and (not IsLiteral(FMask[i])) then S[i] := FSpaceChar;
        //also update FTextOnEnter to reflect new SpaceChar!
        if (FTextOnEnter[i] = OldValue) and (not IsLiteral(FMask[i])) then FTextOnEnter[i] := FSpaceChar;
      end;
      FCurrentText := S;
      SetInheritedText(S);
      CheckCursor;
    end;
  end;
End;




// Set the cursor position and select the char in the control
procedure TCustomMaskEdit.SetCursorPos;
begin
  //no need to do this when in designmode, it actually looks silly if we do
  if not (csDesigning in ComponentState) then
  begin
    if FCursorPos < 0 then FCursorPos := 0
    else if FCursorPos  > Length(FMask) then FCursorPos := Length(FMask);
    if FCursorPos + 1 > Length(FMask) then
      SetSel(FCursorPos, FCursorPos)
    else
      SetSel(FCursorPos, FCursorPos + 1);
  end;
end;

//Move to next char, skip any mask-literals
procedure TCustomMaskEdit.SelectNextChar;
begin
  if (FCursorPos + 1) > Length(FMask) then Exit;
  Inc(FCursorPos);
  While (FCursorPos + 1 < Length(FMask)) and (IsLiteral(FMask[FCursorPos + 1])) do
  begin
    Inc(FCursorPos);
  end;
  if IsLiteral(FMask[FCursorPos + 1]) then Inc(FCursorPos);
  SetCursorPos;
end;

//Move to previous char, skip any mask-literals
procedure TCustomMaskEdit.SelectPrevChar;
var
  P: LongInt;
begin
  if FCursorPos = 0 then Exit;
  P := FCursorPos;
  Dec(FCursorPos);
  While (FCursorPos > 0) and IsLiteral(FMask[FCursorPos + 1]) do
  begin
    Dec(FCursorPos);
  end;
  if (FCursorPos = 0) and (P <> 0) and IsLiteral(FMask[FCursorPos + 1]) then FCursorPos := P;
  SetCursorPos;
end;


procedure TCustomMaskEdit.SelectFirstChar;
begin
  FCursorPos := 0;
  SetCursorPos;
end;

procedure TCustomMaskEdit.GotoEnd;
begin
  FCursorPos := Length(FMask);
  SetCursorPos;
end;

//Jump to next period or comma if possible, otherwise do nothing
procedure TCustomMaskEdit.JumpToNextDot(Dot: Char);
{
  Jumping occurs only if
  - Dot must be in the mask
  - There is a Dot after the current cursorposition
  - If the mask contains both periods and comma's, only the first one
    is jumpable
  - There is no literal after the next dot
  - The next dot is not the last character in the mask
}
var
  HasNextDot, HasCommaAndPeriod, CanJump: Boolean;
  P, P2: Integer;
begin
  if not (Dot in [Period, Comma]) then Exit;
  P := PosEx(Dot, FMask, FCursorPos + 1);
  HasNextDot := P > 0;
  If (Dot = Period) then
  begin
    P2 := Pos(Comma, FMask);
    HasCommaAndPeriod := HasNextDot and (P2 >0)
  end
  else
  begin
    P2 := Pos(Period, FMask);
    HasCommaAndPeriod := HasNextDot and (P2 >0);
  end;
  if HasCommaAndPeriod then
  begin
    //When mask has both period and comma only the first occurence is jumpable
    if P2 < P then HasNextDot := False;
  end;
  CanJump := HasNextDot and (P < Length(FMask)) and (not IsLiteral(FMask[P+1]));
  if CanJump then
  begin
    FCursorPos := P;
    SetCursorPos;
  end;
end;

function TCustomMaskEdit.HasSelection: Boolean;
begin
  Result := (GetSelLength() > 0);
end;

//Return True if Selection > 1, this influences the handling of Backspace
function TCustomMaskEdit.HasExtSelection: Boolean;
begin
  Result := (GetSelLength() > 1);
end;


// Get the current selection
procedure TCustomMaskEdit.GetSel(out _SelStart: Integer; out _SelStop: Integer);
begin
  _SelStart:= GetSelStart();
  _SelStop:= _SelStart + GetSelLength();
end;

// Set the current selection
procedure TCustomMaskEdit.SetSel(const _SelStart: Integer; _SelStop: Integer);
begin
  //in GTK if SelLength <> 0 then setting SelLength also changes SelStart
  SetSelLength(0);
  SetSelStart(_SelStart);
  SetSelLength(_SelStop - _SelStart);
end;


// Transform a single char in a MaskType
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


//Return if the char passed is a literal (so it cannot be altered)
function TCustomMaskEdit.IsLiteral(Ch: Char): Boolean;
begin
  Result := (not IsMaskChar(Ch)) or
    (IsMaskChar(Ch) and (CharToMask(Ch) in [Char_HourSeparator, Char_DateSeparator{, Char_Space}]))
end;


//Return if Value matches the EditMask
function TCustomMaskEdit.TextIsValid(Value: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (Length(Value) <> Length(FMask)) then
  begin
    //DebugLn('  Length(Value) = ',DbgS(Length(Value)),' Length(FMask) = ',DbgS(Length(FMask)));
    Exit; //Actually should never happen??
  end;
  for i := 1 to Length(FMask) do
  begin
    if not CharMatchesMask(Value[i], i) then Exit;
  end;
  Result := True;
end;


function TCustomMaskEdit.CharMatchesMask(const Ch: Char; const Position: Integer): Boolean;
var
  Current: tMaskedType;
  Ok: Boolean;
begin
  Result := False;
  if (Position < 1) or (Position > Length(FMask)) then Exit;
  Current := CharToMask(FMask[Position]);
  case Current Of
    Char_Number              : OK := Ch In ['0'..'9',#32];
    Char_NumberFixed         : OK := Ch In ['0'..'9'];
    Char_NumberPlusMin       : OK := Ch in ['0'..'9','+','-',#32];
    Char_Letter              : OK := Ch In ['a'..'z', 'A'..'Z',#32];
    Char_LetterFixed         : OK := Ch In ['a'..'z', 'A'..'Z'];
    Char_LetterUpCase        : OK := Ch In ['A'..'Z',#32];
    Char_LetterDownCase      : OK := Ch In ['a'..'z',#32];
    Char_LetterFixedUpCase   : OK := Ch In ['A'..'Z'];
    Char_LetterFixedDownCase : OK := Ch In ['a'..'z'];
    Char_AlphaNum            : OK := Ch in ['a'..'z', 'A'..'Z', '0'..'9',#32];
    Char_AlphaNumFixed       : OK := Ch in ['a'..'z', 'A'..'Z', '0'..'9'];
    Char_AlphaNumUpCase      : OK := Ch in ['A'..'Z', '0'..'9',#32];
    Char_AlphaNumDownCase    : OK := Ch in ['a'..'z', '0'..'9',#32];
    Char_AlphaNumFixedUpCase : OK := Ch in ['A'..'Z', '0'..'9'];
    Char_AlphaNumFixedDowncase:OK := Ch in ['a'..'z', '0'..'9'];
    //ToDo: make this UTF8 compatible, for now
    //limit this to lower ASCII set
    Char_All                 : OK := Ch in [#32..#126]; //True;
    Char_AllFixed            : OK := Ch in [#32..#126]; //True;
    Char_AllUpCase           : OK := Ch in [#32..#126]; //True;
    Char_AllDownCase         : OK := Ch in [#32..#126]; //True;
    Char_AllFixedUpCase      : OK := Ch in [#32..#126]; //True;
    Char_AllFixedDownCase    : OK := Ch in [#32..#126]; //True;
   {Char_Space               : OK := Ch in [' ', '_'];  //not Delphi compatible, see notes above}
    Char_HourSeparator       : OK := Ch in [DefaultFormatSettings.TimeSeparator];
    Char_DateSeparator       : OK := Ch in [DefaultFormatSettings.DateSeparator];
    else//it's a literal
    begin
      OK := (Ch = FMask[Position]);
    end;
  end;//case
  //DebugLn('Position = ',DbgS(Position),' Current = ',MaskCharToChar[Current],' Ch = "',Ch,'" Ok = ',DbgS(Ok));
  Result := Ok;
end;


//Set text in the control with FChangeAllowed flag set appropriately
procedure TCustomMaskEdit.SetInheritedText(const Value: String);
begin
  if (Value <> Inherited Text) then
  begin
    FChangeAllowed := True;
    FCurrentText := Value;
    //protect resetting FChangeAllowed := False against unhandled exceptions in user's
    //OnChange, otherwise risk leaving the control in an "unsafe" state regarding text changes
    try
      Inherited Text := Value;
    finally
      FChangeAllowed := False;
    end;//finally
  end;
end;

// Save current mask, then disable mask
// This gives developers the possibility to set any text in the control _without_ messing up the control
// Wether or not the function succeeds: NewText will be set as the new text of the control
// No need to save FMaskSave and FTrimtype, they are only set in SetMask, which sets MaskIsPushed := False
function TCustomMaskEdit.DisableMask(const NewText: String): Boolean;
begin
  if IsMasked and (not FMaskIsPushed) then
  begin
    FPushedMask := FMask;
    FMaskIsPushed := True;
    FMask := '';
    SetMaxLength(0);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
  Text := NewText;
end;

// Restore a saved mask
function TCustomMaskEdit.RestoreMask(const NewText: String): Boolean;
begin
  if FMaskIsPushed and (not IsMasked) then
  begin
    FMaskIsPushed := False;
    SetCharCase(ecNormal);
    FMask := FPushedMask;
    FPushedMask := '';
    SetMaxLength(Length(FMask));
    FTextOnEnter := inherited Text;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
  Text := NewText;
end;


// Clear (virtually) a single char in position Position
function TCustomMaskEdit.ClearChar(Position : Integer) : Char;
begin
  Result := FMask[Position];
  //For Delphi compatibilty, only literals remain, all others will be blanked
  case CharToMask(FMask[Position]) Of
       Char_Number              : Result := FSpaceChar;
       Char_NumberFixed         : Result := FSpaceChar; //'0';
       Char_NumberPlusMin       : Result := FSpaceChar;
       Char_Letter              : Result := FSpaceChar;
       Char_LetterFixed         : Result := FSpaceChar; //'a';
       Char_LetterUpCase        : Result := FSpaceChar;
       Char_LetterDownCase      : Result := FSpaceChar;
       Char_LetterFixedUpCase   : Result := FSpaceChar; //'A';
       Char_LetterFixedDownCase : Result := FSpaceChar; //'a';
       Char_AlphaNum            : Result := FSpaceChar;
       Char_AlphaNumFixed       : Result := FSpaceChar;
       Char_AlphaNumUpCase      : Result := FSpaceChar;
       Char_AlphaNumDownCase    : Result := FSpaceChar;
       Char_AlphaNumFixedUpcase : Result := FSpaceChar;
       Char_AlphaNuMFixedDownCase: Result := FSpaceChar;
       Char_All                 : Result := FSpaceChar;
       Char_AllFixed            : Result := FSpaceChar; //'0';
       Char_AllUpCase           : Result := FSpaceChar;
       Char_AllDownCase         : Result := FSpaceChar;
       Char_AllFixedUpCase      : Result := FSpaceChar; //'0';
       Char_AllFixedDownCase    : Result := FSpaceChar; //'0';
       {Char_Space               : Result := #32; //FSpaceChar?; //not Delphi compatible, see notes above}
       Char_HourSeparator       : Result := DefaultFormatSettings.TimeSeparator;
       Char_DateSeparator       : Result := DefaultFormatSettings.DateSeparator;
  end;
end;



//Insert a single char at the current position of the cursor
procedure TCustomMaskEdit.InsertChar(Ch : Char);
Var
  S    : ShortString;
  i, SelectionStart, SelectionStop: Integer;
begin
  if CanInsertChar(FCursorPos + 1, Ch) then
  begin
    S := Inherited Text;
    if HasSelection then
    begin
      //replace slection with blank chars
      //don't do this via DeleteChars(True), since it will do an unneccesary
      //update of the control and 2 TextChanged's are triggerd for every char we enter
      GetSel(SelectionStart, SelectionStop);
      for i := SelectionStart + 1 to SelectionStop do S[i] := ClearChar(i);
    end;
    S[FCursorPos + 1] := Ch;
    SetInheritedText(S);
    SelectNextChar;
  end
  else
  //If we have a selection > 1 (and cannot insert) then Delete the selected text: Delphi compatibility
  if HasExtSelection then DeleteSelected;
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
     (Current = Char_AllFixedUpCase   ) or
     (Current = Char_AlphaNumUpcase   ) or
     (Current = Char_AlphaNumFixedUpCase)
     then
       Ch := UpCase(Ch);

  // If in LowerCase convert the input char
  if (Current = Char_LetterDownCase     ) Or
     (Current = Char_LetterFixedDownCase) Or
     (Current = Char_AllDownCase        ) Or
     (Current = Char_AllFixedDownCase   ) or
     (Current = Char_AlphaNumDownCase   ) or
     (Current = Char_AlphaNumFixedDownCase )
     then
       Ch := LowerCase(Ch);

  // Check the input (check the valid range)
  case Current Of
       Char_Number              : Result := Ch In ['0'..'9'];
       Char_NumberFixed         : Result := Ch In ['0'..'9'];
       Char_NumberPlusMin       : Result := Ch in ['0'..'9','+','-'];
       Char_Letter              : Result := Ch In ['a'..'z', 'A'..'Z'];
       Char_LetterFixed         : Result := Ch In ['a'..'z', 'A'..'Z'];
       Char_LetterUpCase        : Result := Ch In ['A'..'Z'];
       Char_LetterDownCase      : Result := Ch In ['a'..'z'];
       Char_LetterFixedUpCase   : Result := Ch In ['A'..'Z'];
       Char_LetterFixedDownCase : Result := Ch In ['a'..'z'];
       Char_AlphaNum            : Result := Ch in ['a'..'z', 'A'..'Z', '0'..'9'];
       Char_AlphaNumFixed       : Result := Ch in ['a'..'z', 'A'..'Z', '0'..'9'];
       Char_AlphaNumUpCase      : Result := Ch in ['A'..'Z', '0'..'9'];
       Char_AlphaNumDownCase    : Result := Ch in ['a'..'z', '0'..'9'];
       Char_AlphaNumFixedUpCase : Result := Ch in ['A'..'Z', '0'..'9'];
       Char_AlphaNumFixedDowncase:Result := Ch in ['a'..'z', '0'..'9'];
       //ToDo: make this UTF8 compatible, for now
       //limit this to lower ASCII set
       Char_All                 : Result := Ch in [#32..#126]; //True;
       Char_AllFixed            : Result := Ch in [#32..#126]; //True;
       Char_AllUpCase           : Result := Ch in [#32..#126]; //True;
       Char_AllDownCase         : Result := Ch in [#32..#126]; //True;
       Char_AllFixedUpCase      : Result := Ch in [#32..#126]; //True;
       Char_AllFixedDownCase    : Result := Ch in [#32..#126]; //True;
      {Char_Space               : Result := Ch in [' ', '_'];  //not Delphi compatible, see notes above}
       Char_HourSeparator       : Result := Ch in [DefaultFormatSettings.TimeSeparator];
       Char_DateSeparator       : Result := Ch in [DefaultFormatSettings.DateSeparator];
  end;
end;


// Delete selected chars
procedure TCustomMaskEdit.DeleteSelected;
Var
  SelectionStart, SelectionStop, I : Integer;
  S                                : ShortString;
begin
  if not HasSelection then Exit;
  GetSel(SelectionStart, SelectionStop);
  S := Inherited Text;
  for i := SelectionStart + 1 to SelectionStop do S[i] := ClearChar(i);
  SetInheritedText(S);
  SetCursorPos;
end;


// Delete a single char from position
procedure TCustomMaskEdit.DeleteChars(NextChar : Boolean);
begin
  if NextChar then
  begin//VK_DELETE
    if HasSelection then DeleteSelected
    else
    begin
      //cannot delete beyond length of string
      if FCursorPos < Length(FMask) then
      begin
        //This will select the appropriate char in the control
        SetCursorPos;
        DeleteSelected;
      end;
    end;
  end
  else
  begin//VK_BACK
    //if selected text > 1 char then delete selection
    if HasExtSelection then DeleteSelected
    else
    begin
      //cannot backspace if we are at beginning of string
      if FCursorPos > 0 then
      begin
        Dec(FCursorPos);
        //This will select the appropriate char in the control
        SetCursorPos;
        //then delete this char
        DeleteSelected;
      end;
    end;
  end;
end;



// Get the actual Text
Function TCustomMaskEdit.GetText : String;
{
  Replace al FSPaceChars with #32
  If FMaskSave = False the do trimming of spaces and remove all maskliterals
}
var
  S: String;
  i: Integer;
Begin
  if not IsMasked then
  begin
    Result := InHerited Text;
  end
  else
  begin
    S := StringReplace(Inherited Text, FSpaceChar, #32, [rfReplaceAll]);
    //FSpaceChar can be used as a literal in the mask, so put it back
    for i := 1 to Length(FMask) do
    begin
      if IsLiteral(FMask[i]) and (FMask[i] = FSpaceChar) then
      begin
        S[i] := FSpaceChar;
      end;
    end;
    if not FMaskSave then
    begin
      for i := 1 to Length(FMask) do
      begin
        if IsLiteral(FMask[i]) then S[i] := #1; //We know this char can never be in Text, so this is safe
      end;
      S := StringReplace(S, #1, '', [rfReplaceAll]);
      //Trimming only occurs if FMaskSave = False
      case FTrimType of
        metTrimLeft : S := TrimLeft(S);
        metTrimRight: S := TrimRight(S);
      end;//case
    end;
    Result := S;
  end;
End;


// Set the actual Text
Procedure TCustomMaskEdit.SetText(Value : String);
{ This tries to mimic Delphi behaviour (D3):
  - if mask contains no literals text is set, if necessary padded with blanks,
    LTR or RTL depending on FTrimType
  - if mask contains literals then we search for matching literals in text and
    process each "segment" between matching maskliterals, trimming or padding
    LTR or RTL depending on FTrimType, until there is no more matching maskliteral
    Some examples to clarify:
    EditMask        Text to be set    Result
    99              1                 1_
    !99             1                 _1
    cc-cc           1-2               1_-2_
    !cc-cc          1-2               _1-_2
    cc-cc@cc        1-2@3             1_-2_@3_
                    12@3              12-__@__
    cc-cc@cc        123-456@789       12-45@78
    !cc-cc@cc       123-456@789       23-56@89
    This feauture seems to be invented for easy use of dates:

    99/99/00        23/1/2009         23/1_/20  <- if your locale DateSeparator = '/'
    !99/99/00       23/1/2009         23/_1/09  <- if your locale DateSeparator = '/'

  - The resulting text will always have length = length(FMask)
  - The text that is set, does not need to validate
}
//Helper functions
  Function FindNextMaskLiteral(const StartAt: Integer; out FoundAt: Integer; out ALiteral: Char): Boolean;
  var i: Integer;
  begin
    Result := False;
    for i := StartAt to Length(FMask) do
    begin
      if IsLiteral(FMask[i]) then
      begin
        FoundAt := i;
        ALiteral := ClearChar(i);
        Result := True;
        Exit;
      end;
    end;
  end;
  Function FindMatchingLiteral(const Value: String; const ALiteral: Char; out FoundAt: Integer): Boolean;
  begin
    FoundAt := Pos(ALiteral, Value);
    Result := (FoundAt > 0);
  end;

Var
  S                   : ShortString;
  I, J                : Integer;
  mPrevLit, mNextLit  : Integer; //Position of Previous and Next lietral in FMask
  vNextLit            : Integer; //Position of next matching literal in Value
  HasNextLiteral,
  HasMatchingLiteral,
  Stop                : Boolean;
  Literal             : Char;
  Sub                 : String;
Begin
  //Setting Text while loading has unwanted side-effects
  if (csLoading in ComponentState) then
  begin
    FInitialText := Value;
    Exit;
  end;
  if IsMasked then
  begin
    if (Value = '') then
    begin
      Clear;
      Exit;
    end;

    Value := Utf8ToAscii(Value);

    //First setup a "blank" string that contains all literals in the mask
    S := '';
    for I := 1 To Length(FMask) do  S := S + ClearChar(I);

    if FMaskSave then
    begin
      mPrevLit := 0;
      Stop := False;
      HasNextLiteral := FindNextMaskLiteral(mPrevLit+1, mNextLit, Literal);
      //if FMask starts with a literal, then Value[1] must be that literal
      if HasNextLiteral and (mNextLit = 1) and (Value[1] <> Literal) then Stop := True;
      //debugln('HasNextLiteral = ',dbgs(hasnextliteral),', Stop = ',dbgs(stop));
      While not Stop do
      begin
        if HasNextLiteral then
        begin
          HasMatchingLiteral := FindMatchingLiteral(Value, Literal, vNextLit);
          //debugln('mPrevLit = ',dbgs(mprevlit),' mNextLit = ',dbgs(mnextlit));
          //debugln('HasMatchingLiteral = ',dbgs(hasmatchingliteral));
          if HasMatchingLiteral then
          begin
            //debugln('vNextLit = ',dbgs(vnextlit));
            Sub := Copy(Value, 1, vNextLit - 1); //Copy up to, but not including matching literal
            System.Delete(Value, 1, vNextLit); //Remove this bit from Value (including matching literal)
            if (Length(Value) = 0) then Stop := True;
            //debugln('Sub = "',Sub,'", Value = "',Value,'"');
          end
          else
          begin//HasMatchingLiteral = False
            Stop := True;
            Sub := Value;
            Value := '';
            //debugln('Sub = "',Sub,'", Value = "',Value,'"');
          end;
          //fill S between vPrevLit + 1 and vNextLit - 1, LTR or RTL depending on FTrimType
          if (FTrimType = metTrimRight) then
          begin
            j := 1;
            for i := (mPrevLit + 1) to (mNextLit - 1) do
            begin
              if (J > Length(Sub)) then Break;
              if (Sub[j] = #32) then S[i] := FSpaceChar else S[i] := Sub[j];
              Inc(j);
            end;
          end
          else
          begin//FTrimType = metTrimLeft
            j := Length(Sub);
            for i := (mNextLit - 1) downto (mPrevLit + 1) do
            begin
              if (j < 1) then Break;
              if (Sub[j] = #32) then S[i] := FSpaceChar else S[i] := Sub[j];
              Dec(j);
            end;
          end;
          //debugln('S = ',S);
        end
        else
        begin//HasNextLiteral = False
          //debugln('No more MaskLiterals at this point');
          //debugln('mPrevLit = ',dbgs(mprevlit));
          Stop := True;
          Sub := Value;
          Value := '';
          //debugln('Sub = "',Sub,'", Value = "',Value,'"');
          //fill S from vPrevLit + 1 until end of FMask, LTR or RTL depending on FTrimType
          if (FTrimType = metTrimRight) then
          begin
            j := 1;
            for i := (mPrevLit + 1) to Length(FMask) do
            begin
              //debugln('  i = ',dbgs(i),'  j = ',dbgs(j));
              if (j > Length(Sub)) then Break;
              if (Sub[j] = #32) then S[i] := FSpaceChar else S[i] := Sub[j];
              //debugln('  Sub[j] = "',Sub[j],'" -> S = ',S);
              Inc(j);
            end;
          end
          else
          begin//FTrimType = metTrimLeft
            j := Length(Sub);
            for i := Length(FMask) downto (mPrevLit + 1) do
            begin
              //debugln('  i = ',dbgs(i),'  j = ',dbgs(j));
              if (j < 1) then Break;
              if (Sub[j] = #32) then S[i] := FSpaceChar else S[i] := Sub[j];
              //debugln('  Sub[j] = "',Sub[j],'" -> S = ',S);
              Dec(j);
            end;
          end;
          //debugln('S = ',S);
        end;
        //debugln('Stop = ',dbgs(stop));
        if not Stop then
        begin
          mPrevLit := mNextLit;
          HasNextLiteral := FindNextMaskLiteral(mPrevLit + 1, mNextLit, Literal);
        end;
      end;//while not Stop
    end//FMaskSave = True
    else
    begin//FMaskSave = False
      if FTrimType = metTrimRight then
      begin
        //fill text from left to rigth, skipping MaskLiterals
        j := 1;
        for i := 1 to Length(FMask) do
        begin
          if not IsLiteral(FMask[i]) then
          begin
            if (Value[j] = #32) then S[i] := FSpaceChar else S[i] := Value[j];
            Inc(j);
            if j > Length(Value) then Break;
          end;
        end;
      end
      else
      begin
        //fill text from right to left, skipping MaskLiterals
        j := Length(Value);
        for i := Length(FMask) downto 1 do
        begin
          if not IsLiteral(FMask[i]) then
          begin
            if (Value[j] = #32) then S[i] := FSpaceChar else S[i] := Value[j];
            Dec(j);
            if j < 1 then Break;
          end;
        end;
      end;
    end;//FMaskSave = False
    SetInheritedText(S);
  end//Ismasked
  else
  begin//not IsMasked
    SetInheritedText(Value);
  end;
End;


function TCustomMaskEdit.GetEditText: string;
begin
  Result := Inherited Text;
end;



procedure TCustomMaskEdit.SetEditText(const AValue: string);
//Note: This is not Delphi compatible, but by design
//Delphi lets you just set EditText of any length, which is extremely dangerous!
var
  S: String;
  i: Integer;
begin
  if (not IsMasked) then
  begin
    Inherited Text := AValue;
  end
  else
  begin
    //Make sure we don't copy more or less text into the control than FMask allows for
    S := Copy(UTF8ToAscii(AValue), 1, Length(FMask));
    //Restore all MaskLiterals, or we will potentially leave the control
    //in an unrecoverable state, eventually crashing the app
    for i := 1 to Length(S) do
      if IsLiteral(FMask[i]) then S[i] := ClearChar(i);
    //Pad resulting string with ClearChar if text is too short
    while Length(S) < Length(FMask) do S := S + ClearChar(Length(S)+1);
    SetInheritedText(S);
  end;
end;

// Respond to Text Changed message
procedure TCustomMaskEdit.TextChanged;
{ Purpose: to avoid messing up the control by
  - cut/paste/clear via OS context menu
    (we try to catch these messages and handle them,
    but this is not garantueed to work)
  - dragging selected text in the control with the mouse
  If one of these happens, then the internal logic of cursorpositioning,
  inserting characters is messed up.
  So, we simply restore the text from our backup: FCurrenText
}
begin
  if (not IsMasked) or FChangeAllowed then
  begin
    Inherited TextChanged;
  end
  else
  begin//Undo changes: restore with value of FCurrentText
    //we do not call inherited TextChanged here, because the following SetInheritedText
    //will trigger TextChanged with FChangeAllowed = True and inherited TextChanged is called then
    SetInheritedText(FCurrentText);
    //Reset cursor to last known position
    SetCursorPos;
  end;
end;

procedure TCustomMaskEdit.SetCharCase(Value: TEditCharCase);
begin
  if IsMasked then
  begin
    if (GetCharCase <> ecNormal) then inherited CharCase := ecNormal;
  end
  else
  begin
    inherited CharCase := Value;
  end;
end;

function TCustomMaskEdit.GetCharCase: TEditCharCase;
begin
  Result := inherited CharCase;
end;

procedure TCustomMaskEdit.SetMaxLength(Value: Integer);
begin
  if IsMasked then
  begin
    inherited MaxLength := Length(FMask);
  end
  else
  begin
    inherited MaxLength := Value;
  end;
end;

function TCustomMaskEdit.GetMaxLength: Integer;
begin
  Result := inherited Maxlength;
end;

procedure TCustomMaskEdit.Loaded;
begin
  inherited Loaded;
  if (FInitialMask <> '') then SetMask(FInitialMask);
  if (FInitialText <> '') then SetText(FInitialText);
end;






// Respond to Paste message
procedure TCustomMaskEdit.LMPasteFromClip(var Message: TLMessage);
begin
  if (not IsMasked) or (ReadOnly) then
  begin
    Inherited ;
    Exit;
  end;
  //We handle this message ourself
  Message.Result := 0;
  PasteFromClipBoard;
end;



// Respond to Cut message
procedure TCustomMaskEdit.LMCutToClip(var Message: TLMessage);
begin
  if not IsMasked then
  begin
    inherited;
    Exit;
  end;
  //We handle this message ourself
  Message.Result := 0;
  CutToClipBoard;
end;


// Respond to Clear message
procedure TCustomMaskEdit.LMClearSel(var Message: TLMessage);
begin
  //DebugLn('TCustomMaskEdit.LMClearSel');
  if not IsMasked then
  begin
    inherited;
    Exit;
  end;
  //We handle this message ourself
  Message.Result := 0;
  DeleteSelected;
end;



function TCustomMaskEdit.EditCanModify: Boolean;
begin
  Result := True;
end;



procedure TCustomMaskEdit.Reset;
//Implements an Undo mechanisme from the moment of entering the control
begin
  if IsMasked and (not ReadOnly) then
  begin
    SetinheritedText(FTextOnEnter);
  end;
end;

//Moved from CMEnter message handler
procedure TCustomMaskEdit.DoEnter;
begin
  inherited DoEnter;
  if isMasked then
  begin
    //debugln('TCustomMaskEdit.DoEnter: FValidationFailed = ',DbgS(FValidationFailed));
    FCursorPos := GetSelStart;
    //Only save FTextOnEnter if validation did not fail in last DoExit that occurred
    if not FValidationFailed then
      FTextOnEnter := Inherited Text
    else
      FValidationFailed := False;
    Modified := False;
    if ((FCursorPos = 0) and (IsLiteral(FMask[1]))) then
      //On entering select first editable char
      SelectNextChar
    else
      SetCursorPos;
  end;
end;



procedure TCustomMaskEdit.DoExit;
begin
  //debugln('TCustomMaskEdit.DoExit: FValidationFailed = ',DbgS(FValidationFailed));
  //First give OnExit a change to prevent a EDBEditError
  inherited DoExit;
  {$IFNDEF MASKEDIT_NOVALIDATEONEXIT}
  //Do not validate if FValidationFailed, or risk raising an exception while the previous exception was
  //not handled, resulting in an application crash
  if IsMasked and (FTextOnEnter <> Inherited Text) and (not FValidationFailed) then
  begin
    //assume failure
    try
      //debugln('TCustomMaskedit.DoExit: try ValidateEdit');
      FValidationFailed := True;
      ValidateEdit;
      FValidationFailed := False;
    finally
      if FValidationFailed then
      begin
        //debugln('TCustomMaskedit.DoExit: Validation failed');
        SetFocus;
        SelectAll;
      end;
    end;
  end;
  {$ENDIF}
end;



// Single key down procedure
procedure TCustomMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  Inherited KeyDown(Key, Shift);
  // Not masked -> old procedure
  if not IsMasked then
  begin
    Exit;
  end;
  FCursorPos := GetSelStart;
  // shift and arrowkey -> old procedure
  if (ssShift in Shift) then
  begin
    if (Key = VK_LEFT) or (Key = VK_RIGHT) or
       (Key = VK_HOME) or (Key = VK_END) then
    begin
      Exit;
    end;
  end;
  //Escape Key
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    Reset;
    Key := 0;
    Exit;
  end;
  //Handle clipboard and delete/backspace keys
  if (Key = VK_DELETE) then
  begin
    if not ReadOnly then
    begin
      if (Shift = [ssShift]) then
      begin//Cut
        CutToClipBoard;
      end
      else if (Shift = [ssCtrl]) then
      begin//Clear
        DeleteSelected;
      end
      else if (Shift = []) then
      begin//Plain Delete
        //DeleteChars also works if SelLength = 0
        DeleteChars(True);
      end;
      Key := 0;
      Exit;
    end;
  end;
  if (Key = VK_BACK) then
  begin
    if not ReadOnly then
    begin
      if (Shift = [ssCtrl]) then
      begin//Clear
        DeleteSelected;
      end
      else
      if (Shift = [ssShift]) then
      begin
        CutToClipBoard;
      end
      else
      if (Shift = []) then
      begin
        DeleteChars(False);
      end;
      Key := 0;
      Exit;
    end;
  end;
  if (Key = VK_INSERT) then
  begin//Copy or Paste
    if (Shift = [ssShift]) then
    begin//Paste
      if not ReadOnly then
      begin
        PasteFromClipBoard;
      end;
    end
    else if (Shift = [ssCtrl]) then
    begin//Copy
      CopyToClipBoard;
    end;
    Key := 0;
    Exit;
  end;
  if (Key = VK_C) and (Shift = [ssCtrl]) then
  begin//Copy
    CopyToClipBoard;
    Key := 0;
    Exit;
  end;
  if (Key = VK_X) and (Shift = [ssCtrl]) then
  begin//Cut
    if not ReadOnly then
    begin
      CutToClipBoard;
      Key := 0;
      Exit;
    end;
  end;
  if (Key = VK_V) and (Shift = [ssCtrl]) then
  begin//Paste
    if not ReadOnly then
    begin
      PasteFromClipBoard;
      Key := 0;
      Exit;
    end;
  end;

  // Cursor movement
  //ATM we handle Ctrl+ArrowKey as if it were just ArrowKey
  if (Key = VK_LEFT) then
  begin
    SelectPrevChar;
    Key := 0;
    Exit;
  end;
  if (Key = VK_RIGHT) then
  begin
    SelectNextChar;
    Key := 0;
    Exit;
  end;
  if (Key = VK_HOME) then
  begin
    SelectFirstChar;
    Key := 0;
    Exit;
  end;
  if (Key = VK_END) then
  begin
    GotoEnd;
    Key := 0;
    Exit;
  end;
  // Cursor Up/Down -> not valid
  if (Key = VK_UP) or (Key = VK_DOWN) then
  begin
    Key := 0;
    Exit;
  end;
end;


procedure TCustomMaskEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (not IsMasked) or ReadOnly then
  begin
    Exit;
  end;
  FCursorPos := GetSelStart;
  //Moved from KeyDown, which would only handle uppercase chars...
  // Insert a char
  if (Key In [#32..#255]) then
  begin
    if (Key in [Period, Comma]) and not (CanInsertChar(FCursorPos + 1, Key)) then
    begin//Try to jump to next period or comma, if at all possible
      JumpToNextDot(Key);
    end
    else
    begin//any other key
      InsertChar(Key);
    end;
    //We really need to "eat" all keys we handle ourselves
    //(or widgetset will insert char second time)
    Key:= #0;
  end;
end;


//Moved form LMMButtonUp message handler
procedure TCustomMaskEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if IsMasked then
  begin
    FCursorPos := GetSelStart;
    if not HasSelection then SetCursorPos;
  end;
end;

procedure TCustomMaskEdit.CheckCursor;
begin
  if IsMasked then
    SetCursorPos;
end;

procedure TCustomMaskEdit.CutToClipBoard;
begin
  if not IsMasked then
  begin
    inherited CutToClipBoard;
    Exit;
  end;
  CopyToClipBoard;
  DeleteSelected;
end;

procedure TCustomMaskEdit.PasteFromClipBoard;
{
  Paste only allowed chars, skip literals in the mask
  e.g. if cliptext = '1234' and mask = '00:00' then result will be '12:34'
}
var
  ClipText, S: String;
  P, i: LongInt;
begin
  if not IsMasked then
  begin
    inherited PasteFromClipBoard;
    Exit;
  end;
 if Clipboard.HasFormat(CF_TEXT) then
 begin
   ClipText := UTF8ToAscii(ClipBoard.AsText);
   if (Length(ClipText) > 0) then
   begin
     P := FCursorPos + 1;
     DeleteSelected;
     S := Inherited Text;
     i := 1;
     while (P <= Length(FMask)) do
     begin
       //Skip any literal
       while (P < Length(FMask)) and (IsLiteral(FMask[P])) do Inc(P);
       //Skip any char in ClipText that cannot be inserted at current position
       while (i < Length(ClipText)) and (not CanInsertChar(P, ClipText[i])) do Inc(i);
       if CanInsertChar(P, ClipText[i]) then
       begin
         S[P] := ClipText[i];
         Inc(P);
         Inc(i);
       end
       else
         Break;
     end;
     SetInheritedText(S);
     SetCursorPos;
   end;
 end;
end;


// Clear the controll
procedure TCustomMaskEdit.Clear;
Var
  S : ShortString;
  I : Integer;
begin
  if isMasked then
  begin
    S  := '';
    for I := 1 To Length(FMask) do S := S + ClearChar(I);
    SetinheritedText(S);
    FCursorPos := 0;
    SetCursorPos;
  end
  else Inherited Clear;
end;



procedure TCustomMaskEdit.ValidateEdit;
var
  S: String;
  _MaskSave: Boolean;
begin
  //Only validate if IsMasked
  if IsMasked then
  begin
    {
     if FMaskSave = False then literal and spaces are trimmed from Text
     and TextIsValid might wrongly return False
     We need the text with literals and FSpaceChar translated to #32
    }
    _MaskSave := FMaskSave;
    FMaskSave := True;
    S := Text;
    FMaskSave := _MaskSave;
    if not TextIsValid(S) then
    begin
      SetCursorPos;
      Raise EDBEditError.Create(SMaskEditNoMatch);
    end;
  end;
end;


end.

