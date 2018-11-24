{
 /***************************************************************************
                                 maskedit.pp
                                 -----------
                           Component Library Code

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


{
ToDo List:
 - Better handling of cut/clear/paste messages

Bugs:
 - The Delphi helpt text says that a '_' in EditMask will insert a blank in the text.
   However all versions of Delphi up to D2010 treat it as a literal '_' (unless
   specified in the 3rd field of a multifield EditMask), so I rewrote parts to make it behave like
   that also.
   If, in the future, Delphi actually treats '_' as a blank, we'll re-implement it, for that
   purpose I did not remove the concerning code, but commented it out

Known Utf8 related issues: (Oktober 2012, BB)
 - Utf8 also has what is called de-composed code-points:
   For example the "LATIN SMALL LETTER E WITH DIAERESIS" can be represented with a single codepoint
   (U+00EB), but also by the sequence of codepoints  LATIN SMALL LETTER E (U+0065) +  COMBINING DIAERESIS (U+0308)
   The latter form is not handled correctly ATM, but also does not occur much "in the wild"
   (See discussion at the forum: http://forum.lazarus.freepascal.org/index.php/topic,10530.0.html)
 - Some valid Utf8 sequences do not represent any visible character.
   I have not been able to test how this affects the maskedit unit.


Different behaviour than Delphi, but by design (October 2009, BB)
 - In SetText in Delphi, when MaskNoSave is in EditMask, it is possible to set text longer then the mask
   allowes for. I disallowed that, because it corrupts internal cursor placement etc.
 - SetEditText is not Delphi compatible. Delphi allows setting any text in the control, leaving the control
   in an unrecoverable state, where it is impossible to leave the control because the text can never be validated
   (too short, too long, overwritten maskliterals). The app wil crash as a result of this.
   I have decided to disallow this:
   - EditText is truncated, or padded with ClearChar if necessary so that Utf8Length(EditText) = FMaskLength
   - Restore all MaskLiterals in the text
}

unit MaskEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LMessages, Clipbrd, LCLType, LCLProc, LCLStrConsts, LazUtf8;

const
  { Mask Type }
  cMask_SpecialChar   = '\'; // after this you can set an arbitrary char
  cMask_UpperCase     = '>'; // after this the chars is in upper case
  cMask_LowerCase     = '<'; // after this the chars is in lower case
  cMask_Letter        = 'l'; // only a letter but not necessary
  cMask_LetterFixed   = 'L'; // only a letter
  cMask_AlphaNum      = 'a'; // an alphanumeric char (['A'..'Z','a..'z','0'..'9']) but not necessary
  cMask_AlphaNumFixed = 'A'; // an alphanumeric char
  cMask_AllChars      = 'c'; // any Utf8 char but not necessary
  cMask_AllCharsFixed = 'C'; // any Utf8 char #32 - #255
  cMask_Number        = '9'; // only a number but not necessary
  cMask_NumberFixed   = '0'; // only a number
  cMask_NumberPlusMin = '#'; // only a number or + or -, but not necessary
  cMask_HourSeparator = ':'; // automatically put the hour separator char
  cMask_DateSeparator = '/'; // automatically but the date separator char
  cMask_Hex           = 'h'; // a hexadecimal character (['0'..'9','a'..'f']) but not necessary  (Lazarus extension, not supported by Delphi)
  cMask_HexFixed      = 'H'; // a hexadecimal character                                          (Lazarus extension, not supported by Delphi)
  cMask_Binary        = 'b'; // a binary character (['0'..'1']) but not necessary (Lazarus extension, not supported by Delphi)
  cMask_BinaryFixed   = 'B'; // a binary character                                (Lazarus extension, not supported by Delphi)
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
                 Char_Hex,                   //Lazarus extension, not supported by Delphi
                 Char_HexFixed,              //Lazarus extension, not supported by Delphi
                 Char_HexUpCase,             //Lazarus extension, not supported by Delphi
                 Char_HexDownCase,           //Lazarus extension, not supported by Delphi
                 Char_HexFixedUpCase,        //Lazarus extension, not supported by Delphi
                 Char_HexFixedDownCase,      //Lazarus extension, not supported by Delphi
                 Char_Binary,                //Lazarus extension, not supported by Delphi
                 Char_BinaryFixed,           //Lazarus extension, not supported by Delphi
                 Char_Stop); {currently we have 31 enums, I think we cannot add more  BB}


  TInternalMask = array[1..255] of TUtf8Char;
  TMaskeditTrimType = (metTrimLeft, metTrimRight);

  { Exception class }
type
  EDBEditError = class(Exception);
  //Utf8 handling errors
  EInvalidUtf8 = class(Exception);
  EInvalidCodePoint = class(EInvalidUtf8);

const
  SInvalidCodePoint = 'The (hexadecimal) sequence %s is not a valid UTF8 codepoint.';


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
 write action (in RealSetTextWhileMasked() ).
 We try to intercept the messages for cut/paste/copy/clear and perform the appropriate
 actions instead.
 If this fails, then in TextChanged we check and will see that FChangeAllowed = False
 and we will undo the changes made.

 To make this undo possible it is necessary to set FCurrentText every time you set
 the text in the control!
 This is achieved in RealSetTextWhileMasked() only, so please note:
 !! It is unsafe to make a call to RealSetText unless done so via RealSetTextWhileMasked() !!!

 (Bart Broersma, januari 2009)

 ************************************************************************************************ }


 { TCustomMaskEdit }

 Type

  TCustomMaskEdit = Class(TCustomEdit)
  private
    FRealMask        : String;            // Real mask inserted
    FMask            : TInternalMask;     // Actual internal mask
    FMaskLength      : Integer;           // Length of internal mask
    FFirstFreePos    : Integer;           // First position where user can enter text
    FMaskSave        : Boolean;           // Save mask as part of the data
    FTrimType        : TMaskEditTrimType; // Trim leading or trailing spaces in GetText
    FSpaceChar       : Char;              // Char for space (default '_')
    FCurrentText     : TCaption;          // FCurrentText is our backup. See notes above!
    FTextOnEnter     : String;            // Text when user enters the control, used for Reset()
    FCursorPos       : Integer;           // Current caret position
    FChangeAllowed   : Boolean;           // We do not allow text changes by the OS (cut/clear via context menu)
    FInitialText     : String;            // Text set in the formdesigner (must be handled in Loaded)
    FInitialMask     : String;            // EditMask set in the formdesigner (must be handled in Loaded)
    FSettingInitialText: Boolean;
    FValidationFailed: Boolean;           // Flag used in DoEnter
    FMaskIsPushed    : Boolean;
    FSavedMask       : TInternalMask;
    FSavedMaskLength : Integer;
    FTextChangedBySetText: Boolean;
    FInRealSetTextWhileMasked: Boolean;

    procedure ClearInternalMask(out AMask: TInternalMask; out ALengthIndicator: Integer);
    procedure AddToMask(Value: TUtf8Char);
    function GetModified: Boolean;
    procedure SetMask(Value : String);
    function  GetIsMasked : Boolean;
    procedure SetModified(AValue: Boolean);
    procedure SetSpaceChar(Value : Char);

    procedure SetCursorPos;
    procedure SelectNextChar;
    procedure SelectPrevChar;
    procedure SelectFirstChar;
    procedure GotoEnd;
    procedure JumpToNextDot(Dot: Char);
    function  HasSelection: Boolean;
    function  HasExtSelection: Boolean;

    Function  CharToMask(UCh : TUtf8Char) : tMaskedType;
    Function  MaskToChar(Value : tMaskedType) : Char;
    Function  IsMaskChar(Ch : TUtf8Char) : Boolean;
    Function  IsLiteral(Ch: TUtf8Char): Boolean;
    function  TextIsValid(const Value: String): Boolean;
    function  CharMatchesMask(const Ch: TUtf8Char; const Position: Integer): Boolean;
    function  ClearChar(Position : Integer) : TUtf8Char;

    procedure RealSetTextWhileMasked(const Value: TCaption); //See notes above!
    procedure InsertChar(Ch : TUtf8Char);
    Function  CanInsertChar(Position : Integer; Var Ch : TUtf8Char; IsPasting: Boolean = False) : Boolean;
    procedure DeleteSelected;
    procedure DeleteChars(NextChar : Boolean);
  protected
    function ApplyMaskToText(Value: TCaption): TCaption;
    function CanShowEmulatedTextHint: Boolean; override;
    function DisableMask(const NewText: String): Boolean;
    function RestoreMask(const NewText: String): Boolean;
    procedure RealSetText(const AValue: TCaption); override;
    function RealGetText: TCaption; override;
    Function GetTextWithoutMask(Value: TCaption) : TCaption;
    function GetTextWithoutSpaceChar(Value: TCaption) : TCaption;
    Procedure SetTextApplyMask(Value: TCaption);
    function  GetEditText: string; virtual;
    procedure SetEditText(const AValue: string);

    procedure GetSel(out _SelStart: Integer; out _SelStop: Integer);
    procedure SetSel(const _SelStart: Integer; _SelStop: Integer);
    procedure TextChanged; override;
    procedure Change; override;
    procedure SetCharCase(Value: TEditCharCase);
    function GetCharCase: TEditCharCase;
    procedure SetMaxLength(Value: Integer);
    function GetMaxLength: Integer;
    procedure SetNumbersOnly(Value: Boolean); override;
    procedure Loaded; override;

    procedure LMPasteFromClip(var Message: TLMessage); message LM_PASTE;
    procedure LMCutToClip(var Message: TLMessage); message LM_CUT;
    procedure LMClearSel(var Message: TLMessage); message LM_CLEAR;

    function  EditCanModify: Boolean; virtual;
    procedure Reset; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleKeyPress(var Key: TUtf8Char);
    procedure KeyPress(var Key: Char); override;
    procedure Utf8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure CheckCursor;
    property EditText: string read GetEditText write SetEditText;
    property IsMasked: Boolean read GetIsMasked;
    property SpaceChar: Char read FSpaceChar write SetSpaceChar;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
    property EditMask: string read FRealMask write SetMask;
  public
    procedure CutToClipBoard; override;
    procedure PasteFromClipBoard; override;
    { Required methods }
    constructor Create(TheOwner : TComponent); override;
    procedure Clear;
    procedure ValidateEdit; virtual;
    property Modified: Boolean read GetModified write SetModified;
  end;

  { TMaskEdit }

  TMaskEdit = class(TCustomMaskEdit)
  public
    property IsMasked;
    property EditText;
  published
    property Align;
    property Alignment;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property EditMask;
    property Text;
    property TextHint;
    property SpaceChar;
  end;

function FormatMaskText(const AEditMask: string; const Value: string ): string;
procedure SplitEditMask(AEditMask: String; out AMaskPart: String; out AMaskSave: Boolean; out ASpaceChar: Char);

procedure Register;

implementation


//Define this to prevent validation when the control looses focus
{ $DEFINE MASKEDIT_NOVALIDATEONEXIT}

{$ifdef debug_maskedit}
// For debugging purposes only
const
  MaskCharToChar: array[tMaskedType] of Char = (#0, cMask_Number, cMask_NumberFixed, cMask_NumberPlusMin,
     cMask_Letter, cMask_LetterFixed, cMask_Letter, cMask_Letter, cMask_LetterFixed, cMask_LetterFixed,
     cMask_AlphaNum, cMask_AlphaNumFixed, cMask_AlphaNum, cMask_AlphaNum, cMask_AlphaNumFixed, cMask_AlphaNumFixed,
     cMask_AllChars, cMask_AllCharsFixed, cMask_AllChars, cMask_AllChars, cMask_AllCharsFixed, cMask_AllCharsFixed,
     (*cMask_SpaceOnly,*) cMask_HourSeparator, cMask_DateSeparator, #0);
{$endif}

const
  Period = '.';
  Comma = ',';

//Utf8 helper functions

function GetCodePoint(const S: String; const Index: PtrInt): TUTF8Char;
//equivalent for Result := S[Index], but for Utf8 encoded strings
var
  p: PChar;
  PLen: PtrInt;
  Res: AnsiString; //intermediate needed for PChar -> String -> ShortString assignement
begin
  Result := '';
  p := UTF8CodepointStart(PChar(S), Length(S), Index - 1); //zero-based call
  //determine the length in bytes of this UTF-8 character
  PLen := UTF8CodepointSize(p);
  Res := p;
  //Set correct length for Result (otherwise it returns all chars up to the end of the original string)
  SetLength(Res,PLen);
  Result := Res;
end;

function StringToHex(S: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(S) do Result := Result + '$' + IntToHex(Ord(S[i]),2);
end;

procedure SetCodePoint(var S: String; const Index: PtrInt; CodePoint: TUTF8Char);
//equivalent for S[Index] := CodePoint, but for Utf8 encoded strings
var
  OldCP: TUTF8Char;
begin
  if (Index > Utf8Length(S)) then Exit;
  if (Utf8Length(CodePoint) <> 1) then Raise EInvalidCodePoint.Create(Format(SInvalidCodepoint,[StringToHex(CodePoint)]));
  OldCP := GetCodePoint(S, Index);
  if (OldCP = CodePoint) then Exit;
  Utf8Delete(S, Index, 1);
  Utf8Insert(CodePoint, S, Index);
end;



function FormatMaskText(const AEditMask: string; const Value: string): string;
var
  CME: TCustomMaskEdit;
begin
  CME := TCustomMaskEdit.Create(nil);
  try
    CME.EditMask := AEditMask;
    if CME.IsMasked then
    begin
      Result := CME.ApplyMaskToText(Value);
      //Delphi 7 leaves in the mask regardless of the "MaskSave" value in the specified EditMaske
      //but SpaceChar must be replaced by #32
      Result := CME.GetTextWithoutSpaceChar(Result);
    end
    else
      Result := Value;
  finally
    CME.Free;
  end;
end;

procedure SplitEditMask(AEditMask: String; out AMaskPart: String; out AMaskSave: Boolean; out ASpaceChar: Char);
{
  Retrieve the separate fields for a given EditMask:
  Given an AEditMask of '999.999;0;_'  it will return
  - AMaskPart = '999.999'
  - AMaskSave = False
  - ASpaceChar = '_'
}
begin
  {
    First see if AEditMask is multifield and if we can extract a value for
    AMaskSave and/or ASpaceChar
    If so, extract and remove from AMask (so we know that the remaining part of
    AMask _IS_ the mask to be set)

    A value for SpaceChar is only valid if also a value for MaskSave is specified
    (as by Delphi specifications), so Mask must be at least 4 characters
    These must be the last 2 or 4 characters of EditMask (and there must not be
    an escape character in front!)
  }
  //Assume no SpaceChar and no MaskSave is defined in new mask, so first set it to DefaultBlank and True
  ASpaceChar := DefaultBlank;
  AMaskSave := True;
  //MaskFieldseparator, MaskNoSave, SpaceChar and cMask_SpecialChar are defined as Char (=AnsiChar)
  //so in this case we can use Length (instead of Utf8length) and iterate single chars in the string
  if (Length(AEditMask) >= 4) and (AEditMask[Length(AEditMask)-1] = MaskFieldSeparator) and
     (AEditMask[Length(AEditMask)-3] = MaskFieldSeparator) and
     (AEditMask[Length(AEditMask)-2] <> cMask_SpecialChar) and
     //Length = 4 is OK (AEditMask = ";1;_" for example), but if Length > 4 there must be no escape charater in front
     ((Length(AEditMask) = 4) or ((Length(AEditMask) > 4) and (AEditMask[Length(AEditMask)-4] <> cMask_SpecialChar))) then
  begin
    ASpaceChar := AEditMask[Length(AEditMask)];
    AMaskSave := (AEditMask[Length(AEditMask)-2] <> MaskNosave);
    System.Delete(AEditMask,Length(AEditMask)-3,4);
  end
  //If not both FMaskSave and FSPaceChar are specified, then see if only FMaskSave is specified
  else if (Length(AEditMask) >= 2) and (AEditMask[Length(AEditMask)-1] = MaskFieldSeparator) and
          //Length = 2 is OK, but if Length > 2 there must be no escape charater in front
          ((Length(AEditMask) = 2) or ((Length(AEditMask) > 2) and (AEditMask[Length(AEditMask)-2] <> cMask_SpecialChar))) then
  begin
    AMaskSave := (AEditMask[Length(AEditMask)] <> MaskNoSave);
    //Remove this bit from Mask
    System.Delete(AEditMask,Length(AEditMask)-1,2);
  end;
  //Whatever is left of AEditMask at this point is the MaskPart
  AMaskPart := AEditMask;
end;


// Create object
constructor TCustomMaskEdit.Create(TheOwner: TComponent);
begin
  FSettingInitialText := False;
  FTextChangedBySetText := False;
  FInRealSetTextWhileMasked := False;
  FRealMask      := '';
  ClearInternalMask(FMask, FMaskLength);
  ClearInternalMask(FSavedMask, FSavedMaskLength);
  FSpaceChar     := '_';
  FMaskSave      := True;
  FChangeAllowed := False;
  FTrimType      := metTrimRight;
  Inherited Create(TheOwner);
  FCurrentText   := Inherited RealGetText;
  FTextOnEnter   := Inherited RealGetText;
  FInitialText   := '';
  FInitialMask   := '';
  FValidationFailed := False;
  FMaskIsPushed := False;
end;

procedure TCustomMaskEdit.ClearInternalMask(out AMask: TInternalMask; out ALengthIndicator: Integer);
begin
  {$PUSH}{$HINTS OFF}
  FillChar(AMask, SizeOf(TInternalMask), 0);
  ALengthIndicator := 0;
  {$POP}
end;

procedure TCustomMaskEdit.AddToMask(Value: TUtf8Char);
begin
  Inc(FMaskLength);
  FMask[FMaskLength] := Value;
end;

function TCustomMaskEdit.GetModified: Boolean;
begin
  //This will make Modified = False inside OnChange when text is set by code
  //TCustomEdit.RealSetText sets Modified to False.
  //We handle all input in RealSetTextWhileMasked (which eventually calls RealSetText),
  //so inside RealSetTextWhileMasked Modified must be True,
  //unless we called RealSetTextWhileMasked from SetTextApplyMask, in that case it must be False,
  //in all other cases just return inherited value
  if FTextChangedBySetText then
    Result := False
  else
  begin
    if FInRealSetTextWhileMasked then
      Result := True
    else
      Result := inherited Modified;
  end;
end;

// Prepare the real internal Mask
procedure TCustomMaskEdit.SetMask(Value : String);
Var
  S            : ShortString;
  I            : Integer;
  InUp, InDown : Boolean;
  Special      : Boolean;
  CP           : TUtf8Char;
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
    FValidationFailed := False;
    FMaskIsPushed := False;
    ClearInternalMask(FMask, FMaskLength);
    ClearInternalMask(FSavedMask, FSavedMaskLength);

    SplitEditMask(FRealMask, Value, FMaskSave, FSpaceChar);

    // Construct Actual Internal Mask
    // init
    FTrimType := metTrimRight;
    // Init: No UpCase, No LowerCase, No Special Char
    InUp      := False;
    InDown    := False;
    Special   := False;
    S         := Value;
    for I := 1 To Utf8Length(S) do
    begin
      CP := GetCodePoint(S,I);
      // Must insert a special char
      if Special then
      begin
        AddToMask(CP);
        Special := False;
      end
      else
      begin
        // Check the char to insert

        case CP Of
             cMask_SpecialChar: Special := True;
             cMask_UpperCase: begin
               if (I > 1) and (GetCodePoint(S,I-1) = cMask_LowerCase) then
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
                  AddToMask(MaskToChar(Char_LetterUpCase))
                else
                  if InDown
                  then
                    AddToMask(MaskToChar(Char_LetterDownCase))
                  else
                    AddToMask(MaskToChar(Char_Letter))
             end;

             cMask_LetterFixed: begin
                if InUp
                then
                  AddToMask(MaskToChar(Char_LetterFixedUpCase))
                else
                  if InDown
                  then
                    AddToMask(MaskToChar(Char_LetterFixedDownCase))
                  else
                    AddToMask(MaskToChar(Char_LetterFixed))
             end;

             cMask_AlphaNum: begin
                 if InUp
                 then
                   AddToMask(MaskToChar(Char_AlphaNumUpcase))
                 else
                   if InDown
                   then
                     AddToMask(MaskToChar(Char_AlphaNumDownCase))
                   else
                     AddToMask(MaskToChar(Char_AlphaNum))
             end;

             cMask_AlphaNumFixed: begin
                 if InUp
                 then
                   AddToMask(MaskToChar(Char_AlphaNumFixedUpcase))
                 else
                   if InDown
                   then
                     AddToMask(MaskToChar(Char_AlphaNumFixedDownCase))
                   else
                     AddToMask(MaskToChar(Char_AlphaNumFixed))
             end;

             cMask_AllChars: begin
                if InUp
                then
                  AddToMask(MaskToChar(Char_AllUpCase))
                else
                  if InDown
                  then
                    AddToMask(MaskToChar(Char_AllDownCase))
                  else
                    AddToMask(MaskToChar(Char_All))
             end;

             cMask_AllCharsFixed: begin
                if InUp
                then
                  AddToMask(MaskToChar(Char_AllFixedUpCase))
                else
                  if InDown
                  then
                    AddToMask(MaskToChar(Char_AllFixedDownCase))
                  else
                    AddToMask(MaskToChar(Char_AllFixed))
             end;

             cMask_Number: AddToMask(MaskToChar(Char_Number));

             cMask_NumberFixed: AddToMask(MaskToChar(Char_NumberFixed));

             cMask_NumberPlusMin: AddToMask(MaskToChar(Char_NumberPlusMin));

             cMask_HourSeparator: AddToMask(MaskToChar(Char_HourSeparator));

             cMask_DateSeparator: AddToMask(MaskToChar(Char_DateSeparator));

            {cMask_SpaceOnly: AddToMask(MaskToChar(Char_Space)); //not Delphi compatible, see remarks above}
              cMask_Hex: begin
                 if InUp
                 then
                   AddToMask(MaskToChar(Char_HexUpCase))
                 else
                   if InDown
                   then
                     AddToMask(MaskToChar(Char_HexDownCase))
                   else
                     AddToMask(MaskToChar(Char_Hex))
              end;

              cMask_HexFixed: begin
                 if InUp
                 then
                   AddToMask(MaskToChar(Char_HexFixedUpCase))
                 else
                   if InDown
                   then
                     AddToMask(MaskToChar(Char_HexFixedDownCase))
                   else
                     AddToMask(MaskToChar(Char_HexFixed))
              end;

              cMask_Binary: AddToMask(MaskToChar(Char_Binary));
              cMask_BinaryFixed: AddToMask(MaskToChar(Char_BinaryFixed));

             cMask_NoLeadingBlanks:
             begin
               FTrimType := metTrimLeft;
             end;

             else
             begin
               //It's a MaskLiteral
               AddToMask(CP);
             end;
        end;
      end;
    end;
    FFirstFreePos := 1;
    //Determine first position where text can be entered (needed for DeleteChars()
    while (FFirstFreePos <= FMaskLength) and IsLiteral(FMask[FFirstFreePos])  do Inc(FFirstFreePos);
    if (FMaskLength > 0) then
    begin
      SetCharCase(ecNormal);
      SetNumbersOnly(False);
    end;
    //SetMaxLegth must be before Clear, otherwise Clear uses old MaxLength value!
    SetMaxLength(FMaskLength);
    Clear;
    FTextOnEnter := inherited RealGetText;
  end;
end;


// Return if mask is selected
function TCustomMaskEdit.GetIsMasked : Boolean;
begin
  Result := (FMaskLength > 0);
end;

procedure TCustomMaskEdit.SetModified(AValue: Boolean);
begin
  inherited Modified := AValue;
end;


// Set the current Space Char
procedure TCustomMaskEdit.SetSpaceChar(Value : Char);
Var
  S      : String;
  I      : Integer;
  OldValue: TUtf8Char;
Begin
  if (Value <> FSpaceChar) And
  ((Not IsMaskChar(Value)) {or (CharToMask(Value) = Char_Space)}) then
  begin
    OldValue := FSpaceChar;
    FSpaceChar := Value;
    if IsMasked then
    begin
      S := inherited RealGetText;
      for I := 1 to Utf8Length(S) do
      begin
        if (GetCodePoint(S,i) = OldValue) and (not IsLiteral(FMask[i])) then SetCodePoint(S,i,FSpaceChar);
        //also update FTextOnEnter to reflect new SpaceChar!
        if (GetCodePoint(FTextOnEnter,i) = OldValue) and (not IsLiteral(FMask[i])) then SetCodePoint(FTextOnEnter,i,FSpaceChar);
      end;
      //FCurrentText := S;
      RealSetTextWhileMasked(S);
      CheckCursor;
    end;
  end;
end;




// Set the cursor position and select the char in the control
procedure TCustomMaskEdit.SetCursorPos;
begin
  //no need to do this when in designmode, it actually looks silly if we do
  if not (csDesigning in ComponentState) then
  begin
    if FCursorPos < 0 then FCursorPos := 0
    else if FCursorPos  > FMaskLength then FCursorPos := FMaskLength;
    if (FCursorPos + 1 > FMaskLength) or not Focused then
      SetSel(FCursorPos, FCursorPos)
    else
      SetSel(FCursorPos, FCursorPos + 1);
  end;
end;

//Move to next char, skip any mask-literals
procedure TCustomMaskEdit.SelectNextChar;
begin
  if (FCursorPos + 1) > FMaskLength then Exit;
  Inc(FCursorPos);
  While (FCursorPos + 1 < FMaskLength) and (IsLiteral(FMask[FCursorPos + 1])) do
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
  AStart: Integer;
  AStop: Integer;
begin
  GetSel(AStart, AStop);
  if (FCursorPos = 0) and (AStop - AStart <= 1) then Exit;
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
  FCursorPos := FMaskLength;
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
  function MaskPos(Sub: TUtf8Char; Start: Integer): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := Start to FMaskLength do
    begin
      if (FMask[i] = Sub) then
      begin
        Result := i;
        exit;
      end;
    end;
  end;

var
  HasNextDot, HasCommaAndPeriod, CanJump: Boolean;
  P, P2: Integer;
begin
  if not (Dot in [Period, Comma]) then Exit;
  P := MaskPos(Dot, FCursorPos + 1);
  HasNextDot := P > 0;
  If (Dot = Period) then
  begin
    P2 := MaskPos(Comma, 1);
    HasCommaAndPeriod := HasNextDot and (P2 >0)
  end
  else
  begin
    P2 := MaskPos(Period, 1);
    HasCommaAndPeriod := HasNextDot and (P2 >0);
  end;
  if HasCommaAndPeriod then
  begin
    //When mask has both period and comma only the first occurence is jumpable
    if P2 < P then HasNextDot := False;
  end;
  CanJump := HasNextDot and (P < FMaskLength) and (not IsLiteral(FMask[P+1]));
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
function TCustomMaskEdit.CharToMask(UCh: TUtf8Char): tMaskedType;
var
  Ch: Char;
Begin
  Result := Char_Start;
  if (Length(UCh) <> 1) then exit;
  Ch := UCh[1];
  if (Ord(Ch) > Ord(Char_Start)) and
     (Ord(Ch) < Ord(Char_Stop) )
     then
       Result := tMaskedType(Ord(Ch));
End;


// Trasform a single MaskType into a char
function TCustomMaskEdit.MaskToChar(Value: tMaskedType): Char;
Begin
  Result := Char(Ord(Value));
End;


// Return if the char passed is a valid MaskType char
function TCustomMaskEdit.IsMaskChar(Ch: TUtf8Char): Boolean;
Begin
  Result := (CharToMask(Ch) <> Char_Start);
End;


//Return if the char passed is a literal (so it cannot be altered)
function TCustomMaskEdit.IsLiteral(Ch: TUtf8Char): Boolean;
begin
  Result := (not IsMaskChar(Ch)) or
    (IsMaskChar(Ch) and (CharToMask(Ch) in [Char_HourSeparator, Char_DateSeparator{, Char_Space}]))
end;


//Return if Value matches the EditMask
function TCustomMaskEdit.TextIsValid(const Value: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (Utf8Length(Value) <> FMaskLength) then
  begin
    //DebugLn('  Utf8Length(Value) = ',DbgS(Utf8Length(Value)),' FMaskLength = ',DbgS(FMaskLength));
    Exit; //Actually should never happen??
  end;
  for i := 1 to FMaskLength do
  begin
    if not CharMatchesMask(GetCodePoint(Value, i), i) then Exit;
  end;
  Result := True;
end;


function TCustomMaskEdit.CharMatchesMask(const Ch: TUtf8Char; const Position: Integer): Boolean;
var
  Current: tMaskedType;
  Ok: Boolean;
begin
  Result := False;
  if (Position < 1) or (Position > FMaskLength) then Exit;
  Current := CharToMask(FMask[Position]);
  case Current Of
    Char_Number              : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9',#32]);
    Char_NumberFixed         : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9']);
    Char_NumberPlusMin       : OK := (Length(Ch) = 1) and (Ch[1] in ['0'..'9','+','-',#32]);
    Char_Letter              : OK := (Length(Ch) = 1) and (Ch[1] In ['a'..'z', 'A'..'Z',#32]);
    Char_LetterFixed         : OK := (Length(Ch) = 1) and (Ch[1] In ['a'..'z', 'A'..'Z']);
    Char_LetterUpCase        : OK := (Length(Ch) = 1) and (Ch[1] In ['A'..'Z',#32]);
    Char_LetterDownCase      : OK := (Length(Ch) = 1) and (Ch[1] In ['a'..'z',#32]);
    Char_LetterFixedUpCase   : OK := (Length(Ch) = 1) and (Ch[1] In ['A'..'Z']);
    Char_LetterFixedDownCase : OK := (Length(Ch) = 1) and (Ch[1] In ['a'..'z']);
    Char_AlphaNum            : OK := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', 'A'..'Z', '0'..'9',#32]);
    Char_AlphaNumFixed       : OK := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', 'A'..'Z', '0'..'9']);
    Char_AlphaNumUpCase      : OK := (Length(Ch) = 1) and (Ch[1] in ['A'..'Z', '0'..'9',#32]);
    Char_AlphaNumDownCase    : OK := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', '0'..'9',#32]);
    Char_AlphaNumFixedUpCase : OK := (Length(Ch) = 1) and (Ch[1] in ['A'..'Z', '0'..'9']);
    Char_AlphaNumFixedDowncase:OK := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', '0'..'9']);
    Char_All                 : OK := True;
    Char_AllFixed            : OK := True;
    Char_AllUpCase           : OK := True;
    Char_AllDownCase         : OK := True;
    Char_AllFixedUpCase      : OK := True;
    Char_AllFixedDownCase    : OK := True;
   {Char_Space               : OK := (Length(Ch) = 1) and (Ch in [' ', '_']);  //not Delphi compatible, see notes above}
    Char_HourSeparator       : OK := (Ch = DefaultFormatSettings.TimeSeparator);
    Char_DateSeparator       : OK := (Ch = DefaultFormatSettings.DateSeparator);
    Char_Hex                 : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f','A'..'F',#32]);
    Char_HexFixed            : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f','A'..'F']);
    Char_HexUpCase           : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','A'..'F',#32]);
    Char_HexDownCase         : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f',#32]);
    Char_HexFixedUpCase      : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','A'..'F']);
    Char_HexFixedDownCase    : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f']);
    Char_Binary              : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'1',#32]);
    Char_BinaryFixed         : OK := (Length(Ch) = 1) and (Ch[1] In ['0'..'1']);
    else//it's a literal
    begin
      OK := (Ch = FMask[Position]);
    end;
  end;//case
  //DebugLn('Position = ',DbgS(Position),' Current = ',MaskCharToChar[Current],' Ch = "',Ch,'" Ok = ',DbgS(Ok));
  Result := Ok;
end;


//Set text in the control with FChangeAllowed flag set appropriately
procedure TCustomMaskEdit.RealSetTextWhileMasked(const Value: TCaption);
begin
  if (Value <> inherited RealGetText) then
  begin
    FInRealSetTextWhileMasked := True;
    FChangeAllowed := True;
    FCurrentText := Value;
    //protect resetting FChangeAllowed := False against unhandled exceptions in user's
    //OnChange, otherwise risk leaving the control in an "unsafe" state regarding text changes
    try
      Inherited RealSetText(Value);
    finally
      FChangeAllowed := False;
      FInRealSetTextWhileMasked := False;
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
    ClearInternalMask(FSavedMask, FSavedMaskLength);
    System.Move(FMask[1], FSavedMask[1], SizeOf(TInternalMask));
    FSavedMaskLength := FMaskLength;
    ClearInternalMask(FMask, FMaskLength);
    FMaskIsPushed := True;
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
    ClearInternalMask(FMask, FMaskLength);
    System.Move(FSavedMask[1], FMask[1], SizeOf(TInternalMask));
    FMaskLength := FSavedMaskLength;
    ClearInternalMask(FSavedMask, FSavedMaskLength);
    SetMaxLength(FMaskLength);
    FTextOnEnter := inherited RealGetText;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
  // if NewText = old Text AND the control is now masked,
  // then "Text := NewText" will do nothing,
  // and NO mask will appear, so Clear first ...
  if IsMasked then Clear;
  Text := NewText;
end;

procedure TCustomMaskEdit.RealSetText(const AValue: TCaption);
begin
  //Setting Text while loading has unwanted side-effects
  if (csLoading in ComponentState) {and (not FSettingInitialText)} then
  begin
    FInitialText := AValue;
    Exit;
  end;
  if not IsMasked then
    inherited RealSetText(AValue)
  else
    SetTextApplyMask(AValue);
end;

function TCustomMaskEdit.RealGetText: TCaption;
begin
  Result := inherited RealGetText;  //don't call GetEditText here (issue #0026924)
  if IsMasked then
    Result := GetTextWithoutMask(Result);
end;

// Set the actual Text
procedure TCustomMaskEdit.SetTextApplyMask(Value: TCaption);
var
  S: TCaption;
Begin
  if IsMasked then
  begin
    try
      FTextChangedBySetText := True;
      if (Value = '') then
      begin
        Clear;
        Exit;
      end;
      S := ApplyMaskToText(Value);
      RealSetTextWhileMasked(S);
    finally
      FTextChangedBySetText := False;
    end; //try..finally
  end//Ismasked
  else
  begin//not IsMasked
    RealSetTextWhileMasked(Value);
  end;
End;


function TCustomMaskEdit.GetEditText: string;
begin
  Result := Inherited RealGetText;
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
    Inherited RealsetText(AValue);
  end
  else
  begin
    //Make sure we don't copy more or less text into the control than FMask allows for
    S := Utf8Copy(AValue, 1, FMaskLength);
    //Restore all MaskLiterals, or we will potentially leave the control
    //in an unrecoverable state, eventually crashing the app
    for i := 1 to Utf8Length(S) do
      if IsLiteral(FMask[i]) then SetCodePoint(S,i,ClearChar(i));
    //Pad resulting string with ClearChar if text is too short
    while Utf8Length(S) < FMaskLength do S := S + ClearChar(Utf8Length(S)+1);
    RealSetTextWhileMasked(S);
  end;
end;


// Clear (virtually) a single Utf8 char in position Position
function TCustomMaskEdit.ClearChar(Position : Integer) : TUtf8Char;
begin
  Result := FMask[Position];
  //For Delphi compatibilty, only literals remain, all others will be blanked
  case CharToMask(FMask[Position]) Of
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
    Char_AlphaNumFixedUpcase,
    Char_AlphaNuMFixedDownCase,
    Char_All,
    Char_AllFixed,
    Char_AllUpCase,
    Char_AllDownCase,
    Char_AllFixedUpCase,
    Char_AllFixedDownCase,
    Char_Hex,
    Char_HexFixed,
    Char_HexUpCase,
    Char_HexDownCase,
    Char_HexFixedUpCase,
    Char_HexFixedDownCase,
    Char_Binary,
    Char_BinaryFixed          : Result := FSpaceChar;
    {Char_Space               : Result := #32; //FSpaceChar?; //not Delphi compatible, see notes above}
     Char_HourSeparator       : Result := DefaultFormatSettings.TimeSeparator;
     Char_DateSeparator       : Result := DefaultFormatSettings.DateSeparator;
  end;
end;



//Insert a single Utf8 char at the current position of the cursor
procedure TCustomMaskEdit.InsertChar(Ch : TUtf8Char);
Var
  S: String;
  i, SelectionStart, SelectionStop: Integer;
begin
  if CanInsertChar(FCursorPos + 1, Ch) then
  begin
    S := inherited RealGetText;
    if HasSelection then
    begin
      //replace slection with blank chars
      //don't do this via DeleteChars(True), since it will do an unneccesary
      //update of the control and 2 TextChanged's are triggerd for every char we enter
      GetSel(SelectionStart, SelectionStop);
      for i := SelectionStart + 1 to SelectionStop do SetCodePoint(S, i, ClearChar(i));
    end;
    SetCodePoint(S, FCursorPos + 1, Ch);
    RealSetTextWhileMasked(S);
    SelectNextChar;
  end
  else
  //If we have a selection > 1 (and cannot insert) then Delete the selected text: Delphi compatibility
  if HasExtSelection then DeleteSelected;
end;


//Check if a Utf8 char can be inserted at position Position, also do case conversion if necessary
function TCustomMaskEdit.CanInsertChar(Position: Integer; var Ch: TUtf8Char;
  IsPasting: Boolean = False): Boolean;
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
     (Current = Char_AlphaNumFixedUpCase) or
     (Current = Char_HexUpCase          ) or
     (Current = Char_HexFixedUpCase     )

     then
       Ch := Utf8UpperCase(Ch);

  // If in LowerCase convert the input char
  if (Current = Char_LetterDownCase     ) Or
     (Current = Char_LetterFixedDownCase) Or
     (Current = Char_AllDownCase        ) Or
     (Current = Char_AllFixedDownCase   ) or
     (Current = Char_AlphaNumDownCase   ) or
     (Current = Char_AlphaNumFixedDownCase ) or
     (Current = Char_HexDownCase           ) or
     (Current = Char_HexFixedDownCase      )
     then
       Ch := Utf8LowerCase(Ch);

  // Check the input (check the valid range)
  case Current Of
       Char_Number              : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9']);
       Char_NumberFixed         : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9']);
       Char_NumberPlusMin       : Result := (Length(Ch) = 1) and (Ch[1] in ['0'..'9','+','-',#32]); //yes Delphi allows a space here
       Char_Letter              : Result := (Length(Ch) = 1) and (Ch[1] In ['a'..'z', 'A'..'Z']);
       Char_LetterFixed         : Result := (Length(Ch) = 1) and (Ch[1] In ['a'..'z', 'A'..'Z']);
       Char_LetterUpCase        : Result := (Length(Ch) = 1) and (Ch[1] In ['A'..'Z']);
       Char_LetterDownCase      : Result := (Length(Ch) = 1) and (Ch[1] In ['a'..'z']);
       Char_LetterFixedUpCase   : Result := (Length(Ch) = 1) and (Ch[1] In ['A'..'Z']);
       Char_LetterFixedDownCase : Result := (Length(Ch) = 1) and (Ch[1] In ['a'..'z']);
       Char_AlphaNum            : Result := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', 'A'..'Z', '0'..'9']);
       Char_AlphaNumFixed       : Result := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', 'A'..'Z', '0'..'9']);
       Char_AlphaNumUpCase      : Result := (Length(Ch) = 1) and (Ch[1] in ['A'..'Z', '0'..'9']);
       Char_AlphaNumDownCase    : Result := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', '0'..'9']);
       Char_AlphaNumFixedUpCase : Result := (Length(Ch) = 1) and (Ch[1] in ['A'..'Z', '0'..'9']);
       Char_AlphaNumFixedDowncase:Result := (Length(Ch) = 1) and (Ch[1] in ['a'..'z', '0'..'9']);
       Char_All                 : Result := True;
       Char_AllFixed            : Result := True;
       Char_AllUpCase           : Result := True;
       Char_AllDownCase         : Result := True;
       Char_AllFixedUpCase      : Result := True;
       Char_AllFixedDownCase    : Result := True;
      {Char_Space               : Result := Ch in [' ', '_'];  //not Delphi compatible, see notes above}
       Char_HourSeparator       : Result := (Ch = DefaultFormatSettings.TimeSeparator);
       Char_DateSeparator       : Result := (Ch = DefaultFormatSettings.DateSeparator);
       Char_Hex                 : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f','A'..'F',#32]);
       Char_HexFixed            : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f','A'..'F']);
       Char_HexUpCase           : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','A'..'F',#32]);
       Char_HexDownCase         : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f',#32]);
       Char_HexFixedUpCase      : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','A'..'F']);
       Char_HexFixedDownCase    : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'9','a'..'f']);
       Char_Binary              : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'1',#32]);
       Char_BinaryFixed         : Result := (Length(Ch) = 1) and (Ch[1] In ['0'..'1']);

  end;
  //while typing a space is not allowed in these cases, whilst pasting Delphi allows it nevertheless
  if not Result and IsPasting and (Ch = #32) and
    (Current in [Char_Number, Char_Letter, Char_LetterUpCase, Char_LetterDownCase,
                 Char_AlphaNum, Char_AlphaNumUpCase, Char_AlphaNumDownCase,
                 Char_Hex, Char_HexUpCase, Char_HexDownCase, Char_Binary]) then
    Result := True;
end;


// Delete selected chars
procedure TCustomMaskEdit.DeleteSelected;
Var
  SelectionStart, SelectionStop, I : Integer;
  S: String;
begin
  if not HasSelection then Exit;
  GetSel(SelectionStart, SelectionStop);
  S := inherited RealGetText;
  for i := SelectionStart + 1 to SelectionStop do SetCodePoint(S, i,ClearChar(i));
  RealSetTextWhileMasked(S);
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
      if FCursorPos < FMaskLength then
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
      //cannot backspace if we are at beginning of string, or if all chars in front are MaskLiterals
      if FCursorPos > FFirstFreePos - 1 then
      begin
        //This will select the previous character
        //If there are MaskLiterals just in front of the current position, they will be skipped
        //and the character in front of them will be deleted (Delphi compatibility)
        SelectPrevChar;
        DeleteSelected;
      end;
    end;
  end;
end;

function TCustomMaskEdit.ApplyMaskToText(Value: TCaption): TCaption;
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

  - The resulting text will always have length = FMaskLength
  - The text that is set, does not need to validate
}
//Helper functions
  Function FindNextMaskLiteral(const StartAt: Integer; out FoundAt: Integer; out ALiteral: TUtf8Char): Boolean;
  var i: Integer;
  begin
    Result := False;
    for i := StartAt to FMaskLength do
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
  Function FindMatchingLiteral(const Value: String; const ALiteral: TUtf8Char; out FoundAt: Integer): Boolean;
  begin
    FoundAt := Utf8Pos(ALiteral, Value);
    Result := (FoundAt > 0);
  end;

Var
  S                   : String;
  I, J                : Integer;
  mPrevLit, mNextLit  : Integer; //Position of Previous and Next literal in FMask
  vNextLit            : Integer; //Position of next matching literal in Value
  HasNextLiteral,
  HasMatchingLiteral,
  Stop                : Boolean;
  Literal             : TUtf8Char;
  Sub                 : String;
begin
  //First setup a "blank" string that contains all literals in the mask
  if not IsMasked then
  begin
    Result := Value;
    Exit;
  end;
  S := '';
  for I := 1 To FMaskLength do  S := S + ClearChar(I);

  if FMaskSave then
  begin
    mPrevLit := 0;
    Stop := False;
    HasNextLiteral := FindNextMaskLiteral(mPrevLit+1, mNextLit, Literal);
    //if FMask starts with a literal, then the first CodePoint of Value must be that literal
    if HasNextLiteral and (mNextLit = 1) and (GetCodePoint(Value, 1) <> Literal) then Stop := True;
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
          Sub := Utf8Copy(Value, 1, vNextLit - 1); //Copy up to, but not including matching literal
          Utf8Delete(Value, 1, vNextLit); //Remove this bit from Value (including matching literal)
          if (Utf8Length(Value) = 0) then Stop := True;
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
            if (J > Utf8Length(Sub)) then Break;
            if (GetCodePoint(Sub,j) = #32) then SetCodePoint(S,i,FSpaceChar) else SetcodePoint(S,i,GetCodePoint(Sub,j));
            Inc(j);
          end;
        end
        else
        begin//FTrimType = metTrimLeft
          j := Utf8Length(Sub);
          for i := (mNextLit - 1) downto (mPrevLit + 1) do
          begin
            if (j < 1) then Break;
            if (GetCodePoint(Sub,j) = #32) then SetCodePoint(S,i,FSpaceChar) else SetCodePoint(S,i, GetCodePoint(Sub,j));
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
          for i := (mPrevLit + 1) to FMaskLength do
          begin
            //debugln('  i = ',dbgs(i),'  j = ',dbgs(j));
            if (j > Utf8Length(Sub)) then Break;
            if (GetCodePoint(Sub,j) = #32) then SetCodePoint(S,i,FSpaceChar) else SetCodePoint(S,i, GetCodePoint(Sub,j));
            //debugln('  Sub[j] = "',Sub[j],'" -> S = ',S);
            Inc(j);
          end;
        end
        else
        begin//FTrimType = metTrimLeft
          j := Utf8Length(Sub);
          for i := FMaskLength downto (mPrevLit + 1) do
          begin
            //debugln('  i = ',dbgs(i),'  j = ',dbgs(j));
            if (j < 1) then Break;
            if (GetCodePoint(Sub,j) = #32) then SetCodePoint(S,i,FSpaceChar) else SetCodePoint(S,i, GetCodePoint(Sub,j));
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
      for i := 1 to FMaskLength do
      begin
        if not IsLiteral(FMask[i]) then
        begin
          if (GetCodePoint(Value,j) = #32) then SetCodePoint(S,i,FSpaceChar) else SetCodePoint(S,i, GetCodePoint(Value,j));
          Inc(j);
          if j > Utf8Length(Value) then Break;
        end;
      end;
    end
    else
    begin
      //fill text from right to left, skipping MaskLiterals
      j := Utf8Length(Value);
      for i := FMaskLength downto 1 do
      begin
        if not IsLiteral(FMask[i]) then
        begin
          if (GetCodePoint(Value,j) = #32) then SetCodePoint(S,i,FSpaceChar) else SetCodePoint(S,i, GetCodePoint(Value,j));
          Dec(j);
          if j < 1 then Break;
        end;
      end;
    end;
  end;//FMaskSave = False
  Result := S;
end;

function TCustomMaskEdit.CanShowEmulatedTextHint: Boolean;
begin
  if IsMasked then
    Result := False
  else
    Result := inherited CanShowEmulatedTextHint;
end;



// Get the actual Text
function TCustomMaskEdit.GetTextWithoutMask(Value: TCaption): TCaption;
{
  Replace al FSPaceChars with #32
  If FMaskSave = False then do trimming of spaces and remove all maskliterals
}
var
  S: String;
  i: Integer;
Begin
  S := StringReplace(Value, FSpaceChar, #32, [rfReplaceAll]);
  //FSpaceChar can be used as a literal in the mask, so put it back
  for i := 1 to FMaskLength do
  begin
    if IsLiteral(FMask[i]) and (FMask[i] = FSpaceChar) then
    begin
      SetCodePoint(S, i, FSpaceChar);
    end;
  end;
  if not FMaskSave then
  begin
    for i := 1 to FMaskLength do
    begin
      if IsLiteral(FMask[i]) then SetCodePoint(S, i, #1); //We know this char can never be in Text, so this is safe
    end;
    S := StringReplace(S, #1, '', [rfReplaceAll]);
    //Trimming only occurs if FMaskSave = False
    case FTrimType of
      metTrimLeft : S := TrimLeft(S);
      metTrimRight: S := TrimRight(S);
    end;//case
  end;
  Result := S;
End;

{
  Replace al FSPaceChars with #32
  Leave all mask literals in place
  Needed by FormatMaskText
}
function TCustomMaskEdit.GetTextWithoutSpaceChar(Value: TCaption): TCaption;
var
  i: Integer;
Begin
  Result := StringReplace(Value, FSpaceChar, #32, [rfReplaceAll]);
  //FSpaceChar can be used as a literal in the mask, so put it back
  for i := 1 to FMaskLength do
  begin
    if IsLiteral(FMask[i]) and (FMask[i] = FSpaceChar) then
    begin
      SetCodePoint(Result, i, FSpaceChar);
    end;
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
    //we do not call inherited TextChanged here, because the following RealSetTextWhileMasked
    //will trigger TextChanged with FChangeAllowed = True and inherited TextChanged is called then
    RealSetTextWhileMasked(FCurrentText);
    //Reset cursor to last known position
    SetCursorPos;
  end;
end;

procedure TCustomMaskEdit.Change;
begin
  //suppress OnChange when setting initiall values.
  if not FSettingInitialText then inherited Change;
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
    inherited MaxLength := FMaskLength;
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

procedure TCustomMaskEdit.SetNumbersOnly(Value: Boolean);
begin
  if not IsMasked then
    inherited SetNumbersOnly(Value)
  else
    //NumersOnly interferes with masking
    inherited SetNumbersOnly(False);
end;

procedure TCustomMaskEdit.Loaded;
begin
  inherited Loaded;
  FSettingInitialText := True;
  if (FInitialMask <> '') then SetMask(FInitialMask);
  if (FInitialText <> '') then SetTextApplyMask(FInitialText);
  FSettingInitialText := False;
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
    RealSetTextWhileMasked(FTextOnEnter);
  end;
end;

//Moved from CMEnter message handler
procedure TCustomMaskEdit.DoEnter;
begin
  inherited DoEnter;
  if IsMasked then
  begin
    //debugln('TCustomMaskEdit.DoEnter: FValidationFailed = ',DbgS(FValidationFailed));
    FCursorPos := GetSelStart;
    //Only save FTextOnEnter if validation did not fail in last DoExit that occurred
    if not FValidationFailed then
      FTextOnEnter := inherited RealGetText
    else
      FValidationFailed := False;
    Modified := False;
    if (AutoSelect and not (csLButtonDown in ControlState)) then
    begin
      SelectAll;
      FCursorPos := GetSelStart;
    end
    else
    begin
    if ((FCursorPos = 0) and (IsLiteral(FMask[1]))) then
      //On entering select first editable char
      SelectNextChar
    else
      SetCursorPos;
    end;
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
  if IsMasked and (FTextOnEnter <> inherited RealGetText) then
  begin
    //assume failure
    try
      //debugln('TCustomMaskedit.DoExit: try ValidateEdit');
      if (not FValidationFailed) then
      begin
        ValidateEdit;
        FValidationFailed := False;
      end ;
    finally
      //also check if control can be focussed, otherwise risk an exception while
      //handling an exception, issue #0030482
      if FValidationFailed and CanSetFocus then
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
    if ((inherited RealGetText) <> FTextOnEnter) then
    begin
      Reset;
      Key := 0;
      Exit;
    end;
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
      else if (Shift = [ssModifier]) then
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
    else if (Shift = [ssModifier]) then
    begin//Copy
      CopyToClipBoard;
    end;
    Key := 0;
    Exit;
  end;
  if (Key = VK_C) and (Shift = [ssModifier]) then
  begin//Copy
    CopyToClipBoard;
    Key := 0;
    Exit;
  end;
  if (Key = VK_X) and (Shift = [ssModifier]) then
  begin//Cut
    if not ReadOnly then
    begin
      CutToClipBoard;
      Key := 0;
      Exit;
    end;
  end;
  if (Key = VK_V) and (Shift = [ssModifier]) then
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


//Handle all keys from KeyPress and Utf8KeyPress here
procedure TCustomMaskEdit.HandleKeyPress(var Key: TUtf8Char);
begin
  if (not IsMasked) or ReadOnly then
  begin
    Exit;
  end;
  FCursorPos := GetSelStart;
  //If the cursor is on a MaskLiteral then go to the next writable position if a key is pressed (Delphi compatibility)
  if IsLiteral(FMask[FCursorPos + 1]) then
  begin
    SelectNextChar;
    Key := EmptyStr;
  end
  else
  // Insert a char
  if  not ((Length(Key) = 1) and (Key[1] in [#0..#31])) then
  begin
    if ((Key = Period) or (Key = Comma)) and not (CanInsertChar(FCursorPos + 1, Key)) then
    begin//Try to jump to next period or comma, if at all possible
      JumpToNextDot(Key[1]);
    end
    else
    begin//any other key
      InsertChar(Key);
    end;
    //We really need to "eat" all keys we handle ourselves
    //(or widgetset will insert char second time)
    Key:= EmptyStr;
  end;
end;


procedure TCustomMaskEdit.KeyPress(var Key: Char);
var
  Utf8Key: TUtf8Char;
begin
  inherited KeyPress(Key);
  Utf8Key := Key;
  //All keys are handled in HandleKeyPress, which sets Utf8Key to ''
  HandleKeyPress(Utf8Key);
  if (Length(Utf8Key) = 0) then Key := #0;
end;

procedure TCustomMaskEdit.Utf8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited Utf8KeyPress(UTF8Key);
  //All keys are handled in HandleKeyPress, which sets Utf8Key to ''
  //In Utf8KeyPress do this only for Utf8 sequences, otherwise KeyPress is never called
  //because after this Utf8Key = ''
  if (Length(Utf8Key) > 1) then HandleKeyPress(Utf8Key);
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
  CP: TUTF8Char;
begin
  if not IsMasked then
  begin
    inherited PasteFromClipBoard;
    Exit;
  end;
 if Clipboard.HasFormat(CF_TEXT) then
 begin
   //debugln('TCustomMaskEdit.PasteFromClipBoard A');
   ClipText := ClipBoard.AsText;
   if (Utf8Length(ClipText) > 0) then
   begin
     P := FCursorPos + 1;
     DeleteSelected;
     S := inherited RealGetText;
     i := 1;
     //debugln('TCustomMaskEdit.PasteFromClipBoard B:');
     //debugln('  P = ',dbgs(p));
     //debugln('  S = ',s);
     //debugln('  ClipText = ',ClipText);
     while (P <= FMaskLength) and (i <= Utf8Length(ClipText)) do
     begin
       //Skip any literal
       while (P < FMaskLength) and (IsLiteral(FMask[P])) do Inc(P);
       //debugln('TCustomMaskEdit.PasteFromClipBoard C: P = ',DbgS(p));
       //Skip any char in ClipText that cannot be inserted at current position
       CP := GetCodePoint(ClipText,i);
       //Replace all control characters with spaces
       if (Length(CP) = 1) and (CP[1] in [#0..#31]) then CP := #32;
       while (i < Utf8Length(ClipText)) and (not CanInsertChar(P, CP, True)) do
       begin
         Inc(i);
         CP := GetCodePoint(ClipText,i);
       end;
       if CanInsertChar(P, CP, True) then
       begin
         SetCodePoint(S,P,CP);
         Inc(P);
         Inc(i);
       end
       else
         Break;
     end;
     RealSetTextWhileMasked(S);
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
  if IsMasked then
  begin
    S  := '';
    for I := 1 To FMaskLength do S := S + ClearChar(I);
    RealSetTextWhileMasked(S);
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
      FValidationFailed := True;
      Raise EDBEditError.Create(SMaskEditNoMatch);
    end;
  end;
end;


{ Component registration procedure }
procedure Register;
begin
  RegisterComponents('Additional',[TMaskEdit]);
end;


initialization
  RegisterPropertyToSkip(TCustomMaskEdit, 'TextHintFontColor','Used in a previous version of Lazarus','');
  RegisterPropertyToSkip(TCustomMaskEdit, 'TextHintFontStyle','Used in a previous version of Lazarus','');

end.
