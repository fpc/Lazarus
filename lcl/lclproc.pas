{
 /***************************************************************************
                                  lclproc.pas
                                  -----------
                             Component Library Code


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Useful lower level helper functions and classes.
}
unit LCLProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLStrConsts, LCLType;

type
  { TMethodList - array of TMethod }

  TMethodList = class
  private
    FItems: ^TMethod;
    FCount: integer;
    function GetItems(Index: integer): TMethod;
    procedure SetItems(Index: integer; const AValue: TMethod);
  public
    destructor Destroy; override;
    function Count: integer;
    function NextDownIndex(var Index: integer): boolean;
    function IndexOf(const AMethod: TMethod): integer;
    procedure Delete(Index: integer);
    procedure Remove(const AMethod: TMethod);
    procedure Add(const AMethod: TMethod);
    procedure Add(const AMethod: TMethod; AsLast: boolean);
    procedure Insert(Index: integer; const AMethod: TMethod);
    procedure Move(OldIndex, NewIndex: integer);
    procedure RemoveAllMethodsOfObject(const AnObject: TObject);
  public
    property Items[Index: integer]: TMethod read GetItems write SetItems; default;
  end;


function ShortCutToText(ShortCut: TShortCut): string;
function TextToShortCut(const ShortCutText: string): TShortCut;

// Hooks used to prevent unit circles
type
  TSendApplicationMessageFunction =
    function(Msg: Cardinal; WParam: WParam; LParam: LParam):Longint;
  TOwnerFormDesignerModifiedProc =
    procedure(AComponent: TComponent);
  TSendMessageToInterfaceFunction =
    function(LM_Message: Integer; Sender: TObject; data: pointer): integer
             of object;
             

var
  SendApplicationMessageFunction: TSendApplicationMessageFunction;
  OwnerFormDesignerModifiedProc: TOwnerFormDesignerModifiedProc;

function SendApplicationMessage(Msg: Cardinal; WParam: WParam; LParam: LParam):Longint;
procedure OwnerFormDesignerModified(AComponent: TComponent);
procedure FreeThenNil(var AnObject: TObject);

{ the LCL interfaces finalization sections are called before the finalization
  sections of the LCL. Those parts, that should be finalized after the LCL, can
  be registered here. }
procedure RegisterInterfaceFinalizationHandler(p: TProcedure);
procedure CallInterfaceFinalizationHandlers;

function OffsetRect(var ARect: TRect; dx, dy: Integer): Boolean;
procedure MoveRect(var ARect: TRect; x, y: Integer);
procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
procedure MakeMinMax(var i1, i2: integer);
procedure CalculateLeftTopWidthHeight(X1,Y1,X2,Y2: integer;
  var Left,Top,Width,Height: integer);

function DeleteAmpersands(var Str : String) : Longint;
function BreakString(const s: string; MaxLineLength, Indent: integer): string;

function ComparePointers(p1, p2: Pointer): integer;
function CompareHandles(h1, h2: THandle): integer;
function CompareRect(R1, R2: PRect): Boolean;


function RoundToInt(const e: Extended): integer;
function RoundToCardinal(const e: Extended): cardinal;
function TruncToInt(const e: Extended): integer;
function TruncToCardinal(const e: Extended): cardinal;
function StrToDouble(const s: string): double;


// debugging
procedure RaiseGDBException(const Msg: string);

procedure DebugLn(const S: String; Args: array of const);
procedure DebugLn;
procedure DebugLn(const s: string);
procedure DebugLn(const s1,s2: string);
procedure DebugLn(const s1,s2,s3: string);
procedure DebugLn(const s1,s2,s3,s4: string);
procedure DebugLn(const s1,s2,s3,s4,s5: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16: string);

procedure DbgOut(const s: string);
procedure DbgOut(const s1,s2: string);
procedure DbgOut(const s1,s2,s3: string);
procedure DbgOut(const s1,s2,s3,s4: string);
procedure DbgOut(const s1,s2,s3,s4,s5: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string);

function DbgS(const c: cardinal): string;
function DbgS(const i: integer): string;
function DbgS(const r: TRect): string;
function DbgS(const p: TPoint): string;
function DbgS(const p: pointer): string;
function DbgS(const e: extended): string;
function DbgS(const b: boolean): string;
function DbgSName(const p: TObject): string;
function DbgStr(const StringWithSpecialChars: string): string;

function DbgS(const i1,i2,i3,i4: integer): string;

// UTF utility functions
// MG: Should be moved to the RTL
function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): integer;
function UTF8Length(p: PChar; Count: integer): integer;
function UTF8CharacterToUnicode(p: PChar; var CharLen: integer): Cardinal;
function UnicodeToUTF8(u: cardinal): string;
function UTF8ToDoubleByteString(const s: string): string;
function UTF8ToDoubleByte(UTF8Str: PChar; Len: integer; DBStr: PByte): integer;
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
                                  BytePos: integer): integer;
// find the n-th UTF8 character, ignoring BIDI
function UTF8CharStart(UTF8Str: PChar; Len, Index: integer): PChar;


// ======================================================================
// Endian utility functions
// MWE: maybe to RTL ?
// inline ?
//
// These functions convert a BigEndian or LittleEndian number to 
// a machine Native number and vice versa.
//
// Note: Lazarus resources are streamed using LE. So when writing data
//       use NtoLE(your_value), when reading use LEtoN(read_value)
// ======================================================================

function BEtoN(const AValue: SmallInt): SmallInt;
function BEtoN(const AValue: Word): Word;
function BEtoN(const AValue: LongInt): LongInt;
function BEtoN(const AValue: DWord): DWord;
{$IFNDEF VER1_0} // fpc 1.0.x can't handle 64 bits constants
function BEtoN(const AValue: Int64): Int64;
function BEtoN(const AValue: QWord): QWord;
{$ENDIF}

function LEtoN(const AValue: SmallInt): SmallInt;
function LEtoN(const AValue: Word): Word;
function LEtoN(const AValue: LongInt): LongInt;
function LEtoN(const AValue: DWord): DWord;
{$IFNDEF VER1_0} // fpc 1.0.x can't handle 64 bits constants
function LEtoN(const AValue: Int64): Int64;
function LEtoN(const AValue: QWord): QWord;
{$ENDIF}

function NtoBE(const AValue: SmallInt): SmallInt;
function NtoBE(const AValue: Word): Word;
function NtoBE(const AValue: LongInt): LongInt;
function NtoBE(const AValue: DWord): DWord;
{$IFNDEF VER1_0} // fpc 1.0.x can't handle 64 bits constants
function NtoBE(const AValue: Int64): Int64;
function NtoBE(const AValue: QWord): QWord;
{$ENDIF}

function NtoLE(const AValue: SmallInt): SmallInt;
function NtoLE(const AValue: Word): Word;
function NtoLE(const AValue: LongInt): LongInt;
function NtoLE(const AValue: DWord): DWord;
{$IFNDEF VER1_0} // fpc 1.0.x can't handle 64 bits constants
function NtoLE(const AValue: Int64): Int64;
function NtoLE(const AValue: QWord): QWord;
{$ENDIF}


implementation

var
  InterfaceFinalizationHandlers: TList;
  DebugText: ^Text;


Function DeleteAmpersands(var Str : String) : Longint;
// Replace all &x with x
// and return the position of the first ampersand letter in the resulting Str.
// double ampersands && are converted to a single & and are ignored.
var
  SrcPos, DestPos, SrcLen: Integer;
begin
  Result:=-1;
  SrcLen:=length(Str);
  SrcPos:=1;
  DestPos:=1;
  while SrcPos<=SrcLen do begin
    if (Str[SrcPos]='&') and (SrcPos<SrcLen) then begin
      // & found
      inc(SrcPos); // skip &
      if (Str[SrcPos]<>'&') and (Result<1) then
        Result:=DestPos;
    end;
    if DestPos<SrcPos then
      Str[DestPos]:=Str[SrcPos];
    inc(SrcPos);
    inc(DestPos);
  end;
  if DestPos<SrcPos then
    SetLength(Str,DestPos-1);
end;

//-----------------------------------------------------------------------------
// Keys and shortcuts

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt);

const
  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Ins';
  SmkcDel = 'Del';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  MenuKeyCaps: array[TMenuKeyCap] of string = (
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight,
    SmkcDown, SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt);

function GetSpecialShortCutName(ShortCut: TShortCut): string;
{var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;}
begin
  Result := '';
  // ToDo:
  {
  ScanCode := MapVirtualKey(WordRec(ShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    Result := KeyName;
  end;
  }
end;

function ShortCutToText(ShortCut: TShortCut): string;
var
  Name: string;
begin
  case WordRec(ShortCut).Lo of
    $08, $09:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + WordRec(ShortCut).Lo - $08)];
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + WordRec(ShortCut).Lo - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + WordRec(ShortCut).Lo - $2D)];
    $30..$39: Name := Chr(WordRec(ShortCut).Lo - $30 + Ord('0'));
    $41..$5A: Name := Chr(WordRec(ShortCut).Lo - $41 + Ord('A'));
    $60..$69: Name := Chr(WordRec(ShortCut).Lo - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(WordRec(ShortCut).Lo - $6F);
  else
    Name := GetSpecialShortCutName(ShortCut);
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
    if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
    if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;

function TextToShortCut(const ShortCutText: string): TShortCut;

  function CompareFront(var StartPos: integer; const Front: string): Boolean;
  begin
    if (Front<>'') and (StartPos+length(Front)-1<=length(ShortCutText))
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Front), Length(Front))= 0)
    then begin
      Result:=true;
      inc(StartPos,length(Front));
    end else
      Result:=false;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
  StartPos: integer;
  Name: string;
begin
  Result := 0;
  Shift := 0;
  StartPos:=1;
  while True do
  begin
    if CompareFront(StartPos, MenuKeyCaps[mkcShift]) then
      Shift := Shift or scShift
    else if CompareFront(StartPos, '^') then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcCtrl]) then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcAlt]) then
      Shift := Shift or scAlt
    else
      Break;
  end;
  if ShortCutText = '' then Exit;
  for Key := $08 to $255 do begin { Copy range from table in ShortCutToText }
    Name:=ShortCutToText(Key);
    if (Name<>'') and (length(Name)=length(ShortCutText)-StartPos+1)
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Name), length(Name)) = 0)
    then begin
      Result := Key or Shift;
      Exit;
    end;
  end;
end;

function SendApplicationMessage(Msg: Cardinal; WParam: WParam; LParam: LParam
  ): Longint;
begin
  if SendApplicationMessageFunction<>nil then
    Result:=SendApplicationMessageFunction(Msg, WParam, LParam)
  else
    Result:=0;
end;

procedure OwnerFormDesignerModified(AComponent: TComponent);
begin
  if ([csDesigning,csLoading,csDestroying]*AComponent.ComponentState
    =[csDesigning])
  then begin
    if OwnerFormDesignerModifiedProc<>nil then
      OwnerFormDesignerModifiedProc(AComponent);
  end;
end;

function OffSetRect(var ARect: TRect; dx,dy: Integer): Boolean;
Begin
  with ARect do
  begin
    Left := Left + dx;
    Right := Right + dx;
    Top := Top + dy;
    Bottom := Bottom + dy;
  end;
  if (ARect.Left >= 0) and (ARect.Top >= 0) then
    Result := True
  else
    Result := False;
end;

procedure FreeThenNil(var AnObject: TObject);
begin
  if AnObject<>nil then begin
    AnObject.Free;
    AnObject:=nil;
  end;
end;

procedure RegisterInterfaceFinalizationHandler(p: TProcedure);
begin
  InterfaceFinalizationHandlers.Add(p);
end;

procedure CallInterfaceFinalizationHandlers;
var
  i: Integer;
begin
  for i:=0 to InterfaceFinalizationHandlers.Count-1 do
    TProcedure(InterfaceFinalizationHandlers[i])();
end;

{ TMethodList }

function TMethodList.GetItems(Index: integer): TMethod;
begin
  Result:=FItems[Index];
end;

procedure TMethodList.SetItems(Index: integer; const AValue: TMethod);
begin
  FItems[Index]:=AValue;
end;

destructor TMethodList.Destroy;
begin
  ReAllocMem(FItems,0);
  inherited Destroy;
end;

function TMethodList.Count: integer;
begin
  if Self<>nil then
    Result:=FCount
  else
    Result:=0;
end;

function TMethodList.NextDownIndex(var Index: integer): boolean;
begin
  if Self<>nil then begin
    dec(Index);
    if (Index>=FCount) then
      Index:=FCount-1;
  end else
    Index:=-1;
  Result:=(Index>=0);
end;

function TMethodList.IndexOf(const AMethod: TMethod): integer;
begin
  if Self<>nil then begin
    Result:=FCount-1;
    while Result>=0 do begin
      if (FItems[Result].Code=AMethod.Code)
      and (FItems[Result].Data=AMethod.Data) then exit;
      dec(Result);
    end;
  end else
    Result:=-1;
end;

procedure TMethodList.Delete(Index: integer);
begin
  dec(FCount);
  if FCount>Index then
    System.Move(FItems[Index+1],FItems[Index],(FCount-Index)*SizeOf(TMethod));
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
end;

procedure TMethodList.Remove(const AMethod: TMethod);
var
  i: integer;
begin
  if Self<>nil then begin
    i:=IndexOf(AMethod);
    if i>=0 then Delete(i);
  end;
end;

procedure TMethodList.Add(const AMethod: TMethod);
begin
  inc(FCount);
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
  FItems[FCount-1]:=AMethod;
end;

procedure TMethodList.Add(const AMethod: TMethod; AsLast: boolean);
begin
  if AsLast then
    Add(AMethod)
  else
    Insert(0,AMethod);
end;

procedure TMethodList.Insert(Index: integer; const AMethod: TMethod);
begin
  if Index<FCount then
    System.Move(FItems[Index],FItems[Index+1],(FCount-Index)*SizeOf(TMethod));
  FItems[Index]:=AMethod;
end;

procedure TMethodList.Move(OldIndex, NewIndex: integer);
var
  MovingMethod: TMethod;
begin
  if OldIndex=NewIndex then exit;
  MovingMethod:=FItems[OldIndex];
  if OldIndex>NewIndex then
    System.Move(FItems[NewIndex],FItems[NewIndex+1],
                SizeOf(TMethod)*(OldIndex-NewIndex))
  else
    System.Move(FItems[NewIndex+1],FItems[NewIndex],
                SizeOf(TMethod)*(NewIndex-OldIndex));
  FItems[NewIndex]:=MovingMethod;
end;

procedure TMethodList.RemoveAllMethodsOfObject(const AnObject: TObject);
var
  i: Integer;
begin
  if Self=nil then exit;
  i:=FCount-1;
  while i>=0 do begin
    if TObject(FItems[i].Data)=AnObject then Delete(i);
    dec(i);
  end;
end;

{------------------------------------------------------------------------------
  procedure RaiseGDBException(const Msg: string);

  Raises an exception.
  gdb does normally not catch fpc Exception objects, therefore this procedure
  raises a standard AV which is catched by gdb.
 ------------------------------------------------------------------------------}
procedure RaiseGDBException(const Msg: string);
begin
  debugln(rsERRORInLCL, Msg);
  // creates an exception, that gdb catches:
  debugln(rsCreatingGdbCatchableError);
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

procedure MoveRect(var ARect: TRect; x, y: Integer);
begin
  inc(ARect.Right,x-ARect.Left);
  inc(ARect.Bottom,y-ARect.Top);
  ARect.Left:=x;
  ARect.Top:=y;
end;

procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
// move ARect, so it fits into MaxRect
// if MaxRect is too small, ARect is resized.
begin
  if ARect.Left<MaxRect.Left then begin
    // move rectangle right
    ARect.Right:=Min(ARect.Right+MaxRect.Left-ARect.Left,MaxRect.Right);
    ARect.Left:=MaxRect.Left;
  end;
  if ARect.Top<MaxRect.Top then begin
    // move rectangle down
    ARect.Bottom:=Min(ARect.Bottom+MaxRect.Top-ARect.Top,MaxRect.Bottom);
    ARect.Top:=MaxRect.Top;
  end;
  if ARect.Right>MaxRect.Right then begin
    // move rectangle left
    ARect.Left:=Max(ARect.Left-ARect.Right+MaxRect.Right,MaxRect.Left);
    ARect.Right:=MaxRect.Right;
  end;
  if ARect.Bottom>MaxRect.Bottom then begin
    // move rectangle left
    ARect.Top:=Max(ARect.Top-ARect.Bottom+MaxRect.Bottom,MaxRect.Top);
    ARect.Bottom:=MaxRect.Bottom;
  end;
end;

procedure MakeMinMax(var i1, i2: integer);
var
  h: Integer;
begin
  if i1>i2 then begin
    h:=i1;
    i1:=i2;
    i2:=h;
  end;
end;

procedure CalculateLeftTopWidthHeight(X1, Y1, X2, Y2: integer;
  var Left, Top, Width, Height: integer);
begin
  if X1<=X2 then begin
    Left:=X1;
    Width:=X2 - X1;
  end else begin
    Left:=X2;
    Width:=X1 - X2;
  end;
  if Y1<=Y2 then begin
    Top:=Y1;
    Height:=Y2 - Y1;
  end else begin
    Top:=Y2;
    Height:=Y1 - Y2;
  end;
end;

function BreakString(const s: string; MaxLineLength, Indent: integer): string;
var
  SrcLen: Integer;
  APos: Integer;
  Src: String;
  SplitPos: Integer;
  CurMaxLineLength: Integer;
begin
  Result:='';
  Src:=s;
  CurMaxLineLength:=MaxLineLength;
  if Indent>MaxLineLength-2 then Indent:=MaxLineLength-2;
  if Indent<0 then MaxLineLength:=0;
  repeat
    SrcLen:=length(Src);
    if SrcLen<=CurMaxLineLength then begin
      Result:=Result+Src;
      break;
    end;
    // split line
    SplitPos:=0;
    // search new line chars
    APos:=1;
    while (APos<=CurMaxLineLength) do begin
      if Src[APos] in [#13,#10] then begin
        SplitPos:=APos;
        break;
      end;
      inc(APos);
    end;
    // search a space boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos-1] in [' ',#9])
        and (not (Src[APos] in [' ',#9])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    // search a word boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos] in ['A'..'Z','a'..'z'])
        and (not (Src[APos-1] in ['A'..'Z','a'..'z'])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    if SplitPos=0 then begin
      // no word boundary found -> split chars
      SplitPos:=CurMaxLineLength;
    end;
    // append part and newline
    if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13]) then begin
      // there is already a new line char at position
      inc(SplitPos);
      if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13])
      and (Src[SplitPos]<>Src[SplitPos-1]) then
        inc(SplitPos);
      Result:=Result+copy(Src,1,SplitPos-1);
    end else begin
      Result:=Result+copy(Src,1,SplitPos-1)+LineEnding;
    end;
    // append indent
    if Indent>0 then
      Result:=Result+StringOfChar(' ',Indent);
    // calculate new LineLength
    CurMaxLineLength:=MaxLineLength-Indent;
    // cut string
    Src:=copy(Src,SplitPos,length(Src)-SplitPos+1);
  until false;
end;

function ComparePointers(p1, p2: Pointer): integer;
begin
  if p1>p2 then
    Result:=1
  else if p1<p2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareHandles(h1, h2: THandle): integer;
begin
  if h1>h2 then
    Result:=1
  else if h1<h2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareRect(R1, R2: PRect): Boolean;
begin
  Result:=(R1^.Left=R2^.Left) and (R1^.Top=R2^.Top) and
          (R1^.Bottom=R2^.Bottom) and (R1^.Right=R2^.Right);
  {if not Result then begin
    DebugLn(' DIFFER: ',R1^.Left,',',R1^.Top,',',R1^.Right,',',R1^.Bottom
      ,' <> ',R2^.Left,',',R2^.Top,',',R2^.Right,',',R2^.Bottom);
  end;}
end;

function RoundToInt(const e: Extended): integer;
begin
  Result:=integer(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToInt ',e,' ',Result);
  {$ENDIF}
end;

function RoundToCardinal(const e: Extended): cardinal;
begin
  Result:=cardinal(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function TruncToInt(const e: Extended): integer;
begin
  Result:=integer(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToInt ',e,' ',Result);
  {$ENDIF}
end;

function TruncToCardinal(const e: Extended): cardinal;
begin
  Result:=cardinal(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function StrToDouble(const s: string): double;
begin
  {$IFDEF VerboseRound}
  DebugLn('StrToDouble "',s,'"');
  {$ENDIF}
  Result:=Double(StrToFloat(s));
end;

procedure InitializeDebugOutput;
var
  DebugFileName: string;
  function GetDebugFileName: string;
  const
    DebugLogStart = '--debug-log=';
    DebugLogStartLength = {$IFNDEF VER1_0}length(DebugLogStart){$ELSE}12{$ENDIF};
  var
    i: integer;
    EnvVarName: string;
  begin
    Result := '';
    // first try to find the log file name in the command line parameters
    for i:= 1 to Paramcount do begin
      if copy(ParamStr(i),1, DebugLogStartLength)=DebugLogStart then begin
        Result := copy(ParamStr(i), DebugLogStartLength+1,
                   Length(ParamStr(i))-DebugLogStartLength);
      end;
    end;
    // if not found yet, then try to find in the environment variables
    if (length(result)=0) then begin
      EnvVarName:= ChangeFileExt(ExtractFileName(Paramstr(0)),'') + '_debuglog';
      Result := GetEnvironmentVariable(EnvVarName);
    end;
    if (length(result)>0) then
      Result := ExpandFileName(Result);
  end;
begin
  DebugFileName := GetDebugFileName;
  if (length(DebugFileName)>0) and
    (DirectoryExists(ExtractFileDir(DebugFileName))) then begin

    new(DebugText);
    Assign(DebugText^, DebugFileName);
    if FileExists(DebugFileName) then
      Append(DebugText^)
    else
      Rewrite(DebugText^);
    writeln(DebugText^,'Created.');
  end
  else begin
    if TextRec(Output).Mode=fmClosed then
      DebugText := nil
    else
      DebugText := @Output;
  end;
end;

procedure FinalizeDebugOutput;
begin
  if Assigned(DebugText) and (DebugText<>@Output) then begin
    Close(DebugText^);
    Dispose(DebugText);
  end;
end;

procedure DebugLn(const S: String; Args: array of const);
begin
  DebugLn(Format(S, Args));
end;

procedure DebugLn;
begin
  DebugLn('');
end;

procedure DebugLn(const s: string);
begin
  if Assigned(DebugText) then
    writeln(DebugText^, s);
end;

procedure DebugLn(const s1, s2: string);
begin
  DebugLn(s1+s2);
end;

procedure DebugLn(const s1, s2, s3: string);
begin
  DebugLn(s1+s2+s3);
end;

procedure DebugLn(const s1, s2, s3, s4: string);
begin
  DebugLn(s1+s2+s3+s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: string);
begin
  DebugLn(s1+s2+s3+s4+s5);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
  s12: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
  s13: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14, s15: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14, s15, s16: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16);
end;

procedure DBGOut(const s: string);
begin
  if Assigned(DebugText) then
    write(DebugText^, s);
end;

procedure DBGOut(const s1, s2: string);
begin
  DbgOut(s1+s2);
end;

procedure DbgOut(const s1, s2, s3: string);
begin
  DbgOut(s1+s2+s3);
end;

procedure DbgOut(const s1, s2, s3, s4: string);
begin
  DbgOut(s1+s2+s3+s4);
end;

procedure DbgOut(const s1, s2, s3, s4, s5: string);
begin
  DbgOut(s1+s2+s3+s4+s5);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12: string
  );
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;

function DbgS(const c: cardinal): string;
begin
  Result:=IntToStr(c);
end;

function DbgS(const i: integer): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const r: TRect): string;
begin
  Result:=' x1='+IntToStr(r.Left)+',y1='+IntToStr(r.Top)
         +',x2='+IntToStr(r.Right)+',y2='+IntToStr(r.Bottom);
end;

function DbgS(const p: TPoint): string;
begin
  Result:=' x='+IntToStr(p.x)+',y='+IntToStr(p.y);
end;

function DbgS(const p: pointer): string;
begin
  Result:=HexStr(Cardinal(p),8);
end;

function DbgS(const e: extended): string;
begin
  Result:=FloatToStr(e);
end;

function DbgS(const b: boolean): string;
begin
  if b then Result:='True' else Result:='False';
end;

function DbgSName(const p: TObject): string;
begin
  if p=nil then
    Result:='nil'
  else if p is TComponent then
    Result:=TComponent(p).Name+':'+p.ClassName
  else
    Result:=p.ClassName;
end;

function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    case Result[i] of
    ' '..#126: inc(i);
    else
      s:='#'+IntToStr(ord(Result[i]));
      Result:=copy(Result,1,i-1)+s+copy(Result,i+1,length(Result)-i);
      inc(i,length(s));
    end;
  end;
end;

function DbgS(const i1, i2, i3, i4: integer): string;
begin
  Result:=dbgs(i1)+','+dbgs(i2)+','+dbgs(i3)+','+dbgs(i4);
end;

function UTF8CharacterLength(p: PChar): integer;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is single byte, this is pascal ;)
      Result:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // could be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        Result:=2
      else
        Result:=1;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // could be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then
        Result:=3
      else
        Result:=1;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then
        Result:=4
      else
        Result:=1;
    end
    else
      Result:=1
  end else
    Result:=0;
end;

function UTF8Length(const s: string): integer;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; Count: integer): integer;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (Count>0) do begin
    inc(Result);
    CharLen:=UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(Count,CharLen);
  end;
end;

function UTF8CharacterToUnicode(p: PChar; var CharLen: integer): Cardinal;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CharLen:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // could be double byte character
      if (ord(p[1]) and %11000000) = %10000000 then begin
        Result:=((ord(p^) and %00011111) shl 6)
                or (ord(p[1]) and %00111111);
        CharLen:=2;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // could be triple byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        Result:=((ord(p^) and %00011111) shl 12)
                or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
        CharLen:=3;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        Result:=((ord(p^) and %00011111) shl 18)
                or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6)
                or (ord(p[3]) and %00111111);
        CharLen:=4;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else begin
      Result:=ord(p^);
      CharLen:=1;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UnicodeToUTF8(u: cardinal): string;

  procedure RaiseInvalidUnicode;
  begin
    raise Exception.Create('UnicodeToUTF8: invalid unicode: '+IntToStr(u));
  end;

begin
  case u of
    0..$7f:
      begin
        SetLength(Result,1);
        Result[1]:=char(byte(u));
      end;
    $80..$7ff:
      begin
        SetLength(Result,2);
        Result[1]:=char(byte($c0 or (u shr 6)));
        Result[2]:=char(byte($80 or (u and $3f)));
      end;
    $800..$ffff:
      begin
        SetLength(Result,3);
        Result[1]:=char(byte($e0 or (u shr 12)));
        Result[2]:=char(byte((u shr 6) and $3f) or $80);
        Result[3]:=char(byte(u and $3f) or $80);
      end;
    $10000..$1fffff:
      begin
        SetLength(Result,4);
        Result[1]:=char(byte($f0 or (u shr 18)));
        Result[2]:=char(byte((u shr 12) and $3f) or $80);
        Result[3]:=char(byte((u shr 6) and $3f) or $80);
        Result[4]:=char(byte(u and $3f) or $80);
      end;
  else
    RaiseInvalidUnicode;
  end;
end;

function UTF8ToDoubleByteString(const s: string): string;
var
  Len: Integer;
begin
  Len:=UTF8Length(s);
  SetLength(Result,Len*2);
  if Len=0 then exit;
  UTF8ToDoubleByte(PChar(s),length(s),PByte(Result));
end;

function UTF8ToDoubleByte(UTF8Str: PChar; Len: integer; DBStr: PByte): integer;
// returns number of double bytes
var
  SrcPos: PChar;
  CharLen: LongInt;
  DestPos: PByte;
  u: Cardinal;
begin
  SrcPos:=UTF8Str;
  DestPos:=DBStr;
  Result:=0;
  while Len>0 do begin
    u:=UTF8CharacterToUnicode(SrcPos,CharLen);
    DestPos^:=byte((u shr 8) and $ff);
    inc(DestPos);
    DestPos^:=byte(u and $ff);
    inc(DestPos);
    inc(SrcPos,CharLen);
    dec(Len,CharLen);
    inc(Result);
  end;
end;

function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
var
  CharLen: LongInt;
begin
  Result:=0;
  if UTF8Str<>nil then begin
    if BytePos>Len then BytePos:=Len;
    while (BytePos>0) do begin
      CharLen:=UTF8CharacterLength(UTF8Str);
      dec(BytePos,CharLen);
      if (BytePos<0) then exit;
      inc(Result,CharLen);
      if (BytePos=0) then exit;
    end;
  end;
end;

function UTF8CharStart(UTF8Str: PChar; Len, Index: integer): PChar;
var
  CharLen: LongInt;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (Index>0) and (Len>0) do begin
      CharLen:=UTF8CharacterLength(Result);
      dec(Len,CharLen);
      dec(Index);
      inc(Result,CharLen);
    end;
    if (Index>0) or (Len<0) then
      Result:=nil;
  end;
end;

//==============================================================================
// Endian utils
//==============================================================================

function BEtoN(const AValue: SmallInt): SmallInt;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function BEtoN(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function BEtoN(const AValue: LongInt): LongInt;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

function BEtoN(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

{$IFNDEF VER1_0}
function BEtoN(const AValue: Int64): Int64;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;

function BEtoN(const AValue: QWord): QWord;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;
{$ENDIF}

function LEtoN(const AValue: SmallInt): SmallInt;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function LEtoN(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function LEtoN(const AValue: LongInt): LongInt;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

function LEtoN(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

{$IFNDEF VER1_0}
function LEtoN(const AValue: Int64): Int64;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;

function LEtoN(const AValue: QWord): QWord;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;
{$ENDIF}

function NtoBE(const AValue: SmallInt): SmallInt;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function NtoBE(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function NtoBE(const AValue: LongInt): LongInt;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

function NtoBE(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

{$IFNDEF VER1_0}
function NtoBE(const AValue: Int64): Int64;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;

function NtoBE(const AValue: QWord): QWord;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;
{$ENDIF}

function NtoLE(const AValue: SmallInt): SmallInt;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function NtoLE(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shr 8) or (AValue shl 8);
  {$ENDIF}
end;

function NtoLE(const AValue: LongInt): LongInt;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

function NtoLE(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;

{$IFNDEF VER1_0}
function NtoLE(const AValue: Int64): Int64;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;

function NtoLE(const AValue: QWord): QWord;
begin
  {$IFDEF ENDIAN_LITTLE}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  {$ENDIF}
end;
{$ENDIF}

initialization
  InitializeDebugOutput;
  SendApplicationMessageFunction:=nil;
  OwnerFormDesignerModifiedProc:=nil;
  InterfaceFinalizationHandlers:=TList.Create;
finalization
  InterfaceFinalizationHandlers.Free;
  InterfaceFinalizationHandlers:=nil;
  FinalizeDebugOutput;

end.

