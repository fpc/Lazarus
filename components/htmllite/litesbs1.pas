{Version 7.5}
{*********************************************************}
{*                     LITESBS1.PAS                      *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i LiteCons.inc}

unit LiteSbs1;

interface
uses
  {$IFDEF HL_LAZARUS}
  Classes, SysUtils, LCLType, LCLLinux, Messages, GraphType, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, LiteUn2, LiteSubs;
  {$ELSE}
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, LiteUn2, LiteGif2, LiteSubs;
  {$ENDIF}

Type

  TParagraphSpace = class(TSectionBase)  {spacing for a <p>}
    procedure UpdateSpacing; override;
    procedure CopyToClipboard; override;
    end;

  THeadingSpace = class(TSectionBase)     {spacing for <Hn>}
    HeadingSize: integer;

    constructor Create(AMasterList: TSectionList; AHeadingSize: integer);
    procedure CopyToClipboard; override;
    procedure UpdateSpacing; override;
    end;

  THorzLine = class(TSectionBase)          {a horizontal line, <hr>}
    VSize: integer;
    HWidth: integer;
    AsPercent: boolean;
    Color: TColor;
    Align: JustifyType;
    NoShade: boolean;        
    BkGnd: boolean;
    constructor Create(AMasterList: TSectionList; L: TAttributeList);
    procedure CopyToClipboard; override;
    function DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X : integer; Y: integer) : integer;  override;
    procedure UpdateSpacing; override;
    end;

   TPreFormated = class(TSection)
  {section for preformated, <pre>}
  public
    procedure AddTokenObj(S : TokenObj; NoBreak: boolean); override;   
    function DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    end;

 TUListItem = class(TSection)     {Unordered List}
    Plain: boolean;
    constructor Create(AMasterList: TSectionList;
      {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF}: integer; AFont: TMyFont;
                         AnURL: TUrlTarget);
    end;

  TDListItem = class(TUListItem)     {Definition List}
    constructor Create(AMasterList: TSectionList;
      {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF}: integer; AFont:
                TMyFont; AnURL: TUrlTarget);
    end;

  TOListItem = class(TUListItem)    {Ordered List}
    IndexType: char;  {1,a,A,i,I}
    constructor Create(AMasterList: TSectionList;
      {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF}, ItemNumb: integer;
                Index: char; AFont: TMyFont; AnURL: TUrlTarget);
    end;

  TListBoxFormControlObj = class(TFormControlObj)
  {<select> with Multiple set or Size > 1}
  public
    LBSize, Longest: integer;
    TheOptions: TStringList;
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    destructor Destroy; override;
    procedure AddStr(const S,
      {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}: string; Selected: boolean);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  TComboFormControlObj = class(TListBoxFormControlObj)
  {<select> with size = 1, no multiple}
  public
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  TTextAreaFormControlObj = class(TFormControlObj)
  public
    Rows, Cols: integer;
    TheText: TStringList;
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    destructor Destroy; override;
    procedure AddStr(const S: string);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  TFormControlList = class(TList)  {a list of TFormControlObj's}  {not TFreeList}
  Public
    function FindControl(Posn: integer): TFormControlObj;
    function GetHeightAt(Posn: integer; var BaseLine: boolean) : Integer;
    function GetWidthAt(Posn: integer) : integer;
    function GetControlCountAt(Posn: integer): integer;
    end;


Implementation

uses
  LitePars, htmllite;

{----------------TParagraphSpace.UpdateSpacing}
procedure TParagraphSpace.UpdateSpacing;
begin
SectionHeight := MulDiv(14, ParentSectionList.FontSize, 12);   {scale to FontSize}
end;

procedure TParagraphSpace.CopyToClipboard;
begin
ParentSectionList.CB.AddTextCr('', 0);
end;

{----------------THeadingSpace.Create}
constructor THeadingSpace.Create(AMasterList: TSectionList; AHeadingSize: integer);
begin
inherited Create(AMasterList);
HeadingSize := AHeadingSize;
end;

procedure THeadingSpace.CopyToClipboard;
begin
ParentSectionList.CB.AddTextCR('', 0);
end;

procedure THeadingSpace.UpdateSpacing;
var
  SH: integer;
begin
case HeadingSize of    {these are just a guess}
  0: SH := 8;
  1: SH := 16;
  2: SH := 12;
  3: SH := 10;
  4: SH := 8;
  5: SH := 6;
  6: SH := 4;
  else SH := 8;
  end;
SectionHeight := MulDiv(SH, ParentSectionList.FontSize, 12);   {scale to FontSize}
end;

{----------------THorzLine.Create}
constructor THorzLine.Create(AMasterList: TSectionList; L: TAttributeList);
var
  LwName: string[10];
  I: integer;
begin
inherited Create(AMasterList);
VSize := 2;
HWidth := -1;
Align := Centered;
for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      SizeSy: if (Value > 0) and (Value <= 20) then
        VSize := Value;
      WidthSy:
        if Value > 0 then
          if Pos('%', Name) > 0 then
            begin
            if (Value <= 100) then HWidth := Value;
            AsPercent := True;
            end
          else HWidth := Value;
      ColorSy: BkGnd := GetColor(Name, Color);
      AlignSy:
        begin
        LwName := Lowercase(Name);
        if LwName = 'left' then Align := Left
        else if LwName = 'right' then Align := Right;
        end;
      NoShadeSy: NoShade := True;
      end;
end;

{----------------THorzLine.UpdateSpacing}
procedure THorzLine.UpdateSpacing;
begin
SectionHeight := MulDiv(20, ParentSectionList.FontSize, 12)
                   -2 + VSize;   {scale to FontSize}
end;

procedure THorzLine.CopyToClipboard;
begin
ParentSectionList.CB.AddTextCR('', 0);
end;

function THorzLine.DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
begin
Result := inherited DrawLogic(Canvas, Y, IMgr, MaxWidth, Curs);
end;

{----------------THorzLine.Draw}
function THorzLine.Draw(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X: integer; Y: integer) : integer;
var
  XR, L, R, W2 : integer;
  YT, YO: integer;
  White, BlackBorder: boolean;
begin
Result := inherited Draw(Canvas, ARect, IMgr, X, Y);
YO := Y - ParentSectionList.YOff;
if (YO+SectionHeight >= ARect.Top) and (YO < ARect.Bottom) then
  with Canvas do
    begin
    YT := YO+(SectionHeight - VSize) div 2;
    L := IMgr.LeftIndent(Y);
    R := IMgr.RightSide(Y);
    if HWidth < 0 then
      begin
      X := L+10;
      XR := R - 10;
      end
    else
      begin
      if AsPercent then
        W2 := MulDiv(R-L, HWidth, 100)
      else W2 := HWidth;
      case Align of
        Left: X := L;
        Centered: X := L + (R - L - W2) div 2;
        Right: X := R-W2;
        end;
      XR := X+W2;
      end;
    if BkGnd then
      begin
      Brush.Color := Color or $2000000;
      Brush.Style := bsSolid;
      FillRect(Rect(X, YT, XR, YT+VSize));
      end
    else
      begin
      with ParentSectionList do
        begin
        White := ((Background and $FFFFFF = clWhite) or
            ((Background = clWindow) and (GetSysColor(Color_Window) = $FFFFFF)));
        BlackBorder := NoShade or ((GetDeviceCaps(Handle, BITSPIXEL) = 1) and
            (GetDeviceCaps(Handle, PLANES) = 1));
        end;
      if BlackBorder then Pen.Color := clBlack
        else Pen.Color := clBtnShadow;
      MoveTo(X, YT+VSize);
      LineTo(X, YT);
      LineTo(XR, YT);
      if BlackBorder then
        Pen.Color := clBlack
      else if White then
        Pen.Color := clSilver
      else Pen.Color := clBtnHighLight;
      LineTo(XR, YT+VSize);
      LineTo(X, YT+VSize);
      end;
    end;
end;

procedure TPreformated.AddTokenObj(S : TokenObj; NoBreak: boolean);
var
  L : integer;
begin
if Length(S.S) = 0 then Exit;
if Len > 20000 then  Exit;
L := Len+Length(S.S);
if BuffSize < L+1 then Allocate(L + 100);  {L+1 so there is always extra for font at end}
Move(S.S[1], (Buff+Len)^, Length(S.S));
Move(S.I[1], XP^[Len], Length(S.S)*Sizeof(integer));    
Len := L;
end;

procedure TPreformated.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
begin
if Len = 0 then    
  begin
  Max := Indent;
  Min := Indent;
  end
else
  begin
  Max := FindTextWidth(Canvas, Buff, Len, False) + Indent;
  Min := IntMin(2000, Max);   {arbitrary selection}
  end;
end;

function TPreFormated.DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
var
  Dummy: integer;
  Save: integer;
begin
if Len = 0 then
  begin
  Result := DefFont.Size;
  SectionHeight := Result;
  MaxWidth := 0;
  end
else
  begin
  {call with large width to prevent wrapping}
  Save := IMgr.Width;
  IMgr.Width := 32000;
  Result := inherited DrawLogic(Canvas, Y, IMgr, Dummy, Curs);
  IMgr.Width := Save;
  MinMaxWidth(Canvas, Dummy, MaxWidth);   {return MaxWidth}
  end;
end;

{----------------TUListItem.Create}
constructor TUListItem.Create(AMasterList: TSectionList;
  {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF}: integer;
            AFont: TMyFont; AnURL: TUrlTarget);
begin
inherited Create(AMasterList, {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF},
  AFont, AnURL, Left);
ListType := Unordered;
end;

constructor TDListItem.Create(AMasterList: TSectionList;
  {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF}: integer;
                                           AFont: TMyFont; AnURL: TUrlTarget);
begin
inherited Create(AMasterList,
  {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF},
  AFont, AnURL);  {ancestor is TUListItem}
ListType := Definition;
end;

constructor TOListItem.Create(AMasterList: TSectionList;
  {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF}, ItemNumb:integer;
            Index: char; AFont: TMyFont; AnURL: TUrlTarget);
begin
inherited Create(AMasterList, {$IFDEF HL_LAZARUS}NewLevel{$ELSE}Level{$ENDIF},
  AFont, AnURL);
ListNumb := ItemNumb;
ListType := Ordered;
IndexType := Index;
end;

type
  TOptionObj = class(TObject)   {used by TListBoxFormControlObj}
    Value: String;
    Selected: boolean;
  end;

{----------------TListBoxFormControlObj.Create}
constructor TListBoxFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  T: TAttribute;
  Multiple: boolean;
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
TheOptions := TStringList.Create;
Multiple := L.Find(MultipleSy, T);
if L.Find(SizeSy, T) then
  LBSize := T.Value
else LBSize := -1;
Longest := 3;   {the minimum size}
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TListBox.Create(PntPanel);
with TListBox(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Font.Name := AMasterList.PreFontName;
  Font.Size := 10;
  MultiSelect := Multiple;
  ExtendedSelect := Multiple;
  OnEnter := {$IFDEF HL_LAZARUS}@{$ENDIF}EnterEvent;
  OnExit := {$IFDEF HL_LAZARUS}@{$ENDIF}ExitEvent;
  OnClick := {$IFDEF HL_LAZARUS}@{$ENDIF}FormControlClick;
  end;
end;

destructor TListBoxFormControlObj.Destroy;
var
  I: integer;
begin
for I := 0 to TheOptions.Count-1 do
  with TOptionObj(TheOptions.Objects[I]) do
    Free;
TheOptions.Free;
inherited Destroy;
end;

procedure TListBoxFormControlObj.AddStr(const S,
  {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}: string; Selected: boolean);
var
  Opt: TOptionObj;
begin
Opt := TOptionObj.Create;
Opt.Value := {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF};
Opt.Selected := Selected;
TheOptions.AddObject(S, Opt);
Longest := IntMax(Longest, Length(S));
end;

procedure TListBoxFormControlObj.ResetToValue;
var
  I: Integer;
  Tmp: boolean;
begin
with (FControl as TListBox) do
  begin
  Clear;
  for I := 0 to TheOptions.Count-1 do
    begin
    Items.Add(TheOptions[I]);
    Tmp := (TheOptions.Objects[I] as TOptionObj).Selected;
    if MultiSelect then
      Selected[I] := Tmp
    else if Tmp then
      ItemIndex := I;
    end;
  if ItemIndex < 0 then
    ItemIndex := 0;
  TopIndex := 0;
  end;
end;

procedure TListBoxFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
if not Assigned(FControl.Parent) then
  begin
  FControl.Parent := TPaintPanel(MasterList.PPanel);
  ResetToValue;
  end;
with TListBox(FControl) do
  begin
  Canvas.Font := Font;
  if LBSize = -1 then LBSize := IntMax(1, IntMin(8, TheOptions.Count)); 
  ClientHeight := Canvas.TextHeight('A')*LBSize;
  ClientWidth := Canvas.TextWidth('A')*Longest + 15;
  end;
end;

function TListBoxFormControlObj.GetSubmission(Index: integer;
              var S: string): boolean;
begin
with (FControl as TListBox) do
  if (Index < Items.Count) then
      begin
      Result := True;
      S := '';
      if MultiSelect and Selected[Index] or
                     not MultiSelect and (ItemIndex = Index) then
        begin
        S := Self.Name+'=';
        with TheOptions.Objects[Index] as TOptionObj do
          if Value <> '' then S := S + Value
          else S := S + Items[Index];
        end;
    end
  else Result := False;
end;

{----------------TComboFormControlObj.Create}
constructor TComboFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl.Free;   {don't want the inherited one}
FControl := TComboBox.Create(PntPanel);
with TComboBox(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Font.Name := AMasterList.PreFontName;
  Font.Size := 10;
  Style := csDropDownList;
  OnEnter := {$IFDEF HL_LAZARUS}@{$ENDIF}EnterEvent;
  OnExit := {$IFDEF HL_LAZARUS}@{$ENDIF}ExitEvent;
  OnDropDown := {$IFDEF HL_LAZARUS}@{$ENDIF}FormControlClick;
  OnClick := {$IFDEF HL_LAZARUS}@{$ENDIF}FormControlClick;
  end;
end;

procedure TComboFormControlObj.ResetToValue;
var
  I: Integer;
begin
with (FControl as TComboBox) do
  begin
  Clear;
  for I := 0 to TheOptions.Count-1 do
    begin
    Items.Add(TheOptions[I]);
    if (TheOptions.Objects[I] as TOptionObj).Selected then
      ItemIndex := I;
    end;
  if ItemIndex < 0 then
    ItemIndex := 0;
  end;
end;

procedure TComboFormControlObj.SetHeightWidth(Canvas: TCanvas);
var
  Wid: integer;
  DC: HDC;
  A: Char;
  ExtS: TSize;
begin
if not Assigned(FControl.Parent) then
  begin
  FControl.Parent := TPaintPanel(MasterList.PPanel);
  ResetToValue;
  end;
with TComboBox(FControl) do
  begin
  A := 'A';
  DC := GetDC(0);
  {$ifdef Windows}
  Wid := LoWord(GetTextExtent(DC, @A, 1));
  {$else}
  GetTextExtentPoint32(DC, @A, 1, ExtS);
  Wid := ExtS.cx;
  {$endif}
  ReleaseDC(0, DC);
  ClientWidth := Wid * Longest + 30;
  end;
end;

function TComboFormControlObj.GetSubmission(Index: integer;
              var S: string): boolean;
begin
with (FControl as TComboBox) do
  if (Index < Items.Count) then
      begin
      Result := True;
      S := '';
      if ItemIndex = Index then
        begin
        S := Self.Name+'=';
        with TheOptions.Objects[Index] as TOptionObj do
          if Value <> '' then S := S + Value
          else S := S + Items[Index];
        end;
    end
  else Result := False;
end;

{----------------TTextAreaFormControlObj.Create}
constructor TTextAreaFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  PntPanel: TPaintPanel;
  I: integer;
  Wrap: boolean;
  SB: TScrollStyle;
begin
inherited Create(AMasterList, Position, L);
TheText := TStringList.Create;
Rows := 5;
Cols := 30;
Wrap := False;
SB := ssBoth;

for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      RowsSy: Rows := Value;
      ColsSy: Cols := Value;
      WrapSy:
        if (Lowercase(Name) = 'soft') or (Lowercase(Name) = 'hard') then
          begin
          SB := ssVertical;
          Wrap := True;
          end;
      end;

PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TMemo.Create(PntPanel);
with TMemo(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Font.Name := AMasterList.PreFontName;
  Font.Size := 10;
  ScrollBars := SB;
  Wordwrap := Wrap;
  OnKeyDown := {$IFDEF HL_LAZARUS}@{$ENDIF}MyForm.ControlKeyDown;
  OnEnter := {$IFDEF HL_LAZARUS}@{$ENDIF}EnterEvent;
  OnExit := {$IFDEF HL_LAZARUS}@{$ENDIF}ExitEvent;
  OnClick := {$IFDEF HL_LAZARUS}@{$ENDIF}FormControlClick;
  end;
end;

destructor TTextAreaFormControlObj.Destroy;
begin
TheText.Free;
inherited Destroy;
end;

procedure TTextAreaFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
if not Assigned(FControl.Parent) then  
  begin
  FControl.Parent := TPaintPanel(MasterList.PPanel);
  ResetToValue;
  end;
with TMemo(FControl) do
  begin
  Canvas.Font := Font;
  ClientHeight := Canvas.TextHeight('A')*Rows + 5;
  ClientWidth := Canvas.TextWidth('A')*Cols + 5;
  end;
end;

procedure TTextAreaFormControlObj.AddStr(const S: string);
begin
TheText.Add(S);
end;

procedure TTextAreaFormControlObj.ResetToValue;
begin
with (FControl as TMemo) do
  begin
  Lines := TheText;
  SelStart := 0;
  SelLength := 0;
  end;
end;

function TTextAreaFormControlObj.GetSubmission(Index: integer;
              var S: string): boolean;
var
  I: integer;
begin
if Index = 0 then
  begin
  Result := True;
  S := Name+'=';
  with (FControl as TMemo) do
    for I := 0 to Lines.Count-1 do
      begin
      S := S + Lines[I];
      if (I < Lines.Count-1) and not WordWrap then
        S := S + ^M^J;
      end;
  end
else Result := False;
end;

function TFormControlList.FindControl(Posn: integer): TFormControlObj;
{find the control at a given character position}
var
  I: integer;
begin
for I := 0 to Count-1 do
  if TFormControlObj(Items[I]).Pos = Posn then
    begin
    Result := TFormControlObj(Items[I]);
    Exit;
    end;
Result := Nil;
end;

function TFormControlList.GetHeightAt(Posn: integer;
              var BaseLine: boolean) : Integer;
var
  Ctrl: TFormControlObj;
begin
Ctrl := FindControl(Posn);
if Assigned(Ctrl) then
  begin
  Result := Ctrl.FControl.Height;
  BaseLine := Ctrl.BaseLine;
  end
else Result := -1;
end;

function TFormControlList.GetWidthAt(Posn: integer) : integer;
var
  Ctrl: TFormControlObj;
begin
Ctrl := FindControl(Posn);
if Assigned(Ctrl) then
  Result := Ctrl.FControl.Width
else Result := -1;
end;

function TFormControlList.GetControlCountAt(Posn: integer): integer;
{Return count of chars before the next form control.  0 if at the control,
 9999 if no controls after Posn}
var
  I, Pos: integer;
begin
if Count = 0 then
  begin
  Result := 9999;
  Exit;
  end;
I := 0;
while I < count do
  begin
  Pos := TFormControlObj(Items[I]).Pos;
  if Pos >= Posn then break;
  Inc(I);
  end;
if I = Count then Result := 9999
else
  Result := TFormControlObj(Items[I]).Pos - Posn;
end;

end.

