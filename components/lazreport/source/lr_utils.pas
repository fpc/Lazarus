
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Various routines             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Utils;

interface

{$I LR_Vers.inc}

uses
  SysUtils, Classes, Graphics, Controls,
  LR_DBRel, Forms, StdCtrls, ClipBrd, Menus,Types,db,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  LCLType,LCLIntf,LCLProc;


procedure frReadMemo(Stream: TStream; l: TStrings);
procedure frReadMemo22(Stream: TStream; l: TStrings);
procedure frWriteMemo(Stream: TStream; l: TStrings);
function frReadString(Stream: TStream): String;
function frReadString22(Stream: TStream): String;
{$IFDEF FREEREP2217READ}
function frReadMemoText2217(Stream: TStream): String;
function frReadString2217(Stream: TStream): String;
{$ENDIF}
procedure frWriteString(Stream: TStream; s: String);
procedure frEnableControls(c: Array of TControl; e: Boolean);
function frControlAtPos(Win: TWinControl; p: TPoint): TControl;
function frGetDataSet(ComplexName: String): TfrTDataSet;
procedure frGetDataSetAndField(ComplexName: String;
  var DataSet: TfrTDataSet; var Field: TfrTField);
function frGetFontStyle(Style: TFontStyles): Integer;
function frSetFontStyle(Style: Integer): TFontStyles;
procedure frInitFont(aFont : TFont; aColor : TColor; aSize : Integer; aStyle : TFontStyles);
function frFindComponent(Owner: TComponent; Name: String): TComponent;
procedure frGetComponents(Owner: TComponent; ClassRef: TClass;
  List: TStrings; Skip: TComponent);

function frGetWindowsVersion: String;

function frTypeObjectToStr(ot : Byte):string;
function StrTofrTypeObject(St : string) : Byte;
implementation

uses LR_Class, LR_DSet;

procedure frInitFont(aFont : TFont; aColor : TColor; aSize : Integer; aStyle : TFontStyles);
begin
  with aFont do
  begin
    aFont.BeginUpdate;
    try
      aFont.Name :='default';
      aFont.Color:=aColor;
      aFont.Size :=0; //aSize;
      aFont.Style:=aStyle;
      aFont.Pitch:=fpDefault; //fpFixed;
      aFont.CharSet:=1;
    finally
      aFont.EndUpdate;
    end;
  end;
end;

function frSetFontStyle(Style: Integer): TFontStyles;
begin
  Result := [];
  if (Style and $1) <> 0 then Result := Result + [fsItalic];
  if (Style and $2) <> 0 then Result := Result + [fsBold];
  if (Style and $4) <> 0 then Result := Result + [fsUnderLine];
end;

function frGetFontStyle(Style: TFontStyles): Integer;
begin
  Result := 0;
  if fsItalic in Style then Result := Result or $1;
  if fsBold in Style then Result := Result or $2;
  if fsUnderline in Style then Result := Result or $4;
end;

procedure RemoveQuotes(var s: String);
begin
  if (s[1] = '"') and (s[Length(s)] = '"') then
    s := Copy(s, 2, Length(s) - 2);
end;

procedure frReadMemo(Stream: TStream; l: TStrings);
var
  s: String;
  b: Byte;
  n: Word;
begin
  l.Clear;
  Stream.Read(n, 2);
  if n > 0 then
    repeat
      Stream.Read(n, 2);
      SetLength(s, n);
      Stream.Read(s[1], n);
      l.Add(s);
      Stream.Read(b, 1);
    until b = 0
  else
    Stream.Read(b, 1);
end;

procedure frWriteMemo(Stream: TStream; l: TStrings);
var
  s: String;
  i: Integer;
  n: Word;
  b: Byte;
begin
  n := l.Count;
  Stream.Write(n, 2);
  for i := 0 to l.Count - 1 do
  begin
    s := l[i];
    n := Length(s);
    Stream.Write(n, 2);
    Stream.Write(s[1], n);
    b := 13;
    if i <> l.Count - 1 then Stream.Write(b, 1);
  end;
  b := 0;
  Stream.Write(b, 1);
end;

function frReadString(Stream: TStream): String;
var
  s: String;
  n: Word;
  b: Byte;
begin
  Stream.Read(n, 2);
  SetLength(s, n);
  Stream.Read(s[1], n);
  Stream.Read(b, 1);
  Result := s;
end;

procedure frWriteString(Stream: TStream; s: String);
var
  b: Byte;
  n: Word;
begin
  n := Length(s);
  Stream.Write(n, 2);
  Stream.Write(s[1], n);
  b := 0;
  Stream.Write(b, 1);
end;

procedure frReadMemo22(Stream: TStream; l: TStrings);
var
  s: String;
  i: Integer;
  b: Byte;
begin
  SetLength(s, 4096);
  l.Clear;
  i := 1;
  repeat
    Stream.Read(b,1);
    if (b = 13) or (b = 0) then
    begin
      SetLength(s, i - 1);
      if not ((b = 0) and (i = 1)) then l.Add(s);
      SetLength(s, 4096);
      i := 1;
    end
    else if b <> 0 then
    begin
      s[i] := Chr(b);
      Inc(i);
      if i > 4096 then
        SetLength(s, Length(s) + 4096);
    end;
  until b = 0;
end;

function frReadString22(Stream: TStream): String;
var
  s: String;
  i: Integer;
  b: Byte;
begin
  SetLength(s, 4096);
  i := 1;
  repeat
    Stream.Read(b, 1);
    if b = 0 then
      SetLength(s, i - 1)
    else
    begin
      s[i] := Chr(b);
      Inc(i);
      if i > 4096 then
        SetLength(s, Length(s) + 4096);
    end;
  until b = 0;
  Result := s;
end;

{$IFDEF FREEREP2217READ}
function frReadMemoText2217(Stream: TStream): String;
var
  I: Integer;
begin
  Stream.ReadBuffer(I, SizeOf(I));
  SetLength(Result, I);
  Stream.ReadBuffer(PChar(Result)^, I);
end;

function frReadString2217(Stream: TStream): String;
var
  I: Integer;
begin
  Result := frReadMemoText2217(Stream);
  I := Pos(#13, Result);
  if I > 0 then
    Result := Copy(Result, 1, I - 1);
end;
{$ENDIF}

type
  THackWinControl = class(TWinControl)
  end;

procedure frEnableControls(c: Array of TControl; e: Boolean);
const
  Clr1: Array[Boolean] of TColor = (clGrayText,clWindowText);
  Clr2: Array[Boolean] of TColor = (clBtnFace,clWindow);
var
  i: Integer;
begin
  for i := Low(c) to High(c) do
    if c[i] is TLabel then
      with c[i] as TLabel do
      begin
        Font.Color := Clr1[e];
        Enabled := e;
      end
    else if c[i] is TWinControl then
      with THackWinControl(c[i]) do
      begin
        Color := Clr2[e];
        Enabled := e;
      end;
end;

function frControlAtPos(Win: TWinControl; p: TPoint): TControl;
var
  i: Integer;
  c: TControl;
  p1: TPoint;
begin
  Result := nil;
  with Win do
  begin
    for i := ControlCount - 1 downto 0 do
    begin
      c := Controls[i];
      if c.Visible and PtInRect(Classes.Rect(c.Left, c.Top, c.Left + c.Width, c.Top + c.Height), p) then
        if (c is TWinControl) and (csAcceptsControls in c.ControlStyle) and
           (TWinControl(c).ControlCount > 0) then
        begin
          p1 := p;
          Dec(p1.X, c.Left); Dec(p1.Y, c.Top);
          c := frControlAtPos(TWinControl(c), p1);
          if c <> nil then
          begin
            Result := c;
            Exit;
          end;
        end
        else
        begin
          Result := c;
          Exit;
        end;
    end;
  end;
end;

function frGetDataSet(ComplexName: String): TfrTDataSet;
begin
  Result := nil;
  if not Assigned(frFindComponent(CurReport.Owner, ComplexName)) then exit;
  if frFindComponent(CurReport.Owner, ComplexName) is TDataSet then
    Result := TfrTDataSet(frFindComponent(CurReport.Owner, ComplexName))
  else
   if frFindComponent(CurReport.Owner, ComplexName) is TDataSource then
     Result := TfrTDataSet(TDataSource(frFindComponent(CurReport.Owner, ComplexName)).DataSet);
end;

procedure frGetDataSetAndField(ComplexName: String; var DataSet: TfrTDataSet;
  var Field: TfrTField);
var
  n: Integer;
  f: TComponent;
  s1, s2, s3: String;
begin
  Field := nil;
  f := CurReport.Owner;
  n := Pos('.', ComplexName);
  if n <> 0 then
  begin
    s1 := Copy(ComplexName, 1, n - 1);        // table name
    s2 := Copy(ComplexName, n + 1, 255);      // field name
    if Pos('.', s2) <> 0 then                 // module name present
    begin
      s3 := Copy(s2, Pos('.', s2) + 1, 255);
      s2 := Copy(s2, 1, Pos('.', s2) - 1);
      f := nil;
      if Assigned(Application.FindComponent(S1)) then
        begin
          if Application.FindComponent(S1) is TDataSet then
            f := Application.FindComponent(S1) //FindGlobalComponent(s1);
          else if Application.FindComponent(S1) is TDataSource then
            f := TDataSource(Application.FindComponent(S1)).DataSet;
        end;
      if f <> nil then
      begin
        DataSet := TfrTDataSet(f.FindComponent(s2));
        RemoveQuotes(s3);
        if DataSet <> nil then
          Field := TfrTField(DataSet.FindField(s3));
      end;
    end
    else
    begin
      if Assigned(frFindComponent(f, s1)) then
        begin
          if TfrTDataSet(frFindComponent(f, s1)) is TDataSet then
            DataSet := TfrTDataSet(frFindComponent(f, s1))
          else if frFindComponent(f, s1) is TDataSource then
            DataSet := TfrTDataSet(TDataSource(frFindComponent(f, s1)).DataSet);
        end;
      RemoveQuotes(s2);
      if DataSet <> nil then
        Field := TfrTField(DataSet.FindField(s2));
    end;
  end
  else if DataSet <> nil then
  begin
    RemoveQuotes(ComplexName);
    Field := TfrTField(DataSet.FindField(ComplexName));
  end;
end;

function frFindComponent(Owner: TComponent; Name: String): TComponent;
var
  n: Integer;
  s1, s2: String;
begin
  Result := nil;
  n := Pos('.', Name);
  try
    if n = 0 then
      Result := Owner.FindComponent(Name)
    else
    begin
      s1 := Copy(Name, 1, n - 1);        // module name
      s2 := Copy(Name, n + 1, 255);      // component name
      Owner := Application.FindComponent(S1); //FindGlobalComponent(s1);
      if Owner <> nil then
        Result := Owner.FindComponent(s2);
    end;
  except
    on Exception do
      raise EClassNotFound.Create('Отсутствует ' + Name);
  end;
end;

{$HINTS OFF}
procedure frGetComponents(Owner: TComponent; ClassRef: TClass;
  List: TStrings; Skip: TComponent);
var
  i, j: Integer;

  procedure EnumComponents(f: TComponent);
  var
    i: Integer;
    c: TComponent;
  begin
{$IFDEF Delphi5}
    if f is TForm then
      for i := 0 to TForm(f).ControlCount - 1 do
      begin
        c := TForm(f).Controls[i];
        if c is TFrame then
          EnumComponents(c);
      end;
{$ENDIF}
    for i := 0 to f.ComponentCount - 1 do
    begin
      c := f.Components[i];
      if (c <> Skip) and (c is ClassRef) then
        if f = Owner then
          List.Add(c.Name)
        else if ((f is TForm) or (f is TDataModule)) then
          List.Add(f.Name + '.' + c.Name)
        else
          List.Add(TControl(f).Parent.Name + '.' + f.Name + '.' + c.Name)
    end;
  end;

begin
  {$IFDEF DebugLR}
  DebugLn('frGetComponents 1');
  {$ENDIF}
  List.Clear;
  for i := 0 to Screen.FormCount - 1 do
    EnumComponents(Screen.Forms[i]);

  {$IFDEF DebugLR}
  DebugLn('frGetComponents 2');
  {$ENDIF}
//  for i := 0 to Screen.DataModuleCount - 1 do
//    EnumComponents(Screen.DataModules[i]);

  with Screen do
  begin
    {$IFDEF DebugLR}
    DebugLn('frGetComponents 3');
    {$ENDIF}
    for i := 0 to CustomFormCount - 1 do
    begin
      with CustomForms[i] do
      begin
        if (UpperCase(ClassName)='TDATAMODULEFORM')  then
          for j := 0 to ComponentCount - 1 do
          begin
            if (Components[j] is TDataModule) then
              EnumComponents(Components[j]);
          end;
      end;
    end;
  end;
end;
{$HINTS ON}  

function frGetWindowsVersion: String;
{$IFDEF WIN32}
var Ver: TOsVersionInfo;
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s: Result := '32s';
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          dwBuildNumber := dwBuildNumber and $0000FFFF;
          if (dwMajorVersion > 4) or ((dwMajorVersion = 4) and
            (dwMinorVersion >= 10)) then
            Result := '98' else
            Result := '95';
        end;
      VER_PLATFORM_WIN32_NT: Result := 'NT';
    end;
  end;
end;
{$ELSE}
begin
  Result:='LINUX';
end;
{$ENDIF}

{**
* Return the string value of type object
**}
function frTypeObjectToStr(ot : Byte): string;
begin
  Result:='undef';
  Case ot of
    gtMemo     : result:='gtMemo';
    gtPicture  : result:='gtPicture';
    gtBand     : result:='gtBand';
    gtSubReport: result:='gtSubReport';
    gtLine     : result:='gtLine';
    gtAddIn    : result:='gtAddIn';
  end;
end;

function StrTofrTypeObject(St : string) : Byte;
begin
  Result:=StrToIntDef(St,gtMemo);
  
  if SameText(St,'gtMemo') then
    result:=gtMemo
  else
    if SameText(St,'gtPicture') then
      result:=gtPicture
    else
     if SameText(St,'gtBand') then
       result:=gtBand
      else
       if SameText(St,'gtSubReport') then
          result:=gtSubReport
       else
        if SameText(St,'gtLine') then
          result:=gtLine
        else
         if SameText(St,'gtAddIn') then
           result:=gtAddIn;
end;

end.
