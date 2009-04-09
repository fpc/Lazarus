{  $Id$  }
{
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
unit IniPropStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, IniFiles, PropertyStorage;

type
  { TCustomIniPropStorage }

  TIniFileClass = class of TCustomIniFile;
  
  TCustomIniPropStorage = class(TFormPropertyStorage)
  private
    FCount : Integer;
    FReadOnly : Boolean;
    FIniFile: TCustomIniFile;
    FIniFileName: string;
    FIniSection: string;
  protected
    function IniFileClass: TIniFileClass; virtual;
    function GetIniFileName: string; virtual;
    function RootSection: string; override;
    property IniFile: TCustomIniFile read FIniFile;
  public
    procedure StorageNeeded(ReadOnly: Boolean); override;
    procedure FreeStorage; override;
    function  DoReadString(const Section, Ident, default: string): string; override;
    procedure DoWriteString(const Section, Ident, Value: string); override;
    procedure DoEraseSections(const ARootSection : string);override;
  public
    property IniFileName: string read FIniFileName write FIniFileName;
    property IniSection: string read FIniSection write FIniSection;
  end;
  
  { TIniPropStorage }
  
  TIniPropStorage = class(TCustomIniPropStorage)
  published
    Property StoredValues;
    property IniFileName;
    property IniSection;
    property Active;
    property OnSavingProperties;
    property OnSaveProperties;
    property OnRestoringProperties;
    property OnRestoreProperties;
  end;


procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Misc',[TIniPropStorage]);
end;

{ Should move to strutils when 1.9.6 is out. }

function FindPart(const HelpWilds, InputStr: string): Integer;

var
  I, J: Integer;
  Diff: Integer;

begin
  I := Pos('?', HelpWilds);
  if I = 0 then begin
    { if no '?' in HelpWilds }
    Result := Pos(HelpWilds, InputStr);
    Exit;
  end;
  { '?' in HelpWilds }
  Diff := Length(InputStr) - Length(HelpWilds);
  if Diff < 0 then begin
    Result := 0;
    Exit;
  end;
  { now move HelpWilds over InputStr }
  for I := 0 to Diff do begin
    for J := 1 to Length(HelpWilds) do begin
      if (InputStr[I + J] = HelpWilds[J]) or
        (HelpWilds[J] = '?') then
      begin
        if J = Length(HelpWilds) then begin
          Result := I + 1;
          Exit;
        end;
      end
      else Break;
    end;
  end;
  Result := 0;
end;



function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;

 function SearchNext(var Wilds: string): Integer;
 { looking for next *, returns position and string until position }
 begin
   Result := Pos('*', Wilds);
   if Result > 0 then Wilds := Copy(Wilds, 1, Result - 1);
 end;

var
  CWild, CInputWord: Integer; { counter for positions }
  I, LenHelpWilds: Integer;
  MaxInputWord, MaxWilds: Integer; { Length of InputStr and Wilds }
  HelpWilds: string;
begin
  if Wilds = InputStr then begin
    Result := True;
    Exit;
  end;
  repeat { delete '**', because '**' = '*' }
    I := Pos('**', Wilds);
    if I > 0 then
      Wilds := Copy(Wilds, 1, I - 1) + '*' + Copy(Wilds, I + 2, MaxInt);
  until I = 0;
  if Wilds = '*' then begin { for fast end, if Wilds only '*' }
    Result := True;
    Exit;
  end;
  MaxInputWord := Length(InputStr);
  MaxWilds := Length(Wilds);
  if IgnoreCase then begin { upcase all letters }
    InputStr := AnsiUpperCase(InputStr);
    Wilds := AnsiUpperCase(Wilds);
  end;
  if (MaxWilds = 0) or (MaxInputWord = 0) then begin
    Result := False;
    Exit;
  end;
  CInputWord := 1;
  CWild := 1;
  Result := True;
  repeat
    if InputStr[CInputWord] = Wilds[CWild] then begin { equal letters }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '?' then begin { equal to '?' }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '*' then begin { handling of '*' }
      HelpWilds := Copy(Wilds, CWild + 1, MaxWilds);
      I := SearchNext(HelpWilds);
      LenHelpWilds := Length(HelpWilds);
      if I = 0 then begin
        { no '*' in the rest, compare the ends }
        if HelpWilds = '' then Exit; { '*' is the last letter }
        { check the rest for equal Length and no '?' }
        for I := 0 to LenHelpWilds - 1 do begin
          if (HelpWilds[LenHelpWilds - I] <> InputStr[MaxInputWord - I]) and
            (HelpWilds[LenHelpWilds - I]<> '?') then
          begin
            Result := False;
            Exit;
          end;
        end;
        Exit;
      end;
      { handle all to the next '*' }
      Inc(CWild, 1 + LenHelpWilds);
      I := FindPart(HelpWilds, Copy(InputStr, CInputWord, MaxInt));
      if I= 0 then begin
        Result := False;
        Exit;
      end;
      CInputWord := I + LenHelpWilds;
      Continue;
    end;
    Result := False;
    Exit;
  until (CInputWord > MaxInputWord) or (CWild > MaxWilds);
  { no completed evaluation }
  if CInputWord <= MaxInputWord then Result := False;
  if (CWild <= MaxWilds) and (Wilds[MaxWilds] <> '*') then Result := False;
end;



{ TCustomIniPropStorage }

function TCustomIniPropStorage.IniFileClass: TIniFileClass;
begin
  Result:=TIniFile;
end;

procedure TCustomIniPropStorage.StorageNeeded(ReadOnly: Boolean);
begin
  If (FIniFile=Nil) or (ReadOnly<>FReadOnly) then
    begin
    If (FiniFile<>Nil) then
      begin
      // Force free.
      FCount:=0;
      FreeStorage;
      end;
    FReadOnly:=ReadOnly;
    FInifile:=IniFileClass.Create(GetIniFileName);
    end;
  Inc(FCount);
end;

procedure TCustomIniPropStorage.FreeStorage;
begin
  Dec(FCount);
  If FCount<=0 then
    begin
    FCount:=0;
    FreeAndNil(FIniFile);
    end;
end;

function TCustomIniPropStorage.GetIniFileName: string;
begin
  If (FIniFileName<>'') then
    Result:=FIniFileName
  else
{$ifdef unix}
    Result:=IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('HOME'))
            +'.'+ExtractFileName(Application.ExeName);

{$else}
    Result:=ChangeFileExt(Application.ExeName,'.ini');
{$endif}
end;

function TCustomIniPropStorage.RootSection: String;
begin
  if (FIniSection='') then
    Result:=inherited RootSection
  else
    Result:=FIniSection;
end;

function TCustomIniPropStorage.DoReadString(const Section, Ident, Default: string): string;
begin
  Result:=FIniFile.ReadString(Section, Ident, Default);
end;

procedure TCustomIniPropStorage.DoWriteString(const Section, Ident, Value: string);
begin
  FIniFile.WriteString(Section, Ident, Value);
end;

procedure TCustomIniPropStorage.DoEraseSections(const ARootSection: String);

var
  Lines: TStrings;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    FInifile.ReadSections(Lines);
    for I := 0 to Lines.Count - 1 do begin
      if (Lines[I] = IniSection) or
        (IsWild(Lines[I], IniSection + '.*', False) or
        IsWild(Lines[I], IniSection + '\*', False)) then
        FInifile.EraseSection(Lines[I]);
    end;
  finally
    Lines.Free;
  end;
end;

end.
