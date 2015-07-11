{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

{$mode objfpc}
{$h+}

unit AppUtils;

interface

uses
  Classes, Controls, FileUtil, Forms, IniFiles, Grids;

function GetDefaultSection(Component: TComponent): string;
procedure GetDefaultIniData(Control: TControl; var IniFileName, Section: string );
function GetDefaultIniName: string;

type
  TOnGetDefaultIniName = function: string;

const
  OnGetDefaultIniName: TOnGetDefaultIniName = nil;

var
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;

function FindForm(FormClass: TFormClass): TForm;
function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
function ShowDialog(FormClass: TFormClass): Boolean;
function InstantiateForm(FormClass: TFormClass; var Reference): TForm;

procedure SaveFormPlacement(Form: TForm; const IniFileName: string);
procedure RestoreFormPlacement(Form: TForm; const IniFileName: string);
procedure WriteFormPlacement(Form: TForm; IniFile: TCustomIniFile;  const Section: string);
procedure ReadFormPlacement(Form: TForm; IniFile: TCustomIniFile;  const Section: string; LoadState, LoadPosition: Boolean);
procedure SaveMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
procedure RestoreMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile; Const Section : String);
procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile; Const Section : string);

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;

function StrToIniStr(const Str: string): string;
function IniStrToStr(const Str: string): string;

{ Internal using utilities }

implementation

uses
  SysUtils, Placement, LCLStrConsts;

{ Copied. Need to be moved somewhere in the RTL, actually }

Type
  TCharset = Set of Char;

function WordPosition(const N: Integer; const S: string; const WordDelims: TCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
    begin
    while (I <= Length(S)) and (S[I] in WordDelims) do
      Inc(I);
    if I <= Length(S) then
      Inc(Count);
    if Count <> N then
      while (I <= Length(S)) and not (S[I] in WordDelims) do
        Inc(I)
    else
      Result := I;
    end;
end;

function ExtractWord(N: Integer; const S: string; const WordDelims: TCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not(S[I] in WordDelims) do begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;



function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then begin
    if Component is TCustomForm then Result := Component.ClassName
    else begin
      Result := Component.Name;
      if Component is TControl then begin
        F := GetParentForm(TControl(Component));
        if F <> nil then Result := F.ClassName + Result
        else begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else Result := '';
end;

function GetDefaultIniName: string;
begin
  if Assigned(OnGetDefaultIniName) then
    Result:= OnGetDefaultIniName()
  else
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.INI'));
end;


procedure GetDefaultIniData(Control: TControl; var IniFileName, Section: string);
var
  I: Integer;
begin
  IniFileName := EmptyStr;
  with Control do
    if Owner is TCustomForm then
      for I := 0 to Owner.ComponentCount - 1 do
        if (Owner.Components[I] is TFormPlacement) then begin
          IniFileName := TFormPlacement(Owner.Components[I]).IniFileName;
          Break;
        end;
  Section := GetDefaultSection(Control);
  if IniFileName = EmptyStr then
    IniFileName := GetDefaultIniName;
end;

function FindForm(FormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is FormClass then begin
      Result := Screen.Forms[I];
      Break;
    end;
  end;
end;

function InternalFindShowForm(FormClass: TFormClass;
  const Caption: string; Restore: Boolean): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is FormClass then
      if (Caption = '') or (Caption = Screen.Forms[I].Caption) then begin
        Result := Screen.Forms[I];
        Break;
      end;
  end;
  if Result = nil then begin
    Application.CreateForm(FormClass, Result);
    if Caption <> '' then Result.Caption := Caption;
  end;
  with Result do begin
    if Restore and (WindowState = wsMinimized) then WindowState := wsNormal;
    Show;
  end;
end;

function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
begin
  Result := InternalFindShowForm(FormClass, Caption, True);
end;

function ShowDialog(FormClass: TFormClass): Boolean;
var
  Dlg: TForm;
begin
  Application.CreateForm(FormClass, Dlg);
  try
    Result := byte(Dlg.ShowModal) in [mrOk, mrYes];
  finally
    Dlg.Free;
  end;
end;

function InstantiateForm(FormClass: TFormClass; var Reference): TForm;
begin
  if TForm(Reference) = nil then
    Application.CreateForm(FormClass, Reference);
  Result := TForm(Reference);
end;

function StrToIniStr(const Str: string): string;

begin
  Result:=StringReplace(Str,LineEnding,'\n',[rfReplaceAll]);
end;

function IniStrToStr(const Str: string): string;

begin
  Result:=StringReplace(Str,'\n',LineEnding,[rfReplaceAll]);
end;

const
{ The following strings should not be localized }
  siFlags     = 'Flags';
  //siShowCmd   = 'ShowCmd';
  //siMinMaxPos = 'MinMaxPos';
  siNormPos   = 'NormPos';
  siPixels    = 'PixelsPerInch';
  siMDIChild  = 'MDI Children';
  siListCount = 'Count';
  siItem      = 'Item%d';


procedure SaveMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
{$ifdef nevertrue}
var
  I: Integer;
{$endif}
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(SNoMDIForm);
  IniFile.EraseSection( siMDIChild);
//!! MVC: Needs fixing !
{$ifdef nevertrue}
  if MainForm.MDIChildCount > 0 then begin
    IniWriteInteger(IniFile, siMDIChild, siListCount,
      MainForm.MDIChildCount);
    for I := 0 to MainForm.MDIChildCount - 1 do
      IniWriteString(IniFile, siMDIChild, Format(siItem, [I]),
        MainForm.MDIChildren[I].ClassName);
  end;
{$endif}
end;

procedure RestoreMDIChildren(MainForm: TForm; IniFile: TCustomIniFile);
var
  I: Integer;
  Count: Integer;
  FormClass: TFormClass;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(SNoMDIForm);
  Count := IniFile.ReadInteger(siMDIChild, siListCount, 0);
  if Count > 0 then begin
    for I := 0 to Count - 1 do begin
      FormClass := TFormClass(GetClass(Inifile.ReadString(siMDIChild,
              Format(siItem, [Count - I - 1]), '')));
      if FormClass <> nil then
        InternalFindShowForm(FormClass, '', False);
    end;
  end;
end;


procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile;
  const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    Inifile.WriteInteger(Section, Format(siItem, [I]),TDrawGrid(Grid).ColWidths[I]);
end;

procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TCustomIniFile;
  const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    TDrawGrid(Grid).ColWidths[I] := IniFile.ReadInteger(Section,
      Format(siItem, [I]), TDrawGrid(Grid).ColWidths[I]);
end;

function CrtResString: string;
begin
  //!! bogus function
  Result := Format('(%dx%d)', [1200,1024]);
end;

function ReadPosStr(IniFile: TCustomInifile; const Section, Ident: string): string;
begin
  Result := IniFile.ReadString(Section, Ident + CrtResString, '');
  if Result = '' then
    Result := IniFile.ReadString(Section, Ident, '');
end;

procedure WritePosStr(IniFile: TCustomInifile; const Section, Ident, Value: string);
begin
  IniFile.WriteString(Section, Ident + CrtResString, Value);
  IniFile.WriteString(Section, Ident, Value);
end;

procedure WriteFormPlacement(Form: TForm; IniFile: TCustomInifile; const Section: string);

begin
  with Form do
    begin
    IniFile.WriteInteger(Section, siFlags, Ord(WindowState));
    IniFile.WriteInteger(Section, siPixels, Screen.PixelsPerInch);
    WritePosStr(IniFile, Section, siNormPos, Format('%d,%d,%d,%d',[Left, Top, Width,Height]));
    end;
end;


procedure SaveFormPlacement(Form: TForm; const IniFileName: string);

var
  IniFile: TInifile;
begin
  IniFile := TIniFile.Create(UTF8ToSys(IniFileName));
  try
    WriteFormPlacement(Form, IniFile, Form.ClassName);
  finally
    IniFile.Free;
  end;
end;


type
  {$IFNDEF LCL}
  //!! MVC: dirty VCL/CLX hack, not needed in Lazarus
  TNastyForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
//    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FWindowState: TWindowState; { !! }
  end;
  {$ENDIF}

  THackComponent = class(TComponent)
  end;

procedure ReadFormPlacement(Form: TForm; IniFile: TCustomIniFile;
  const Section: string; LoadState, LoadPosition: Boolean);

const
  Delims = [',',' '];
  
var
  PosStr: string;
  PI,L,T,H,W : Integer;

begin
  //Writeln('ReadFormPlaceMent');
  if not (LoadState or LoadPosition) then
    Exit;
  PI:=IniFile.ReadInteger(Section, siPixels,Screen.PixelsPerInch);
  if LoadPosition and (Screen.PixelsPerInch=PI) then
    with Form do
      begin
      //Writeln('Loading position');
      PosStr:=ReadPosStr(IniFile, Section, siNormPos);
      if PosStr <> '' then
        begin
        //Writeln('Have position');
        L := StrToIntDef(ExtractWord(1, PosStr, Delims), Left);
        T := StrToIntDef(ExtractWord(2, PosStr, Delims), Top);
        W := StrToIntDef(ExtractWord(3, PosStr, Delims), Width);
        H := StrToIntDef(ExtractWord(4, PosStr, Delims), Height);
        If not (BorderStyle in [bsSizeable , bsSizeToolWin ]) then
          begin
          if (Position in [poScreenCenter , poDesktopCenter ]) and
            not (csDesigning in ComponentState) then
            begin
            THackComponent(Form).SetDesigning(True);
            try
              Position := poDesigned;
            finally
              THackComponent(Form).SetDesigning(False);
            end;
            end;
          end;
        //Writeln('Set bounds');
        SetBounds(L,T,W,H);
        end;
      end;
  if LoadState then
    With Form do
      begin
      //Writeln('Loading state');
      PI := IniFile.ReadInteger(Section, siFlags,Ord( WindowState));
      If (Ord(Low(TWindowState))<=PI) and (PI<=Ord(High(TWindowState))) then
        WindowState:=TWindowState(PI);
      end;
end;

procedure RestoreFormPlacement(Form: TForm; const IniFileName: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(UTF8ToSys(IniFileName));
  try
    ReadFormPlacement(Form, IniFile, Form.ClassName, True, True);
  finally
    IniFile.Free;
  end;
end;

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;
var
  CurrentName: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to MaxInt do begin
    CurrentName := Format(FileNameMask, [I]);
    if not FileExistsUTF8(IncludeTrailingPathDelimiter(Path) + CurrentName) then
      begin
      Result := CurrentName;
      Exit;
    end;
  end;
end;

end.
