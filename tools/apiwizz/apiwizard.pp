{  $Id$  }
{
 /***************************************************************************
                                APIWizard.pp
                             -------------------
                   APIWiZZ is an API wizard to generate WINAPI
                   Templates for the LCL widgetsets.

                   Initial Revision  : 05-02-2000


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{
@author(M. Weustink <marc@dommelstein.net>)                       
@created(02-May-2000)
}
unit APIWizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, ExtCtrls, LCLproc, ComCtrls,
  // LazUtils
  LazFileUtils, LazStringUtils, LazUTF8, LazUTF8Classes;

type

  { TApiWizForm }

  TApiWizForm = class(TForm)
    cmdCopy: TButton;
    cmdScan: TButton;
    cbgLCLPlatform: TCHECKGROUP;
    cmdGenerate: TButton;
    Groupbox1: TGROUPBOX;
    lvExisting: TListView;
    txtDeclarations: TMemo;
    rbIndependent: TRADIOBUTTON;
    rbDependent: TRADIOBUTTON;
    rdgApiType: TRadioGroup;
    lblDeclare: TLabel;
    txtLazarus: TEdit;
    lblLazarus: TLabel;
    procedure cmdCopyClick(Sender: TObject);
    procedure cmdGenerateClick(Sender: TObject);
    procedure ApiWizFormCreate(Sender: TObject);
    procedure cmdScanClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rbPlatformDependencyClick(Sender: TObject);
    procedure rdgApiTypeClick (Sender: TObject );
  private
    FLineInfo: TStringList;
    procedure Clear;
  public
  end;

  TProcType = (ptFunction, ptProcedure);

  TApiWidgetset = (
    awCarbon,
    awCocoa,
    awFpgui,
    awGtk,
    awGtk2,
    awGtk3,
    awMUI,
    awNoGui,
    awQt,
    awWin32,
    awWinCE);
  TApiWidgetsets = set of TApiWidgetset;

  TApiLine = class
    Declaration: String;
    Independent: Boolean;
    WinApi: Boolean;
    Widgetsets: TApiWidgetsets;
    Base: Boolean;
  end;

var
  ApiWizForm: TApiWizForm;

implementation

{$R apiwizard.lfm}

const
  DECL_OFFSET: array[TProctype] of Integer = (9, 10);

  WS_NAME: array[TApiWidgetset] of string = (
    'carbon',
    'cocoa',
    'fpgui',
    'gtk',
    'gtk2',
    'gtk3',
    'mui',
    'nogui',
    'qt',
    'win32',
    'wince'
  );

function GetName(const ADeclaration: String): String;
var
  n, NameEnd: Integer;
begin
  Result := LowerCase(ADeclaration);
  n := Pos('function ', Result);
  if n <> 0
  then Result := Trim(Copy(ADeclaration, n + DECL_OFFSET[ptFunction], Length(ADeclaration)))
  else begin
    n := Pos('procedure ', Result);
    if n <> 0
    then Result := Trim(Copy(ADeclaration, n + DECL_OFFSET[ptProcedure], Length(ADeclaration)))
    else Result := '';
  end;

  if Result = '' then Exit;

  NameEnd := Pos('(', Result);
  if NameEnd = 0
  then NameEnd := Pos(':', Result);
  if NameEnd = 0
  then NameEnd := Pos(';', Result);
  if NameEnd = 0
  then NameEnd := Pos(' ', Result);

  n := Pos('.', Result);

  if (n > 0) and (((NameEnd > 0) and (n < NameEnd)) or (NameEnd = 0))
  then begin
    Result := Trim(Copy(Result, n + 1, Length(Result)));
    if NameEnd > 0 then Dec(NameEnd, n);
  end;
  if NameEnd > 0
  then Result := Trim(Copy(Result, 1, NameEnd - 1));

end;

function SplitDeclaration(const ADeclaration: String;
  out ProcType: TProcType; out ProcName: String; const ProcParams: TStringList): Boolean;

  function ProcessParams(Params: String): Boolean;
  var
    n: Integer;
    S, part: String;
  begin
    Result := False;
    while Params <> '' do
    begin
      n := Pos(';', Params);
      if n = 0
      then begin
        Part := Params;
        Params :=  '';
      end
      else begin
        Part := Copy(Params, 1, n - 1);
        Params := Trim(Copy(Params, n + 1, Length(Params)));
        if Params = '' then Exit;
      end;
      n := Pos(':', Part);
      if n <> 0 then Part := Copy(Part, 1, n - 1);
      Part := Trim(Part);

      if (LowerCase(Copy(Part, 1, 4)) = 'var ')
      then Part := Trim(Copy(Part, 5, Length(Part)))
      else if (LowerCase(Copy(Part, 1, 4)) = 'out ')
      then Part := Trim(Copy(Part, 5, Length(Part)))
      else if (LowerCase(Copy(Part, 1, 6)) = 'const ')
      then Part := Trim(Copy(Part, 7, Length(Part)));

      repeat
        n := Pos(',', Part);
        if n = 0
        then begin
          S := Part;
          Part := '';
        end
        else begin
          S := Trim(Copy(Part, 1, n - 1));
          Part := Trim(Copy(Part, n + 1, Length(Part)));
        end;
        if S = ''
        then Exit
        else ProcParams.Add(S);

      until Part = '';
    end;
    Result := True;
  end;
var
  ParamStart, ParamEnd, ParamLen: Integer;
  S: String;
begin
  ProcParams.Clear;

  Result := False;

  ProcName := GetName(ADeclaration);
  if ProcName = '' then Exit;

  S := LowerCase(Trim(Copy(ADeclaration, 1, Pos(ProcName, ADeclaration) - 2)));
  if S = 'function'
  then ProcType := ptFunction
  else
    if S = 'procedure'
    then ProcType := ptProcedure
    else Exit;


  ParamStart := Pos('(', ADeclaration);
  ParamEnd := Pos(')', ADeclaration);
  if (ParamStart <> 0)
  and (ParamEnd <> 0)
  then ParamLen := ParamEnd - ParamStart - 1
  else ParamLen := 0;

  if ((ParamStart <> 0) <> (ParamEnd <> 0))
  or (ParamLen < 0)
  then Exit;

  if (ParamLen > 0)
  then Result := ProcessParams(Trim(Copy(ADeclaration, ParamStart + 1, ParamLen)))
  else Result := True;
end;

function FindPrevious(const Lines: TStringList; const AStart: Integer; const ATag, AFind: String): Integer;
var
  StartTag: String;
  InsertFound: Boolean;
begin
  InsertFound := False;
  StartTag := Format('##apiwiz##s%s##', [ATag]);
  for Result := AStart downto 0 do
  begin
    InsertFound := (Pos(AFind, Lines[Result]) <> 0) or (Pos(StartTag, Lines[Result]) <> 0);
    if InsertFound then Break;
  end;
  if InsertFound
  then Inc(Result)
  else Result := -1
end;

function FindPreviousNonBlank(const Lines: TStringList; const AStart: Integer; const ATag: String): Integer;
var
  StartTag: String;
  InsertFound: Boolean;
begin
  InsertFound := False;
  StartTag := Format('##apiwiz##s%s##', [ATag]);
  for Result := AStart downto 0 do
  begin
    InsertFound := (Lines[Result] <> '') or (Pos(StartTag, Lines[Result]) <> 0);
    if InsertFound then Break;
  end;
  if InsertFound
  then Inc(Result)
  else Result := -1
end;

function FindInsertPoint(const Lines: TStringList; const Tag, ProcName, FileName: String; const IsHeader: Boolean): Integer;
var
  StartFound, InsertFound: Boolean;
  StartTag, EndTag: String;
  Line, SearchName, LineName: String;
  n: Integer;
begin

  StartFound := False;
  InsertFound := False;
  StartTag := Format('##apiwiz##s%s##', [Tag]);
  EndTag   := Format('##apiwiz##e%s##', [Tag]);
  SearchName := LowerCase(ProcName);

  for Result := 0 to Lines.Count - 1 do
  begin
    Line := Lines[Result];
    if StartFound
    then begin
      if Pos(EndTag, Line) <> 0
      then begin
        LineName := '';
        InsertFound := True;
        Break;
      end;

      if (Line = '') or (Line[1] = ' ') then Continue;
      LineName := GetName(Line);
      if LineName = '' then Continue;

      n := CompareText(SearchName, LineName);
      if n = 0
      then begin
        ShowMessageFmt('Function exists in [%s]', [FileName]);
        Break;
      end
      else if n < 0
      then begin
        InsertFound := True;
        Break;
      end;
    end
    else begin
      StartFound := Pos(StartTag, Line) <> 0;
      Continue;
    end;
  end;

  if InsertFound
  then begin
    if IsHeader
    then begin
      if (LineName <> '')
      and (LowerCase(LineName[1]) <> LowerCase(Procname[1]))
      then Result := FindPreviousNonBlank(Lines, Result - 1, Tag);
    end
    else begin
      Result := FindPrevious(Lines, Result - 1, Tag, 'end;');
    end;
  end
  else Result := -1
end;

procedure InsertLines(const Idx: Integer; const TargetLines, SourceLines: TStringList);
var
  n: Integer;
begin
  for n := SourceLines.Count - 1 downto 0 do
  begin
    TargetLines.Insert(Idx, SourceLines[n]);
  end;
end;

procedure Scan(const AFileName, ATag: String; AResult: TStringList);
var
  StartFound: Boolean;
  StartTag, EndTag: String;
  Line, LineName: String;
  Lines: TStringListUTF8;
  n: Integer;
begin
  AResult.Clear;
  if not FileExistsUTF8(AFileName) then Exit;

  Lines := TStringListUTF8.Create;
  Lines.LoadFromFile(AFileName);

  StartFound := False;
  StartTag := Format('##apiwiz##s%s##', [ATag]);
  EndTag   := Format('##apiwiz##e%s##', [ATag]);

  for n := 0 to Lines.Count - 1 do
  begin
    Line := Lines[n];
    if Length(Line) < 8 then Continue; // min lenght of function,procedure,tag

    if not StartFound
    then begin
      StartFound := Pos(StartTag, Line) <> 0;
      Continue;
    end;

    if Pos(EndTag, Line) <> 0
    then begin
      Break;
    end;

    if Line[1] = ' ' then Continue;
    if (Line[1] = '/') and (Line[2] = '/') then Continue;

    LineName := GetName(Line);
    if LineName = '' then Continue;
    Line := GetPart([], ['{$'], Line);

    AResult.Add(line);
  end;

  Lines.Free;
end;

{ TApiWizForm }

procedure TApiWizForm.ApiWizFormCreate(Sender: TObject);
var
  n: Integer;
  S: String;
  WS: TApiWidgetset;
  C: TListColumn;
begin
  cbgLCLPlatform.Items.Clear;
  for WS := Low(WS) to High(Ws) do
    cbgLCLPlatform.Items.Add(WS_NAME[WS]);

  lvExisting.Columns.BeginUpdate;
  for WS := Low(WS) to High(Ws) do
  begin
    C := lvExisting.Columns.Add;
    C.Caption := WS_NAME[WS];
    C.Alignment := taCenter;
    C.AutoSize := True;
  end;
  // add dummy
  C := lvExisting.Columns.Add;
  C.Caption := '';
  C.AutoSize := True;
  lvExisting.Columns.EndUpdate;

  FLineInfo := TStringList.Create;
  FLineInfo.Sorted := True;
  FLineInfo.Duplicates := dupIgnore;
  FLineInfo.CaseSensitive := False;

  S := ParamStrUTF8(1);
  // find the lazarus dir
  if S = ''
  then begin
    S := ExtractFilePath(ParamStrUTF8(0));
    n := Pos('apiwizz', S);
    if n <> 0
    then S := Copy(S, 1, n - 7)
    else S := '';
  end;

  if S = ''
  then begin
    S := GetCurrentDirUTF8;
    n := Pos('apiwizz', S);
    if n <> 0 then S := Copy(S, 1, n - 7);
  end;
  txtLazarus.Text := S;
end;

procedure TApiWizForm.Clear;
var
  n: Integer;
begin
  lvExisting.Clear;
  for n := 0 to FLineInfo.Count - 1 do
    FLineInfo.Objects[n].Free;

  FLineInfo.Clear;
end;

procedure TApiWizForm.cmdCopyClick(Sender: TObject);
var
  Line: TApiLine;
  WS: TApiWidgetset;
begin
  if lvExisting.Selected = nil then Exit;

  Line := TApiLine(lvExisting.Selected.Data);
  if Line = nil then Exit; //???

  if txtDeclarations.Lines.Count = 0
  then begin
    txtDeclarations.Lines.Add(Line.Declaration);

    if Line.WinApi
    then rdgApiType.ItemIndex := 0
    else rdgApiType.ItemIndex := 1;

    rbIndependent.Checked := Line.Independent;
    rbDependent.Checked := not Line.Independent;

    for WS := Low(WS) to High(Ws) do
    begin
      cbgLCLPlatform.Checked[Ord(WS)] := not(WS in Line.Widgetsets);
    end;
    Exit;
  end;

  // Check match
  if Line.WinApi <> (rdgApiType.ItemIndex = 0)
  then begin
    ShowMessage('API type mismatch');
    Exit;
  end;

  if rbIndependent.Checked <> Line.Independent
  then begin
    ShowMessage('Dependency mismatch');
    Exit;
  end;

  txtDeclarations.Lines.Add(Line.Declaration);
  for WS := Low(WS) to High(Ws) do
  begin
    if (WS in Line.Widgetsets) then Continue;
    cbgLCLPlatform.Checked[Ord(WS)] := True;
  end;
end;

procedure TApiWizForm.cmdGenerateClick(Sender: TObject);
const
  RETURN_PROC: array[TProcType] of String = ('',  ' Nothing');
  PROC_DESC: array[TProcType] of String = ('Function',  'Procedure');
  PROC_RESULT: array[TProcType] of String = ('Result := ',  '');
var
  ApiText, ProcLines, ProcParams: TStringListUTF8;
  S, DeclarationText: String;
  ProcName, FileName, IntfBase: String;
  PlatformPrefix, PlatformDir, PlatformObject: String;
  Line, LineCount, n, Idx: Integer;
  ProcType: TProcType;
  WS: TApiWidgetset;

  procedure CreateLeadingCR;
  var
    LineName: String;
  begin
    // create leading CR if needed
    LineName := GetName(ApiText[Idx - 1]);
    if ((LineName <> '') and (LowerCase(ProcName)[1] <> LowerCase(LineName)[1]))
    or (Pos('##apiwiz##', ApiText[Idx]) <> 0)
    then ProcLines.Add('');
  end;
begin
  if txtDeclarations.Lines.Count = 0
  then begin
    ShowMessage('No declaration specified');
    Exit;
  end;

  case rdgApiType.ItemIndex of
    0: begin
      FileName := 'winapi';
      IntfBase := 'intfbasewinapi.inc';
    end;
    1: begin
      FileName := 'lclintf';
      IntfBase := 'intfbaselcl.inc';
    end;
  else
    ShowMessage('No API type selected');
    Exit;
  end;

  LineCount := 0;
  ProcParams := TStringListUTF8.Create;
  ApiText := TStringListUTF8.Create;
  ProcLines := TStringListUTF8.Create;
  try
    for Line := 0 to txtDeclarations.Lines.Count - 1 do
    begin
      DeclarationText := Trim(txtDeclarations.Lines[Line]);

      while (Length(DeclarationText) > 0) and (DeclarationText[Length(DeclarationText)] in [';', ' ']) do
      begin
        Delete(DeclarationText, Length(DeclarationText), 1);
      end;

      if Length(DeclarationText) = 0 then Continue;

      if not SplitDeclaration(DeclarationText, ProcType, ProcName, ProcParams)
      then begin
        if MessageDlg(Format('Line %d, bad formatted declaration: %s', [Line + 1, DeclarationText]),
                   mtError, [mbIgnore, mbAbort], 0) = mrIgnore then Continue;
        Exit;
      end;
      Inc(LineCount);

      //--------------------------------
      // open winapih.inc / lclintfh.inc
      //--------------------------------
      ApiText.LoadFromFile(txtLazarus.text + '/lcl/include/' + FileName + 'h.inc');
      Idx := FindInsertPoint(ApiText, 'ps', ProcName, FileName + 'h.inc', True);
      if Idx <> -1
      then begin
        ProcLines.Clear;
        CreateLeadingCR;
        if rbIndependent.Checked
        then ProcLines.Add(Format('//%s %s --> independent', [LowerCase(PROC_DESC[proctype]), procname]))
        else ProcLines.Add(DeclarationText + '; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}');

        InsertLines(Idx, ApiText, ProcLines);

        if rbIndependent.Checked
        then begin
          Idx := FindInsertPoint(ApiText, 'pi', ProcName, FileName + 'h.inc', True);
          if Idx <> -1
          then begin
            ProcLines.Clear;
            CreateLeadingCR;
            ProcLines.Add(DeclarationText + ';');
            InsertLines(Idx, ApiText, ProcLines);
          end;
        end;
        ApiText.SaveToFile(txtLazarus.text + '/lcl/include/' + FileName + 'h.inc');
      end;
      //------------------------------
      // open winapi.inc / lclintf.inc
      //------------------------------
      ApiText.LoadFromFile(txtLazarus.text + '/lcl/include/' + FileName + '.inc');
      if rbIndependent.Checked
      then begin
        Idx := FindInsertPoint(ApiText, 'pi', ProcName, FileName + '.inc', False);
        if Idx <> -1
        then begin
          with ProcLines do
          begin
            Clear;
            Add('');
            Add('{------------------------------------------------------------------------------');
            Add('  ' + PROC_DESC[ProcType] + ': ' + ProcName);
            if ProcParams.Count = 0
            then Add('  Params: none' )
            else begin
              Add('  Params: ' + ProcParams[0] + ':');
              for n := 1 to ProcParams.Count  - 1  do
                Add('          ' + ProcParams[n] + ':');
            end;
            Add('  Returns:' + RETURN_PROC[ProcType]);
            Add('');
            Add(' ------------------------------------------------------------------------------}');
            Add(DeclarationText + ';');
            Add('begin');
            Add('//TODO:'+ProcName);
            Add('  // your code here');
            Add('end;');
          end;
        end;
      end
      else begin
        Idx := FindInsertPoint(ApiText, 'ps', ProcName, FileName + '.inc', False);
        if Idx <> -1
        then begin

          if ProcParams.Count = 0
          then S := ''
          else begin
            S := '(' + ProcParams[0];
            for n := 1 to ProcParams.Count - 1 do
              S := S + ', ' + ProcParams[n];
            S := S + ')'
          end;

          with ProcLines do
          begin
            Clear;
            Add('');
            Add(DeclarationText + ';');
            Add('begin');
            Add('  ' + PROC_RESULT[ProcType] + 'WidgetSet.' + ProcName + S + ';');
            Add('end;');
          end;
        end;
      end;
      if Idx <> -1
      then begin
        InsertLines(Idx, ApiText, ProcLines);
        ApiText.SaveToFile(txtLazarus.text + '/lcl/include/' + FileName + '.inc');
      end;

      // ++++++++++++++++++++++++++++++ //
      // from here only dependent stuff //
      // ++++++++++++++++++++++++++++++ //
      if rbDependent.Checked
      then begin
        //------------------------------------------
        // open intfbasewinapi.inc / intfbaselcl.inc
        //------------------------------------------
        ApiText.LoadFromFile(txtLazarus.text + '/lcl/include/' + IntfBase);
        Idx := FindInsertPoint(ApiText, 'ps', ProcName, IntfBase, False);
        if Idx <> -1
        then begin
          S := DeclarationText;
          // Remove spaces
          while S[DECL_OFFSET[ProcType]] = ' ' do Delete(S, DECL_OFFSET[ProcType], 1);

          System.Insert(' TWidgetSet.', S, DECL_OFFSET[ProcType]);
          with ProcLines do
          begin
            Clear;
            Add('');
            Add(S + ';');
            Add('begin');
            if ProcType = ptFunction
            then begin
              Add('  // Your default here');
              Add('  // Result := ');
            end;
            Add('end;');
          end;
          InsertLines(Idx, ApiText, ProcLines);
          ApiText.SaveToFile(txtLazarus.text + '/lcl/include/' + IntfBase);
        end;

        for WS := Low(WS) to High(Ws) do
        begin
          if not cbgLCLPlatform.Checked[Ord(WS)] then Continue;

          PlatformPrefix := WS_NAME[WS];
          PlatformDir := WS_NAME[WS];
          PlatformObject := 'T' + WS_NAME[WS] + 'WidgetSet';
          PlatformObject[2] := UpCase(PlatformObject[2]);

          //------------------
          // open *winapih.inc
          //------------------
          ApiText.LoadFromFile(txtLazarus.text + '/lcl/interfaces/' + PlatformDir + '/' + PlatformPrefix + FileName + 'h.inc');
          Idx := FindInsertPoint(ApiText, 'ps', ProcName, PlatformPrefix + FileName + 'h.inc', True);
          if IDX <> -1
          then begin
          	ProcLines.Clear;
            CreateLeadingCR;
            ProcLines.Add(DeclarationText + '; override;');
            InsertLines(Idx, ApiText, ProcLines);
            ApiText.SaveToFile(txtLazarus.text + '/lcl/interfaces/' + PlatformDir + '/' + PlatformPrefix + FileName + 'h.inc');
          end;

          //-----------------
          // open *winapi.inc
          //-----------------
          ApiText.LoadFromFile(txtLazarus.text + '/lcl/interfaces/' + PlatformDir + '/' + PlatformPrefix + FileName + '.inc');
          Idx := FindInsertPoint(ApiText, 'ps', ProcName, PlatformPrefix + FileName + '.inc', False);
          if Idx <> -1
          then begin
            S := DeclarationText;
            // Remove spaces
            while S[DECL_OFFSET[ProcType]] = ' ' do Delete(S, DECL_OFFSET[ProcType], 1);

            System.Insert(' ' + PlatformObject + '.', S, DECL_OFFSET[ProcType]);
            with ProcLines do
            begin
              Clear;
              Add('');
              Add('{------------------------------------------------------------------------------' );
              Add('  ' + PROC_DESC[ProcType] + ': ' + ProcName );
              if ProcParams.Count = 0
              then Add('  Params: none' )
              else begin
                Add('  Params: ' + ProcParams[0] + ':');
                for n := 1 to ProcParams.Count  - 1  do
                  Add('          ' + ProcParams[n] + ':');
              end;
              Add('  Returns:' + RETURN_PROC[ProcType] );
              Add('' );
              Add(' ------------------------------------------------------------------------------}' );
              Add(S + ';');
              Add('begin' );
              Add('  // Your code here' );
              Add('end;');
            end;
            InsertLines(Idx, ApiText, ProcLines);
            ApiText.SaveToFile(txtLazarus.text + '/lcl/interfaces/' + PlatformDir + '/' + PlatformPrefix + FileName + '.inc');
          end;
        end;
      end;
    end;
  finally
    ApiText.Free;
    ProcLines.Free;
    ProcParams.Free;

    ShowMessage(Format('%d lines inserted', [LineCount]));
  end;
end;

procedure TApiWizForm.cmdScanClick(Sender: TObject);
var
  Lines: TStringlist;
  S: String;
  n, idx: Integer;
  Line: TApiLine;
  Item: TListItem;
  WS: TApiWidgetset;
begin
  Screen.Cursor := crHourGlass;
  lvExisting.BeginUpdate;
  Clear;

  Lines := TStringList.Create;
  Scan(txtLazarus.text + '/lcl/include/winapih.inc', 'ps', Lines);
  for n := 0 to Lines.Count - 1 do
  begin
    Line := TApiLine.Create;
    Line.Declaration := Lines[n];
    Line.WinApi := True;
    S := GetName(Lines[n]);
    FLineInfo.AddObject(S, Line);
  end;

  Scan(txtLazarus.text + '/lcl/include/winapih.inc', 'pi', Lines);
  for n := 0 to Lines.Count - 1 do
  begin
    S := GetName(Lines[n]);
    if FLineInfo.IndexOf(s) >= 0 then Continue;  //overloaded

    Line := TApiLine.Create;
    Line.Independent := True;
    Line.WinApi := True;
    Line.Declaration := Lines[n];
    FLineInfo.AddObject(S, Line);
  end;

  Scan(txtLazarus.text + '/lcl/include/lclintfh.inc', 'ps', Lines);
  for n := 0 to Lines.Count - 1 do
  begin
    Line := TApiLine.Create;
    Line.Declaration := Lines[n];
    S := GetName(Lines[n]);
    FLineInfo.AddObject(S, Line);
  end;

  Scan(txtLazarus.text + '/lcl/include/lclintfh.inc', 'pi', Lines);
  for n := 0 to Lines.Count - 1 do
  begin
    S := GetName(Lines[n]);
    if FLineInfo.IndexOf(s) >= 0 then Continue;  //overloaded

    Line := TApiLine.Create;
    Line.Independent := True;
    Line.Declaration := Lines[n];
    FLineInfo.AddObject(S, Line);
  end;

  // implementations

  Scan(txtLazarus.text + '/lcl/include/intfbasewinapi.inc', 'ps', Lines);
  for n := 0 to Lines.Count - 1 do
  begin
    S := GetName(Lines[n]);
    idx := FLineInfo.IndexOf(S);
    if idx = -1 then Continue;

    Line := TApiLine(FLineInfo.Objects[idx]);
    Line.Base := True;
  end;

  Scan(txtLazarus.text + '/lcl/include/intfbaselcl.inc', 'ps', Lines);
  for n := 0 to Lines.Count - 1 do
  begin
    S := GetName(Lines[n]);
    idx := FLineInfo.IndexOf(S);
    if idx = -1 then Continue;

    Line := TApiLine(FLineInfo.Objects[idx]);
    Line.Base := True;
  end;

  for WS := Low(WS) to High(Ws) do
  begin
    Scan(txtLazarus.text + '/lcl/interfaces/' + WS_NAME[WS] + '/' +  WS_NAME[WS] + 'winapih.inc', 'ps', Lines);
    for n := 0 to Lines.Count - 1 do
    begin
      S := GetName(Lines[n]);
      idx := FLineInfo.IndexOf(S);
      if idx = -1 then Continue;

      Line := TApiLine(FLineInfo.Objects[idx]);
      Include(Line.Widgetsets, WS);
    end;

    Scan(txtLazarus.text + '/lcl/interfaces/' + WS_NAME[WS] + '/' +  WS_NAME[WS] + 'lclintfh.inc', 'ps', Lines);
    for n := 0 to Lines.Count - 1 do
    begin
      S := GetName(Lines[n]);
      idx := FLineInfo.IndexOf(S);
      if idx = -1 then Continue;

      Line := TApiLine(FLineInfo.Objects[idx]);
      Include(Line.Widgetsets, WS);
    end;
  end;


  for n := 0 to FLineInfo.Count - 1 do
  begin
    Item := lvExisting.Items.Add;
    Item.Caption := FLineInfo[n];
    Line := TApiLine(FLineInfo.Objects[n]);
    Item.Data := Line;

    if Line.WinApi
    then Item.SubItems.Add('Win')
    else Item.SubItems.Add('LCL');

    if Line.Independent
    then Item.SubItems.Add('X')
    else Item.SubItems.Add('');

    if Line.Base
    then Item.SubItems.Add('X')
    else begin
      if Line.Independent
      then Item.SubItems.Add('-')
      else Item.SubItems.Add('');
    end;

    for WS := Low(WS) to High(Ws) do
    begin
      if WS in Line.Widgetsets
      then Item.SubItems.Add('X')
      else begin
        if Line.Independent
        then Item.SubItems.Add('-')
        else Item.SubItems.Add('');
      end;
    end;
  end;

  lvExisting.EndUpdate;

  Lines.Free;

  Screen.Cursor := crDefault;
end;

procedure TApiWizForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLineInfo);
end;

procedure TApiWizForm.rbPlatformDependencyClick (Sender: TObject );
begin
  cbgLCLPlatform.Enabled := rbDependent.Checked;
end;

procedure TApiWizForm.rdgApiTypeClick (Sender: TObject );
begin

end;

end.
