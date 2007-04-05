{  $Id$  }
{
 /***************************************************************************
                                APIWiZZard.pp 
                             -------------------
                   APIWiZZ is an API wizzard to generate WINAPI
                   Templates for GTK.

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type
  TApiWizForm = class(TForm)
    cbgLCLPlatform: TCHECKGROUP;
    cmdGenerate: TButton;
    Groupbox1: TGROUPBOX;
    txtDeclarations: TMemo;
    rbIndependent: TRADIOBUTTON;
    rbDependent: TRADIOBUTTON;
    rdgApiType: TRadioGroup;
    lblDeclare: TLabel;
    txtLazarus: TEdit;
    lblLazarus: TLabel;
    procedure cmdGenerateClick(Sender: TObject);
    procedure ApiWizFormCreate(Sender: TObject);
    procedure rbPlatformDependencyClick(Sender: TObject);
    procedure rdgApiTypeClick (Sender: TObject );
  private
  public
  end;

  TProcType = (ptFunction, ptProcedure);

var
  ApiWizForm: TApiWizForm;

implementation

const
  DECL_OFFSET: array[TProctype] of Integer = (9, 10);

{ TApiWizForm }

procedure TApiWizForm.ApiWizFormCreate(Sender: TObject);
var
  n: Integer;
  S: String;
begin
  S := ParamStr(1);
  // find the lazarus dir
  if S = ''
  then begin
    S := ExtractFilePath(ParamStr(0));
    n := Pos('apiwizz', S);
    if n <> 0
    then S := Copy(S, 1, n - 7)
    else S := '';
  end;

  if S = ''
  then begin
    S := GetCurrentDir;
    n := Pos('apiwizz', S);
    if n <> 0 then S := Copy(S, 1, n - 7);
  end;
  txtLazarus.Text := S;
end;

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

function SplitDeclaration(const ADeclaration: String; var ProcType: TProcType; var ProcName: String; const ProcParams: TStringList): Boolean;
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

procedure TApiWizForm.cmdGenerateClick(Sender: TObject);
const
  RETURN_PROC: array[TProcType] of String = ('',  ' Nothing');
  PROC_DESC: array[TProcType] of String = ('Function',  'Procedure');
  PROC_RESULT: array[TProcType] of String = ('Result := ',  '');
var
  ApiText, ProcLines, ProcParams: TStringList;
  S, DeclarationText: String;
  ProcName, FileName, IntfBase: String;
  PlatformPrefix, PlatformDir, PlatformObject: String;
  Line, LineCount, n, Idx, PlatformIdx: Integer;
  ProcType: TProcType;

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
  ProcParams := TStringList.Create;
  ApiText := TStringList.Create;
  ProcLines := TStringList.Create;
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

        for PlatformIdx := 0 to cbgLCLPlatform.Items.Count - 1 do
        begin
          if not cbgLCLPlatform.Checked[PlatformIdx] then Continue;

          // for now they can all be based on the check caption
          PlatformPrefix := cbgLCLPlatform.Items[PlatformIdx];
          PlatformDir := PlatformPrefix;
          PlatformObject := 'T' + PlatformPrefix + 'WidgetSet';
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

procedure TApiWizForm.rbPlatformDependencyClick (Sender: TObject );
begin
  cbgLCLPlatform.Enabled := rbDependent.Checked;
end;

procedure TApiWizForm.rdgApiTypeClick (Sender: TObject );
begin

end;

initialization
  {$I apiwizard.lrs}

end.
