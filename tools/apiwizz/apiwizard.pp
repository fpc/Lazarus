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
@author(M. Weustink <weus@quicknet.nl>)                       
@created(02-May-2000)
}
unit APIWizard;

interface

{$Mode objfpc}

uses
  LCLLinux, buttons, 
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    txtDeclare: TEdit;
    lblDeclare: TLabel;
    txtLazarus: TEdit;
    lblLazarus: TLabel;
    chkIndependent: TCheckBox;
    cmdGenerate: TButton;
    procedure cmdGenerateClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TProcType = (ptFunction, ptProcedure);

var
  Form1: TForm1;

implementation

const
  DECL_OFFSET: array[TProctype] of Integer = (9, 10);

constructor TForm1.Create(AOwner: TComponent);
var
  n: Integer;
  S: String;
begin
  inherited Create(AOwner);

  Width := 513;
  Height := 186;
  Caption := 'ApiWiZZ';

  lblDeclare := TLabel.Create(Self);
  with lblDeclare do
  begin
  	Parent := Self;
    Left := 4;
    Top := 68;
    Width := 120;
    Height := 16;
    Caption := 'Declaration:';
    Visible := True;
  end;

  lblLazarus := TLabel.Create(Self);
  with lblLazarus do
  begin
  	Parent := Self;
    Left := 4;
    Top := 16;
    Width := 120;
    Height := 16;
    Caption := 'Lazarus dir:';
    Visible := True;
  end;

  txtDeclare := TEdit.Create(Self);
  with txtDeclare do
  begin
  	Parent := Self;
    Left := 4;
    Top := 88;
    Width := 493;
    Height := 24;
    TabOrder := 0;
    Visible := True;
  end;

  txtLazarus := TEdit.Create(Self);
  with txtLazarus do
  begin
  	Parent := Self;
    Left := 4;
    Top := 36;
    Width := 493;
    Height := 24;
    TabOrder := 1;
    Visible := True;
  end;

  chkIndependent := TCheckBox.Create(Self);
  with chkIndependent do
  begin
  	Parent := Self;
    Left := 4;
    Top := 124;
    Width := 400;
    Height := 17;
    Caption := 'Platform independent';
    TabOrder := 2;
    Visible := True;
  end;

  cmdGenerate := TButton.Create(Self);
  with cmdGenerate do
  begin
  	Parent := Self;
    Left := 420;
    Top := 120;
    Width := 75;
    Height := 25;
    Caption := 'Generate';
    TabOrder := 3;
    OnClick := @cmdGenerateClick;
    Visible := True;
  end;


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

procedure ShowMessage(const Msg: string);
var
  pStr: PChar;
begin
//  ShowMessagePos(Msg, -1, -1);
  pStr := StrAlloc(Length(Msg) + 1);
  try
    StrPCopy(pStr, Msg);
    MessageBox(0, pStr, '', 0);
  finally
    strDispose(pStr);
  end;
end;

procedure ShowMessageFmt(const Msg: string; Params: array of const);
begin
  ShowMessage(Format(Msg, Params));
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

procedure TForm1.cmdGenerateClick(Sender: TObject);
const
  RETURN_PROC: array[TProcType] of String = ('',  ' Nothing');
  PROC_DESC: array[TProcType] of String = ('Function',  'Procedure');
  PROC_RESULT: array[TProcType] of String = ('Result := ',  '');
var
  ApiText, ProcLines, ProcParams: TStringList;
  S, DeclarationText: String;
  ProcName: String;
  n, Idx: Integer;
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
  DeclarationText := Trim(txtDeclare.Text);
  while DeclarationText[Length(DeclarationText)] = ';' do Delete(DeclarationText, Length(DeclarationText), 1);

  if Trim(DeclarationText) = '' then Exit;

  ProcParams := TStringList.Create;
  try
    if not SplitDeclaration(DeclarationText, ProcType, ProcName, ProcParams)
    then begin
      ShowMessage('Bad formatted declaration');
      Exit;
    end;

    ApiText := TStringList.Create;
    ProcLines := TStringList.Create;
    try
      //-----------------
      // open winapih.inc
      //-----------------
      ApiText.LoadFromFile(txtLazarus.text + '/lcl/include/winapih.inc');
      Idx := FindInsertPoint(ApiText, 'ps', ProcName, 'winapih.inc', True);
      if Idx <> -1
      then begin
        ProcLines.Clear;
        CreateLeadingCR;
        if chkIndependent.Checked
        then ProcLines.Add(Format('//%s %s --> independent', [LowerCase(PROC_DESC[proctype]), procname]))
        else ProcLines.Add(DeclarationText + '; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}');

        InsertLines(Idx, ApiText, ProcLines);

        if chkIndependent.Checked
        then begin
          Idx := FindInsertPoint(ApiText, 'pi', ProcName, 'winapih.inc', True);
          if Idx <> -1
          then begin
            ProcLines.Clear;
            CreateLeadingCR;
            ProcLines.Add(DeclarationText + ';');
            InsertLines(Idx, ApiText, ProcLines);
          end;
        end;
        ApiText.SaveToFile(txtLazarus.text + '/lcl/include/winapih.inc');
      end;
      //-----------------
      // open winapi.inc
      //-----------------
      ApiText.LoadFromFile(txtLazarus.text + '/lcl/include/winapi.inc');
      if chkIndependent.Checked
      then begin
        Idx := FindInsertPoint(ApiText, 'pi', ProcName, 'winapi.inc', False);
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
        Idx := FindInsertPoint(ApiText, 'ps', ProcName, 'winapi.inc', False);
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
            Add('  ' + PROC_RESULT[ProcType] + 'InterfaceObject.' + ProcName + S + ';');
            Add('end;');
          end;
        end;
      end;
      if Idx <> -1
      then begin
        InsertLines(Idx, ApiText, ProcLines);
        ApiText.SaveToFile(txtLazarus.text + '/lcl/include/winapi.inc');
      end;

      // ++++++++++++++++++
      // from here only dependent  stuff
      // ++++++++++++++++++
      if not chkIndependent.Checked
      then begin
        //-----------------
        // open interfacebase.inc
        //-----------------
        ApiText.LoadFromFile(txtLazarus.text + '/lcl/include/interfacebase.inc');
        Idx := FindInsertPoint(ApiText, 'ps', ProcName, 'interfacebase.inc', False);
        if Idx <> -1
        then begin
          S := DeclarationText;
          // Remove spaces
          while S[DECL_OFFSET[ProcType]] = ' ' do Delete(S, DECL_OFFSET[ProcType], 1);

          System.Insert(' TInterfaceBase.', S, DECL_OFFSET[ProcType]);
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
          ApiText.SaveToFile(txtLazarus.text + '/lcl/include/interfacebase.inc');
        end;

        //-----------------
        // open gtkwinapih.inc
        //-----------------
        ApiText.LoadFromFile(txtLazarus.text + '/lcl/interfaces/gtk/gtkwinapih.inc');
        Idx := FindInsertPoint(ApiText, 'ps', ProcName, 'gtkwinapih.inc', True);
        if IDX <> -1
        then begin
        	ProcLines.Clear;
          CreateLeadingCR;
          ProcLines.Add(DeclarationText + '; override;');
          InsertLines(Idx, ApiText, ProcLines);
          ApiText.SaveToFile(txtLazarus.text + '/lcl/interfaces/gtk/gtkwinapih.inc');
        end;

        //-----------------
        // open gtkwinapi.inc
        //-----------------
        ApiText.LoadFromFile(txtLazarus.text + '/lcl/interfaces/gtk/gtkwinapi.inc');
        Idx := FindInsertPoint(ApiText, 'ps', ProcName, 'gtkwinapi.inc', False);
        if Idx <> -1
        then begin
          S := DeclarationText;
          // Remove spaces
          while S[DECL_OFFSET[ProcType]] = ' ' do Delete(S, DECL_OFFSET[ProcType], 1);

          System.Insert(' TGTKObject.', S, DECL_OFFSET[ProcType]);
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
          ApiText.SaveToFile(txtLazarus.text + '/lcl/interfaces/gtk/gtkwinapi.inc');
        end;
      end;
    finally
      ApiText.Free;
      ProcLines.Free;
    end;
  finally
    ProcParams.Free;
  end;
end;

end.
{ =============================================================================

  $Log$
  Revision 1.2  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

  Revision 1.2  2000/05/08 15:57:00  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.1  2000/05/03 21:48:45  lazarus
  MWE:
    * Fixed wizard  typo
    + added phony entries to makefile

  Revision 1.1  2000/05/03 00:27:06  lazarus
  MWE:
    + First rollout of the API wizard.

}
