{  $Id$  }
{
 /***************************************************************************
                          delphiunit2laz.pas
                          ------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Functions to convert delphi units to lcl units.

}
unit DelphiUnit2Laz;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, FileCtrl,
  // Components
  SynEdit, CodeCache, CodeToolManager, DefineTemplates,
  // IDE
  DialogProcs, IDEProcs, LazarusIDEStrConsts;

type
  TDelphi2LazarusDialog = class(TForm)
  private
  public
  end;

var
  Delphi2LazarusDialog: TDelphi2LazarusDialog;
  
function CheckDelphiFileExt(const Filename: string): TModalResult;
function CheckFilenameForLCLPaths(const Filename: string): TModalResult;
function ConvertDelphiToLazarusFilename(const DelphiFilename: string): string;
function ConvertDFMToLFMFilename(const DFMFilename: string;
  KeepCase: boolean): string;
function FindDFMFileForDelphiUnit(const DelphiFilename: string): string;
function RenameDelphiUnitToLazarusUnit(const DelphiFilename: string;
  RenameDFMFile: boolean): TModalResult;
function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
function ConvertDelphiSourceToLazarusSource(const LazarusUnitFilename: string;
  AddLRSCode: boolean): TModalResult;
function LoadUnitAndLFMFile(const UnitFileName: string;
  var UnitCode, LFMCode: TCodeBuffer): TModalResult;
function ConvertLFMtoLRSfile(const LFMFilename: string): TModalResult;

implementation

function CheckDelphiFileExt(const Filename: string): TModalResult;
begin
  if CompareFileExt(Filename,'.pas',false)<>0 then begin
    Result:=MessageDlg('Not a Delphi unit',
      'The file "'+Filename+'" is not a Delphi unit.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  Result:=mrOk;
end;

function CheckFilenameForLCLPaths(const Filename: string): TModalResult;
// check if the unitpath of the directory of filename contains the path to the
// LCL
var
  Directory: String;
  UnitPath: String;
  LazarusSrcDir: string;
  LCLPath: String;
  NextStartPos: Integer;
begin
  // get directory of filename
  Directory:=ExtractFilePath(Filename);
  // get unitpath definition of directory
  UnitPath:=CodeToolBoss.GetUnitPathForDirectory(Directory);
  // get lazarus source directory
  LazarusSrcDir:=
           CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LazarusDir'];
  // create base path to LCL compiled units <LazarusSrcDir>/lcl/units/
  LCLPath:=TrimFilename(LazarusSrcDir+SetDirSeparators('/lcl/units/'));
  NextStartPos:=1;
  writeln('CheckFilenameForLCLPaths UnitPath="',UnitPath,'" LCLPath="',LCLPath,'"');
  if GetNextUsedDirectoryInSearchPath(UnitPath,LCLPath,NextStartPos)='' then
  begin
    LCLPath:=LCLPath+'$(TargetCPU)'+PathDelim+'$(TargetOS)';
    Result:=MessageDlg('LCL unit path missing',
      'The current unit path for the file'#13
      +'"'+Filename+'" is'#13
      +'"'+UnitPath+'".'#13
      +#13
      +'The path to the LCL units "'+LCLPath+'" is missing.'#13
      +#13
      +'Hint for newbies:'#13
      +'Create a lazarus application and put the file into the project directory.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  Result:=mrOk;
end;

function ConvertDelphiToLazarusFilename(const DelphiFilename: string): string;
begin
  Result:=ExtractFilePath(DelphiFilename)
          +lowercase(ExtractFileName(DelphiFilename));
end;

function ConvertDFMToLFMFilename(const DFMFilename: string;
  KeepCase: boolean): string;
begin
  if DFMFilename<>'' then begin
    // platform and fpc independent unitnames are lowercase, so are the lfm files
    Result:=lowercase(ExtractFilenameOnly(DFMFilename));
    if KeepCase then
      Result:=ExtractFilenameOnly(DFMFilename);
    Result:=ExtractFilePath(DFMFilename)+Result+'.lfm';
  end else
    Result:='';
end;

function FindDFMFileForDelphiUnit(const DelphiFilename: string): string;
begin
  Result:=ChangeFileExt(DelphiFilename,'.dfm');
  if FileExists(Result) then exit;
  Result:=ChangeFileExt(DelphiFilename,'.DFM');
  if FileExists(Result) then exit;
  Result:='';
end;

function RenameDelphiUnitToLazarusUnit(const DelphiFilename: string;
  RenameDFMFile: boolean): TModalResult;
var
  LazarusFilename: String;
  DFMFilename: String;
  LFMFilename: String;
begin
  LazarusFilename:=ConvertDelphiToLazarusFilename(DelphiFilename);
  LFMFilename:='';
  Result:=RenameFileWithErrorDialogs(DelphiFilename,LazarusFilename,[mbAbort]);
  if Result<>mrOK then exit;
  if RenameDFMFile then begin
    DFMFilename:=FindDFMFileForDelphiUnit(DelphiFilename);
    if DFMFilename<>'' then begin
      LFMFilename:=ConvertDFMToLFMFilename(DFMFilename,false);
      Result:=RenameFileWithErrorDialogs(DFMFilename,LFMFilename,[mbAbort]);
      if Result<>mrOK then exit;
    end;
  end;
  Result:=mrOk;
end;

function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
var
  DFMStream, LFMStream: TMemoryStream;
  LFMFilename: string;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    try
      DFMStream.LoadFromFile(DFMFilename);
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsReadError, Format(
          lisUnableToReadFileError, ['"', DFMFilename, '"', #13, E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
    try
      FormDataToText(DFMStream,LFMStream);
    except
      on E: Exception do begin
        Result:=MessageDlg(lisFormatError,
          Format(lisUnableToConvertFileError, ['"', DFMFilename, '"', #13,
            E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
    // converting dfm file, without renaming unit -> keep case
    LFMFilename:=ConvertDFMToLFMFilename(DFMFilename,true);
    try
      LFMStream.SaveToFile(LFMFilename);
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsWriteError,
          Format(lisUnableToWriteFileError, ['"', LFMFilename, '"', #13,
            E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
  finally
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;

function ConvertDelphiSourceToLazarusSource(const LazarusUnitFilename: string;
  AddLRSCode: boolean): TModalResult;
var
  LazUnitCode: TCodeBuffer;
  CTResult: Boolean;
begin
  Result:=LoadCodeBuffer(LazUnitCode,LazarusUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  CTResult:=CodeToolBoss.ConvertDelphiToLazarusSource(LazUnitCode,AddLRSCode);
  if not CTResult then begin
    Result:=mrCancel;
    exit;
  end;
  Result:=mrOk;
end;

function LoadUnitAndLFMFile(const UnitFileName: string;
  var UnitCode, LFMCode: TCodeBuffer): TModalResult;
var
  LFMFilename: string;
begin
  UnitCode:=nil;
  LFMCode:=nil;
  Result:=LoadCodeBuffer(UnitCode,UnitFileName,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  LFMFilename:=ChangeFileExt(UnitFileName,'.lfm');
  if FileExists(LFMFilename) then begin
    Result:=LoadCodeBuffer(LFMCode,LFMFilename,
                           [lbfCheckIfText,lbfUpdateFromDisk]);
    if Result<>mrOk then exit;
  end;
end;

function ConvertLFMtoLRSfile(const LFMFilename: string): TModalResult;
begin
  if not LFMtoLRSfile(LFMFilename) then begin
    Result:=MessageDlg('Error creating lrs',
      'Unable to convert lfm to lrs and write lrs file.',
      mtError,[mbCancel],0);
    exit;
  end;
  Result:=mrOk;
end;

initialization
  {$I delphiunit2laz.lrs}

end.

