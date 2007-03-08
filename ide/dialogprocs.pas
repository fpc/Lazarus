{  $Id$  }
{
 /***************************************************************************
                            dialogprocs.pas
                            ---------------

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
    Common IDE functions with MessageDlg(s) for errors.
}
unit DialogProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Dialogs, FileUtil,
  CodeCache, CodeToolManager, AVL_Tree,
  LazIDEIntf,
  IDEProcs, LazarusIDEStrConsts, IDEDialogs;

type
  // load buffer flags
  TLoadBufferFlag = (
    lbfUpdateFromDisk,
    lbfRevert,
    lbfCheckIfText,
    lbfQuiet,
    lbfCreateClearOnError
    );
  TLoadBufferFlags = set of TLoadBufferFlag;
  
  TOnBackupFileInteractive =
                       function(const Filename: string): TModalResult of object;
                       
var
  OnBackupFileInteractive: TOnBackupFileInteractive = nil;

function BackupFileInteractive(const Filename: string): TModalResult;
function RenameFileWithErrorDialogs(const SrcFilename, DestFilename: string;
                                    ExtraButtons: TMsgDlgButtons): TModalResult;
function CopyFileWithErrorDialogs(const SrcFilename, DestFilename: string;
                                  ExtraButtons: TMsgDlgButtons): TModalResult;
function LoadCodeBuffer(var ACodeBuffer: TCodeBuffer; const AFilename: string;
                        Flags: TLoadBufferFlags): TModalResult;
function SaveCodeBuffer(ACodeBuffer: TCodeBuffer): TModalResult;
function SaveCodeBufferToFile(ACodeBuffer: TCodeBuffer;
                         const Filename: string; Backup: boolean = false): TModalResult;
function LoadStringListFromFile(const Filename, ListTitle: string;
                                var sl: TStrings): TModalResult;
function CreateEmptyFile(const Filename: string;
                         ErrorButtons: TMsgDlgButtons): TModalResult;
function CheckCreatingFile(const AFilename: string;
                           CheckReadable: boolean;
                           WarnOverwrite: boolean = false;
                           CreateBackup: boolean = false
                           ): TModalResult;
function CheckFileIsWritable(const Filename: string;
                             ErrorButtons: TMsgDlgButtons): TModalResult;
function ForceDirectoryInteractive(Directory: string;
                                   ErrorButtons: TMsgDlgButtons): TModalResult;
function DeleteFileInteractive(const Filename: string;
                               ErrorButtons: TMsgDlgButtons): TModalResult;
function SaveStringToFile(const Filename, Content: string;
                        ErrorButtons: TMsgDlgButtons; const Context: string = ''
                        ): TModalResult;
function ConvertLFMToLRSFileInteractive(const LFMFilename,
                                        LRSFilename: string): TModalResult;
function IfNotOkJumpToCodetoolErrorAndAskToAbort(Ok: boolean;
                            Ask: boolean; out NewResult: TModalResult): boolean;
function JumpToCodetoolErrorAndAskToAbort(Ask: boolean): TModalResult;
procedure NotImplementedDialog(const Feature: string);

implementation

function BackupFileInteractive(const Filename: string): TModalResult;
begin
  if Assigned(OnBackupFileInteractive) then
    Result:=OnBackupFileInteractive(Filename)
  else
    Result:=mrOk;
end;

function RenameFileWithErrorDialogs(const SrcFilename, DestFilename: string;
  ExtraButtons: TMsgDlgButtons): TModalResult;
var
  DlgButtons: TMsgDlgButtons;
begin
  if SrcFilename=DestFilename then begin
    Result:=mrOk;
    exit;
  end;
  repeat
    if RenameFile(SrcFilename,DestFilename) then begin
      break;
    end else begin
      DlgButtons:=[mbCancel,mbRetry]+ExtraButtons;
      Result:=IDEMessageDialog(lisUnableToRenameFile,
        Format(lisUnableToRenameFileTo2, ['"', SrcFilename, '"', #13, '"',
          DestFilename, '"']),
        mtError,DlgButtons);
      if (Result<>mrRetry) then exit;
    end;
  until false;
  Result:=mrOk;
end;

function CopyFileWithErrorDialogs(const SrcFilename, DestFilename: string;
  ExtraButtons: TMsgDlgButtons): TModalResult;
var
  DlgButtons: TMsgDlgButtons;
begin
  if CompareFilenames(SrcFilename,DestFilename)=0 then begin
    Result:=mrAbort;
    IDEMessageDialog(lisUnableToCopyFile,
      Format(lisSourceAndDestinationAreTheSame, [#13, SrcFilename]), mtError, [
        mbAbort]);
    exit;
  end;
  repeat
    if CopyFile(SrcFilename,DestFilename) then begin
      break;
    end else begin
      DlgButtons:=[mbCancel,mbRetry]+ExtraButtons;
      Result:=IDEMessageDialog(lisUnableToCopyFile,
        Format(lisUnableToCopyFileTo2, ['"', SrcFilename, '"', #13, '"',
          DestFilename, '"']),
        mtError,DlgButtons);
      if (Result<>mrRetry) then exit;
    end;
  until false;
  Result:=mrOk;
end;

function LoadCodeBuffer(var ACodeBuffer: TCodeBuffer; const AFilename: string;
  Flags: TLoadBufferFlags): TModalResult;
var
  ACaption, AText: string;
  FileReadable: boolean;
begin
  if [lbfUpdateFromDisk,lbfRevert]*Flags=[] then begin
    // can use cache
    ACodeBuffer:=CodeToolBoss.LoadFile(AFilename,false,false);
    if ACodeBuffer<>nil then begin
      // file is in cache
      if (not (lbfCheckIfText in Flags)) or ACodeBuffer.SourceIsText then begin
        Result:=mrOk;
        exit;
      end;
    end;
  end;
  repeat
    FileReadable:=true;
    if (lbfCheckIfText in Flags)
    and (not FileIsText(AFilename,FileReadable)) and FileReadable
    then begin
      if lbfQuiet in Flags then begin
        Result:=mrCancel;
      end else begin
        ACaption:=lisFileNotText;
        AText:=Format(lisFileDoesNotLookLikeATextFileOpenItAnyway2, ['"',
          AFilename, '"', #13, #13]);
        Result:=IDEMessageDialog(ACaption, AText, mtConfirmation,
                           [mbOk, mbIgnore, mbAbort]);
      end;
      if Result<>mrOk then break;
    end;
    if FileReadable then
      ACodeBuffer:=CodeToolBoss.LoadFile(AFilename,lbfUpdateFromDisk in Flags,
                                         lbfRevert in Flags)
    else
      ACodeBuffer:=nil;
                                         
    if ACodeBuffer<>nil then begin
      Result:=mrOk;
    end else begin
      if lbfQuiet in Flags then
        Result:=mrCancel
      else begin
        ACaption:=lisReadError;
        AText:=Format(lisUnableToReadFile2, ['"', AFilename, '"']);
        Result:=IDEMessageDialog(ACaption,AText,mtError,[mbAbort,mbRetry,mbIgnore]);
      end;
      if Result=mrAbort then break;
    end;
  until Result<>mrRetry;
  if (ACodeBuffer=nil) and (lbfCreateClearOnError in Flags) then begin
    ACodeBuffer:=CodeToolBoss.CreateFile(AFilename);
    if ACodeBuffer<>nil then
      Result:=mrOk;
  end;
end;

function SaveCodeBuffer(ACodeBuffer: TCodeBuffer): TModalResult;
begin
  repeat
    if ACodeBuffer.Save then begin
      Result:=mrOk;
    end else begin
      Result:=IDEMessageDialog('Write error',
        'Unable to write "'+ACodeBuffer.Filename+'"',
        mtError,[mbAbort,mbRetry,mbIgnore]);
    end;
  until Result<>mrRetry;
end;

function SaveCodeBufferToFile(ACodeBuffer: TCodeBuffer; const Filename: string;
  Backup: boolean): TModalResult;
var
  ACaption,AText:string;
begin
  if Backup then begin
    Result:=BackupFileInteractive(Filename);
    if Result<>mrOk then exit;
  end else
    Result:=mrOk;
  repeat
    if ACodeBuffer.SaveToFile(Filename) then begin
      Result:=mrOk;
    end else begin
      ACaption:=lisWriteError;
      AText:=Format(lisUnableToWriteToFile, ['"', Filename, '"']);
      Result:=IDEMessageDialog(ACaption,AText,mtError,
                               [mbAbort, mbRetry, mbIgnore]);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end;
  until Result<>mrRetry;
end;

function LoadStringListFromFile(const Filename, ListTitle: string;
  var sl: TStrings): TModalResult;
begin
  Result:=mrCancel;
  if sl=nil then
    sl:=TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    Result:=mrOk;
  except
    on E: Exception do begin
      IDEMessageDialog('Error','Error loading '+ListTitle+' from'#13
      +Filename+#13#13
        +E.Message,mtError,[mbOk]);
    end;
  end;
end;

function CreateEmptyFile(const Filename: string; ErrorButtons: TMsgDlgButtons
  ): TModalResult;
var
  Buffer: TCodeBuffer;
begin
  repeat
    Buffer:=CodeToolBoss.CreateFile(Filename);
    if Buffer<>nil then begin
      break;
    end else begin
      Result:=IDEMessageDialog(lisUnableToCreateFile,
        Format(lisUnableToCreateFilename, ['"', Filename, '"']),
        mtError,ErrorButtons+[mbCancel]);
      if Result<>mrRetry then exit;
    end;
  until false;
  repeat
    if Buffer.Save then begin
      break;
    end else begin
      Result:=IDEMessageDialog(lisUnableToWriteFile,
        Format(lisUnableToWriteFile2, ['"', Buffer.Filename, '"']),
        mtError,ErrorButtons+[mbCancel]);
      if Result<>mrRetry then exit;
    end;
  until false;
  Result:=mrOk;
end;

function CheckCreatingFile(const AFilename: string;
  CheckReadable: boolean; WarnOverwrite: boolean; CreateBackup: boolean
  ): TModalResult;
var
  fs: TFileStream;
  c: char;
begin
  // create if not yet done
  if not FileExistsCached(AFilename) then begin
    try
      InvalidateFileStateCache;
      fs:=TFileStream.Create(AFilename,fmCreate);
      fs.Free;
    except
      Result:=IDEMessageDialog(lisUnableToCreateFile,
        Format(lisUnableToCreateFilename, ['"', AFilename, '"']), mtError, [
          mbCancel, mbAbort]);
      exit;
    end;
  end else begin
    // file already exists
    if WarnOverwrite then begin
      Result:=QuestionDlg(lisOverwriteFile,
        Format(lisAFileAlreadyExistsReplaceIt, ['"', AFilename, '"', #13]),
        mtConfirmation,
        [mrYes, lisOverwriteFileOnDisk, mbCancel], 0);
      if Result=mrCancel then exit;
    end;
    if CreateBackup then begin
      Result:=BackupFileInteractive(AFilename);
      if Result in [mrCancel,mrAbort] then exit;
      Result:=CheckCreatingFile(AFilename,CheckReadable,false,false);
      exit;
    end;
  end;
  // check writable
  try
    if CheckReadable then begin
      InvalidateFileStateCache;
      fs:=TFileStream.Create(AFilename,fmOpenWrite)
    end else
      fs:=TFileStream.Create(AFilename,fmOpenReadWrite);
    try
      fs.Position:=fs.Size;
      fs.Write(' ',1);
    finally
      fs.Free;
    end;
  except
    Result:=IDEMessageDialog(lisUnableToWriteFile,
      Format(lisUnableToWriteFilename, ['"', AFilename, '"']), mtError, [
        mbCancel, mbAbort]);
    exit;
  end;
  // check readable
  try
    InvalidateFileStateCache;
    fs:=TFileStream.Create(AFilename,fmOpenReadWrite);
    try
      fs.Position:=fs.Size-1;
      fs.Read(c,1);
    finally
      fs.Free;
    end;
  except
    Result:=IDEMessageDialog(lisUnableToReadFile,
      Format(lisUnableToReadFilename, ['"', AFilename, '"']), mtError, [
        mbCancel, mbAbort]);
    exit;
  end;
  Result:=mrOk;
end;

function CheckFileIsWritable(const Filename: string;
  ErrorButtons: TMsgDlgButtons): TModalResult;
begin
  Result:=mrOk;
  while not FileIsWritable(Filename) do begin
    Result:=IDEMessageDialog(lisFileIsNotWritable,
      Format(lisUnableToWriteToFile2, ['"', Filename, '"']),
      mtError,ErrorButtons+[mbCancel]);
    if Result<>mrRetry then exit;
  end;
end;

function ForceDirectoryInteractive(Directory: string;
  ErrorButtons: TMsgDlgButtons): TModalResult;
var i: integer;
  Dir: string;
begin
  DoDirSeparators(Directory);
  Directory:=AppendPathDelim(Directory);
  i:=1;
  while i<=length(Directory) do begin
    if Directory[i]=PathDelim then begin
      Dir:=copy(Directory,1,i-1);
      if not DirPathExists(Dir) then begin
        while not CreateDir(Dir) do begin
          Result:=IDEMessageDialog(lisPkgMangUnableToCreateDirectory,
            Format(lisUnableToCreateDirectory2, ['"', Dir, '"']),
            mtError,ErrorButtons+[mbCancel]);
          if Result<>mrRetry then exit;
        end;
      end;
    end;
    inc(i);
  end;
  Result:=mrOk;
end;

function DeleteFileInteractive(const Filename: string;
  ErrorButtons: TMsgDlgButtons): TModalResult;
begin
  repeat
    Result:=mrOk;
    if not FileExists(Filename) then exit;
    if not DeleteFile(Filename) then begin
      Result:=IDEMessageDialog(lisDeleteFileFailed,
        Format(lisPkgMangUnableToDeleteFile, ['"', Filename, '"']),
        mtError,[mbCancel,mbRetry]);
      if Result<>mrRetry then exit;
    end;
  until false;
end;

function SaveStringToFile(const Filename, Content: string;
  ErrorButtons: TMsgDlgButtons; const Context: string): TModalResult;
var
  fs: TFileStream;
begin
  try
    InvalidateFileStateCache;
    fs:=TFileStream.Create(Filename,fmCreate);
    try
      if Content<>'' then
        fs.Write(Content[1],length(Content));
    finally
      fs.Free;
    end;
    Result:=mrOk;
  except
    on E: Exception do begin
      Result:=IDEMessageDialog('Write error',
         'Write error: '+E.Message+#13
         +'File: '+Filename+#13
         +Context,
         mtError,[mbAbort]+ErrorButtons);
    end;
  end;
end;

function ConvertLFMToLRSFileInteractive(const LFMFilename,
  LRSFilename: string): TModalResult;
var
  LFMMemStream, LRSMemStream: TMemoryStream;
  LFMBuffer: TCodeBuffer;
  LRSBuffer: TCodeBuffer;
begin
  // read lfm file
  Result:=LoadCodeBuffer(LFMBuffer,LFMFilename,[lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  LFMMemStream:=nil;
  LRSMemStream:=nil;
  try
    LFMMemStream:=TMemoryStream.Create;
    LFMBuffer.SaveToStream(LFMMemStream);
    LFMMemStream.Position:=0;
    LRSMemStream:=TMemoryStream.Create;
    // convert
    if not LFMtoLRSstream(LFMMemStream,LRSMemStream) then begin
      Result:=IDEMessageDialog('Stream Error',
        'Unable to update the binary resource file'#13
        +LRSFilename+#13
        +'from file the text resource file'#13
        +LFMFilename+#13
        +#13
        +'Probably the text file is corrupt.',
        mtError,[mbCancel,mbAbort,mbIgnore]);
      exit;
    end;
    LRSMemStream.Position:=0;
    // save lrs file
    LRSBuffer:=CodeToolBoss.CreateFile(LRSFilename);
    if (LRSBuffer<>nil) then begin
      LRSBuffer.LoadFromStream(LRSMemStream);
      Result:=SaveCodeBuffer(LRSBuffer);
    end else begin
      Result:=mrCancel;
      debugln('ConvertLFMToLRSFileInteractive unable to create codebuffer ',LRSFilename);
    end;
  finally
    LFMMemStream.Free;
    LRSMemStream.Free;
  end;
end;

function IfNotOkJumpToCodetoolErrorAndAskToAbort(Ok: boolean;
  Ask: boolean; out NewResult: TModalResult): boolean;
begin
  if Ok then begin
    NewResult:=mrOk;
    Result:=true;
  end else begin
    NewResult:=JumpToCodetoolErrorAndAskToAbort(Ask);
    Result:=NewResult<>mrAbort;
  end;
end;

function JumpToCodetoolErrorAndAskToAbort(Ask: boolean): TModalResult;
// returns mrCancel or mrAbort
var
  ErrMsg: String;
begin
  ErrMsg:=CodeToolBoss.ErrorMessage;
  LazarusIDE.DoJumpToCodeToolBossError;
  if Ask then begin
    Result:=QuestionDlg('Error',
      'The codetools found an error:'#13
      +ErrMsg+#13,
      mtWarning,[mrIgnore,'Ignore and continue',mrAbort],0);
    if Result=mrIgnore then Result:=mrCancel;
  end else begin
    Result:=mrCancel;
  end;
end;

procedure NotImplementedDialog(const Feature: string);
begin
  IDEMessageDialog('Not implemented','Not implemented yet:'#13+Feature,mtError,
                   [mbCancel]);
end;

end.

