unit ResourceConvertProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes,
  // LazUtils
  LazFileCache, LazFileUtils, LazLoggerBase, ProjResProc,
  // CodeTools
  CodeCache, CodeToolManager,
  // IdeConfig
  DialogProcs,
  // BuildIntf
  LazMsgWorker;

function ConvertLFMToLRSFileInteractive(const LFMFilename,
                         LRSFilename: string; ShowAbort: boolean): TModalResult;

implementation

function ConvertLFMToLRSFileInteractive(const LFMFilename,
  LRSFilename: string; ShowAbort: boolean): TModalResult;
var
  LFMMemStream, LRSMemStream, BinStream: TMemoryStream;
  LFMBuffer, LRSBuffer: TCodeBuffer;
  FormClassName: String;
begin
  // read lfm file
  Result:=LoadCodeBuffer(LFMBuffer,LFMFilename,[lbfUpdateFromDisk],ShowAbort);
  if Result<>mrOk then exit;
  //debugln(['ConvertLFMToLRSFileInteractive ',LFMBuffer.Filename,' DiskEncoding=',LFMBuffer.DiskEncoding,' MemEncoding=',LFMBuffer.MemEncoding]);
  LFMMemStream:=nil;
  LRSMemStream:=nil;
  try
    LFMMemStream:=TMemoryStream.Create;
    LFMBuffer.SaveToStream(LFMMemStream);
    LFMMemStream.Position:=0;
    LRSMemStream:=TMemoryStream.Create;
    try
      FormClassName:=FindLFMClassName(LFMMemStream);
      //debugln(['ConvertLFMToLRSFileInteractive FormClassName="',FormClassName,'"']);
      BinStream:=TMemoryStream.Create;
      try
        LRSObjectTextToBinary(LFMMemStream,BinStream);
        BinStream.Position:=0;
        BinaryToLazarusResourceCode(BinStream,LRSMemStream,FormClassName,'FORMDATA');
      finally
        BinStream.Free;
      end;
    except
      on E: Exception do begin
        DebugLn('LFMtoLRSstream ',E.Message);
        DebugLn(['Error: (lazarus) [ConvertLFMToLRSFileInteractive] unable to convert '
          +LFMFilename+' to '+LRSFilename+':'+LineEnding+E.Message]);
        Result:=LazMessageDialogAb('Error',
          'Error while converting '+LFMFilename+' to '+LRSFilename+':'+LineEnding
          +E.Message,mtError,[mbCancel,mbIgnore],ShowAbort);
        exit;
      end;
    end;
    LRSMemStream.Position:=0;
    // save lrs file
    LRSBuffer:=CodeToolBoss.CreateFile(LRSFilename);
    if (LRSBuffer<>nil) then begin
      LRSBuffer.LoadFromStream(LRSMemStream);
      Result:=SaveCodeBuffer(LRSBuffer);
    end else begin
      Result:=mrCancel;
      DebugLn('Error: (lazarus) [ConvertLFMToLRSFileInteractive] unable to create codebuffer ',LRSFilename);
    end;
  finally
    LFMMemStream.Free;
    LRSMemStream.Free;
  end;
end;

end.

