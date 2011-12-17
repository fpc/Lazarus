{
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
    Common functions.
}
unit CodyUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, LCLIntf, Clipbrd, LCLType, LResources,
  // IDEIntf
  IDEDialogs, LazIDEIntf, SrcEditorIntf, IDEHelpIntf,
  FileProcs, CodeToolManager, CodeCache, SourceLog, BasicCodeTools,
  EventCodeTool, LinkScanner, PascalParserTool, CodeTree, SourceChanger,
  CodeBeautifier,
  CodyStrConsts;

type

  { TCodyClipboardData }

  TCodyClipboardData = class
  public
    AsText: string;
    procedure WriteString(MemStream: TMemoryStream; const s: string);
    function ReadString(MemStream: TMemoryStream): string;
    procedure WriteToStream(MemStream: TMemoryStream); virtual; abstract;
    procedure ReadFromStream(MemStream: TMemoryStream); virtual; abstract;
    procedure Execute({%H-}SrcEdit: TSourceEditorInterface; {%H-}LogXY: TPoint); virtual;
  end;
  TCodyClipboardFormat = class of TCodyClipboardData;

  { TCodyClipboardSrcData }

  TCodyClipboardSrcData = class(TCodyClipboardData)
  public
    SourceFilename: string;
    SourceX: integer;
    SourceY: integer;
    procedure SetSourcePos(const SrcPos: TCodeXYPosition);
    procedure WriteToStream(MemStream: TMemoryStream); override;
    procedure ReadFromStream(MemStream: TMemoryStream); override;
  end;

  { TCody }

  TCody = class
  private
    FClipboardFormats: TFPList;
    function GetClipboardFormats(Index: integer): TCodyClipboardFormat;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DecodeLoaded(Sender: TSourceLog; const Filename: string;
                           var Source, DiskEncoding, MemEncoding: string);

    // clipboard
    class function ClipboardFormatId: TClipboardFormat;
    function CanReadFromClipboard(AClipboard: TClipboard): Boolean;
    function ReadFromClipboard(AClipboard: TClipboard;
        SrcEdit: TSourceEditorInterface; LogXY: TPoint; AText: string): boolean;
    function WriteToClipboard(Data: TCodyClipboardData;
                              AClipboard: TClipboard = nil): Boolean;
    procedure RegisterClipboardFormat(ccFormat: TCodyClipboardFormat);
    function FindClipboardFormat(aName: string): TCodyClipboardFormat;
    function ClipboardFormatCount: integer;
    property ClipboardFormats[Index: integer]: TCodyClipboardFormat
                                                       read GetClipboardFormats;
    procedure SrcEditCopyPaste(SrcEdit: TSourceEditorInterface;
      var AText: String; var {%H-}AMode: TSemSelectionMode; ALogStartPos: TPoint;
      var AnAction: TSemCopyPasteAction);
  end;

var
  Cody: TCody;

type
  TCUParseError = (
    cupeNoSrcEditor,
    cupeMainCodeNotFound, // the file of the unit start was not found
    cupeParseError,
    cupeCursorNotInCode, // e.g. in front of the keyword 'unit'
    cupeSuccess
    );

procedure ExplodeAWithBlockCmd(Sender: TObject);
procedure InsertFileAtCursor(Sender: TObject);
procedure InsertCallInherited(Sender: TObject);

function ParseTilCursor(out Tool: TCodeTool; out CleanPos: integer;
   out Node: TCodeTreeNode; out ErrorHandled: boolean;
   JumpToError: boolean; CodePos: PCodeXYPosition = nil): TCUParseError;
function ParseUnit(out Tool: TCodeTool; out CleanPos: integer;
   out Node: TCodeTreeNode; out ErrorHandled: boolean;
   JumpToError: boolean; CodePos: PCodeXYPosition = nil;
   TilCursor: boolean = false): TCUParseError;
procedure OpenCodyHelp(Path: string);

implementation

procedure ExplodeAWithBlockCmd(Sender: TObject);

  procedure ErrorNotInWithVar;
  begin
    IDEMessageDialog(crsCWError,
      crsCWPleasePlaceTheCursorOfTheSourceEditorOnAWithVariab,
      mtError,[mbCancel]);
  end;

var
  SrcEdit: TSourceEditorInterface;
begin
  // commit changes form source editor to codetools
  if not LazarusIDE.BeginCodeTools then exit;
  // check context at cursor
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then begin
    ErrorNotInWithVar;
    exit;
  end;
  if not CodeToolBoss.RemoveWithBlock(SrcEdit.CodeToolsBuffer as TCodeBuffer,
    SrcEdit.CursorTextXY.X,SrcEdit.CursorTextXY.Y)
  then begin
    // syntax error or not in a class
    if CodeToolBoss.ErrorMessage<>'' then
      LazarusIDE.DoJumpToCodeToolBossError
    else
      ErrorNotInWithVar;
    exit;
  end;
end;

procedure InsertFileAtCursor(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Filter: String;
  Filename: String;
  Code: TCodeBuffer;
  SrcEdit: TSourceEditorInterface;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;

  OpenDialog:=TOpenDialog.Create(nil);
  Code:=nil;
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:=crsCUSelectFileToInsertAtCursor;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    Filter:=crsCUPascalPasPpPasPp;
    Filter:=Format(crsCUAllFiles, [Filter, FileMask, FileMask]);
    OpenDialog.Filter:=Filter;
    if not OpenDialog.Execute then exit;
    Filename:=OpenDialog.FileName;
    if not FileIsText(Filename) then begin
      if IDEMessageDialog(crsCUWarning, crsCUTheFileSeemsToBeABinaryProceed,
        mtConfirmation,[mbOk,mbCancel])<>mrOK then exit;
    end;
    Code:=TCodeBuffer.Create;
    Code.Filename:=Filename;
    Code.OnDecodeLoaded:=@Cody.DecodeLoaded;
    if not Code.LoadFromFile(Filename) then begin
      IDEMessageDialog(crsCWError, Format(crsCUUnableToLoadFile, [Filename, #13
        , Code.LastError]),
        mtError,[mbCancel]);
      exit;
    end;

    SrcEdit.Selection:=Code.Source;
  finally
    OpenDialog.Free;
    Code.Free;
  end;
end;

procedure InsertCallInherited(Sender: TObject);

  procedure ErrorNotInMethod;
  begin
    IDEMessageDialog(crsCWError,
      crsCUPleasePlaceTheCursorOfTheSourceEditorInAnImplement,
      mtError,[mbCancel]);
  end;

var
  Handled: boolean;
  Tool: TEventsCodeTool;
  CleanPos: integer;
  CursorNode: TCodeTreeNode;
  ProcNode: TCodeTreeNode;
  DeclNode: TCodeTreeNode;
  NewCode: String;
  SrcEdit: TSourceEditorInterface;
  Indent: LongInt;
  IndentContextSensitive: Boolean;
  NewIndent: TFABIndentationPolicy;
  NewLine: Boolean;
  Gap: TGapTyp;
  FromPos: Integer;
  ToPos: Integer;
  NewXY: TPoint;
begin
  if (ParseTilCursor(Tool,CleanPos,CursorNode,Handled,true)<>cupeSuccess)
  and not Handled then begin
    ErrorNotInMethod;
    exit;
  end;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  try
    try
      ProcNode:=CursorNode.GetNodeOfType(ctnProcedure);
      if not Tool.NodeIsMethodBody(ProcNode) then begin
        debugln(['InsertCallInherited not in a method body']);
        exit;
      end;
      // search the declaration (the header of the body may be incomplete)
      DeclNode:=Tool.FindCorrespondingProcNode(ProcNode);
      if DeclNode=nil then
        DeclNode:=ProcNode;
      Handled:=true;
      NewCode:='inherited '+Tool.ExtractProcHead(DeclNode,
        [phpWithoutClassName,phpWithParameterNames,phpWithoutParamTypes,
         phpWithoutSemicolon]);
      NewCode:=StringReplace(NewCode,';',',',[rfReplaceAll])+';';
      //debugln(['InsertCallInherited NewCode="',NewCode,'"']);
      NewLine:=true;
      Gap:=gtNone;
      if Tool.NodeIsFunction(DeclNode) then begin
        if FindFirstNonSpaceCharInLine(Tool.Src,CleanPos)<CleanPos then begin
          // insert function behind some code
          // e.g. InheritedValue:=|
          Indent:=0;
          NewLine:=false;
        end else begin
          // store the old result value
          NewCode:='Result:='+NewCode;
        end;
      end else
        NewLine:=true; // procedures always on a separate line
      FromPos:=CleanPos;
      ToPos:=CleanPos;

      if NewLine then begin
        // auto indent
        Gap:=gtNewLine;
        Indent:=SrcEdit.CursorScreenXY.X-1;
        IndentContextSensitive:=true;
        if CodeToolBoss.Indenter.GetIndent(Tool.Src,CleanPos,
          Tool.Scanner.NestedComments,
          true,NewIndent,IndentContextSensitive,NewCode)
        and NewIndent.IndentValid then begin
          Indent:=NewIndent.Indent;
        end;
        while (FromPos>1) and (Tool.Src[FromPos-1] in [' ',#9]) do
          dec(FromPos);
        NewCode:=GetIndentStr(Indent)+NewCode;
        //debugln(['InsertCallInherited Indent=',Indent,' Line="',GetLineInSrc(Tool.Src,CleanPos),'"']);
      end;

      NewCode:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
        NewCode,Indent,[bcfDoNotIndentFirstLine],GetPosInLine(Tool.Src,FromPos));
      CodeToolBoss.SourceChangeCache.MainScanner:=Tool.Scanner;
      // move editor cursor in front of insert position
      NewXY:=Point(GetPosInLine(Tool.Src,FromPos)+1,SrcEdit.CursorTextXY.Y);
      //debugln(['InsertCallInherited NewXY=',dbgs(NewXY),' FromPos=',Tool.CleanPosToStr(FromPos),' ToPos=',Tool.CleanPosToStr(ToPos)]);
      if not CodeToolBoss.SourceChangeCache.Replace(Gap,Gap,FromPos,ToPos,NewCode)
      then begin
        debugln(['InsertCallInherited CodeToolBoss.SourceChangeCache.Replace failed']);
        exit;
      end;
      SrcEdit.BeginUndoBlock;
      try
        SrcEdit.CursorTextXY:=NewXY;
        if not CodeToolBoss.SourceChangeCache.Apply then begin
          debugln(['InsertCallInherited CodeToolBoss.SourceChangeCache.Apply failed']);
          exit;
        end;
      finally
        SrcEdit.EndUndoBlock;
      end;
    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    // syntax error or not in a method
    if not Handled then begin
      if CodeToolBoss.ErrorMessage<>'' then
        LazarusIDE.DoJumpToCodeToolBossError
      else
        ErrorNotInMethod;
    end;
  end;
end;

function ParseTilCursor(out Tool: TCodeTool; out CleanPos: integer;
  out Node: TCodeTreeNode; out ErrorHandled: boolean;
  JumpToError: boolean; CodePos: PCodeXYPosition): TCUParseError;
begin
  Result:=ParseUnit(Tool,CleanPos,Node,ErrorHandled,JumpToError,CodePos,true);
end;

function ParseUnit(out Tool: TCodeTool; out CleanPos: integer;
  out Node: TCodeTreeNode; out ErrorHandled: boolean; JumpToError: boolean;
  CodePos: PCodeXYPosition; TilCursor: boolean): TCUParseError;
var
  SrcEdit: TSourceEditorInterface;
  CursorPos: TCodeXYPosition;
begin
  Tool:=nil;
  CleanPos:=0;
  Node:=nil;
  ErrorHandled:=false;
  if CodePos<>nil then CodePos^:=CleanCodeXYPosition;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then begin
    debugln(['CodyUtils.ParseTilCursor: no source editor']);
    exit(cupeNoSrcEditor);
  end;
  if not LazarusIDE.BeginCodeTools then exit;

  CursorPos.Code:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
  CursorPos.X:=SrcEdit.CursorTextXY.X;
  CursorPos.Y:=SrcEdit.CursorTextXY.Y;
  if CodePos<>nil then
    CodePos^:=CursorPos;
  try
    if not CodeToolBoss.InitCurCodeTool(CursorPos.Code) then
      exit(cupeMainCodeNotFound);
    try
      Tool:=CodeToolBoss.CurCodeTool;
      Result:=cupeParseError;
      //Range:=trTillRange;
      if TilCursor then
        Tool.BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanPos,
                                    [btSetIgnoreErrorPos])
      else
        Tool.BuildTreeAndGetCleanPos(trTillRange,lsrEnd,CursorPos,CleanPos,[]);
      Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
      if Node=nil then
        exit(cupeCursorNotInCode);
      Result:=cupeSuccess;
    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    if (CodeToolBoss.ErrorMessage<>'') and JumpToError then begin
      ErrorHandled:=true;
      LazarusIDE.DoJumpToCodeToolBossError;
    end;
  end;
end;

procedure OpenCodyHelp(Path: string);
var
  BasePath: String;
begin
  BasePath:='http://wiki.lazarus.freepascal.org/Cody';
  OpenURL(BasePath+Path);
end;

{ TCodyClipboardSrcData }

procedure TCodyClipboardSrcData.SetSourcePos(const SrcPos: TCodeXYPosition);
begin
  SourceFilename:=SrcPos.Code.Filename;
  SourceX:=SrcPos.X;
  SourceY:=SrcPos.Y;
end;

procedure TCodyClipboardSrcData.WriteToStream(MemStream: TMemoryStream);
begin
  WriteString(MemStream,SourceFilename);
  WriteLRSInteger(MemStream,SourceY);
  WriteLRSInteger(MemStream,SourceX);
end;

procedure TCodyClipboardSrcData.ReadFromStream(MemStream: TMemoryStream);
begin
  SourceFilename:=ReadString(MemStream);
  SourceY:=ReadLRSInteger(MemStream);
  SourceX:=ReadLRSInteger(MemStream);
end;

{ TCodyClipboardData }

procedure TCodyClipboardData.WriteString(MemStream: TMemoryStream;
  const s: string);
var
  b: byte;
  l: Integer;
begin
  if length(s)<255 then begin
    b:=length(s);
    MemStream.Write(b,1);
    if b>0 then
      MemStream.Write(s[1],b);
  end else begin
    b:=255;
    MemStream.Write(b,1);
    l:=length(s);
    WriteLRSInteger(MemStream,l);
    MemStream.Write(s[1],l);
  end;
end;

function TCodyClipboardData.ReadString(MemStream: TMemoryStream): string;
var
  b: byte;
  l: integer;
begin
  Result:='';
  b:=0;
  if MemStream.Read(b,1)<>1 then exit;
  if b<255 then begin
    SetLength(Result,b);
    if Result<>'' then
      MemStream.Read(Result[1],b);
  end else begin
    l:=ReadLRSInteger(MemStream);
    if l<=0 then exit;
    SetLength(Result,l);
    MemStream.Read(Result[1],l);
  end;
  //debugln(['TCodyClipboardData.ReadString Result="',Result,'"']);
end;

procedure TCodyClipboardData.Execute(SrcEdit: TSourceEditorInterface;
  LogXY: TPoint);
begin
  raise Exception.Create('not implemented yet: '+ClassName+'.Execute');
end;

{ TCody }

function TCody.GetClipboardFormats(Index: integer): TCodyClipboardFormat;
begin
  Result:=TCodyClipboardFormat(FClipboardFormats[Index]);
end;

constructor TCody.Create;
begin
  FClipboardFormats:=TFPList.Create;
end;

destructor TCody.Destroy;
begin
  FreeAndNil(FClipboardFormats);
  inherited Destroy;
end;

procedure TCody.DecodeLoaded(Sender: TSourceLog; const Filename: string;
  var Source, DiskEncoding, MemEncoding: string);
begin
  //debugln(['TCody.DecodeLoaded ',Filename]);
  if (Sender is TCodeBuffer)
  and Assigned(CodeToolBoss.SourceCache.OnDecodeLoaded) then
    CodeToolBoss.SourceCache.OnDecodeLoaded(TCodeBuffer(Sender),Filename,
      Source,DiskEncoding,MemEncoding);
end;

class function TCody.ClipboardFormatId: TClipboardFormat;
const
  CodyClipboardMimeType = 'Application/X-Laz-Cody';
var
  ID: TClipboardFormat = 0;
begin
  if ID = 0 then
    ID := ClipboardRegisterFormat(CodyClipboardMimeType);
  Result := ID;
end;

function TCody.CanReadFromClipboard(AClipboard: TClipboard): Boolean;
begin
  Result := AClipboard.HasFormat(ClipboardFormatId);
end;

function TCody.ReadFromClipboard(AClipboard: TClipboard;
  SrcEdit: TSourceEditorInterface; LogXY: TPoint; AText: string): boolean;

  procedure InvalidStream;
  begin
    raise Exception.Create('The Cody clipboard data is invalid');
  end;

var
  MemStream: TMemoryStream;
  ID: ShortString;
  aFormat: TCodyClipboardFormat;
  Data: TCodyClipboardData;
begin
  Result:=false;
  if not AClipboard.HasFormat(ClipboardFormatId) then exit;
  Result:=true;
  MemStream:=TMemoryStream.Create;
  Data:=nil;
  try
    Result:=AClipboard.GetFormat(ClipboardFormatId,MemStream);
    ID:='';
    MemStream.Position:=0;
    if MemStream.Read(ID[0],1)<>1 then
      InvalidStream;
    if MemStream.Read(ID[1],ord(ID[0]))<>ord(ID[0]) then
      InvalidStream;
    aFormat:=FindClipboardFormat(ID);
    if aFormat=nil then
      InvalidStream;
    Data:=aFormat.Create;
    Data.AsText:=AText;
    Data.ReadFromStream(MemStream);
    Data.Execute(SrcEdit,LogXY);
  finally
    Data.Free;
    MemStream.Free;
  end;
end;

function TCody.WriteToClipboard(Data: TCodyClipboardData; AClipboard: TClipboard
  ): Boolean;
var
  MemStream: TMemoryStream;
  ID: ShortString;
begin
  if AClipboard=nil then AClipboard:=Clipboard;
  AClipboard.AsText:=Data.AsText;
  if not AClipboard.HasFormat(CF_TEXT) then
    raise Exception.Create('Write to clipboard failed');
  MemStream:=TMemoryStream.Create;
  try
    ID:=Data.ClassName;
    MemStream.Write(ID[0],length(ID)+1);
    Data.WriteToStream(MemStream);
    MemStream.Position:=0;
    Result:=AClipboard.AddFormat(ClipboardFormatId,MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TCody.RegisterClipboardFormat(ccFormat: TCodyClipboardFormat);
begin
  if FindClipboardFormat(ccFormat.ClassName)<>nil then
    raise Exception.Create('cody clipboard format "'+ccFormat.ClassName+'" is already registered');
  FClipboardFormats.Add(ccFormat);
end;

function TCody.FindClipboardFormat(aName: string): TCodyClipboardFormat;
var
  i: Integer;
begin
  for i:=0 to ClipboardFormatCount-1 do begin
    Result:=ClipboardFormats[i];
    if SysUtils.CompareText(Result.ClassName,aName)=0 then exit;
  end;
  Result:=nil;
end;

function TCody.ClipboardFormatCount: integer;
begin
  Result:=FClipboardFormats.Count;
end;

procedure TCody.SrcEditCopyPaste(SrcEdit: TSourceEditorInterface;
  var AText: String; var AMode: TSemSelectionMode; ALogStartPos: TPoint;
  var AnAction: TSemCopyPasteAction);
var
  AClipBoard: TClipboard;
begin
  // ToDo: use the right clipboard
  AClipBoard:=Clipboard;
  try
    if not ReadFromClipboard(AClipBoard,SrcEdit,ALogStartPos,AText) then exit;
  except
    on E: Exception do begin
      IDEMessageDialog('Error','Unable to paste Cody data.'#13+E.Message,
        mtError,[mbCancel]);
    end;
  end;
  AnAction:=semcaAbort;
end;

initialization
  Cody:=TCody.Create;
finalization
  FreeAndNil(Cody);

end.

