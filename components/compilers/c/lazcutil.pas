{ Various Lazarus IDE extensions for C sources and compilers.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit LazCUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, IDEMsgIntf, LazCStrConsts, SrcEditorIntf, LCLType;

type

  { TGCCMessageScanner }

  TGCCMessageScanner = class(TIDEMsgScanner)
  public
    function ParseLine(MsgLine: TIDEMessageLine; var Show: boolean): boolean; override;// true if line was handled
  end;
  
  { TGCCMsgScannerType }

  TGCCMsgScannerType = class(TIDEMsgScannerType)
  public
    function ShortDescription: string; override;
    function Description: string; override;
    function StartScan(Lines: TIDEMessageLineList): TIDEMsgScanner; override;
  end;

  { TCSrcEditCompletion }

  TCSrcEditCompletion = class(TSourceEditorCompletionPlugin)
  private
    FEditor: TSourceEditorInterface;
    FFilteredList: TStrings;
    FLastPrefix: string;
    FList: TStrings;
    procedure SetLastPrefix(const AValue: string);
    procedure RebuildFilteredList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cancel; override;
    function Collect(List: TStrings): boolean; override;
    procedure Complete(var Value: string; SourceValue: string; var SourceStart,
      SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState); override;
    procedure CompletePrefix(var Prefix: string); override;
    procedure IndexChanged(Position: integer); override;
    procedure Init(SrcEdit: TSourceEditorInterface; JumpToError: boolean;
      var Handled, Abort: boolean; var Prefix: string; var BoxX, BoxY: integer
      ); override;
    procedure PrefixChanged(const NewPrefix: string; var NewIndex: integer;
      var s: TStrings); override;
    property Editor: TSourceEditorInterface read FEditor;
    property List: TStrings read FList;
    property FilteredList: TStrings read FFilteredList;
    property LastPrefix: string read FLastPrefix write SetLastPrefix;
  end;

var
  CSrcEditCompletion: TCSrcEditCompletion = nil;

procedure Register;

implementation

procedure Register;
var
  Scanner: TGCCMsgScannerType;
begin
  Scanner:=TGCCMsgScannerType.Create(nil);
  Scanner.Name:='GCC';
  IDEMsgScanners.RegisterType(Scanner);
  CSrcEditCompletion:=TCSrcEditCompletion.Create(nil);
  SourceEditorManagerIntf.RegisterCompletionPlugin(CSrcEditCompletion);
end;

{ TGCCMessageScanner }

function TGCCMessageScanner.ParseLine(MsgLine: TIDEMessageLine;
  var Show: boolean): boolean;
begin
  DebugLn(['TGCCMessageScanner.ParseLine "',MsgLine.Msg,'"']);
  Result:=false;
end;

{ TGCCMsgScannerType }

function TGCCMsgScannerType.ShortDescription: string;
begin
  Result:=lcGNUProjectCAndCCompiler;
end;

function TGCCMsgScannerType.Description: string;
begin
  Result:=ShortDescription;
end;

function TGCCMsgScannerType.StartScan(Lines: TIDEMessageLineList
  ): TIDEMsgScanner;
begin
  Result:=TGCCMessageScanner.Create(Self,Lines);
end;

{ TCSrcEditCompletion }

procedure TCSrcEditCompletion.SetLastPrefix(const AValue: string);
begin
  if FLastPrefix=AValue then exit;
  FLastPrefix:=AValue;
  RebuildFilteredList;
end;

procedure TCSrcEditCompletion.RebuildFilteredList;
var
  i: Integer;
  s: string;
  len: Integer;
  p: PChar;
begin
  FFilteredList.Clear;
  len:=length(LastPrefix);
  p:=PChar(LastPrefix);
  for i:=0 to FList.Count-1 do begin
    s:=FList[i];
    if length(s)<len then continue;
    if (len=0) or CompareMem(PChar(s),p,len) then
      FFilteredList.Add(s);
  end;
end;

constructor TCSrcEditCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList:=TStringList.Create;
  FFilteredList:=TStringList.Create;
end;

destructor TCSrcEditCompletion.Destroy;
begin
  if Self=CSrcEditCompletion then CSrcEditCompletion:=nil;
  FreeAndNil(FFilteredList);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TCSrcEditCompletion.Cancel;
begin
  //DebugLn(['TCSrcEditCompletion.Cancel ']);
  FEditor:=nil;
  FList.Clear;
  FFilteredList.Clear;
end;

function TCSrcEditCompletion.Collect(List: TStrings): boolean;
begin
  //DebugLn(['TCSrcEditCompletion.Collect ',FFilteredList.Count]);
  List.Assign(FFilteredList);
  Result:=true;
end;

procedure TCSrcEditCompletion.Complete(var Value: string; SourceValue: string;
  var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
begin
  // remove formatting tags
  //DebugLn(['TCSrcEditCompletion.Complete Value=',dbgstr(Value),' SourceValue=',dbgstr(SourceValue)]);
end;

procedure TCSrcEditCompletion.CompletePrefix(var Prefix: string);
begin
  //DebugLn(['TCSrcEditCompletion.CompletePrefix Prefx=',Prefix]);
  LastPrefix:=Prefix;
end;

procedure TCSrcEditCompletion.IndexChanged(Position: integer);
begin
  //DebugLn(['TCSrcEditCompletion.IndexChanged ',Position]);
end;

procedure TCSrcEditCompletion.Init(SrcEdit: TSourceEditorInterface;
  JumpToError: boolean; var Handled, Abort: boolean; var Prefix: string;
  var BoxX, BoxY: integer);
var
  Ext: String;
begin
  Ext:=ExtractFileExt(SrcEdit.FileName);
  if not ((Ext='.c') or (Ext='.C') or (Ext='.cc') or (Ext='.CC')
    or (Ext='.cpp') or (Ext='.CPP'))
  then begin
    // not responsible
    exit;
  end;
  //DebugLn(['TCSrcEditCompletion.Init ',SrcEdit.FileName,' Prefix=',Prefix]);
  // provide completion for c source files
  Handled:=true;
  FList.Clear;
  FList.Add('int');
  FList.Add('int8');
  FList.Add('int16');
  FList.Add('int32');
  FList.Add('char');
  FList.Add('long');
  FList.Add('bool');
  FList.Add('void');
  FList.Add('float');
  FList.Add('double');
  FList.Add('signed');
  FList.Add('struct');
  FList.Add('typedef');
  FList.Add('unsigned');
  FList.Add('public');
  FList.Add('private');
  FList.Add('define');
  FLastPrefix:=Prefix;
  RebuildFilteredList;
end;

procedure TCSrcEditCompletion.PrefixChanged(const NewPrefix: string;
  var NewIndex: integer; var s: TStrings);
begin
  //DebugLn(['TCSrcEditCompletion.PrefixChanged NewPrefix=',NewPrefix]);
  NewIndex:=0;
  LastPrefix:=NewPrefix;
  s.Assign(FFilteredList);
end;

finalization
  FreeAndNil(CSrcEditCompletion);

end.

