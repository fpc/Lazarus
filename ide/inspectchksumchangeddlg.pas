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
    Dialog to show information about the message
      "recompiling unit1, checksum changed for unit2"

  ToDo:
    - show the location(s) of the first unit
    - show the location(s) of the second unit
    - actions:
      - open a source file
      - open a package
      - delete a ppu+o file
      - recompile a package clean (remove the .compiled file)
}
unit InspectChksumChangedDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  contnrs, StdCtrls, ExtCtrls, ComCtrls, ButtonPanel,
  // codetools
  CodeCache, CodeToolManager, FileProcs, DirectoryCacher, DefineTemplates,
  // IDEIntf
  LazIDEIntf, TextTools, IDEMsgIntf, PackageIntf,
  LazarusIDEStrConsts;

type
  TInspectChksumChgDialog = class;

  { TICCAction }

  TICCAction = class
  public
    Dlg: TInspectChksumChgDialog;
    Caption: string;
    constructor Create(aDlg: TInspectChksumChgDialog; aCaption: string);
  end;

  { TICCFile }

  TICCFile = class(TComponent)
  public
    Filename: string;
    Age: integer;
    OwnerName: string;
  end;

  { TICCFiles }

  TICCFiles = class(TComponentList)
  private
    function GetFiles(Index: integer): TICCFile;
    procedure SetFiles(Index: integer; const AValue: TICCFile);
  public
    property Files[Index: integer]: TICCFile read GetFiles write SetFiles; default;
  end;

  { TInspectChksumChgDialog }

  TInspectChksumChgDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    InfoGroupBox: TGroupBox;
    ProgressBar1: TProgressBar;
    ActionsRadioGroup: TRadioGroup;
    Splitter1: TSplitter;
    InfoTreeView: TTreeView;
    procedure CancelClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMsg: string;
    FUnit1: string;
    FUnit1Files: TICCFiles;
    FUnit2: string;
    FUnit2Files: TICCFiles;
    function SearchUnit(anUnitName, SearchPath: string): TICCFiles;
    procedure AddNodesForUnit(anUnitName: string; Files: TICCFiles);
  public
    procedure InitWithMsg(aMsg: TIDEMessageLine);
    property Msg: string read FMsg;
    property Unit1: string read FUnit1;
    property Unit2: string read FUnit2;
    property Unit1Files: TICCFiles read FUnit1Files;
    property Unit2Files: TICCFiles read FUnit2Files;
  end;

  { TQuickFixRecompilingChecksumChanged }

  TQuickFixRecompilingChecksumChanged = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

procedure InitInspectChecksumChangedQuickFixItems;

implementation

procedure InitInspectChecksumChangedQuickFixItems;
begin
  RegisterIDEMsgQuickFix(TQuickFixRecompilingChecksumChanged.Create);
end;

{ TICCFiles }

function TICCFiles.GetFiles(Index: integer): TICCFile;
begin
  Result:=TICCFile(Items[Index]);
end;

procedure TICCFiles.SetFiles(Index: integer; const AValue: TICCFile);
begin
  Items[Index]:=AValue;
end;

{ TICCAction }

constructor TICCAction.Create(aDlg: TInspectChksumChgDialog; aCaption: string);
begin
  Dlg:=aDlg;
  Caption:=aCaption;
end;

{$R *.lfm}

{ TInspectChksumChgDialog }

procedure TInspectChksumChgDialog.FormCreate(Sender: TObject);
begin
  Caption:='Inspect checksum changed message';
  InfoGroupBox.Caption:='Hints:';
  ActionsRadioGroup.Caption:='Actions';

  ButtonPanel1.OKButton.OnClick:=@OkClick;
  ButtonPanel1.CancelButton.OnClick:=@CancelClick;
  ProgressBar1.Visible:=false;
end;

procedure TInspectChksumChgDialog.OkClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TInspectChksumChgDialog.CancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TInspectChksumChgDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUnit1Files);
  FreeAndNil(FUnit2Files);
end;

function TInspectChksumChgDialog.SearchUnit(anUnitName, SearchPath: string
  ): TICCFiles;

  procedure SeachDirectory(Dir: string);
  var
    DirCache: TCTDirectoryCache;
    i: Integer;
    Filename: PChar;
    Ext: String;
    aFile: TICCFile;
  begin
    if (Dir='') or (not FilenameIsAbsolute(Dir)) then exit;
    // search in directory for all files that could be sources or ppu files of this unit
    DirCache:=CodeToolBoss.DirectoryCachePool.GetCache(Dir,true,false);
    if (DirCache=nil) or (DirCache.Listing=nil) then exit;
    for i:=0 to DirCache.Listing.NameCount-1 do begin
      Filename:=PChar(@DirCache.Listing.Names[DirCache.Listing.NameStarts[i]]);
      Ext:=lowercase(ExtractFileExt(Filename));
      if ((Ext='.pas') or (Ext='.pp') or (Ext='.p') or (Ext='.ppu'))
      and (SysUtils.CompareText(anUnitName,ExtractFileNameOnly(Filename))=0)
      then begin
        //debugln(['TInspectChksumChgDialog.SearchUnit Unit="',anUnitName,'" Filename="',Filename,'"']);
        aFile:=TICCFile.Create(nil);
        aFile.Filename:=AppendPathDelim(Dir)+Filename;
        aFile.Age:=FileAgeCached(aFile.Filename);
        Result.Add(aFile);
      end;
    end;
  end;

  procedure SearchInSearchPath;
  var
    CurDir: String;
    p: LongInt;
    l: Integer;
    StartPos: Integer;
  begin
    // search in search path
    StartPos:=1;
    l:=length(SearchPath);
    while StartPos<=l do begin
      p:=StartPos;
      while (p<=l) and (SearchPath[p]<>';') do inc(p);
      CurDir:=TrimFilename(copy(SearchPath,StartPos,p-StartPos));
      SeachDirectory(CurDir);
      StartPos:=p+1;
    end;
  end;

  procedure SearchInFPCFiles;
  var
    UnitSetID: String;
    UnitSet: TFPCUnitSetCache;
    CfgCache: TFPCTargetConfigCache;
    i: Integer;
    HasChanged: boolean;
    CurDir: String;
  begin
    // search in fpc unit paths
    UnitSetID:=CodeToolBoss.GetUnitSetIDForDirectory('');
    if UnitSetID='' then exit;
    UnitSet:=CodeToolBoss.FPCDefinesCache.FindUnitSetWithID(UnitSetID,HasChanged,false);
    if UnitSet=nil then exit;
    CfgCache:=UnitSet.GetConfigCache(false);
    if CfgCache=nil then exit;
    if CfgCache.UnitPaths=nil then exit;
    for i:=0 to CfgCache.UnitPaths.Count-1 do begin
      CurDir:=TrimFilename(CfgCache.UnitPaths[i]);
      SeachDirectory(CurDir);
    end;
  end;

begin
  Result:=TICCFiles.create(true);
  if (anUnitName='') then exit;

  SearchInSearchPath;
  SearchInFPCFiles;
end;

procedure TInspectChksumChgDialog.AddNodesForUnit(anUnitName: string;
  Files: TICCFiles);
var
  UnitNode: TTreeNode;
  i: Integer;
  aFile: TICCFile;
begin
  UnitNode:=InfoTreeView.Items.Add(nil,'Unit '+anUnitName);
  if Files<>nil then begin
    for i:=0 to Files.Count-1 do begin
      aFile:=Files[i];
      InfoTreeView.Items.AddChildObject(UnitNode,aFile.Filename,aFile);
    end;
  end;
  UnitNode.Expanded:=true;
end;

procedure TInspectChksumChgDialog.InitWithMsg(aMsg: TIDEMessageLine);
var
  SearchPath: String;
begin
  FMsg:=aMsg.Msg;
  REMatches(Msg,'Recompiling ([a-z_][a-z_0-9]*), checksum changed for ([a-z_][a-z_0-9]*)','i');
  FUnit1:=REVar(1);
  FUnit2:=REVar(2);
  FreeAndNil(FUnit1Files);
  FreeAndNil(FUnit2Files);

  SearchPath:=CodeToolBoss.GetCompleteSrcPathForDirectory('');
  debugln(['TInspectChksumChgDialog.InitWithMsg SearchPath=',SearchPath]);
  FUnit1Files:=SearchUnit(Unit1,SearchPath);
  FUnit2Files:=SearchUnit(Unit2,SearchPath);

  InfoTreeView.BeginUpdate;
  InfoTreeView.Items.Clear;

  InfoTreeView.Items.Add(nil,'Message: '+dbgstr(Msg));

  AddNodesForUnit(Unit1,Unit1Files);
  AddNodesForUnit(Unit2,Unit2Files);

  InfoTreeView.EndUpdate;
end;

{ TQuickFixRecompilingChecksumChanged }

constructor TQuickFixRecompilingChecksumChanged.Create;
begin
  Name:='Show dialog for message Recompiling Unit1, checksum changed for Unit1';
  Caption:='Explore message "checksum changed"';
  Steps:=[imqfoMenuItem];
end;

function TQuickFixRecompilingChecksumChanged.IsApplicable(Line: TIDEMessageLine
  ): boolean;
begin
  Result:=false;
  if not REMatches(Line.Msg,'Recompiling ([a-z_][a-z_0-9]*), checksum changed for ([a-z_][a-z_0-9]*)','i')
  then exit;
  Result:=true;
end;

procedure TQuickFixRecompilingChecksumChanged.Execute(
  const Msg: TIDEMessageLine; Step: TIMQuickFixStep);
var
  Dlg: TInspectChksumChgDialog;
begin
  if Step=imqfoMenuItem then begin
    debugln(['TQuickFixRecompilingChecksumChanged.Execute  ']);
    if not REMatches(Msg.Msg,'Recompiling ([a-z_][a-z_0-9]*), checksum changed for ([a-z_][a-z_0-9]*)','i')
    then exit;
    debugln(['TQuickFixRecompilingChecksumChanged.Execute Unit1=',REVar(1),', checksum changed for Unit2=',REVar(2)]);
    Dlg:=TInspectChksumChgDialog.Create(nil);
    try
      Dlg.InitWithMsg(Msg);
      Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
  end;
end;

end.

