{
/***************************************************************************
                               UnitEditor.pp
                             -------------------

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

  Abstract:
    Dialog for the options of the code explorer.
}
unit CodeExplOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  Laz_XMLCfg, Buttons, ExtCtrls, FileUtil,
  LazConf, IDEProcs, LazarusIDEStrConsts, StdCtrls;

type
  { TCodeExplorerOptions }
  
  TCodeExplorerRefresh = (
    cerManual,  // only via refresh button
    cerSwitchEditorPage,// everytime the source editor switches to another page
    cerOnIdle // on idle
    );
  
  TCodeExplorerOptions = class(TPersistent)
  private
    FFollowCursor: boolean;
    FOptionsFilename: string;
    FRefresh: TCodeExplorerRefresh;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure Load;
    procedure Save;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  public
    property Refresh: TCodeExplorerRefresh read FRefresh write FRefresh;
    property OptionsFilename: string read FOptionsFilename write FOptionsFilename;
    property FollowCursor: boolean read FFollowCursor write FFollowCursor;
  end;

  { TCodeExplorerDlg }

  TCodeExplorerDlg = class(TForm)
    FollowCursorCheckBox: TCheckBox;
    MainNotebook: TNotebook;
    OkButton: TButton;
    CancelButton: TButton;
    RefreshRadioGroup: TRadioGroup;
    UpdatePage: TPage;
    procedure CodeExplorerDlgCreate(Sender: TObject);
    procedure CodeExplorerDlgDestroy(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FOptions: TCodeExplorerOptions;
    procedure SetOptions(const AValue: TCodeExplorerOptions);
    procedure LoadFormFromOptions;
    procedure SaveFormToOptions;
  public
    property Options: TCodeExplorerOptions read FOptions write SetOptions;
  end;
  
const
  CodeExplorerVersion = 1;

  cerDefault = cerSwitchEditorPage;

  CodeExplorerRefreshNames: array[TCodeExplorerRefresh] of string = (
    'Manual',
    'SwitchEditorPage',
    'OnIdle'
    );
    

var
  CodeExplorerOptions: TCodeExplorerOptions;// set by the IDE

function CodeExplorerRefreshNameToEnum(const s: string): TCodeExplorerRefresh;

function ShowCodeExplorerOptions: TModalResult;


implementation

function CodeExplorerRefreshNameToEnum(const s: string): TCodeExplorerRefresh;
begin
  for Result:=Low(TCodeExplorerRefresh) to High(TCodeExplorerRefresh) do
    if CompareText(CodeExplorerRefreshNames[Result],s)=0 then exit;
  Result:=cerDefault;
end;

function ShowCodeExplorerOptions: TModalResult;
var
  CodeExplorerDlg: TCodeExplorerDlg;
begin
  CodeExplorerDlg:=TCodeExplorerDlg.Create(nil);
  try
    CodeExplorerDlg.Options:=CodeExplorerOptions;
    Result:=CodeExplorerDlg.ShowModal;
    if Result=mrOk then
      CodeExplorerOptions.Assign(CodeExplorerDlg.Options);
  finally
    CodeExplorerDlg.Free;
  end;
end;

{ TCodeExplorerOptions }

constructor TCodeExplorerOptions.Create;
begin
  FOptionsFilename:=
                AppendPathDelim(GetPrimaryConfigPath)+'codeexploreroptions.xml';
  Clear;
end;

destructor TCodeExplorerOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeExplorerOptions.Clear;
begin
  FRefresh:=cerDefault;
  FFollowCursor:=true;
end;

procedure TCodeExplorerOptions.Assign(Source: TPersistent);
var
  Src: TCodeExplorerOptions;
begin
  if Source is TCodeExplorerOptions then begin
    Src:=TCodeExplorerOptions(Source);
    FRefresh:=Src.Refresh;
    FFollowCursor:=Src.FollowCursor;
  end else
    inherited Assign(Source);
end;

procedure TCodeExplorerOptions.Load;
var
  XMLConfig: TXMLConfig;
  //FileVersion: integer;
begin
  if not FileExists(FOptionsFilename) then begin
    Clear;
    exit;
  end;
  try
    XMLConfig:=TXMLConfig.Create(FOptionsFilename);
    //FileVersion:=XMLConfig.GetValue('CodeExplorer/Version/Value',0);
    LoadFromXMLConfig(XMLConfig,'CodeExplorer/');
    XMLConfig.Free;
  except
    on E: Exception do begin
      DebugLn('[TCodeExplorerOptions.Load]  error reading "',FOptionsFilename,'" ',E.Message);
    end;
  end;
end;

procedure TCodeExplorerOptions.Save;
var
  XMLConfig: TXMLConfig;
begin
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.CreateClean(FOptionsFilename);
    XMLConfig.SetDeleteValue('CodeExplorer/Version/Value',
      CodeExplorerVersion,0);
    SaveToXMLConfig(XMLConfig,'CodeExplorer/');
    XMLConfig.Flush;
    XMLConfig.Free;
  except
    on E: Exception do begin
      DebugLn('[TCodeExplorerOptions.Save]  error writing "',FOptionsFilename,'" ',E.Message);
    end;
  end;
end;

procedure TCodeExplorerOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  Clear;
  FRefresh:=CodeExplorerRefreshNameToEnum(
                                   XMLConfig.GetValue(Path+'Refresh/Value',''));
  FFollowCursor:=XMLConfig.GetValue(Path+'FollowCursor',true);
end;

procedure TCodeExplorerOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Refresh/Value',
                           CodeExplorerRefreshNames[FRefresh],
                           CodeExplorerRefreshNames[cerDefault]);
  XMLConfig.SetDeleteValue(Path+'FollowCursor',FFollowCursor,true);
end;

{ TCodeExplorerDlg }

procedure TCodeExplorerDlg.OkButtonClick(Sender: TObject);
begin
  SaveFormToOptions;
  ModalResult:=mrOk;
end;

procedure TCodeExplorerDlg.SetOptions(const AValue: TCodeExplorerOptions);
begin
  if FOptions=AValue then exit;
  FOptions.Assign(AValue);
  LoadFormFromOptions;
end;

procedure TCodeExplorerDlg.LoadFormFromOptions;
begin
  case Options.Refresh of
  cerManual: RefreshRadioGroup.ItemIndex:=0;
  cerSwitchEditorPage: RefreshRadioGroup.ItemIndex:=1;
  cerOnIdle: RefreshRadioGroup.ItemIndex:=2;
  else
    RefreshRadioGroup.ItemIndex:=1;
  end;
  FollowCursorCheckBox.Checked:=Options.FollowCursor;
end;

procedure TCodeExplorerDlg.SaveFormToOptions;
begin
  case RefreshRadioGroup.ItemIndex of
  0: FOptions.Refresh:=cerManual;
  1: FOptions.Refresh:=cerSwitchEditorPage;
  2: FOptions.Refresh:=cerOnIdle;
  end;
  Options.FollowCursor:=FollowCursorCheckBox.Checked;
end;

procedure TCodeExplorerDlg.CodeExplorerDlgCreate(Sender: TObject);
begin
  FOptions:=TCodeExplorerOptions.Create;
  Caption:=lisCEOCodeExplorer;
  OkButton.Caption:=dlgButApply;
  CancelButton.Caption:=dlgCancel;
  UpdatePage.Caption:=lisCEOUpdate;
  RefreshRadioGroup.Caption:=lisCEORefreshAutomatically;
  with RefreshRadioGroup do begin
    Items[0]:=lisCEONeverOnlyManually;
    Items[1]:=lisCEOWhenSwitchingFile;
    Items[2]:=lisCEOOnIdle;
  end;
  FollowCursorCheckBox.Caption:=lisCEFollowCursor;
end;

procedure TCodeExplorerDlg.CodeExplorerDlgDestroy(Sender: TObject);
begin
  FOptions.Free;
  FOptions:=nil;
end;

initialization
  CodeExplorerOptions:=nil;
  {$I codeexplopts.lrs}
  
finalization
  CodeExplorerOptions.Free;
  CodeExplorerOptions:=nil;

end.

