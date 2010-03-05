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

  Author: Michael Van Canneyt
}
unit frmBuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FileUtil, Buttons, ComCtrls, EditBtn, ExtCtrls, ActnList, Grids, Process,
  UTF8Process;

type
  { TBuildForm }
  TGetListEvent = Procedure(List : TStrings) of object;

  { TBuildForm }

  TBuildForm = class(TForm)
    ALoad: TAction;
    ASave: TAction;
    ADescrAddAll: TAction;
    ASourceAdd: TAction;
    ASourceEdit: TAction;
    ASourceDelete: TAction;
    ADescrAdd: TAction;
    ADescrEdit: TAction;
    ADescrDelete: TAction;
    ABuild: TAction;
    ActionList1: TActionList;
    BClose: TButton;
    BBuild: TButton;
    BLoad: TButton;
    BSave: TButton;
    BAddDescription: TButton;
    BDeleteDescription: TButton;
    BEditDescription: TButton;
    BSourceAdd: TButton;
    BSourceDelete: TButton;
    BSourceEdit: TButton;
    BAddAll: TButton;
    CBHideProtected: TCheckBox;
    CBPackage: TComboBox;
    CBFormat: TComboBox;
    CBContent: TCheckBox;
    CBShowPrivate: TCheckBox;
    CBWarnNoNode: TCheckBox;
    ETargetOS: TEdit;
    ETargetCPU: TEdit;
    EOutput: TEditButton;
    FEImportFIle: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LTargetCPU: TLabel;
    LCBPackage: TLabel;
    LCBFormat: TLabel;
    LBDescrFiles: TListBox;
    ListBox1: TListBox;
    MLog: TMemo;
    ODSettings: TOpenDialog;
    ODDescription: TOpenDialog;
    Panel1: TPanel;
    Compiller: TProcessUTF8;
    PSources: TPanel;
    PBDescr: TPanel;
    PCOptions: TPageControl;
    SDSettings: TSaveDialog;
    TSBuild: TTabSheet;
    TSOther: TTabSheet;
    TSDescription: TTabSheet;
    TSSources: TTabSheet;
    TSextra: TTabSheet;
    SGSources: TStringGrid;
    procedure ABuildExecute(Sender: TObject);
    procedure ABuildUpdate(Sender: TObject);
    procedure ADescrAddAllExecute(Sender: TObject);
    procedure ADescrAddAllUpdate(Sender: TObject);
    procedure ADescrAddExecute(Sender: TObject);
    procedure ADescrDeleteExecute(Sender: TObject);
    procedure ADescrDeleteUpdate(Sender: TObject);
    procedure ADescrEditExecute(Sender: TObject);
    procedure ADescrEditUpdate(Sender: TObject);
    procedure ALoadExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure ASourceAddExecute(Sender: TObject);
    procedure ASourceDeleteExecute(Sender: TObject);
    procedure ASourceDeleteUpdate(Sender: TObject);
    procedure ASourceEditExecute(Sender: TObject);
    procedure ASourceEditUpdate(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BuildFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure BuildFormCreate(Sender: TObject);
    procedure BuildFormDestroy(Sender: TObject);
    procedure EOutputButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FFileName: String;
    OldBuf: String;
    FOnGetList: TGetListEvent;
    FOptions : TStrings;
    { private declarations }
    Procedure SaveOptions(Const AFileName : String);
    Procedure LoadOptions(Const AFileName : String);
    procedure SetFileName(const AValue: String);
    procedure AddToLog(const S : String);
    procedure AppendBufToLog(Buf : PChar; BufSize : Integer);
    Function BuildCommandLine : String ;
    procedure BuildDocs;
    // Description methods
    Procedure ClearDescr;
    Function DescrFileCount : Integer;
    Function DescrFile(Index : Integer) : String;
    Function CurrentDescrFileIndex : Integer;
    Function CurrentSourceIndex : Integer;
    Function AddDescr(Const ASource : String) : Integer;
    Procedure DeleteDescrFile(Index : Integer);
    Procedure SetDescrFile(Index : Integer; Const AFileName : String);
    procedure DoGetAllDescrFiles;
    // Sources methods
    Function SourceFileCount : Integer;
    Function AddSource(Const ASource,AOptions : String) : Integer;
    Procedure GetSourceOptions(Index : Integer; Out ASource, AOptions: String);
    Procedure SetSourceOptions(Index : Integer; Const ASource, AOptions: String);
    Procedure ClearSources;
    procedure DisplaySources;
    Procedure DeleteSource(Index : Integer);
  public
    { public declarations }
    Procedure Load;
    Property FileName : String Read FFileName Write SetFileName;
    Property OnGetList : TGetListEvent Read FOnGetList Write FOnGetList;
  end;

var
  BuildForm: TBuildForm;

implementation

uses IniFiles, frmSource, lazdeopts, lazdemsg;

{$R *.lfm}

Const
  // Do not localize !!
  SGlobal      = 'Global';
  SDescr       = 'Descriptions';
  SSources     = 'Sources';
  
  KeyCount     = 'Count';
  KeyPackage   = 'Package';
  KeyFormat    = 'Format';
  KeyOutput    = 'Output';
  KeyContent   = 'Content';
  //KeyLanguage  = 'Language';
  KeyImport    = 'Import';
  KeyFile      = 'File%d';
  KeyOptions   = 'Options%d';
  KeyTargetOS  = 'TargetOS';
  KeyTargetCPU = 'TargetCPU';
  KeyHideProtected = 'HideProtected';
  KeyShowPrivate   = 'ShowPrivate';

  DefaultFormat        = 'html';
  DefaultContent       = False;
  DefaultHideProtected = False;
  DefaultShowPrivate   = False;
  DefaultTargetOS      = '';
  DefaultTargetCPU     = '';
  DefaultImport        = '';

  
{ TBuildForm }

{ ---------------------------------------------------------------------
  Actions of form.
  ---------------------------------------------------------------------}
  
procedure TBuildForm.ADescrAddExecute(Sender: TObject);
begin
  if Sender=nil then ;
  With ODDescription do
    begin
    Title:=SAddDescriptionFile;
    If Execute then
      AddDescr(FileName);
    end;
end;

procedure TBuildForm.ABuildExecute(Sender: TObject);
begin
  if Sender=nil then ;
  BuildDocs;
end;

procedure TBuildForm.ABuildUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CBPackage.Text<>'')
                               and (CBFormat.Text<>'')
                               and (SourceFileCount>0)
                               and (DescrFileCount>0);
end;

procedure TBuildForm.ADescrAddAllExecute(Sender: TObject);
begin
  if Sender=nil then ;
  DoGetAllDescrFiles;
end;

procedure TBuildForm.ADescrAddAllUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(FOnGetList);
end;

procedure TBuildForm.ADescrDeleteExecute(Sender: TObject);
begin
  if Sender=nil then ;
  DeleteDescrFile(CurrentDescrFileIndex)
end;

procedure TBuildForm.ADescrDeleteUpdate(Sender: TObject);
begin
  if Sender=nil then ;
  (Sender as TAction).Enabled:=(CurrentDescrFileIndex>=0);
end;

procedure TBuildForm.ADescrEditExecute(Sender: TObject);

Var
  Index : Integer;

begin
  if Sender=nil then ;
  Index:=CurrentDescrFileIndex;
  With ODDescription do
    begin
    FileName:=DescrFile(Index);
    Title:=SEditDescriptionFile;
    If Execute then
      SetDescrFile(Index,FileName);
    end;
end;

procedure TBuildForm.ADescrEditUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentDescrFileIndex>=0);
end;

procedure TBuildForm.ALoadExecute(Sender: TObject);
begin
  if Sender=nil then ;
  With ODSettings do
    If Execute then
      begin
      LoadOptions(FileName);
      FFileName := FileName;
      end;
end;

procedure TBuildForm.ASaveExecute(Sender: TObject);
begin
  if Sender=nil then ;
  With SDSettings do
    begin
    FileName:=Self.FileName;
    If Execute then
       begin
       SaveOptions(FileName);
       FFileName:=FileName;
       end;
    end;
end;

procedure TBuildForm.ASourceAddExecute(Sender: TObject);
begin
  if Sender=nil then ;
  With TSourceForm.Create(Self) do
    Try
      If ShowModal=mrOK then
        AddSource(Source,Options)
    Finally
      Free;
    end;
end;

procedure TBuildForm.ASourceDeleteExecute(Sender: TObject);
begin
  if Sender=nil then ;
  DeleteSource(CurrentSourceIndex);
end;

procedure TBuildForm.ASourceDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentSourceIndex>=0);
end;

procedure TBuildForm.ASourceEditExecute(Sender: TObject);

Var
  Index : Integer;
  FN,Opt : String;

begin
  if Sender=nil then ;
  Index:=CurrentSourceIndex;
  GetSourceOptions(Index,FN,Opt);
  With TSourceForm.Create(Self) do
    Try
      Source:=FN;
      Options:=Opt;
      If (ShowModal=mrOk) then
        SetSourceOptions(Index,Source,Options);
    Finally
      Free;
    end;
end;

procedure TBuildForm.ASourceEditUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentSourceIndex>=0);
end;

procedure TBuildForm.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TBuildForm.BuildFormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=True; // Check here for modifications.
end;

procedure TBuildForm.BuildFormCreate(Sender: TObject);
begin
  FOptions:=TStringList.Create;
  //i18n
  Caption:=sBuildDocumentation;
  LCBPackage.Caption:=sPackage;
  LCBFormat.Caption:=sFormat;
  Label1.Caption:=sOutput;
  CBContent.Caption:=sCreateContentFile;
  BBuild.Caption:=sBuild;
  BLoad.Caption:=sLoad;
  BSave.Caption:=sSave;
  BClose.Caption:=sClose;

  BAddDescription.Caption:=sAdd;//'&Add';
  BDeleteDescription.Caption:=sDelete; //'&Delete';
  BEditDescription.Caption:=sEdit;//'&Edit';
  BAddAll.Caption:=sAddAll;//'Add All';

  BSourceAdd.Caption:=sAdd;
  BSourceDelete.Caption:=sDelete;
  BSourceEdit.Caption:=sEdit;

  TSDescription.Caption:=sDescription;//'Description';
  TSSources.Caption:=sSourcesCapt;//'Sources';
  TSOther.Caption:=sOtherOptions;//'Other options';
  TSBuild.Caption:=sBuildOutput;//'Build output';

  CBHideProtected.Caption:=sHideProtectedMethods;//'&Hide protected methods';
  Label2.Caption:=sImportContentFile;//'Import content file';
  Label3.Caption:=sTargetOS; //'Target OS';
  LTargetCPU.Caption:=sCPU;  //'CPU';
  CBShowPrivate.Caption:=sShowPrivateMethods;//'Show p&rivate methods';
  CBWarnNoNode.Caption:=sWarnIfNoDocumentationNodeFound;//'Warn if no documentation node found';

end;

procedure TBuildForm.BuildFormDestroy(Sender: TObject);
begin
  if Sender=nil then ;
  FOptions.Free;
end;

procedure TBuildForm.EOutputButtonClick(Sender: TObject);
begin
  if (lowercase(cbFormat.text)='html') then
    With TSelectDirectoryDialog.Create(Self) do
      begin
      Title:=SSelectOutputDirectory;
      FileName:=Eoutput.Text;
      If EXecute then
        EOutput.Text:=FileName
      end
   else
     With TSaveDialog.Create(Self) do
       begin
       Title:=SSelectOutputFile;
       // Set filter ?
       FileName:=Eoutput.Text;
       If Execute then
         EOutput.Text:=FileName
       end
end;

procedure TBuildForm.FormResize(Sender: TObject);
begin
  EOutput.Width:= Panel1.Left - EOutput.ButtonWidth - 12;
end;

{ ---------------------------------------------------------------------
  general loading and saving of form.
  ---------------------------------------------------------------------}


procedure TBuildForm.SaveOptions(const AFileName: String);

Var
  Ini : TMemInifile;
  I,ACount : Integer;
  S,O : String;
  
begin
  Ini:=TMemIniFile.Create(AFileName);
  Try
    With Ini Do
      begin
      WriteString(SGLobal,KeyPackage,CBPackage.Text);
      WriteString(SGlobal,KeyFormat,CBFormat.Text);
      WriteString(SGlobal,KeyOutput,EOutput.Text);
      WriteBool  (SGlobal,KeyContent,CBContent.Checked);
      WriteString(SGlobal,KeyTargetOS,ETargetOS.Text);
      WriteString(SGlobal,KeyTargetCPU,ETargetCPU.Text);
      WriteBool  (SGlobal,KeyHideProtected,CBHideProtected.Checked);
      WriteBool  (SGLobal,KeyShowPrivate,CBShowPrivate.Checked);
      WriteString(SGlobal,KeyImport,FEImportFIle.Text);
      ACount:=DescrFileCount;
      WriteInteger(SDescr,KeyCount,ACount);
      For I:=1 to ACount do
        WriteString(SDescr,Format(KeyFile,[i]),DescrFile(I-1));
      ACount:=SourceFileCount;
      WriteInteger(SSources,KeyCount,ACount);
      For I:=1 to ACount do
        begin
        GetSourceOptions(I-1,S,O);
        WriteString(SSources,Format(KeyFile,[i]),S);
        WriteString(SSources,Format(KeyOptions,[i]),O);
        end;
      end;
  Finally
    Ini.Free;
  end;
end;

procedure TBuildForm.LoadOptions(const AFileName: String);
Var
  Ini : TMemInifile;
  I,ACount : Integer;
  S,O : String;

begin
  Ini:=TMemIniFile.Create(AFileName);
  Try
    With Ini Do
      begin
      CBPackage.Text:=ReadString(SGLobal,KeyPackage,'');
      With CBFormat do
         ItemIndex:=Items.IndexOf(ReadString(SGlobal,KeyFormat,DefaultFormat));
      EOutput.Text:=ReadString(SGlobal,KeyOutput,'');
      CBContent.Checked:=ReadBool(SGlobal,KeyContent,DefaultContent);
      ETargetOS.Text:=ReadString(SGlobal,KeyTargetOS,DefaultTargetOS);
      ETargetCPU.Text:=ReadString(SGlobal,KeyTargetCPU,DefaultTargetCPU);
      CBHideProtected.Checked:=ReadBool(SGlobal,KeyHideProtected,DefaultHideProtected);
      CBShowPrivate.Checked:=ReadBool(SGLobal,KeyShowPrivate,DefaultShowPrivate);
      FEImportFIle.Text:=ReadString(SGlobal,KeyImport,DefaultImport);
      ACount:=ReadInteger(SDescr,KeyCount,0);
      ClearDescr;
      For I:=1 to ACount do
        begin
        S:=ReadString(SDescr,Format(KeyFile,[i]),'');
        If (S<>'') then
          AddDescr(S)
        end;
      ACount:=ReadInteger(SSources,KeyCount,0);
      ClearSources;
      For I:=1 to ACount do
        begin
        S:=ReadString(SSources,Format(KeyFile,[i]),'');
        If (S<>'') then
          begin
          O:=ReadString(SSources,Format(KeyOptions,[i]),'');
          AddSource(S,O);
          end;
        end;
      end;
  Finally
    Ini.Free;
  end;
end;

procedure TBuildForm.Load;
begin
  LoadOptions(FileName);
end;

procedure TBuildForm.SetFileName(const AValue: String);
begin
  if FFileName=AValue then exit;
  FFileName:=AValue;
  LoadOptions(AValue);
end;

{ ---------------------------------------------------------------------
  Description tab methods
  ---------------------------------------------------------------------}

procedure TBuildForm.ClearDescr;
begin
  LBDescrFiles.Items.Clear;
end;

function TBuildForm.DescrFileCount: Integer;
begin
  Result:=LBDescrFiles.Items.Count;
end;

function TBuildForm.AddDescr(const ASource: String): Integer;
begin
  Result:=LBDescrFiles.Items.Add(ASource);
end;

function TBuildForm.DescrFile(Index: Integer): String;
begin
  Result:=LBDescrFiles.Items[Index];
end;

function TBuildForm.CurrentDescrFileIndex: Integer;
begin
  Result:=LBDescrFiles.ItemIndex;
end;

procedure TBuildForm.DeleteDescrFile(Index: Integer);
begin
  LBDescrFiles.Items.Delete(Index);
end;

Procedure TBuildForm.SetDescrFile(Index: Integer; Const AFileName : String);
begin
  LBDescrFiles.Items[Index]:=AFileName;
end;

procedure TBuildForm.DoGetAllDescrFiles;

Var
  L : TStringList;
  I : INteger;
  S : String;

begin
  L:=TStringList.Create;
  Try
    FOnGetList(L);
    // This is very rudimentary; We should take care of relative paths etc.
    For I:=0 to L.Count-1 do
      begin
      S:=L[i];
      If (LBDescrFiles.Items.IndexOf(S)=-1) and
         (LBDescrFiles.Items.IndexOf(ExpandFileNameUTF8(S))=-1) then
        LBDescrFiles.Items.Add(S);
      end;
  Finally
    L.Free;
  end;
end;

{ ---------------------------------------------------------------------
  Sources tab methods
  ---------------------------------------------------------------------}

function TBuildForm.AddSource(const ASource, AOptions: String): Integer;
begin
  Result:=FOptions.Add(ASource+'='+AOptions);
  DisplaySources;
end;

Procedure TBuildForm.SetSourceOptions(Index : Integer; const ASource, AOptions: String);
begin
  FOptions[Index]:=ASource+'='+AOptions;
end;

Procedure TBuildForm.GetSourceOptions(Index : Integer;
  Out ASource, AOptions: String);

Var
  N : Integer;
  
begin
  AOptions:=FOptions[Index];
  N:=Pos('=',AOptions);
  If N=0 then
    N:=Length(AOptions)+1;
  ASource:=Copy(AOptions,1,N-1);
  System.Delete(AOptions,1,N);
end;

Procedure TBuildForm.DeleteSource(Index : Integer);

begin
  FOptions.Delete(Index);
end;

procedure TBuildForm.ClearSources;
begin
  FOptions.Clear;
  DisplaySources;
end;

function TBuildForm.SourceFileCount: Integer;
begin
  Result:=FOptions.Count;
end;

procedure TBuildForm.DisplaySources;

Var
  C,I : Integer;
  N,O : String;

begin
  C:=FOptions.Count;
  If (C=0) then
    SGSources.RowCount:=2
  else
    begin
    SGSources.RowCount:=C+1;
    For I:=0 to C-1 do
      begin
      GetSourceOptions(I,N,O);
      SGSources.Cells[0,I+1]:=N;
      SGSources.Cells[1,I+1]:=O;
      end;
    end;
end;

Function TBuildForm.CurrentSourceIndex : Integer;

begin
  Result:=SGSources.Row-1;
end;

procedure TBuildForm.AddToLog(const S : String);
begin
  MLog.Lines.Add(S);
end;

procedure TBuildForm.AppendBufToLog(Buf : PChar; BufSize : Integer);

Var
  S,L : String;
  P : Integer;
  
begin
  Buf[BufSize]:=#0;
  S:=OldBuf+StrPas(Buf);
  P:=Pos(LineEnding,S);
  While P>0 do
    begin
    L:=Copy(S,1,P-1);
    AddToLog(L);
    Delete(S,1,P+Length(LineEnding)-1);
    P:=Pos(LineEnding,S);
    end;
  OldBuf:=S;
end;

Function TBuildForm.BuildCommandLine : String ;
var
  I : Integer;
  FN,O : String;
begin
  Result:=cmdFPDoc;
  Result:=Result+' --package='+CBPackage.Text;
  Result:=Result+' --output='+Eoutput.Text;
  Result:=Result+' --format='+CBFormat.Text;
  If CBShowPrivate.Checked then
    Result:=Result+' --show-private';
  If CBHideProtected.Checked then
    Result:=Result+' --hide-protected';
  if CBContent.Checked then
    Result:=Result+' --content';
  if CBWarnNoNode.Checked then
    Result:=Result+' --warn-no-node';
  if (FEImportFile.FileName<>'') then
    Result:=Result+' --import='+FEImportFile.FileName;
  if (ETargetOS.Text<>'') then
    Result:=Result+' --ostarget='+ETargetOS.Text;
  if (ETargetCPU.Text<>'') then
    Result:=Result+' --cputarget='+ETargetCPU.Text;
  For I:=1 to DescrFileCount do
    Result:=Result+' --descr='+DescrFile(I-1);

  for i:=1 to SourceFileCount do
  begin
    GetSourceOptions(I-1,FN,O);
    if (O<>'') then
      FN:=FN+' '+O;
    Result:=Result+' --input='+FN+'';
  end;
end;


procedure TBuildForm.BuildDocs;

Const
  BufSize = 1024;

var
  Buf : Array[0..BufSize] of char;
  Cmd : String;
  R, E : Integer;
begin
  PCOptions.ActivePage:=TSBuild;
  MLog.Lines.Clear;

  OldBuf:='';
  Buf[BufSize]:=#0;

  Cmd:=BuildCommandLine;
  Compiller.CommandLine:=Cmd;
  AddToLog(SUsingCommand);
  AddToLog(Cmd);
  Compiller.Execute;
  R:=Compiller.Output.Read(Buf,BufSize);
  while (R>0) do
  begin
    AppendBufToLog(@Buf,R);
    R:=Compiller.Output.Read(Buf,BufSize);
  end;
  If (OldBuf<>'') then
    AddToLog(OldBuf);

  E:=Compiller.ExitStatus;
  If E <> 0 then
    MessageDLG(Format(SErrfpdoc,[E]),mtError,[mbOK],0)
  else
    AddToLog(SBuildOK);
end;


end.

