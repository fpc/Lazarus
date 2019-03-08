{
 /***************************************************************************
                           inputfiledialog.pas
                           -------------------
          TInputFileDialog is a dialog to let the user set some filenames.


 ***************************************************************************/
 
  Author: Mattias Gaertner

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit InputFileDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, LResources,
  // LazUtils
  LazFileUtils, LazFileCache,
  // IdeIntf
  IDEDialogs,
  // IDE
  LazarusIDEStrConsts, TransferMacros, InputHistory;

type
  TInputFileFlag = (iftDirectory, iftFilename, iftCmdLine,
                    iftNotEmpty, iftMustExist);
  TInputFileFlags = set of TInputFileFlag;

  TInputFileDialog = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    OpenDialog: TOpenDialog;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure InputFileDlgButtonClick(Sender: TObject);
  private
    FFileCount: integer;
    FFileTitles: TStringList;
    FFileDescs: TStringList;
    FFileNames: TStringList;
    FFileFlags: ^TInputFileFlags;
    FInputGroupboxes: TList; // list TGroupBox
    FInputLabels: TList; // list of list of TLabel
    FInputDescs: TStringList;
    FInputEdits: TList;  // list of TEdit
    FInputFileDlgButtons: TList; // list of TButton
    FTransferMacros: TTransferMacroList;
    FUpdateCount: integer;
    FForceUpdate: boolean;
    function GetFileDescriptions(Index: integer): string;
    function GetFileTitles(Index: integer): string;
    function GetFileFlags(Index: integer): TInputFileFlags;
    function GetFilenames(Index: integer): string;
    procedure SetFileCount(const AValue: integer);
    procedure SetFileDescriptions(Index: integer; const AValue: string);
    procedure SetFileTitles(Index: integer; const AValue: string);
    procedure SetFileFlags(Index: integer; const AValue: TInputFileFlags);
    procedure SetFilenames(Index: integer; const AValue: string);
    procedure SetTransferMacros(const AValue: TTransferMacroList);
    function UpdateNeeded: boolean;
    function GetInputEdit(Index: integer): TEdit;
    function GetInputFileDlgButton(Index: integer): TButton;
    function GetLabelList(Index: integer): TList;
    function GetLabel(Index, Line: integer): TLabel;
    function GetGroupBox(Index: integer): TGroupBox;
    function LabelListCount(Index: integer): integer;
    function FileIndexOfFileDlgBtn(Button: TButton): integer;
    procedure CreateInputComponents;
    procedure CreateMissingGroupBoxes;
    procedure CreateEditComponents;
    procedure CreateFileDlgButtonComponents;
    procedure CreateLabelComponents;
    procedure DeleteUnusedGroupBoxes;
    procedure DeleteLabelList(Index: integer);
    procedure DeleteAllLabels;
    procedure ResizeComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateDlg;
    function FilenameIsValidForFileIndex(Filename: string;
      Index: integer): boolean;
  public
    property FileCount: integer read FFileCount write SetFileCount;
    property FileTitles[Index: integer]: string
      read GetFileTitles write SetFileTitles;
    property FileNames[Index: integer]: string
      read GetFileNames write SetFileNames;
    property FileDescs[Index: integer]: string
      read GetFileDescriptions write SetFileDescriptions;
    property FileFlags[Index: integer]: TInputFileFlags
      read GetFileFlags write SetFileFlags;
    property Macros: TTransferMacroList
      read FTransferMacros write SetTransferMacros;
  end;

function GetInputFileDialog: TInputFileDialog;

  
implementation


var InputFileDlg: TInputFileDialog;

function GetInputFileDialog: TInputFileDialog;
begin
  if InputFileDlg=nil then
    InputFileDlg:=TInputFileDialog.Create(nil);
  Result:=InputFileDlg;
end;

{ TInputFileDialog }

procedure TInputFileDialog.OkButtonClick(Sender: TObject);
var i: integer;
  CurEdit: TEdit;
begin
  for i:=0 to FileCount-1 do begin
    CurEdit:=GetInputEdit(i);
    CurEdit.Text:=CurEdit.Text;
    if not FilenameIsValidForFileIndex(CurEdit.Text,i) then begin
      if IDEMessageDialog(lisA2PInvalidFile,
        Format(lisCodeToolsDefsValueIsInvalid,
               [GetGroupBox(i).Caption, LineEnding, CurEdit.Text]),
        mtInformation, [mbCancel,mbAbort])=mrAbort
      then
        ModalResult:=mrCancel
      else
        exit;
    end;
    FFileNames[i]:=CurEdit.Text;
  end;
  ModalResult:=mrOk
end;

procedure TInputFileDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TInputFileDialog.FormResize(Sender: TObject);
begin
  ResizeComponents;
end;

procedure TInputFileDialog.InputFileDlgButtonClick(Sender: TObject);
var
  FileIndex: integer;
  AFilename: string;
begin
  FileIndex:=FileIndexOfFileDlgBtn(TButton(Sender));
  if FileIndex<0 then exit;
  if OpenDialog=nil then OpenDialog:=IDEOpenDialogClass.Create(Self);
  with OpenDialog do begin
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    Title:=GetGroupBox(FileIndex).Caption;
    if (not Execute) then exit;
    InputHistories.StoreFileDialogSettings(OpenDialog);
    AFilename:=Filename;
    if not FilenameIsValidForFileIndex(AFilename,FileIndex) then exit;
    GetInputEdit(FileIndex).Text:=AFilename;
  end;
end;

function TInputFileDialog.GetFileDescriptions(Index: integer): string;
begin
  Result:=FFileDescs[Index];
end;

function TInputFileDialog.GetFileTitles(Index: integer): string;
begin
  Result:=FFileTitles[Index];
end;

function TInputFileDialog.GetFileFlags(Index: integer): TInputFileFlags;
begin
  Result:=FFileFlags[Index];
end;

function TInputFileDialog.GetFilenames(Index: integer): string;
begin
  Result:=FFileNames[Index];
end;

procedure TInputFileDialog.SetFileCount(const AValue: integer);

  procedure SetStringListCount(sl: TStringlist);
  begin
    while sl.Count>AValue do
      sl.Delete(sl.Count-1);
    while sl.Count<AValue do
      sl.Add('');
  end;

var i: integer;
begin
  if FFileCount=AValue then exit;
  BeginUpdate;
  SetStringListCount(FFileNames);
  SetStringListCount(FFileDescs);
  SetStringListCount(FFileTitles);
  if FFileFlags<>nil then begin
    FreeMem(FFileFlags);
    FFileFlags:=nil;
  end;
  if AValue>0 then begin
    Getmem(FFileFlags,SizeOf(TInputFileFlags)*AValue);
    for i:=0 to AValue-1 do FFileFlags[i]:=[iftDirectory,iftNotEmpty];
  end;
  FFileCount:=AValue;
  EndUpdate;
end;

procedure TInputFileDialog.SetFileDescriptions(Index: integer;
  const AValue: string);
begin
  FFileDescs[Index]:=AValue;
  UpdateDlg;
end;

procedure TInputFileDialog.SetFileTitles(Index: integer; const AValue: string);
begin
  FFileTitles[Index]:=AValue;
  UpdateDlg;
end;

procedure TInputFileDialog.SetFileFlags(Index: integer;
  const AValue: TInputFileFlags);
begin
  FFileFlags[Index]:=AValue;
  UpdateDlg;
end;

procedure TInputFileDialog.SetFilenames(Index: integer; const AValue: string);
begin
  FFileNames[Index]:=AValue;
  UpdateDlg;
end;

procedure TInputFileDialog.SetTransferMacros(const AValue: TTransferMacroList);
begin
  FTransferMacros:=AValue;
end;

procedure TInputFileDialog.UpdateDlg;
begin
  if (FUpdateCount<>0) or (not UpdateNeeded) then exit;
  CreateInputComponents;
  FormResize(Self);
end;

function TInputFileDialog.FilenameIsValidForFileIndex(Filename: string;
  Index: integer): boolean;
var CurFileFlags: TInputFileFlags;
begin
  Result:=false;
  CurFileFlags:=FileFlags[Index];
  if (iftNotEmpty in CurFileFlags) and (Filename='') then exit;
  if ([iftMustExist,iftCmdLine]*CurFileFlags=[iftMustExist])
  and (Filename<>'') then begin
    if FTransferMacros<>nil then
      Macros.SubstituteStr(Filename);
    Filename:=ExpandFileNameUTF8(Filename);
    if (not (iftDirectory in CurFileFlags)) and DirPathExistsCached(Filename)
    then
      exit;
    if (not (iftFilename in CurFileFlags)) and FileExistsUTF8(Filename)
    and (not DirPathExistsCached(Filename))
    then
      exit;
  end;
  Result:=true;
end;

function TInputFileDialog.UpdateNeeded: boolean;
var i: integer;
begin
  Result:=true;
  if FForceUpdate then exit;
  FForceUpdate:=true;
  
  // check file count
  if FileCount<>FInputEdits.Count then exit;
  
  // check files
  for i:=0 to FileCount-1 do begin
    if FFileTitles[i]<>GetGroupBox(I).Caption then exit;
    if FFileNames[i]<>GetInputEdit(I).Text then exit;
    if FFileDescs[i]<>FInputDescs[i] then exit;
  end;

  FForceUpdate:=false;
  Result:=false;
end;

function TInputFileDialog.GetInputEdit(Index: integer): TEdit;
begin
  Result:=TEdit(FInputEdits[Index]);
end;

function TInputFileDialog.GetInputFileDlgButton(Index: integer): TButton;
begin
  Result:=TButton(FInputFileDlgButtons[Index]);
end;

function TInputFileDialog.GetLabelList(Index: integer): TList;
begin
  Result:=TList(FInputLabels[Index]);
end;

function TInputFileDialog.GetLabel(Index, Line: integer): TLabel;
begin
  Result:=TLabel(GetLabelList(Index)[Line]);
end;

function TInputFileDialog.GetGroupBox(Index: integer): TGroupBox;
begin
  Result:=TGroupBox(FInputGroupboxes[Index]);
end;

function TInputFileDialog.LabelListCount(Index: integer): integer;
begin
  Result:=GetLabelList(Index).Count;
end;

function TInputFileDialog.FileIndexOfFileDlgBtn(Button: TButton): integer;
begin
  for Result:=0 to FInputFileDlgButtons.Count-1 do
    if GetInputFileDlgButton(Result)=Button then exit;
  Result:=-1;
end;

procedure TInputFileDialog.CreateInputComponents;
begin
  CreateMissingGroupBoxes;
  CreateEditComponents;
  CreateFileDlgButtonComponents;
  CreateLabelComponents;
  DeleteUnusedGroupBoxes;
end;

procedure TInputFileDialog.CreateMissingGroupBoxes;
var
  NewGroupBox: TGroupBox;
begin
  // add new TGroupBoxes
  while FInputGroupboxes.Count<FFileCount do begin
    NewGroupBox:=TGroupBox.Create(Self);
    with NewGroupBox do begin
      Name:='InputGroupBox'+IntToStr(FInputGroupboxes.Count);
      Parent:=Self;
      Visible:=true;
    end;
    FInputGroupboxes.Add(NewGroupBox);
  end;
end;

procedure TInputFileDialog.CreateEditComponents;
var
  NewEdit: TEdit;
  i: integer;
begin
  // add new TEdits
  while FInputEdits.Count<FFileCount do begin
    i:=FInputEdits.Count;
    NewEdit:=TEdit.Create(Self);
    with NewEdit do begin
      Name:='InputEdit'+IntToStr(i);
      Parent:=GetGroupBox(i);
      Visible:=true;
    end;
    FInputEdits.Add(NewEdit);
  end;
  // remove old unused TEdits
  while FInputEdits.Count>FFileCount do begin
    GetInputEdit(FInputEdits.Count-1).Free;
    FInputEdits.Delete(FInputEdits.Count-1);
  end;
  // upadte existing TEdits
  for i:=0 to FInputEdits.Count-1 do
    GetInputEdit(i).Text:=FFileNames[i];
end;

procedure TInputFileDialog.CreateFileDlgButtonComponents;
var NewButton: TButton;
  i: integer;
begin
  // add new TButtons
  while FInputFileDlgButtons.Count<FFileCount do begin
    i:=FInputFileDlgButtons.Count;
    NewButton:=TButton.Create(Self);
    with NewButton do begin
      Name:='InputFileDlgButtin'+IntToStr(i);
      Parent:=GetGroupBox(i);
      Caption:='...';
      OnClick:=@InputFileDlgButtonClick;
      Visible:=true;
    end;
    FInputFileDlgButtons.Add(NewButton);
  end;
  // remove unused TButtons
  while FInputFileDlgButtons.Count>FFileCount do begin
    GetInputFileDlgButton(FInputFileDlgButtons.Count-1).Free;
    FInputFileDlgButtons.Delete(FInputFileDlgButtons.Count-1);
  end;
end;

procedure TInputFileDialog.CreateLabelComponents;
var
  NewLabelList: TList;
  LabelsAsText: TStringList;
  ListIndex, i: integer;
  NewLabel: TLabel;
begin
  LabelsAsText:=TStringList.Create;
  // add new TLabels
  for ListIndex:=0 to FFileCount-1 do begin
    // create TLabel list
    if FInputLabels.Count<=ListIndex then begin
      NewLabelList:=TList.Create;
      FInputLabels.Add(NewLabelList);
    end else
      NewLabelList:=GetLabelList(ListIndex);
    LabelsAsText.Text:=FFileDescs[ListIndex];
    // create one TLabel for every line
    for i:=0 to LabelsAsText.Count-1 do begin
      // create TLabel
      if NewLabelList.Count<=i then begin
        NewLabel:=TLabel.Create(Self);
        NewLabelList.Add(NewLabel);
      end else
        NewLabel:=GetLabel(ListIndex,i);
      with NewLabel do begin
        Name:='NewLabel'+IntToStr(ListIndex)+'_'+IntToStr(i);
        Parent:=GetGroupBox(ListIndex);
        Visible:=true;
      end;
    end;
    // remove unused TLabels
    while NewLabelList.Count>LabelsAsText.Count do begin
      GetLabel(ListIndex,NewLabelList.Count-1).Free;
      NewLabelList.Delete(NewLabelList.Count-1);
    end;
  end;
  // remove unused LabelLists
  while FInputLabels.Count>FFileCount do begin
    DeleteLabelList(FInputLabels.Count-1);
  end;
  // update label text
  for ListIndex:=0 to FInputLabels.Count-1 do begin
    // split description into lines
    LabelsAsText.Text:=FFileDescs[ListIndex];
    for i:=0 to LabelListCount(ListIndex)-1 do begin
      GetLabel(ListIndex,i).Caption:=LabelsAsText[i];
    end;
  end;
  LabelsAsText.Free;
end;

procedure TInputFileDialog.DeleteUnusedGroupBoxes;
var
  i: integer;
begin
  // remove old unused TGroupBoxes
  while FInputGroupboxes.Count>FFileCount do begin
    GetGroupBox(FInputGroupboxes.Count-1).Free;
    FInputGroupboxes.Delete(FInputGroupboxes.Count-1);
  end;
  // update existing TGroupBoxes
  for i:=0 to FInputGroupboxes.Count-1 do
    GetGroupBox(i).Caption:=FFileTitles[i];
end;

procedure TInputFileDialog.DeleteLabelList(Index: integer);
var i: integer;
  LabelList: TList;
begin
  LabelList:=GetLabelList(Index);
  for i:=LabelList.Count-1 downto 0 do begin
    GetLabel(Index,i).Free;
    LabelList.Delete(i);
  end;
  LabelList.Free;
  FInputLabels.Delete(Index);
end;

procedure TInputFileDialog.ResizeComponents;
var
  y, GroupBoxWidth, GroupBoxLeft, GroupBoxHeight, FileIndex,
  LabelIndex, LabelLeft, LabelHeight, GroupBoxSpacing, LabelTop, LabelWidth,
  ButtonWidth, ButtonHeight: integer;
  CurLabel: TLabel;
  CurEdit: TEdit;
  CurButton: TButton;
begin
  GroupBoxSpacing:=10;
  GroupBoxLeft:=GroupBoxSpacing;
  GroupBoxWidth:=ClientWidth-GroupBoxLeft*2;
  y:=GroupBoxSpacing;
  LabelHeight:=25;
  // resize input components
  for FileIndex:=0 to FileCount-1 do begin
    GroupBoxHeight:=60+LabelListCount(FileIndex)*LabelHeight;
    GetGroupBox(FileIndex).SetBounds(
                                 GroupBoxLeft,y,GroupBoxWidth,GroupBoxHeight);
    LabelTop:=7;
    LabelLeft:=10;
    LabelWidth:=GroupBoxWidth-LabelLeft*2;
    for LabelIndex:=0 to LabelListCount(FileIndex)-1 do begin
      CurLabel:=GetLabel(FileIndex,LabelIndex);
      CurLabel.SetBounds(LabelLeft,LabelTop,LabelWidth,CurLabel.Height);
      inc(LabelTop,LabelHeight);
    end;
    CurButton:=GetInputFileDlgButton(FileIndex);
    ButtonWidth:=CurButton.Height;
    ButtonHeight:=CurButton.Height;
    CurEdit:=GetInputEdit(FileIndex);
    CurEdit.SetBounds(LabelLeft,LabelTop,
                      LabelWidth-10-ButtonWidth,CurEdit.Height);
    CurButton.SetBounds(CurEdit.Left+CurEdit.Width+3,LabelTop,
                        ButtonWidth,ButtonHeight);
    inc(y,GroupBoxHeight+GroupBoxSpacing);
  end;
  inc(y,GroupBoxSpacing);
  // resize ok and cancel button
  OkButton.SetBounds(Max(GroupBoxLeft,GroupBoxLeft+GroupBoxWidth-250),y,
                     120,OkButton.Height);
  CancelButton.SetBounds(OkButton.Left+OkButton.Width+10,OkButton.Top,
                         OkButton.Width,OkButton.Height);
  inc(y,OkButton.Height+GroupBoxSpacing);
  Height:=y;
end;

procedure TInputFileDialog.DeleteAllLabels;
var i: integer;
begin
  for i:=FInputLabels.Count-1 downto 0 do
    DeleteLabelList(i);
end;

constructor TInputFileDialog.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner, 1);
  Position:=poScreenCenter;
  Width:=500;
  
  OnResize:=@FormResize;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Caption:=lisMenuOk;
    OnClick:=@OkButtonClick;
    Visible:=true;
  end;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Caption:=lisCancel;
    OnClick:=@CancelButtonClick;
    Visible:=true;
  end;

  FFileCount:=0;
  FFileTitles:=TStringList.Create;
  FFileDescs:=TStringList.Create;
  FFileNames:=TStringList.Create;
  FInputGroupboxes:=TList.Create;
  FInputLabels:=TList.Create;
  FInputDescs:=TStringList.Create;
  FInputEdits:=TList.Create;
  FInputFileDlgButtons:=TList.Create;
  FUpdateCount:=0;
end;

destructor TInputFileDialog.Destroy;
begin
  DeleteAllLabels;
  FFileTitles.Free;
  FFileDescs.Free;
  FFileNames.Free;
  if FFileFlags<>nil then FreeMem(FFileFlags);
  FInputGroupboxes.Free;
  FInputLabels.Free;
  FInputDescs.Free;
  FInputEdits.Free;
  FInputFileDlgButtons.Free;
  inherited Destroy;
end;

procedure TInputFileDialog.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TInputFileDialog.EndUpdate;
begin
  if FUpdateCount<=0 then exit;
  dec(FUpdateCount);
  if FUpdateCount=0 then
    UpdateDlg;
end;

end.

