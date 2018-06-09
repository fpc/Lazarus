{
 *****************************************************************************
  This file is part of the EducationLaz package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Frame to setup the "new program" - a menu item to create simple
    single file programs.
}
unit EduNewProgram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  FileUtil, LazConfigStorage,
  // LCL
  LCLProc, LResources, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Dialogs,
  // SynEdit
  SynEdit, SynHighlighterPas,
  // CodeTools
  BasicCodeTools,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, MenuIntf, IDEImagesIntf, LazIDEIntf, ProjectIntf,
  IDECommands, IDEDialogs,
  // Education
  EduOptions;

const
  FileDescNameSingleFileProgram = 'Single file program for education';
  EduDefaultNewPrgSource =
     '{%BuildCommand $(CompPath) $(EdFile) -Mobjfpc -CirotR -g -gl -vhnwe}'+LineEnding
    +'program program1;'+LineEnding
    +''+LineEnding
    +'uses'+LineEnding
    +'  Classes, SysUtils;'+LineEnding
    +''+LineEnding
    +'begin'+LineEnding
    +''+LineEnding
    +'end.'+LineEnding;

type

  { TFileDescSingleFileProgram }

  TFileDescSingleFileProgram = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function CreateSource(const {%H-}Filename, SourceName,
                          {%H-}ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TEduNewPrgOptions }

  TEduNewPrgOptions = class(TEduOptionsNode)
  private
    FAddButton: boolean;
    FAddMenuItem: boolean;
    FAddToNewDialog: boolean;
    FButton: TToolButton;
    FSource: TStrings;
    procedure SetSource(const AValue: TStrings);
    procedure FButtonClick(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    procedure Apply(Enable: boolean); override;
    procedure CreateButton;
    function GetToolBar: TToolBar;
    property AddButton: boolean read FAddButton write FAddButton;
    property AddMenuItem: boolean read FAddMenuItem write FAddMenuItem;
    property AddToNewDialog: boolean read FAddToNewDialog write FAddToNewDialog;
    property Source: TStrings read FSource write SetSource;
    property Button: TToolButton read FButton;
  end;

  { TEduNewPrgFrame }

  TEduNewPrgFrame = class(TAbstractIDEOptionsEditor)
    AddNewPrgBtnCheckBox: TCheckBox;
    AddNewPrgMenuItmCheckBox: TCheckBox;
    AddToNewDlgCheckBox: TCheckBox;
    OptsPanel: TPanel;
    SrcGroupBox: TGroupBox;
    SrcSynEdit: TSynEdit;
    SrcSynFreePascalSyn: TSynFreePascalSyn;
    SrcToolBar: TToolBar;
    SrcLoadDefaultSrcToolButton: TToolButton;
    SrcLoadFileToolButton: TToolButton;
    procedure SrcLoadDefaultSrcToolButtonClick(Sender: TObject);
    procedure SrcLoadFileToolButtonClick(Sender: TObject);
  private
  public
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

var
  EduNewPrgOptions: TEduNewPrgOptions = nil;
  FileDescSingleFileProgram: TFileDescSingleFileProgram = nil;
  SingleFileProgramMenuItem: TIDEMenuCommand = nil;
  SingleFileProgramCommand: TIDECommand = nil;

procedure OnNewSingleFileProgramClick(Sender: TObject);
procedure Register;

implementation

procedure OnNewSingleFileProgramClick(Sender: TObject);
begin
  LazarusIDE.DoNewEditorFile(FileDescSingleFileProgram,'','',
    [nfCreateDefaultSrc,nfOpenInEditor,nfAskForFilename,nfSave,nfAddToRecent]);
end;

procedure Register;
begin
  EduNewPrgOptions:=TEduNewPrgOptions.Create;
  EducationOptions.Root.Add(EduNewPrgOptions);
  EduOptionNewPrgID:=RegisterIDEOptionsEditor(EduOptionID,TEduNewPrgFrame,
                                              EduOptionNewPrgID)^.Index;

  FileDescSingleFileProgram:=TFileDescSingleFileProgram.Create;
  RegisterProjectFileDescriptor(FileDescSingleFileProgram,FileDescGroupName);

  SingleFileProgramCommand:=RegisterIDECommand(IDECommandList.FindCategoryByName('FileMenu'),
     'New single file education program', ersNewSingleFileEducationProgram, nil,
     @OnNewSingleFileProgramClick);

  SingleFileProgramMenuItem:=RegisterIDEMenuCommand(itmFileNew,'EduSingleFileProgram',
    ersNewProgram, nil, nil, SingleFileProgramCommand);
end;

{ TEduNewPrgOptions }

procedure TEduNewPrgOptions.FButtonClick(Sender: TObject);
begin
  OnNewSingleFileProgramClick(Sender);
end;

procedure TEduNewPrgOptions.SetSource(const AValue: TStrings);
begin
  if FSource=AValue then exit;
  FSource.Assign(AValue);
end;

constructor TEduNewPrgOptions.Create;
begin
  inherited Create;
  Name:='NewProgram';
  AddToNewDialog:=true;
  AddButton:=true;
  AddMenuItem:=true;
  FSource:=TStringList.Create;
  FSource.Text:=EduDefaultNewPrgSource;
end;

destructor TEduNewPrgOptions.Destroy;
begin
  FreeAndNil(FSource);
  inherited Destroy;
end;

function TEduNewPrgOptions.Load(Config: TConfigStorage): TModalResult;
begin
  FAddToNewDialog:=Config.GetValue('AddToNewDialog',true);
  FAddMenuItem:=Config.GetValue('AddMenuItem',true);
  FAddButton:=Config.GetValue('AddButton',true);
  FSource.Text:=Config.GetValue('Source/Value',EduDefaultNewPrgSource);
  Result:=inherited Load(Config);
end;

function TEduNewPrgOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetDeleteValue('AddToNewDialog',FAddToNewDialog,true);
  Config.SetDeleteValue('AddMenuItem',FAddMenuItem,true);
  Config.SetDeleteValue('AddButton',FAddButton,true);
  Config.SetDeleteValue('Source/Value',FSource.Text,EduDefaultNewPrgSource);
  Result:=inherited Save(Config);
end;

procedure TEduNewPrgOptions.Apply(Enable: boolean);
begin
  inherited Apply(Enable);
  if Enable then begin
    FileDescSingleFileProgram.VisibleInNewDialog:=AddToNewDialog;
    SingleFileProgramMenuItem.Visible:=AddMenuItem;
    CreateButton;
    if Button<>nil then Button.Visible:=AddButton;
  end else begin
    FileDescSingleFileProgram.VisibleInNewDialog:=false;
    SingleFileProgramMenuItem.Visible:=false;
  end;
end;

procedure TEduNewPrgOptions.CreateButton;
var
  ToolBar: TToolBar;
begin
  if Button<>nil then exit;
  ToolBar:=GetToolBar;
  if ToolBar=nil then exit;
  FButton := TToolButton.Create(LazarusIDE.OwningComponent);
  with FButton do
  begin
    Name := 'EduNewSingleFileProgramBtn';
    Parent := ToolBar;
    Enabled := True;
    OnClick :=@FButtonClick;
    ImageIndex := IDEImages.LoadImage('item_unit');
    Hint := ersNewSingleFileProgram;
  end;
end;

function TEduNewPrgOptions.GetToolBar: TToolBar;
var
  AComponent: TComponent;
begin
  AComponent:=LazarusIDE.OwningComponent.FindComponent('tbStandard');
  if AComponent is TToolBar then
    Result:=TToolBar(AComponent)
  else
    Result:=nil;
end;

{ TEduNewPrgFrame }

procedure TEduNewPrgFrame.SrcLoadDefaultSrcToolButtonClick(Sender: TObject);
begin
  if MessageDlg(ersLoadDefaultCode,
    ersReplaceCurrentWithDefaultSourceCode, mtConfirmation, [mbYes, mbNo], '')<>
      mrYes then exit;
  SrcSynEdit.Lines.Text:=EduDefaultNewPrgSource;
end;

procedure TEduNewPrgFrame.SrcLoadFileToolButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  sl: TStringList;
begin
  OpenDialog:=TOpenDialog.Create(Self);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:='Load program source (*.pas)';
    OpenDialog.Filter:='Pascal program (*.pas;*.pp;*.lpr;*.dpr)|*.pas;*.pp;*.lpr;*.dpr';
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    if OpenDialog.Execute then begin
      sl:=TStringList.Create;
      try
        try
          sl.LoadFromFile(OpenDialog.FileName);
          SrcSynEdit.Lines.Text:=sl.Text;
        except
          on E: Exception do begin
            MessageDlg('Error',
              'Read error: '+e.Message,mtError,[mbCancel],'');
          end;
        end;
      finally
        sl.Free;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TEduNewPrgFrame.GetTitle: String;
begin
  Result:=ersEduNewProgramTitle;
end;

procedure TEduNewPrgFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    AddNewPrgBtnCheckBox.Checked:=EduNewPrgOptions.AddButton;
    AddNewPrgMenuItmCheckBox.Checked:=EduNewPrgOptions.AddMenuItem;
    AddToNewDlgCheckBox.Checked:=EduNewPrgOptions.AddToNewDialog;
    SrcSynEdit.Lines.Text:=EduNewPrgOptions.Source.Text;
  end;
end;

procedure TEduNewPrgFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  AddNewPrgBtnCheckBox.Caption:=ersAddIcon;
  AddNewPrgBtnCheckBox.ShowHint:=true;
  AddNewPrgBtnCheckBox.Hint:=
    ersAddASpeedButtonToTheIDEToolbarToCreateANewProgram;

  AddNewPrgMenuItmCheckBox.Caption:=ersAddMenuItem;
  AddNewPrgMenuItmCheckBox.ShowHint:=true;
  AddNewPrgMenuItmCheckBox.Hint:=
    ersAddAnIDEMenuItemToCreateANewProgram;

  AddToNewDlgCheckBox.Caption:=Format(ersAddToNewDialog, ['"', '"']);
  AddToNewDlgCheckBox.ShowHint:=true;
  AddToNewDlgCheckBox.Hint:=Format(
    ersAddAnEntryToTheNewDialogToCreateANewProgram, ['"', '"']);

  SrcGroupBox.Caption:=ersSource;
  SrcToolBar.Images:=IDEImages.Images_16;
  SrcLoadDefaultSrcToolButton.ShowHint:=true;
  SrcLoadDefaultSrcToolButton.Hint:=ersReplaceCurrentSourceWithDefaultSourceCode;
  SrcLoadDefaultSrcToolButton.ImageIndex:=IDEImages.LoadImage('item_unit');
  SrcLoadFileToolButton.ShowHint:=true;
  SrcLoadFileToolButton.Hint:=ersLoadSourceFromFile;
  SrcLoadFileToolButton.ImageIndex:=IDEImages.LoadImage('laz_open');
end;

class function TEduNewPrgFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=EducationIDEOptionsClass;
end;

procedure TEduNewPrgFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    EduNewPrgOptions.AddButton:=AddNewPrgBtnCheckBox.Checked;
    EduNewPrgOptions.AddMenuItem:=AddNewPrgMenuItmCheckBox.Checked;
    EduNewPrgOptions.AddToNewDialog:=AddToNewDlgCheckBox.Checked;
    EduNewPrgOptions.Source:=SrcSynEdit.Lines;
  end;
end;

{ TFileDescSingleFileProgram }

constructor TFileDescSingleFileProgram.Create;
begin
  inherited Create;
  Name:=FileDescNameSingleFileProgram;
  DefaultFilename:='program1.pas';
  DefaultSourceName:='Program1';
  IsPascalUnit:=false;
  BuildFileIfActive:=true;
  RunFileIfActive:=true;
  AddToProject:=true;
end;

function TFileDescSingleFileProgram.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
var
  StartPos: integer;
  EndPos: integer;
begin
  Result:=EduDefaultNewPrgSource;
  FindProgramNameInSource(Result,StartPos,EndPos);
  Result:=copy(Result,1,StartPos-1)+SourceName+copy(Result,EndPos,length(Result));
end;

function TFileDescSingleFileProgram.GetLocalizedName: string;
begin
  Result:=ersSingleFileProgram;
end;

function TFileDescSingleFileProgram.GetLocalizedDescription: string;
begin
  Result:=ersASimpleProgramOnlyOneFileIsCreatedAndAddedToTheCur;
end;

{$R *.lfm}

end.
