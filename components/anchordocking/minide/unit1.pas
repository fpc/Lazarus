unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLProc, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, Buttons, ComCtrls, XMLPropStorage,
  LazFileUtils,
  SimpleFrm, AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg;

type

  { TMainIDE }

  TMainIDE = class(TForm)
    ImageList1: TImageList;
    InfoButton: TButton;
    MainMenu1: TMainMenu;
    ComponentPalette: TPageControl;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    LoadLayoutMenuItem: TMenuItem;
    SaveLayoutAsMenuItem: TMenuItem;
    NewFileMenuItem: TMenuItem;
    OpenFileMenuItem: TMenuItem;
    Page1: TTabSheet;
    Page2: TTabSheet;
    BtnPanel: TPanel;
    QuitMenuItem: TMenuItem;
    FileMenuItem: TMenuItem;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    SaveLayoutToolButton: TToolButton;
    LoadLayoutToolButton: TToolButton;
    ViewDbgOutToolButton: TToolButton;
    ViewProjInspToolButton: TToolButton;
    ViewFPDocEditorToolButton: TToolButton;
    ViewMessagesToolButton: TToolButton;
    ViewOIToolButton: TToolButton;
    ViewCodeExplToolButton: TToolButton;
    ViewSrcEdit2ToolButton: TToolButton;
    ViewSrcEditor1ToolButton: TToolButton;
    procedure FileMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InfoButtonClick(Sender: TObject);
    procedure LoadLayoutMenuItemClick(Sender: TObject);
    procedure LoadLayoutToolButtonClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure SaveLayoutAsMenuItemClick(Sender: TObject);
    procedure SaveLayoutToolButtonClick(Sender: TObject);
    procedure ViewCodeExplToolButtonClick(Sender: TObject);
    procedure ViewDbgOutToolButtonClick(Sender: TObject);
    procedure ViewFPDocEditorToolButtonClick(Sender: TObject);
    procedure ViewMessagesToolButtonClick(Sender: TObject);
    procedure ViewOIToolButtonClick(Sender: TObject);
    procedure ViewProjInspToolButtonClick(Sender: TObject);
    procedure ViewSrcEdit2ToolButtonClick(Sender: TObject);
    procedure ViewSrcEditor1ToolButtonClick(Sender: TObject);
  private
    procedure DockMasterCreateControl(Sender: TObject; aName: string; var
      AControl: TControl; DoDisableAutoSizing: boolean);
    procedure GetInfo(sl: TStringList);
  public
    procedure ShowForm(AForm: TCustomForm; FormEnableAutosizing: boolean);
    procedure SaveLayout(Filename: string);
    procedure LoadLayout(Filename: string);
  end;

var
  MainIDE: TMainIDE;

implementation

{$R *.lfm}

{ TMainIDE }

procedure TMainIDE.FileMenuItemClick(Sender: TObject);
begin
  close;
end;

procedure TMainIDE.DockMasterCreateControl(Sender: TObject; aName: string; var
  AControl: TControl; DoDisableAutoSizing: boolean);

  procedure CreateForm(Caption: string; NewBounds: TRect);
  begin
    AControl:=CreateSimpleForm(aName,Caption,NewBounds,DoDisableAutoSizing);
  end;

begin
  if aName='CodeExplorer' then
    CreateForm('Code Explorer',Bounds(700,230,100,250))
  else if aName='FPDocEditor' then
    CreateForm('FPDoc Editor',Bounds(200,720,300,100))
  else if aName='Messages' then
    CreateForm('Messages',Bounds(230,650,350,100))
  else if aName='ObjectInspector' then
    CreateForm('Object Inspector',Bounds(10,200,100,350))
  else if aName='SourceEditor1' then
    CreateForm('Source Editor 1',Bounds(230,200,400,400))
  else if aName='SourceEditor2' then
    CreateForm('Source Editor 2',Bounds(260,230,350,350))
  else if aName='ProjectInspector' then
    CreateForm('Project Inspector',Bounds(10,230,150,250))
  else if aName='DebugOutput' then
    CreateForm('Debug Output',Bounds(400,400,350,150));
end;

procedure TMainIDE.GetInfo(sl: TStringList);
var
  i: Integer;
  aMonitor: TMonitor;
  aForm: TCustomForm;
begin
  // monitors
  sl.Add('Monitors:');
  for i:=0 to Screen.MonitorCount-1 do begin
    aMonitor:=Screen.Monitors[i];
    sl.Add('  '+dbgs(i)+'/'+dbgs(Screen.MonitorCount)
      +' MonitorNum='+IntToStr(aMonitor.MonitorNum)
      +' BoundsRect='+dbgs(aMonitor.BoundsRect)
      +' WorkareaRect='+dbgs(aMonitor.WorkareaRect)
      +' Primary='+dbgs(aMonitor.Primary)
      );
  end;
  sl.Add('');

  // forms
  sl.Add('Visible top level Forms:');
  for i:=0 to Screen.CustomFormCount-1 do begin
    aForm:=Screen.CustomForms[i];
    if not aForm.Visible then continue;
    if aForm.Parent<>nil then continue;
    sl.Add('  '+dbgs(i)+'/'+dbgs(Screen.CustomFormCount)
      +' Caption="'+aForm.Caption+'"'
      +' Name="'+DbgSName(aForm)+'"'
      +' BoundsRect='+dbgs(aForm.BoundsRect)
      +' ClientRect='+dbgs(aForm.ClientRect)
      +' MonitorNum='+dbgs(aForm.Monitor.MonitorNum)
      );
  end;
  sl.Add('');
end;

procedure TMainIDE.FormCreate(Sender: TObject);
begin
  ViewOIToolButton.Hint:='View Object Inspector';
  ViewCodeExplToolButton.Hint:='View Code Explorer';
  ViewSrcEditor1ToolButton.Hint:='View Source Editor 1';
  ViewSrcEdit2ToolButton.Hint:='View Source Editor 2';
  ViewFPDocEditorToolButton.Hint:='View FPDoc Editor';
  ViewMessagesToolButton.Hint:='View Messages';
  ViewProjInspToolButton.Hint:='View Project Inspector';
  ViewDbgOutToolButton.Hint:='View Debug Output';
  SaveLayoutToolButton.Hint:='Save Layout to layout.xml';
  LoadLayoutToolButton.Hint:='Load layout from layout.xml';

  DockMaster.MakeDockSite(Self,[akBottom],admrpChild);
  DockMaster.OnCreateControl:=@DockMasterCreateControl;
  DockMaster.OnShowOptions:=@ShowAnchorDockOptions;

  SetBounds(100,50,600,80);
  ViewSrcEditor1ToolButtonClick(Self);
  ViewMessagesToolButtonClick(Self);
  ViewOIToolButtonClick(Self);
  ViewFPDocEditorToolButtonClick(Self);
end;

procedure TMainIDE.InfoButtonClick(Sender: TObject);
var
  aForm: TForm;
  aMemo: TMemo;
  sl: TStringList;
begin
  aForm:=TForm.CreateNew(Self);
  sl:=TStringList.Create;
  try
    aForm.Name:='InfoForm';
    aForm.Position:=poScreenCenter;
    aMemo:=TMemo.Create(aForm);
    with aMemo do
    begin
      Name:='aMemo';
      Parent:=aForm;
      Align:=alClient;
      ScrollBars:=ssBoth;
    end;
    GetInfo(sl);
    aMemo.Lines.Assign(sl);

    aForm.ShowModal;
  finally
    sl.Free;
    aForm.Free;
  end;
end;

procedure TMainIDE.LoadLayoutMenuItemClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Dlg.InitialDir:=GetCurrentDirUTF8;
    Dlg.Title:='Open layout file ...';
    Dlg.Filter:='*.xml|*.xml';
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    if not Dlg.Execute then exit;
    LoadLayout(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

procedure TMainIDE.LoadLayoutToolButtonClick(Sender: TObject);
begin
  LoadLayout('layout.xml');
end;

procedure TMainIDE.QuitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainIDE.SaveLayoutAsMenuItemClick(Sender: TObject);
var
  Dlg: TSaveDialog;
  Filename: String;
begin
  Dlg:=TSaveDialog.Create(nil);
  try
    Dlg.InitialDir:=GetCurrentDirUTF8;
    Dlg.Title:='Save layout as ...';
    Dlg.Filter:='*.xml|*.xml';
    Dlg.Options:=Dlg.Options+[ofPathMustExist,ofHideReadOnly,ofOverwritePrompt];
    if not Dlg.Execute then exit;
    Filename:=Dlg.FileName;
    if ExtractFileExt(Filename)='' then
      Filename:=Filename+'.xml';
    SaveLayout(FileName);
  finally
    Dlg.Free;
  end;
end;

procedure TMainIDE.SaveLayoutToolButtonClick(Sender: TObject);
begin
  SaveLayout('layout.xml');
end;

procedure TMainIDE.ViewCodeExplToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('CodeExplorer',true);
end;

procedure TMainIDE.ViewDbgOutToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('DebugOutput',true);
end;

procedure TMainIDE.ViewFPDocEditorToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('FPDocEditor',true);
end;

procedure TMainIDE.ViewMessagesToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('Messages',true);
end;

procedure TMainIDE.ViewOIToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('ObjectInspector',true);
end;

procedure TMainIDE.ViewProjInspToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('ProjectInspector',true);
end;

procedure TMainIDE.ViewSrcEdit2ToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('SourceEditor2',true);
end;

procedure TMainIDE.ViewSrcEditor1ToolButtonClick(Sender: TObject);
begin
  DockMaster.ShowControl('SourceEditor1',true);
end;

procedure TMainIDE.ShowForm(AForm: TCustomForm; FormEnableAutosizing: boolean);
begin
  DockMaster.MakeDockable(AForm);
  if FormEnableAutosizing then
    AForm.EnableAutoSizing;
end;

procedure TMainIDE.SaveLayout(Filename: string);
var
  XMLConfig: TXMLConfigStorage;
begin
  try
    // create a new xml config file
    XMLConfig:=TXMLConfigStorage.Create(Filename,false);
    try
      // save the current layout of all forms
      DockMaster.SaveLayoutToConfig(XMLConfig);
      DockMaster.SaveSettingsToConfig(XMLConfig);
      XMLConfig.WriteToDisk;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error',
        'Error saving layout to file '+Filename+':'#13+E.Message,mtError,
        [mbCancel],0);
    end;
  end;
end;

procedure TMainIDE.LoadLayout(Filename: string);
var
  XMLConfig: TXMLConfigStorage;
begin
  try
    // load the xml config file
    XMLConfig:=TXMLConfigStorage.Create(Filename,True);
    try
      // restore the layout
      // this will close unneeded forms and call OnCreateControl for all needed
      DockMaster.LoadLayoutFromConfig(XMLConfig,true);
      DockMaster.LoadSettingsFromConfig(XMLConfig);
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error',
        'Error loading layout from file '+Filename+':'#13+E.Message,mtError,
        [mbCancel],0);
    end;
  end;
end;

end.

