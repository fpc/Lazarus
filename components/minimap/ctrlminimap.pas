{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    Mini map controller
}
unit CtrlMiniMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IDEOptEditorIntf, SrcEditorIntf, LazConfigStorage, pnlMiniMap;

const
  DefaultEnabled = True;
  DefaultAlignLeft = False;

type

  { TMinimapController }

  TMinimapController = Class(TComponent)
  private
    FAlignLeft: Boolean;
    FConfigFrame: TAbstractIDEOptionsEditorClass;
    FList: TFPList;
    FEnabled: Boolean;
    FMiniMapCount: Integer;
    FNeedSave : Boolean;
    FInitialViewFontSize: Integer;
    FMapWidth: Integer;
    FViewWindowColor: TColor;
    FViewWindowTextColor: TColor;
    procedure EditorReconfigured(Sender: TObject);
    function FindMiniMapForEditor(aEditor: TSourceEditorInterface): TMiniMapControl;
    function GetMiniMap(aIndex : Integer): TMiniMapControl;
    procedure SetAlignLeft(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetInitialViewFontSize(AValue: Integer);
    procedure SetMapWidth(AValue: Integer);
    procedure SetMiniMapCount(AValue: Integer);
    procedure SetViewWindowColor(AValue: TColor);
    procedure SetViewWindowTextColor(AValue: TColor);
  protected
    procedure NewEditorCreated(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ConfigPanel(aPanel: TMiniMapControl; aFull: Boolean = False);
    Property MiniMaps[aIndex : Integer] : TMiniMapControl Read GetMiniMap;
    Property MiniMapCount : Integer read FMiniMapCount write SetMiniMapCount;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure LoadConfig;
    procedure SaveConfig;
    function ShowConfig: Boolean;
    Procedure ReconfigurePanels;

    Property Enabled : Boolean Read FEnabled Write SetEnabled;
    Property MapWidth : Integer Read FMapWidth Write SetMapWidth;
    Property AlignLeft : Boolean Read FAlignLeft Write SetAlignLeft;
    Property InitialViewFontSize : Integer Read FInitialViewFontSize Write SetInitialViewFontSize;
    Property ViewWindowColor : TColor Read FViewWindowColor Write SetViewWindowColor;
    Property ViewWindowTextColor : TColor Read FViewWindowTextColor Write SetViewWindowTextColor;
    Property ConfigFrame : TAbstractIDEOptionsEditorClass Read FConfigFrame Write FConfigFrame;
  end;

Var
  MinimapController : TMinimapController;

implementation

uses Controls, ExtCtrls, Forms, LazIDEIntf, BaseIDEIntf, strMiniMap;

{ TMinimapController }

procedure TMinimapController.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  FNeedSave:=True;
  if SourceEditorManagerIntf <> nil then
    if AValue then
      begin
      SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate,@NewEditorCreated);
      SourceEditorManagerIntf.RegisterChangeEvent(semEditorMoved,@NewEditorCreated);
      SourceEditorManagerIntf.RegisterChangeEvent(semEditorCloned,@NewEditorCreated);
      SourceEditorManagerIntf.RegisterChangeEvent(semEditorReConfigured,@EditorReconfigured);
      end
    else
      begin
      SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorCreate,@NewEditorCreated);
      SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorMoved,@NewEditorCreated);
      SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorCloned,@NewEditorCreated);
      SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorReConfigured,@EditorReconfigured);
      end;
end;

procedure TMinimapController.SetAlignLeft(AValue: Boolean);
begin
  if FAlignLeft=AValue then Exit;
  FAlignLeft:=AValue;
  FNeedSave:=True;
end;

function TMinimapController.FindMiniMapForEditor(aEditor : TSourceEditorInterface) : TMiniMapControl;

var
  Idx : integer;

begin
  Result:=Nil;
  Idx:=MiniMapCount-1;
  While (Result=Nil) and (Idx>=0) do
    begin
    Result:=MiniMaps[Idx];
    if Result.SourceEditor<>aEditor then
      Result:=Nil;
    Dec(Idx);
    end;
end;

procedure TMinimapController.EditorReconfigured(Sender: TObject);

var
  Idx : integer;
  MiniMap : TMiniMapControl;

begin
  For Idx:=0 to MiniMapCount-1 do
    begin
    MiniMap:=MiniMaps[Idx];
    if MiniMap.SourceEditor=Sender then
      MiniMap.Reconfigure;
    end;
end;

function TMinimapController.GetMiniMap(aIndex : Integer): TMiniMapControl;
begin
  Result:=TMiniMapControl(FList[aIndex]);
end;

procedure TMinimapController.SetInitialViewFontSize(AValue: Integer);
begin
  if FInitialViewFontSize=AValue then Exit;
  FInitialViewFontSize:=AValue;
  FNeedSave:=True;
end;

procedure TMinimapController.SetMapWidth(AValue: Integer);
begin
  if FMapWidth=AValue then Exit;
  FMapWidth:=AValue;
  FNeedSave:=True;
end;

procedure TMinimapController.SetMiniMapCount(AValue: Integer);
begin
  if FMiniMapCount=AValue then Exit;
  FMiniMapCount:=AValue;
end;

procedure TMinimapController.SetViewWindowColor(AValue: TColor);
begin
  if FViewWindowColor=AValue then Exit;
  FViewWindowColor:=AValue;
  FNeedSave:=True;
end;

procedure TMinimapController.SetViewWindowTextColor(AValue: TColor);
begin
  if FViewWindowTextColor=AValue then Exit;
  FViewWindowTextColor:=AValue;
  FNeedSave:=True;
end;

procedure TMinimapController.NewEditorCreated(Sender: TObject);

var
  Aligns : Array[Boolean] of TAlign = (alRight,alLeft);

var
  Editor : TSourceEditorInterface absolute Sender;
  EditorWindow : TSourceEditorWindowInterface;
  Panel : TMiniMapControl;

begin
  if FindMiniMapForEditor(Editor)<>Nil then
    exit;
  EditorWindow:=SourceEditorManagerIntf.SourceWindowWithEditor(Editor);
  Panel:=TMiniMapControl.Create(EditorWindow);
  FList.Add(Panel);
  Panel.FreeNotification(Self);
  Panel.SourceEditor:=Editor;
  ConfigPanel(Panel,True);
  EditorWindow.AddControlToEditor(Editor,Panel,Aligns[AlignLeft]);
end;

procedure TMinimapController.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent is TMiniMapControl) then
    FList.Remove(AComponent);
end;

procedure TMinimapController.ConfigPanel(aPanel: TMiniMapControl; aFull : Boolean);

begin
  aPanel.Width:=MapWidth;
  aPanel.ViewWindowColor:=ViewWindowColor;
  aPanel.ViewWindowTextColor:=ViewWindowTextColor;
  If aFull then
    aPanel.ViewFontSize:=InitialViewFontSize;
end;

constructor TMinimapController.Create(aOwner: TComponent);
begin
  Inherited;
  FList:=TFPList.Create;
  FMapWidth:=DefaultMapWidth;
  FInitialViewFontSize:=DefaultViewFontSize;
  FViewWindowColor:=DefaultViewWindowColor;
  FViewWindowTextColor:=DefaultViewWindowTextColor;
  FAlignLeft:=DefaultAlignLeft;
  Enabled:=True;
end;


destructor TMinimapController.Destroy;
begin
  if FNeedSave then
    SaveConfig;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMinimapController.LoadConfig;

var
  Storage : TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      Enabled:=GetValue(KeyEnabled,DefaultEnabled);
      AlignLeft:=GetValue(KeyAlignLeft,DefaultAlignLeft);
      MapWidth:=GetValue(KeyWidth,DefaultMapWidth);
      ViewWindowColor:=GetValue(KeyViewWindowColor,DefaultViewWindowColor);
      ViewWindowTextColor:=GetValue(KeyViewWindowTextColor,DefaultViewWindowTextColor);
      InitialViewFontSize:=GetValue(KeyInitialFontSize,DefaultViewFontSize);
      FNeedSave := False;
    finally
      Free;
    end;
end;

procedure TMinimapController.SaveConfig;
var
  Storage : TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      SetDeleteValue(KeyEnabled,Enabled,DefaultEnabled);
      SetDeleteValue(KeyAlignLeft,AlignLeft,DefaultAlignLeft);
      SetDeleteValue(KeyWidth,MapWidth,DefaultMapWidth);
      SetDeleteValue(KeyViewWindowColor,ViewWindowColor,DefaultViewWindowColor);
      SetDeleteValue(KeyViewWindowTextColor,ViewWindowTextColor,DefaultViewWindowTextColor);
      SetDeleteValue(KeyInitialFontSize,InitialViewFontSize,DefaultViewFontSize);
      FNeedSave := False;
    finally
      Free;
    end;
end;

function TMinimapController.ShowConfig: Boolean;
begin
  Result:=LazarusIDE.DoOpenIDEOptions(ConfigFrame);
  if Result then
    ReconfigurePanels;
end;

procedure TMinimapController.ReconfigurePanels;

var
  I : Integer;

begin
  For I:=0 to FList.Count-1 do
    ConfigPanel(TMiniMapControl(Flist[i]),False);
end;

end.

