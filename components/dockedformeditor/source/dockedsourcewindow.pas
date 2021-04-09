{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Maciej Izak
          Michael W. Vogel

 This is the access to one SourceEditor window. There can be more then just one
 (that list is SourceWindows).
 Each SourceEditor window can hold a lot of units, forms etc. available per
 SourceEditorWindowInterface in PageControlList.

}

unit DockedSourceWindow;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

{ $define DEBUGDOCKEDFORMEDITOR}

interface

uses
  // RTL
  Classes, SysUtils, fgl,
  // LCL
  Forms, Controls, LCLProc,
  // IDEIntf
  SrcEditorIntf, LazIDEIntf, FormEditingIntf, ExtendedNotebook,
  // DockedFormEditor
  DockedSourceEditorPageControls, DockedDesignForm, DockedModulePageControl,
  DockedOptionsIDE, DockedTools;

type

  { TSourceWindow }

  TSourceWindow = class
  private
    FActiveDesignForm: TDesignForm;
    FLastActiveSourceEditor: TSourceEditorInterface;
    FLastTopParent: TControl;
    FNotebookPageChanged: TNotifyEvent;
    FPageControlList: TSourceEditorPageControls;
    FSourceEditorNotebook: TExtendedNotebook;
    FSourceWindowIntf: TSourceEditorWindowInterface;
    function GetActiveEditor: TSourceEditorInterface;
    procedure HookIntoOnPageChanged;
    procedure SetActiveDesignForm(const AValue: TDesignForm);
    procedure SourceEditorPageChanged(Sender: TObject);
    procedure UpdateEditorPageCaption(Sender: TObject);
  public
    constructor Create(ASourceWindowIntf: TSourceEditorWindowInterface);
    destructor Destroy; override;
    procedure AddPageCtrl(ASourceEditor: TSourceEditorInterface; APageControl: TModulePageControl);
    procedure AdjustPageControl;
    function  FindModulePageControl(ASourceEditor: TSourceEditorInterface): TModulePageControl;
    procedure RemoveActiveDesignForm;
    procedure RemovePageCtrl(ASourceEditor: TSourceEditorInterface);
  public
    property ActiveDesignForm: TDesignForm read FActiveDesignForm write SetActiveDesignForm;
    property ActiveEditor: TSourceEditorInterface read GetActiveEditor;
    property LastActiveSourceEditor: TSourceEditorInterface read FLastActiveSourceEditor write FLastActiveSourceEditor;
    property LastTopParent: TControl read FLastTopParent write FLastTopParent;
    property PageControlList: TSourceEditorPageControls read FPageControlList;
    property SourceWindowIntf: TSourceEditorWindowInterface read FSourceWindowIntf;
  end;

  { TSourceWindows }

  TSourceWindows = class(specialize TFPGList<TSourceWindow>)
  private
    FLastActiveSourceWindow: TSourceEditorWindowInterface;
    function GetLastActiveModulePageControl: TModulePageControl;
    function GetLastActiveSourceEditor: TSourceEditorInterface;
    function GetSourceWindowIntf(ASrcEditor: TSourceWindow): TSourceEditorWindowInterface;
    function GetSourceWindow(ASourceWindowIntf: TSourceEditorWindowInterface): TSourceWindow;
    procedure SetLastActiveSourceEditor(AValue: TSourceEditorInterface);
  public
    constructor CreateNew;
    destructor Destroy; override;
    function  Contains(ASourceWindowIntf: TSourceEditorWindowInterface): Boolean;
    function  Contains(ASrcEditor: TSourceWindow): Boolean;
    procedure DeleteItem(Index: Integer);
    function  FindDesignForm(AModulePageCtrl: TModulePageControl): TDesignForm;
    function  FindModulePageControl(ASrcEditor: TSourceEditorInterface): TModulePageControl;
    function  IndexOf(ASourceWindowIntf: TSourceEditorWindowInterface): Integer; overload;
    function  LastSourceEditorNotFound: Boolean;
    procedure RefreshActivePageControls;
    procedure RefreshAllPageControls;
    procedure Remove(ASourceWindowIntf: TSourceEditorWindowInterface); overload;
    procedure ShowCodeTabSkipCurrent(CurrentPageCtrl: TModulePageControl; ADesignForm: TDesignForm);
  public
    property LastActiveSourceWindow: TSourceEditorWindowInterface read FLastActiveSourceWindow write FLastActiveSourceWindow;
    property LastActiveSourceEditor: TSourceEditorInterface read GetLastActiveSourceEditor write SetLastActiveSourceEditor;
    property LastActiveModulePageControl: TModulePageControl read GetLastActiveModulePageControl;
    property SourceWindowIntf[ASrcEditor: TSourceWindow]: TSourceEditorWindowInterface read GetSourceWindowIntf;
    property SourceWindow[ASourceWindowIntf: TSourceEditorWindowInterface]: TSourceWindow read GetSourceWindow;
  end;

var
  SourceWindows: TSourceWindows;

implementation

{ TSourceWindow }

procedure TSourceWindow.HookIntoOnPageChanged;
var
  i: Integer;
begin
  for i := 0 to FSourceWindowIntf.ControlCount - 1 do
    if FSourceWindowIntf.Controls[i] is TExtendedNotebook then
    begin
      FSourceEditorNotebook := TExtendedNotebook(FSourceWindowIntf.Controls[i]);
      Break;
    end;
  if not Assigned(FSourceEditorNotebook) then Exit;
  FNotebookPageChanged := FSourceEditorNotebook.OnChange;
  FSourceEditorNotebook.OnChange := @SourceEditorPageChanged;
end;

function TSourceWindow.GetActiveEditor: TSourceEditorInterface;
begin
  Result := FSourceWindowIntf.ActiveEditor;
end;

procedure TSourceWindow.SetActiveDesignForm(const AValue: TDesignForm);
var
  LPageCtrl: TModulePageControl;
begin
  if FActiveDesignForm = AValue then Exit;
  if FActiveDesignForm <> nil then
    // don't hide now if soon form will be hidden (for example on the IDE start)
    if not FActiveDesignForm.Hiding then
      FActiveDesignForm.HideWindow;
  FActiveDesignForm := AValue;

  LPageCtrl := FindModulePageControl(ActiveEditor);
  // important when we want back to tab where was oppened form
  if (AValue <> nil) then
    LazarusIDE.DoShowDesignerFormOfSrc(ActiveEditor);

  if LPageCtrl = nil then Exit;
  LPageCtrl.DesignForm := AValue;
end;

procedure TSourceWindow.SourceEditorPageChanged(Sender: TObject);
var
  LPageCtrl: TModulePageControl;
begin
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TSourceWindow.SourceEditorPageChanged SourceEditorWindow[' + FSourceWindowIntf.Caption + ']'); {$ENDIF}
  FNotebookPageChanged(Sender);
  LPageCtrl := FindModulePageControl(ActiveEditor);
  if not Assigned(LPageCtrl) then Exit;
  if LPageCtrl.DesignerPageActive then
  begin
    LPageCtrl.AdjustPage;
    {$IF DEFINED(LCLGtk2)}
      LPageCtrl.DesignerSetFocusAsync;
    {$ELSE}
      LPageCtrl.DesignerSetFocus;
    {$ENDIF}
  end;
end;

procedure TSourceWindow.UpdateEditorPageCaption(Sender: TObject);
var
  LSourceEditor: TSourceEditorInterface;
  LSourceWindowIntf: TSourceEditorWindowInterface;
begin
  // if a unit is cloned to undocked empty source editor window, the ModulePageControl
  // is not created, the only workaround I found is, to activate the new created
  // source editor in this window
  if not (Sender is TSourceEditorInterface) then Exit;
  if SourceEditorManagerIntf.ActiveSourceWindow = nil then Exit;
  LSourceEditor := TSourceEditorInterface(Sender);
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TSourceWindow.UpdateEditorPageCaption [' + SourceWindowCaption(LSourceEditor) + ']'); {$ENDIF}
  LSourceWindowIntf := SourceWindowGet(LSourceEditor);
  if not Assigned(LSourceWindowIntf)
  or (SourceEditorManagerIntf.ActiveSourceWindow = LSourceWindowIntf)
  or (SourceWindows.LastActiveSourceWindow = LSourceWindowIntf)
  then
    Exit;
  LSourceWindowIntf.ActiveEditor := LSourceEditor;
end;

constructor TSourceWindow.Create(ASourceWindowIntf: TSourceEditorWindowInterface);
begin
  FLastActiveSourceEditor := nil;
  FSourceWindowIntf := ASourceWindowIntf;
  FPageControlList := TSourceEditorPageControls.Create;
  FSourceEditorNotebook := nil;
  HookIntoOnPageChanged;
  FSourceWindowIntf.AddUpdateEditorPageCaptionHandler(@UpdateEditorPageCaption);
end;

destructor TSourceWindow.Destroy;
begin
  if Assigned(FSourceEditorNotebook) then
    FSourceEditorNotebook.OnChange := FNotebookPageChanged;
  FPageControlList.Free;
  inherited Destroy;
end;

procedure TSourceWindow.AddPageCtrl(ASourceEditor: TSourceEditorInterface; APageControl: TModulePageControl);
begin
  FPageControlList.Add(ASourceEditor, APageControl);
end;

procedure TSourceWindow.AdjustPageControl;
var
  LPageCtrl: TModulePageControl;
begin
  LPageCtrl := FindModulePageControl(ActiveEditor);
  if LPageCtrl <> nil then
    LPageCtrl.AdjustPage;
end;

function TSourceWindow.FindModulePageControl(ASourceEditor: TSourceEditorInterface): TModulePageControl;
var
  LParent: TWinControl;
begin
  if ASourceEditor = nil then
    Exit(nil);
  LParent := ASourceEditor.EditorControl.Parent;
  while LParent <> nil do
  begin
    if LParent is TModulePageControl then
      Exit(TModulePageControl(LParent));
    LParent := LParent.Parent;
  end;
  Result := nil;
end;

procedure TSourceWindow.RemoveActiveDesignForm;
begin
  FActiveDesignForm := nil;
end;

procedure TSourceWindow.RemovePageCtrl(ASourceEditor: TSourceEditorInterface);
begin
  FPageControlList.Remove(ASourceEditor);
  if LastActiveSourceEditor = ASourceEditor then
    LastActiveSourceEditor := nil;
end;

{ TSourceWindows }

function TSourceWindows.GetSourceWindowIntf(ASrcEditor: TSourceWindow): TSourceEditorWindowInterface;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ASrcEditor);
  if LIndex >= 0 then
    Result := Items[LIndex].SourceWindowIntf
  else
    Result := nil;
end;

function TSourceWindows.GetLastActiveModulePageControl: TModulePageControl;
begin
  Result := FindModulePageControl(LastActiveSourceEditor);
end;

function TSourceWindows.GetLastActiveSourceEditor: TSourceEditorInterface;
var
  LSourceWindow: TSourceWindow;
begin
  Result := nil;
  if not Assigned(LastActiveSourceWindow) then Exit;
  LSourceWindow := SourceWindow[LastActiveSourceWindow];
  if not Assigned(LSourceWindow) then Exit;
  Result := LSourceWindow.LastActiveSourceEditor;
end;

function TSourceWindows.GetSourceWindow(ASourceWindowIntf: TSourceEditorWindowInterface): TSourceWindow;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ASourceWindowIntf);
  if LIndex >= 0 then
    Result := Items[LIndex]
  else
    Result := nil;
end;

procedure TSourceWindows.SetLastActiveSourceEditor(AValue: TSourceEditorInterface);
var
  LSourceWindow: TSourceWindow;
begin
  if not Assigned(LastActiveSourceWindow) then Exit;
  LSourceWindow := SourceWindow[LastActiveSourceWindow];
  if not Assigned(LSourceWindow) then Exit;
  LSourceWindow.LastActiveSourceEditor := AValue;
end;

constructor TSourceWindows.CreateNew;
begin
  inherited Create;
  FLastActiveSourceWindow := nil;
end;

destructor TSourceWindows.Destroy;
begin
  while Count > 0 do
    DeleteItem(0);
  inherited Destroy;
end;

function TSourceWindows.Contains(ASourceWindowIntf: TSourceEditorWindowInterface): Boolean;
begin
  Result := IndexOf(ASourceWindowIntf) >= 0;
end;

function TSourceWindows.Contains(ASrcEditor: TSourceWindow): Boolean;
begin
  Result := IndexOf(ASrcEditor) >= 0;
end;

procedure TSourceWindows.DeleteItem(Index: Integer);
var
  LSourceWindow: TSourceWindow;
begin
  LSourceWindow := Items[Index];
  LSourceWindow.Free;
  Delete(Index);
end;

function TSourceWindows.FindDesignForm(AModulePageCtrl: TModulePageControl): TDesignForm;
var
  LSourceWindow: TSourceWindow;
  LSourceEditorInterface: TSourceEditorInterface;
begin
  Result := nil;
  if AModulePageCtrl = nil then Exit;
  for LSourceWindow in Self do
  begin
    if AModulePageCtrl.Owner = LSourceWindow.SourceWindowIntf then
    begin
      LSourceEditorInterface := LSourceWindow.ActiveEditor;
      if LSourceEditorInterface = nil then Exit;
      Result := DesignForms.Find(LSourceEditorInterface.GetDesigner(True));
      Exit;
    end;
  end;
end;

function TSourceWindows.FindModulePageControl(ASrcEditor: TSourceEditorInterface): TModulePageControl;
var
  LSourceWindow: TSourceWindow;
begin
  Result := nil;
  for LSourceWindow in Self do
    if LSourceWindow.PageControlList.Contains(ASrcEditor) then
      Exit(LSourceWindow.PageControlList.PageControl[ASrcEditor]);
end;

function TSourceWindows.IndexOf(ASourceWindowIntf: TSourceEditorWindowInterface): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].SourceWindowIntf = ASourceWindowIntf then
      Exit(i);
end;

function TSourceWindows.LastSourceEditorNotFound: Boolean;
var
  i: Integer;
  LSourceEditorPageControl: TSourceEditorPageControl;
  LSourceWindow: TSourceWindow;
begin
  if (LastActiveSourceWindow = nil) or (LastActiveSourceEditor = nil) then
    Exit(False);

  LSourceWindow := SourceWindow[LastActiveSourceWindow];
  for LSourceEditorPageControl in LSourceWindow.PageControlList do
  begin
    Result := True;
    for i := 0 to LastActiveSourceWindow.Count - 1 do
      if LSourceEditorPageControl.SourceEditor = LastActiveSourceWindow.Items[i] then
      begin
        Result := False;
        Break;
      end;
    if Result then
    begin
      // after moving code editor into other window, sometimes IDE switch to other tab
      // this line prevent this.
      LSourceWindow.LastActiveSourceEditor := LSourceEditorPageControl.SourceEditor;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TSourceWindows.RefreshActivePageControls;
var
  LSourceWindow: TSourceWindow;
  LPageCtrl: TModulePageControl;
begin
  for LSourceWindow in Self do
  begin
    LPageCtrl := LSourceWindow.FindModulePageControl(LSourceWindow.ActiveEditor);
    // for example LPageCtrl is nil when we clone module to new window
    if (LPageCtrl = nil) or (csDestroying in LSourceWindow.SourceWindowIntf.ComponentState) then
      Continue;
    if (LSourceWindow.ActiveEditor = nil)
    or (LSourceWindow.ActiveEditor.GetDesigner(True) <> nil)
    then
      LPageCtrl.RemoveDesignPages
    else
      if Assigned(LPageCtrl.DesignForm) then
      begin
        LPageCtrl.CreateTabSheetDesigner;
        if not (LPageCtrl.DesignForm.Form is TNonControlProxyDesignerForm) then
          LPageCtrl.CreateTabSheetAnchors;
      end;
  end;
end;

procedure TSourceWindows.RefreshAllPageControls;
var
  LSourceWindow: TSourceWindow;
  LSourceEditorPageControl: TSourceEditorPageControl;
begin
  for LSourceWindow in SourceWindows do
    for LSourceEditorPageControl in LSourceWindow.PageControlList do
    begin
      LSourceEditorPageControl.PageControl.TabPosition := DockedOptions.TabPosition;
      LSourceEditorPageControl.PageControl.RefreshResizer;
    end;
end;

procedure TSourceWindows.Remove(ASourceWindowIntf: TSourceEditorWindowInterface);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ASourceWindowIntf);
  if LIndex < 0 then Exit;
  DeleteItem(LIndex);
  if LastActiveSourceWindow = ASourceWindowIntf then
    LastActiveSourceWindow := nil;
end;

procedure TSourceWindows.ShowCodeTabSkipCurrent(CurrentPageCtrl: TModulePageControl; ADesignForm: TDesignForm);
var
  LSourceWindow: TSourceWindow;
  LSourceEditorPageControl: TSourceEditorPageControl;
begin
  for LSourceWindow in Self do
    for LSourceEditorPageControl in LSourceWindow.PageControlList do
      if LSourceEditorPageControl.PageControl = CurrentPageCtrl then
      begin
        LSourceEditorPageControl.PageControl.DesignForm := ADesignForm;
        LSourceEditorPageControl.PageControl.InitPage;
      end else
        if LSourceEditorPageControl.PageControl.DesignForm = ADesignForm then
          LSourceEditorPageControl.PageControl.ShowCode;
end;

initialization
  SourceWindows := TSourceWindows.CreateNew;

finalization
  SourceWindows.Free;

end.

