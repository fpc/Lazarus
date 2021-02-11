{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Maciej Izak
           Michael W. Vogel

  DaThoX 2004-2015
  FreeSparta.com

  Known Issues:
    - when form / frame is moved out of screen, there the form has no designer
      grid (dots)
    - ObjectInspector eats focus of AnchorDesigner (bug or feature?)
    - Qt5 shows own menu in form, this isn't shown in anchor editor
  TODO:
    - Lazarus AnchorEditor isn't refreshed after control is changed in DockedAnchorDesigner
    - center form (popup Designer) destroys form position in DockedFormEditor
    - popup for grips to fix control/point/side at parent

}

unit DockedMainIDE;

{$mode objfpc}{$H+}
{ $define DEBUGDOCKEDFORMEDITOR}

interface

uses
  // RTL
  Classes, SysUtils, Contnrs,
  // LCL
  LCLIntf, Controls, Forms,
  // IdeIntf
  SrcEditorIntf, LazIDEIntf, FormEditingIntf, PropEdits, LazLoggerBase,
  // DockedFormEditor
  DockedResizer, DockedModulePageControl, DockedTools,
  DockedSourceEditorPageControls, DockedOptionsIDE, DockedDesignForm,
  DockedSourceEditorWindow;

type

  { TDockedTabMaster }

  TDockedTabMaster = class(TIDETabMaster)
  private
    // enable autosizing for docked form editor forms, see issue #32207 - disabled
    // in SourceFileManager per PreventAutoSize
    FAutoSizeControlList: TObjectList;
  protected
    function GetTabDisplayState: TTabDisplayState; override;
    function GetTabDisplayStateEditor(Index: TSourceEditorInterface): TTabDisplayState; override;
  public
    constructor Create;
    destructor Destroy; override;
    function AutoSizeInShowDesigner(AControl: TControl): Boolean; override;
    procedure EnableAutoSizing(AControl: TControl);
    procedure ToggleFormUnit; override;
    procedure JumpToCompilerMessage(ASourceEditor: TSourceEditorInterface); override;
    procedure ShowCode(ASourceEditor: TSourceEditorInterface); override;
    procedure ShowDesigner(ASourceEditor: TSourceEditorInterface; AIndex: Integer = 0); override;
    procedure ShowForm(AForm: TCustomForm); override;
    procedure OptionsModified;
  end;

  { TDockedMainIDE }

  TDockedMainIDE = class(TObject)
  public
    class function GetCurrentPageControl: TModulePageControl;

    class procedure Screen_FormAdded(Sender: TObject; AForm: TCustomForm);
    class procedure Screen_FormDel(Sender: TObject; AForm: TCustomForm);

    class procedure WindowCreate(Sender: TObject);
    class procedure WindowDestroy(Sender: TObject);
    class procedure WindowHide(Sender: TObject);
    class procedure WindowShow(Sender: TObject);

    class procedure EditorActivated(Sender: TObject);
    class procedure EditorCreate(Sender: TObject);
    class procedure EditorDestroyed(Sender: TObject);

    class procedure TabChange(Sender: TObject);

    class procedure GlobalSNOnChangeBounds(Sender: TObject);
    class procedure OnShowDesignerForm(Sender: TObject; {%H-}AEditor: TSourceEditorInterface;
                      AComponentPaletteClassSelected: Boolean);
    class procedure OnShowSrcEditor(Sender: TObject);

    class procedure OnDesignShowMethod(const Name: String);
    class procedure OnDesignRefreshPropertyValues;
    class procedure OnDesignModified(Sender: TObject);
    class procedure OnDesignPersistentAdded({%H-}APersistent: TPersistent; {%H-}Select: Boolean);
    class procedure OnDesignPersistentDeleted({%H-}APersistent: TPersistent);
    class procedure OnDesignMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
                      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    class procedure OnDesignSetSelection(const ASelection: TPersistentSelectionList);
  end;

var
  LastActiveSourceEditorWindow: TSourceEditorWindowInterface = nil;
  LastActiveSourceEditor: TSourceEditorInterface = nil;
  DockedTabMaster: TDockedTabMaster absolute IDETabMaster;

implementation

{ TDockedTabMaster }

function TDockedTabMaster.GetTabDisplayState: TTabDisplayState;
begin
  Result := GetTabDisplayStateEditor(SourceEditorManagerIntf.ActiveEditor);
end;

function TDockedTabMaster.GetTabDisplayStateEditor(Index: TSourceEditorInterface): TTabDisplayState;
var
  LPageCtrl: TModulePageControl;
begin
  if Index = nil then
    Exit(tdsNone);
  LPageCtrl := SourceEditorWindows.FindModulePageControl(Index);
  if LPageCtrl = nil then Exit(tdsNone);
  case LPageCtrl.PageIndex of
    0: Exit(tdsCode);
    1: Exit(tdsDesign);
    2: Exit(tdsDesign);
    else
      Exit(tdsNone);
  end;
end;

constructor TDockedTabMaster.Create;
begin
  FAutoSizeControlList := TObjectList.Create(False);
end;

destructor TDockedTabMaster.Destroy;
begin
  FAutoSizeControlList.Free;
  inherited Destroy;
end;

function TDockedTabMaster.AutoSizeInShowDesigner(AControl: TControl): Boolean;
begin
  // enable autosizing for docked form editor forms, see issue #32207 - disabled
  // in SourceFileManager per PreventAutoSize
  FAutoSizeControlList.Add(AControl);
  Result := True;
end;

procedure TDockedTabMaster.EnableAutoSizing(AControl: TControl);
var
  AIndex: Integer;
  AutoSizeControl: TControl;
begin
  // enable autosizing for docked form editor forms, see issue #32207 - disabled
  // in SourceFileManager per PreventAutoSize
  if not Assigned(AControl) then Exit;
  AutoSizeControl := nil;

  if AControl is TNonFormProxyDesignerForm then
  begin
    if (TNonFormProxyDesignerForm(AControl).LookupRoot is TControl) then
      AutoSizeControl := TControl(TNonFormProxyDesignerForm(AControl).LookupRoot);
  end else
    AutoSizeControl := AControl;

  AIndex := FAutoSizeControlList.IndexOf(AutoSizeControl);
  if (AIndex >= 0) then
  begin
    FAutoSizeControlList.Delete(AIndex);
    AutoSizeControl.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TAnchorDockMaster Delayed'){$ENDIF};
  end;
end;

procedure TDockedTabMaster.ToggleFormUnit;
begin
  case TabDisplayState of
    tdsCode:
      ShowDesigner(SourceEditorManagerIntf.ActiveEditor);
    tdsDesign:
      ShowCode(SourceEditorManagerIntf.ActiveEditor);
  end;
end;

procedure TDockedTabMaster.JumpToCompilerMessage(
  ASourceEditor: TSourceEditorInterface);
begin
  SourceEditorManagerIntf.ActiveEditor := ASourceEditor;
  ShowCode(ASourceEditor);
end;

procedure TDockedTabMaster.ShowCode(ASourceEditor: TSourceEditorInterface);
var
  LPageCtrl: TModulePageControl;
begin
  if ASourceEditor = nil then Exit;
  LPageCtrl := SourceEditorWindows.FindModulePageControl(ASourceEditor);
  LPageCtrl.PageIndex := 0;
end;

procedure TDockedTabMaster.ShowDesigner(ASourceEditor: TSourceEditorInterface; AIndex: Integer);
var
  LPageCtrl: TModulePageControl;
begin
  if ASourceEditor = nil then Exit;
  LPageCtrl := SourceEditorWindows.FindModulePageControl(ASourceEditor);
  if (AIndex = 0) or not (LPageCtrl.Pages[AIndex].TabVisible) then
    AIndex := 1;
  if not LPageCtrl.Pages[AIndex].TabVisible then Exit;
  LPageCtrl.PageIndex := AIndex;
  LPageCtrl.OnChange(LPageCtrl);
end;

procedure TDockedTabMaster.ShowForm(AForm: TCustomForm);
var
  LEditor: TSourceEditorInterface;
begin
  LEditor := FindSourceEditorForDesigner(AForm.Designer);
  SourceEditorManagerIntf.ActiveEditor := LEditor;
  ShowDesigner(LEditor);
end;

procedure TDockedTabMaster.OptionsModified;
var
  LSourceEditorWindow: TSourceEditorWindow;
  LSourceEditorPageControl: TSourceEditorPageControl;
  LPageCtrl: TModulePageControl;
  LRefreshDesigner: Boolean;
  LPageIndex: Integer;
begin
  LPageCtrl := SourceEditorWindows.FindModulePageControl(LastActiveSourceEditor);
  LRefreshDesigner := Assigned(LPageCtrl) and (LPageCtrl.PageIndex in [1, 2]);
  LPageIndex := LPageCtrl.PageIndex;
  LPageCtrl.ShowAnchorPage;

  DesignForms.RemoveAllAnchorDesigner;

  for LSourceEditorWindow in SourceEditorWindows do
    for LSourceEditorPageControl in LSourceEditorWindow.PageControlList do
    begin
      LSourceEditorPageControl.PageControl.TabPosition := DockedOptions.TabPosition;
      LSourceEditorPageControl.PageControl.RefreshResizer;
      ShowCode(LSourceEditorPageControl.SourceEditor);
    end;

  if not LRefreshDesigner then Exit;
  LSourceEditorWindow := SourceEditorWindows.SourceEditorWindow[LastActiveSourceEditorWindow];
  LSourceEditorWindow.ActiveDesignForm := nil;
  ShowDesigner(LastActiveSourceEditor, LPageIndex);
end;

{ TDockedMainIDE }

class function TDockedMainIDE.GetCurrentPageControl: TModulePageControl;
var
  LForm: TCustomForm;
  LDesignForm: TDesignForm;
  LSourceEditorWindowInterface: TSourceEditorWindowInterface;
begin
  Result := nil;
  if (FormEditingHook = nil) or (GlobalDesignHook = nil) then Exit;
  LForm := FormEditingHook.GetDesignerForm(GlobalDesignHook.LookupRoot);
  LDesignForm := DesignForms.Find(LForm);
  if LDesignForm = nil then Exit;
  LSourceEditorWindowInterface := LDesignForm.LastActiveSourceWindow;
  Result := SourceEditorWindows.FindModulePageControl(LSourceEditorWindowInterface);
end;

class procedure TDockedMainIDE.Screen_FormAdded(Sender: TObject; AForm: TCustomForm);
var
  LSourceEditor: TSourceEditorInterface;
  LDesignForm: TDesignForm;
  LPageCtrl: TModulePageControl;
begin
  if IsFormDesign(AForm) then
  begin
    {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TDockedMainIDE.Screen_FormAdded ', DbgSName(AForm)); {$ENDIF}

    // AForm like TForm1 etc...
    if not (csDesignInstance in AForm.ComponentState) and not (AForm is TNonFormProxyDesignerForm) then
      Exit;

    LDesignForm := TDesignForm.Create(AForm);
    LDesignForm.Hiding := True;
    DesignForms.Add(LDesignForm);
    LSourceEditor := FindSourceEditorForDesigner(AForm.Designer);
    if LSourceEditor <> nil then
    begin
      LPageCtrl := SourceEditorWindows.FindModulePageControl(LSourceEditor);
      if LPageCtrl <> nil then
      begin
        LPageCtrl.DesignForm := LDesignForm;
        LPageCtrl.ShowDesignPage;
        if LDesignForm.IsAnchorDesign then
          LPageCtrl.ShowAnchorPage;
      end;
    end;
    SetTimer(AForm.Handle, WM_SETNOFRAME, 10, nil);
  end else begin
    if AForm is TSourceEditorWindowInterface then
      AForm.AddHandlerOnChangeBounds(@GlobalSNOnChangeBounds);
  end;
end;

class procedure TDockedMainIDE.Screen_FormDel(Sender: TObject; AForm: TCustomForm);
var
  LSourceEditorWindow: TSourceEditorWindow;
  LSourceEditorPageControl: TSourceEditorPageControl;
begin
  if IsFormDesign(AForm) then
  begin
    {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TDockedMainIDE.Screen_FormDel ', DbgSName(AForm)); {$ENDIF}
    AForm.Parent := nil;

    for LSourceEditorWindow in SourceEditorWindows do
    begin
      if LSourceEditorWindow.ActiveDesignForm <> nil then
        if LSourceEditorWindow.ActiveDesignForm.Form = AForm then
          // don't set ActiveDesignForm := nil! we can't call OnChange tab, because tab don't exist anymore
          LSourceEditorWindow.RemoveActiveDesignForm;

      for LSourceEditorPageControl in LSourceEditorWindow.PageControlList do
        if LSourceEditorPageControl.PageControl.DesignForm <> nil then
          if LSourceEditorPageControl.PageControl.DesignForm.Form = AForm then
          begin
            LSourceEditorPageControl.PageControl.DesignForm := nil;
            LSourceEditorPageControl.PageControl.PageIndex := 0;
          end;
    end;
    DesignForms.Remove(AForm);
  end
  else
    if AForm is TSourceEditorWindowInterface then
      AForm.RemoveHandlerOnChangeBounds(@GlobalSNOnChangeBounds);
end;

class procedure TDockedMainIDE.WindowCreate(Sender: TObject);
var
  LSourceEditorWindow: TSourceEditorWindowInterface;
begin
  if Sender.ClassNameIs('TSourceNotebook') then
  begin
    LSourceEditorWindow := Sender as TSourceEditorWindowInterface;
    SourceEditorWindows.Add(TSourceEditorWindow.Create(LSourceEditorWindow));
  end;
end;

class procedure TDockedMainIDE.WindowDestroy(Sender: TObject);
var
  LDesignForm: TDesignForm;
begin
  for LDesignForm in DesignForms do
    if LDesignForm.LastActiveSourceWindow = Sender then
      LDesignForm.LastActiveSourceWindow := nil;
  SourceEditorWindows.Remove(Sender as TSourceEditorWindowInterface);
  if LastActiveSourceEditorWindow = Sender then
    LastActiveSourceEditorWindow := nil;
end;

class procedure TDockedMainIDE.WindowHide(Sender: TObject);
var
  LSourceEditorWindow: TSourceEditorWindow;
  LDesignForm: TDesignForm;
begin
  LSourceEditorWindow := SourceEditorWindows.SourceEditorWindow[Sender as TSourceEditorWindowInterface];
  if not Assigned(LSourceEditorWindow) or (LSourceEditorWindow.ActiveDesignForm = nil) then
    Exit;
  LDesignForm := LSourceEditorWindow.ActiveDesignForm;
  LDesignForm.HideWindow;
end;

class procedure TDockedMainIDE.WindowShow(Sender: TObject);
var
  LSourceEditorWindow: TSourceEditorWindow;
  LDesignForm: TDesignForm;
begin
  LSourceEditorWindow := SourceEditorWindows.SourceEditorWindow[Sender as TSourceEditorWindowInterface];
  if not Assigned(LSourceEditorWindow) or (LSourceEditorWindow.ActiveDesignForm = nil) then
    Exit;
  LDesignForm := LSourceEditorWindow.ActiveDesignForm;
  LDesignForm.ShowWindow;
end;

class procedure TDockedMainIDE.EditorActivated(Sender: TObject);
var
  LDesigner: TIDesigner;
  LSourceEditor: TSourceEditorInterface;

  function LastSourceEditorNotFound: Boolean;
  var
    i: Integer;
    LSourceEditorPageControl: TSourceEditorPageControl;
    LSourceEditorWindow: TSourceEditorWindow;
  begin
    if (LastActiveSourceEditorWindow = nil) or (LastActiveSourceEditor = nil) then
      Exit(False);

    LSourceEditorWindow := SourceEditorWindows.SourceEditorWindow[LastActiveSourceEditorWindow];
    for LSourceEditorPageControl in LSourceEditorWindow.PageControlList do
    begin
      Result := True;
      for i := 0 to LastActiveSourceEditorWindow.Count - 1 do
        if LSourceEditorPageControl.SourceEditor = LastActiveSourceEditorWindow.Items[i] then
        begin
          Result := False;
          Break;
        end;
      if Result then
      begin
        LastActiveSourceEditor := LSourceEditorPageControl.SourceEditor; // after moving code editor into other window, sometimes IDE switch to other tab :\ damn... this line prevent this.
        Exit;
      end;
    end;
    Result := False;
  end;

var
  LPageCtrl: TModulePageControl;
  LSourceEditorWindowInterface: TSourceEditorWindowInterface;
  LDesignForm: TDesignForm;
begin
  if Sender is TSourceEditorInterface then
  begin
    LSourceEditor := TSourceEditorInterface(Sender);
    // if we create directly new project then Activate is called without EditorCreate...
    if not (LSourceEditor.EditorControl.Parent.Parent is TModulePageControl) then
    begin
      // possible is situation when we moved tab into other window
      // then was not called event EditorDestroy - that generates problems with switching tabs
      // or when we moving tab to first window ( then is raising : duplicates not allowed in dictionary).
      if LastSourceEditorNotFound then
        EditorDestroyed(nil);
      EditorCreate(Sender);
    end;
    LDesigner := LSourceEditor.GetDesigner(True);
    // should be performed during EditorCreate (parent of parent is module page ctrl)
    LPageCtrl := TModulePageControl(LSourceEditor.EditorControl.Parent.Parent);
    if LPageCtrl = nil then Exit;

    LDesignForm := SourceEditorWindows.FindDesignForm(LPageCtrl);
    if LDesigner = nil then
      LPageCtrl.HideDesignPages
    else begin
      if not Assigned(LPageCtrl.Resizer) then
        LPageCtrl.CreateResizer;
      LPageCtrl.ShowDesignPage;
      if LDesignForm.IsAnchorDesign then
        LPageCtrl.ShowAnchorPage;
    end;

    LSourceEditorWindowInterface := TSourceEditorWindowInterface(LPageCtrl.Owner);
    LastActiveSourceEditorWindow := LSourceEditorWindowInterface;
    LastActiveSourceEditor := LSourceEditor;

    // when we switch tab, design form should be hidden
    if (LDesigner = nil) or (LDesignForm = nil) then
      SourceEditorWindows.SourceEditorWindow[LSourceEditorWindowInterface].ActiveDesignForm := nil
    else begin
      // during form  loading for example from package, ActiveDesignForm assignment,
      // blocks the message queue responsible for hiding form
      // We can't check it because there are some forms where designing is not handled yet.
      // (for that kind of forms is returned empty designform data)
      // maybe we can fix this in future
      if not LDesignForm.Hiding then
        // Prevent unexpected events (when is deactivated some control outside designed form)
        if (LDesignForm.LastActiveSourceWindow = LSourceEditorWindowInterface)
        // important!!! for many error - switching between editors...
        and (LPageCtrl.PageIndex in [1, 2]) then
          SourceEditorWindows.SourceEditorWindow[LSourceEditorWindowInterface].ActiveDesignForm := LDesignForm
        else
          SourceEditorWindows.SourceEditorWindow[LSourceEditorWindowInterface].ActiveDesignForm := nil;
    end;

    if (LPageCtrl.PageIndex in [1, 2]) then
    begin
      if not LDesignForm.Hiding then
      begin
        LPageCtrl.AdjustPage;
        LPageCtrl.DesignerSetFocus;
      end;
    end else begin
      if LDesignForm <> nil then
        LDesignForm.HideWindow;
    end;
  end
  else
    SourceEditorWindows.RefreshAllSourceWindowsModulePageControl;
end;

class procedure TDockedMainIDE.EditorCreate(Sender: TObject);
var
  LSourceEditor: TSourceEditorInterface;
  LSourceEditorWindowInterface: TSourceEditorWindowInterface;
  LParent: TWinControl;
  LPageCtrl: TModulePageControl;
begin
  LSourceEditor := Sender as TSourceEditorInterface;
  if LSourceEditor.EditorControl.Parent.Parent is TModulePageControl then Exit;
  LParent := LSourceEditor.EditorControl.Parent;
  LPageCtrl := TModulePageControl.Create(LSourceEditor.EditorControl.Owner);
  LSourceEditor.EditorControl.Parent := LPageCtrl.Pages[0];  // ! SynEdit :)
  LPageCtrl.OnChange := @TabChange;
  LPageCtrl.Parent := LParent;
  LSourceEditorWindowInterface := TSourceEditorWindowInterface(LPageCtrl.Owner);
  SourceEditorWindows.SourceEditorWindow[LSourceEditorWindowInterface].AddPageCtrl(LSourceEditor, LPageCtrl);
end;

class procedure TDockedMainIDE.EditorDestroyed(Sender: TObject);
var
  LSourceEditor: TSourceEditorInterface;
  LPageCtrl: TModulePageControl;
  LSourceEditorWindowInterface: TSourceEditorWindowInterface;
  LSourceEditorWindow: TSourceEditorWindow;
  LDesignForm: TDesignForm;
begin
  // sender is here as special parameter, because is possible situation where is moved editor
  // to another window and was not triggered EditorDestroy - for more info goto editoractivate
  if Sender = nil then
    LSourceEditor := LastActiveSourceEditor
  else
    LSourceEditor := TSourceEditorInterface(Sender);

  // parent don't exist anymore and we must search in each window...
  if Sender = nil then // but not for Sender = nil :P
  begin
    LSourceEditorWindow := SourceEditorWindows.SourceEditorWindow[LastActiveSourceEditorWindow];
    LPageCtrl := LSourceEditorWindow.PageControlList.PageControl[LastActiveSourceEditor]
  end else
    LPageCtrl := SourceEditorWindows.FindModulePageControl(LSourceEditor);

  if LPageCtrl = nil then Exit;

  LDesignForm := DesignForms.Find(LSourceEditor.GetDesigner(False));

  // goto first comment (forced destroy)
  if Sender = nil then
    LSourceEditorWindowInterface := LastActiveSourceEditorWindow
  else
    LSourceEditorWindowInterface := TSourceEditorWindowInterface(LPageCtrl.Owner);

  if LDesignForm <> nil then
  begin
    SourceEditorWindows.SourceEditorWindow[LSourceEditorWindowInterface].ActiveDesignForm := nil;
    LDesignForm.LastActiveSourceWindow := nil;
  end;

  SourceEditorWindows.SourceEditorWindow[LSourceEditorWindowInterface].RemovePageCtrl(LSourceEditor);
  LPageCtrl.Free;

  if LastActiveSourceEditor = LSourceEditor then
    LastActiveSourceEditor := nil;
end;

class procedure TDockedMainIDE.TabChange(Sender: TObject);
var
  LActiveSourceWindowInterface: TSourceEditorWindowInterface;
  LSourceEditorWindow: TSourceEditorWindow;
  LSourceEditorPageControl: TSourceEditorPageControl;
  LDesigner: TIDesigner;
  LDesignForm: TDesignForm;
  LPageCtrl: TModulePageControl;
begin
  // activate proper source editor window when user is clicking on page.
  // (at clicking time can be active other source window)
  LActiveSourceWindowInterface := TComponent(Sender).Owner as TSourceEditorWindowInterface;
  if LActiveSourceWindowInterface <> SourceEditorManagerIntf.ActiveSourceWindow then
    SourceEditorManagerIntf.ActiveSourceWindow := LActiveSourceWindowInterface;

  LPageCtrl := TModulePageControl(Sender);
  if LActiveSourceWindowInterface.ActiveEditor = nil then Exit;
  if LPageCtrl = nil then Exit;

  // in case there is no module and is visible page other than code page.
  LDesigner := LActiveSourceWindowInterface.ActiveEditor.GetDesigner(True);
  LDesignForm := DesignForms.Find(LDesigner);
  if LDesignForm = nil then Exit;
  LSourceEditorWindow := SourceEditorWindows.SourceEditorWindow[LActiveSourceWindowInterface];
  if LSourceEditorWindow = nil then Exit;

  if not (LPageCtrl.PageIndex in [1, 2]) then
    LSourceEditorWindow.ActiveDesignForm := nil
  else begin
    // deactivate design tab in other source editor page control
    for LSourceEditorWindow in SourceEditorWindows do
      if LSourceEditorWindow.SourceEditorWindowInterface = LActiveSourceWindowInterface then
        Continue
      else begin
        for LSourceEditorPageControl in LSourceEditorWindow.PageControlList do
          if (LSourceEditorPageControl.PageControl.DesignForm = LDesignForm) and (LSourceEditorPageControl.PageControl <> Sender) then
            IDETabMaster.ShowCode(LSourceEditorPageControl.SourceEditor);
      end;

    LSourceEditorWindow.ActiveDesignForm := LDesignForm;
    // enable autosizing after creating a new form
    DockedTabMaster.EnableAutoSizing(LDesignForm.Form);
    LPageCtrl.InitPage;
    LPageCtrl.DesignerSetFocus;
  end;
end;

class procedure TDockedMainIDE.GlobalSNOnChangeBounds(Sender: TObject);
var
  LSourceEditorWindowInterface: TSourceEditorWindowInterface;
  LSourceEditorWindow: TSourceEditorWindow;
  LDesignForm: TDesignForm;
  LPageCtrl: TModulePageControl;
  LResizer: TResizer;
begin
  // Check parent. Maybe is different? If yes then window changed state (docked/undocked) and we need to perform few actions
  LSourceEditorWindowInterface := Sender as TSourceEditorWindowInterface;

  // dock/undock event :)
  LSourceEditorWindow := SourceEditorWindows.SourceEditorWindow[LSourceEditorWindowInterface];
  if LSourceEditorWindow = nil then Exit;
  if LSourceEditorWindow.LastTopParent <> LSourceEditorWindowInterface.GetTopParent then
  begin
    LSourceEditorWindow.LastTopParent := LSourceEditorWindowInterface.GetTopParent;
    // refresh for popupparent
    LDesignForm := LSourceEditorWindow.ActiveDesignForm;
    LSourceEditorWindow.ActiveDesignForm := nil;
    LSourceEditorWindow.ActiveDesignForm := LDesignForm;
    if LDesignForm <> nil then
    begin
      LPageCtrl := SourceEditorWindows.FindModulePageControl(LSourceEditorWindowInterface);
      if not Assigned(LPageCtrl) then Exit;
      LResizer := LPageCtrl.Resizer;
      if not Assigned(LResizer) then Exit;
      LDesignForm.Form.Parent := LResizer.FormContainer;
      SetTimer(LDesignForm.Form.Handle, WM_BOUNDTODESIGNTABSHEET, 10, nil);
    end;
  end;
end;

class procedure TDockedMainIDE.OnShowDesignerForm(Sender: TObject; AEditor: TSourceEditorInterface;
  AComponentPaletteClassSelected: Boolean);
var
  LDesignForm: TDesignForm;
  LPageCtrl: TModulePageControl;
  LSourceEditorWindow: TSourceEditorWindow;
  LSourceEditorInterface: TSourceEditorInterface;
begin
  LDesignForm := DesignForms.Find(TCustomForm(Sender).Designer);
  if (LDesignForm = nil) or LDesignForm.Hiding then Exit;
  LPageCtrl := SourceEditorWindows.FindModulePageControl(SourceEditorManagerIntf.ActiveEditor);
  if LPageCtrl = nil then Exit;

  if AComponentPaletteClassSelected then
  begin
    // if form is already opened do nothing, if not then show form for active module.
    for LSourceEditorWindow in SourceEditorWindows do
    begin
      LSourceEditorInterface := LSourceEditorWindow.SourceEditorWindowInterface.ActiveEditor;
      if (LSourceEditorInterface = nil) or (LSourceEditorInterface.GetDesigner(True) <> LDesignForm.Designer) then
        Continue;
      LPageCtrl := SourceEditorWindows.FindModulePageControl(LSourceEditorInterface);
      if LPageCtrl.PageIndex = 1 then
        Exit;
    end;
  end;
  IDETabMaster.ShowDesigner(SourceEditorManagerIntf.ActiveEditor);
end;

class procedure TDockedMainIDE.OnShowSrcEditor(Sender: TObject);
begin
  IDETabMaster.ShowCode(Sender as TSourceEditorInterface);
end;

class procedure TDockedMainIDE.OnDesignShowMethod(const Name: String);
var
  LDesignForm: TDesignForm;
  LSecondEditor: TSourceEditorInterface = nil;
  LSourceEditorWindowInterface: TSourceEditorWindowInterface;
  i: Integer;
begin
  if FormEditingHook = nil then Exit;
  LDesignForm := DesignForms.Find(FormEditingHook.GetCurrentDesigner);
  if LDesignForm = nil then Exit;
  for i := 0 to SourceEditorManagerIntf.SourceWindowCount - 1 do
  begin
    LSourceEditorWindowInterface := SourceEditorManagerIntf.SourceWindows[i];
    if LDesignForm.LastActiveSourceWindow = LSourceEditorWindowInterface then
      Continue;
    if LSourceEditorWindowInterface.ActiveEditor <> nil then
      if LSourceEditorWindowInterface.ActiveEditor.GetDesigner(True) = LDesignForm.Designer then
      begin
        LSecondEditor := LSourceEditorWindowInterface.ActiveEditor;
        Break;
      end;
  end;

  if Assigned(LSecondEditor) then
  begin
    IDETabMaster.ShowCode(LSecondEditor);
    LazarusIDE.DoShowMethod(LSecondEditor, Name);
  end else
    if Assigned(LDesignForm.LastActiveSourceWindow) then
      IDETabMaster.ShowCode(LDesignForm.LastActiveSourceWindow.ActiveEditor);
end;

class procedure TDockedMainIDE.OnDesignRefreshPropertyValues;
var
  LPageCtrl: TModulePageControl;

  function RootIsSelected: Boolean;
  var
    LSelection: TPersistentSelectionList;
    i: Integer;
  begin
    Result := False;
    LSelection := TPersistentSelectionList.Create;
    try
      GlobalDesignHook.GetSelection(LSelection);
      for i := 0 to LSelection.Count - 1 do
        if LSelection.Items[i] = GlobalDesignHook.LookupRoot then
        begin
          Result := True;
          Break;
        end;
    finally
      LSelection.Free;
    end;
  end;

begin
  if not (GlobalDesignHook.LookupRoot is TCustomFrame)
  and not (GlobalDesignHook.LookupRoot is TCustomForm) then Exit;

  LPageCtrl := GetCurrentPageControl;
  if not Assigned(LPageCtrl) then Exit;
  if not ((LPageCtrl.PageIndex = 2) or RootIsSelected) then Exit;
  LPageCtrl.AdjustPage;
end;

class procedure TDockedMainIDE.OnDesignModified(Sender: TObject);
var
  LPageCtrl: TModulePageControl;
begin
  LPageCtrl := GetCurrentPageControl;
  if not Assigned(LPageCtrl) then Exit;
  if not Assigned(LPageCtrl.Resizer) then Exit;
  LPageCtrl.Resizer.ResizeFrame.OnModified;
end;

class procedure TDockedMainIDE.OnDesignPersistentAdded(APersistent: TPersistent; Select: Boolean);
begin
  OnDesignModified(nil);
end;

class procedure TDockedMainIDE.OnDesignPersistentDeleted(APersistent: TPersistent);
begin
  OnDesignModified(nil);
end;

class procedure TDockedMainIDE.OnDesignMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LPageCtrl: TModulePageControl;
begin
  LPageCtrl := GetCurrentPageControl;
  if not Assigned(LPageCtrl) then Exit;
  LPageCtrl.DesignerSetFocus;
end;

class procedure TDockedMainIDE.OnDesignSetSelection(
  const ASelection: TPersistentSelectionList);
var
  LDesignForm: TDesignForm;
  LSelectedControl: TControl;
begin
  LDesignForm := DesignForms.Find(FormEditingHook.GetCurrentDesigner);
  if LDesignForm = nil then Exit;
  if (ASelection.Count = 1) and (ASelection[0] is TControl) then
    LSelectedControl := TControl(ASelection[0]);
  if LSelectedControl = LDesignForm.SelectedControl then Exit;
  LDesignForm.SelectedControl := LSelectedControl;
  OnDesignModified(nil);
end;

end.

