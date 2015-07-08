{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_MainIDE;

{$mode delphi}{$H+}
{$IFDEF USE_POPUP_PARENT_DESIGNER}
{$IFNDEF WINDOWS}
{$MESSAGE Error 'USE_POPUP_PARENT_DESIGNER mode can be used only on Windows'}
{$ENDIF}
{$ELSE}
{.$DEFINE POPUP_WINDOWS}
{$ENDIF}

interface

uses
  Classes, SysUtils, SrcEditorIntf, LazIDEIntf, ComCtrls, Controls, Forms, {$IFDEF USE_POPUP_PARENT_DESIGNER}Windows,{$ENDIF} IDEImagesIntf,
  Buttons, ExtCtrls, Graphics, IDEWindowIntf, 
  sparta_DesignedForm, sparta_resizer, PropEdits, PropEditUtils, FormEditingIntf, ComponentEditors, EditBtn,
{$IFDEF USE_GENERICS_COLLECTIONS}
  Generics.Collections, Generics.Defaults,
{$ELSE}
  ghashmap, sparta_HashUtils, gvector,
{$ENDIF}
  TypInfo, LCLIntf, LCLType, LMessages, sparta_FakeForm, sparta_FakeFrame, sparta_ComponentPalette, SpartaAPI;

const
  WM_SETNOFRAME = WM_USER;
  WM_BoundToDesignTabSheet = WM_USER + 1;

type
  { TDesignFormData }

{$IFDEF USE_GENERICS_COLLECTIONS}
  TDesignFormData = class(TSingletonImplementation, IDesignedForm)
{$ELSE}
  TDesignFormData = class(TComponent, IDesignedForm)
{$ENDIF}
  private
    FWndMethod: TWndMethod;

{$IFDEF USE_POPUP_PARENT_DESIGNER}
    FWinD: Integer;
{$ENDIF}

    FForm: IDesignedForm;
    FLastScreenshot: TBitmap;
    FPopupParent: TSourceEditorWindowInterface;
    FHiding: boolean;
{$IFDEF USE_GENERICS_COLLECTIONS}
    FFormImages: TList<TImage>;
{$ELSE}
    FFormImages: TList;
{$ENDIF}
  protected
    procedure WndMethod(var TheMessage: TLMessage);

    procedure SetPopupParent(AVal: TSourceEditorWindowInterface);
    procedure DoAddForm;
  public
{$IFDEF USE_GENERICS_COLLECTIONS}
    class var AddFormEvents: TList<TNotifyEvent>;
{$ELSE}
    class var AddFormEvents: TVector<TNotifyEvent>;
{$ENDIF}

    class constructor Init;
    class destructor Finit;

    procedure AddFormImage(AImage: TImage);
    procedure RemoveFormImage(AImage: TImage);
    procedure RepaintFormImages;

    property Form: IDesignedForm read FForm implements IDesignedForm;
    property LastScreenshot: TBitmap read FLastScreenshot;
    property PopupParent: TSourceEditorWindowInterface read FPopupParent write SetPopupParent;

    constructor Create(AForm: TCustomForm);
    destructor Destroy; override;
  end;

  { TModulePageControl }

  TModulePageControl = class(TPageControl)
  private
    FResizer: TResizer;
    FDesignFormData: TDesignFormData;
  protected
    procedure SetDesignFormData(const AValue: TDesignFormData); virtual;
  public
    destructor Destroy; override;

    procedure ShowDesignPage;
    procedure HideDesignPage;

    property Resizer: TResizer read FResizer;

    property DesignFormData: TDesignFormData read FDesignFormData write SetDesignFormData;

    procedure BoundToDesignTabSheet;
  end;

  { TSourceEditorWindowData }

  TSourceEditorWindowData = class
  private
    FActiveDesignFormData: TDesignFormData;
  private
    FWndMethod: TWndMethod;
    FForm: TSourceEditorWindowInterface;
{$IFDEF USE_GENERICS_COLLECTIONS}
    FPageCtrlList: TDictionary<TSourceEditorInterface, TModulePageControl>;
{$ELSE}
    FPageCtrlList: THashmap<TSourceEditorInterface, TModulePageControl, THash_TObject>;
{$ENDIF}
    FLastTopParent: TControl;

    procedure SetActiveDesignFormData(const AValue: TDesignFormData);
  protected
    procedure WndMethod(var TheMessage: TLMessage);
    constructor Create(AForm: TSourceEditorWindowInterface);
    destructor Destroy; override;
    procedure OnChangeBounds(Sender: TObject);
    procedure AddPageCtrl(ASrcEditor: TSourceEditorInterface; APage: TModulePageControl);
    procedure RemovePageCtrl(ASrcEditor: TSourceEditorInterface);
  public
    property ActiveDesignFormData: TDesignFormData read FActiveDesignFormData write SetActiveDesignFormData;
  end;

  { TDTXTabMaster }

  TDTXTabMaster = class(TIDETabMaster)
  protected
    function GetTabDisplayState: TTabDisplayState; override;
    function GetTabDisplayStateEditor(Index: TSourceEditorInterface): TTabDisplayState; override;
  public
    procedure ToggleFormUnit; override;
    procedure JumpToCompilerMessage(ASourceEditor: TSourceEditorInterface); override;

    procedure ShowCode(ASourceEditor: TSourceEditorInterface); override;
    procedure ShowDesigner(ASourceEditor: TSourceEditorInterface; AIndex: Integer = 0); override;
    procedure ShowForm(AForm: TCustomForm); override;
  end;

  { TDTXComponentsMaster }

  TDTXComponentsMaster = class(TIDEComponentsMaster)
    function DrawNonVisualComponents(ALookupRoot: TComponent): Boolean; override;
  end;

  TFormHack = class(TCustomForm);

  { TSpartaMainIDE }

  TSpartaMainIDE = class(TObject)
    procedure Screen_FormAdded(Sender: TObject; Form: TCustomForm);
    procedure Screen_FormDel(Sender: TObject; Form: TCustomForm);

    procedure WindowCreate(Sender: TObject);
    procedure WindowDestroy(Sender: TObject);
    procedure WindowShow(Sender: TObject);
    procedure WindowHide(Sender: TObject);

    procedure EditorActivated(Sender: TObject);
    procedure EditorDestroyed(Sender: TObject);
    procedure EditorCreate(Sender: TObject);


    procedure TabChange(Sender: TObject);

    procedure GlobalOnChangeBounds(Sender: TObject);
    procedure GlobalSNOnChangeBounds(Sender: TObject);
{$IFDEF USE_POPUP_PARENT_DESIGNER}
    procedure OnBeforeClose(Sender: TObject);
{$ENDIF}

    procedure OnShowDesignerForm(Sender: TObject; AEditor: TSourceEditorInterface;
                                 AComponentPaletteClassSelected: Boolean);
    procedure OnShowSrcEditor(Sender: TObject);

    procedure OnShowMethod(const Name: String);
    procedure OnDesignRefreshPropertyValues;

    class function ComponentPageControl: TPageControl; static;

    procedure eFilterChange(Sender: TObject);
    procedure eFilterClear(Sender: TObject);
    procedure mnuHideHideComponentPageControlClicked(Sender: TObject);
  end;

var
  spartaIDE: TSpartaMainIDE = nil;
  Forms: Classes.TList; // normal forms
  dsgForms: Classes.TList; // design forms
{$IFDEF USE_GENERICS_COLLECTIONS}
  SourceEditorWindows: TObjectDictionary<TSourceEditorWindowInterface, TSourceEditorWindowData>;
{$ELSE}
  SourceEditorWindows: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>;
{$ENDIF}

  MainComponentsPalette: TComponentsPalette;
  MainComponentsPaletteFilter: TEditButton;
  LastActiveSourceEditorWindow: TSourceEditorWindowInterface = nil;
  LastActiveSourceEditor: TSourceEditorInterface = nil;

  BoundInitialized: Boolean;
{$IFDEF USE_POPUP_PARENT_DESIGNER}
  isIdeDestroyed: boolean = False;
{$ENDIF}

function FindModulePageControl(AForm: TSourceEditorWindowInterface): TModulePageControl; overload;
function FindSourceEditorForDesigner(ADesigner: TIDesigner): TSourceEditorInterface;

implementation

// FUTURE USE
//
//function FindDesignForm(ADesigner: TIDesigner): TCustomForm;
//var
//  f: TDesignFormData;
//begin
//  for Pointer(f) in dsgForms do
//    with f as IDesignedForm do
//    if Form.Designer = ADesigner then
//      Exit(Form);
//
//  Result := nil;
//end;
//
//function FindDesignFormData(AForm: TSourceEditorWindowInterface): TDesignFormData; overload;
//begin
//  Result := FindDesignFormData(
//    FindModulePageControl(AForm)
//  );
//end;
//
//procedure HideAllForms;
//var
//  f: TDesignFormData;
//begin
//  for Pointer(f) in dsgForms do
//    ShowWindow(f.Form.Form.Handle, SW_HIDE);
//end;

function FindModulePageControl(ASourceEditor: TSourceEditorInterface): TModulePageControl; overload;
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

function FindModulePageControl(AForm: TSourceEditorWindowInterface): TModulePageControl; overload;
begin
  Result := FindModulePageControl(AForm.ActiveEditor);
end;

function AbsoluteFindModulePageControl(ASrcEditor: TSourceEditorInterface): TModulePageControl;
var
  LSEWD: TSourceEditorWindowData;
{$IFNDEF USE_GENERICS_COLLECTIONS}
  LIterator: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.TIterator;
{$ENDIF}
begin
  Result := nil;
{$IFDEF USE_GENERICS_COLLECTIONS}
  for LSEWD in SourceEditorWindows.Values do
    if LSEWD.FPageCtrlList.ContainsKey(ASrcEditor) then
      Exit(LSEWD.FPageCtrlList[ASrcEditor]);
{$ELSE}
  LIterator := SourceEditorWindows.Iterator;
  if LIterator <> nil then
  repeat
    LSEWD := LIterator.Value;
    if LSEWD.FPageCtrlList.contains(ASrcEditor) then
    begin
      LIterator.Free;
      Exit(LSEWD.FPageCtrlList[ASrcEditor]);
    end;
  until not LIterator.next;
  LIterator.Free;
{$ENDIF}

end;

function FindSourceEditorForDesigner(ADesigner: TIDesigner): TSourceEditorInterface;
var
  i: Integer;
begin
  for i := 0 to SourceEditorManagerIntf.SourceEditorCount - 1 do
    if SourceEditorManagerIntf.SourceEditors[i].GetDesigner(False) = ADesigner then
      Exit(SourceEditorManagerIntf.SourceEditors[i]);
  Result := nil;
end;

function FindDesignFormData(ADesigner: TIDesigner): TDesignFormData; overload;
var
  p: Pointer;
  f: TDesignFormData absolute p;
  fi: IDesignedForm = nil;
begin
  Result := nil;

  if ADesigner = nil then
    Exit;

  for p in dsgForms do
  begin
    fi := f.FForm;
    with fi do
    begin
      if (Form.Designer = ADesigner) then
      begin
        Exit(f);
      end;
    end;
  end;
end;

procedure RefreshAllSourceWindowsModulePageControl;
var
  LWindow: TSourceEditorWindowInterface;
  LPageCtrl: TModulePageControl;
{$IFNDEF USE_GENERICS_COLLECTIONS}
  LIterator: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.TIterator;
{$ENDIF}
begin
{$IFDEF USE_GENERICS_COLLECTIONS}
  for LWindow in SourceEditorWindows.Keys do
  begin
    LPageCtrl := FindModulePageControl(LWindow);

    // for example LPageCtrl is nil when we clone module to new window
    if (LPageCtrl = nil) or (csDestroying in LWindow.ComponentState) then
      Continue;

    if LWindow.ActiveEditor = nil then
      LPageCtrl.HideDesignPage
    else
      if LWindow.ActiveEditor.GetDesigner(True) <> nil then
        // TODO some check function: is displayed right form?
        LPageCtrl.ShowDesignPage
      else
        LPageCtrl.HideDesignPage;
  end;
{$ELSE}
  LIterator := SourceEditorWindows.Iterator;
  if LIterator <> nil then
  repeat
    LWindow := LIterator.Key;

    LPageCtrl := FindModulePageControl(LWindow);

    // for example LPageCtrl is nil when we clone module to new window
    if (LPageCtrl = nil) or (csDestroying in LWindow.ComponentState) then
      Continue;

    if LWindow.ActiveEditor = nil then
      LPageCtrl.HideDesignPage
    else
      if LWindow.ActiveEditor.GetDesigner(True) <> nil then
        // TODO some check function: is displayed right form?
        LPageCtrl.ShowDesignPage
      else
        LPageCtrl.HideDesignPage;
  until not LIterator.next;
  LIterator.Free;
{$ENDIF}
end;

// sometimes at some level of initialization form can not contain TIDesigner (during ide run and when is oppened default project with some TForm1)
function FindDesignFormData(AForm: TCustomForm): TDesignFormData; overload;
var
  f: TDesignFormData;
begin
  Result := nil;

  if AForm = nil then
    Exit;

  for Pointer(f) in dsgForms do
    with f as IDesignedForm do
      if (Form = AForm) then
        Exit(f);
end;

function FindDesignFormData(AModulePageCtrl: TModulePageControl): TDesignFormData; overload;
var
  LSourceWindow: TSourceEditorWindowInterface;
  LSourceEditor: TSourceEditorInterface;
{$IFNDEF USE_GENERICS_COLLECTIONS}
  LIterator: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.TIterator;
{$ENDIF}
begin
  Result := nil;

  if AModulePageCtrl = nil then
    Exit;

{$IFDEF USE_GENERICS_COLLECTIONS}
  for LSourceWindow in SourceEditorWindows.Keys do
  begin
    if AModulePageCtrl.Owner = LSourceWindow then
    begin
      LSourceEditor := LSourceWindow.ActiveEditor;
      if LSourceEditor = nil then
        Exit;

      Result := FindDesignFormData(LSourceEditor.GetDesigner(True));

      Exit;
    end;
  end;
{$ELSE}
  LIterator := SourceEditorWindows.Iterator;
  if LIterator <> nil then
  try
    repeat
      LSourceWindow := LIterator.Key;

      if AModulePageCtrl.Owner = LSourceWindow then
      begin
        LSourceEditor := LSourceWindow.ActiveEditor;
        if LSourceEditor = nil then
          Exit;

        Result := FindDesignFormData(LSourceEditor.GetDesigner(True));

        Exit;
      end;
    until not LIterator.next;
  finally
    LIterator.Free;
  end;
{$ENDIF}
end;

{ TDesignFormData }

procedure TDesignFormData.WndMethod(var TheMessage: TLMessage);

  // Without this button F12 don't work. (after creating new for editor is inactive) :<
  procedure FixF12_ActiveEditor;
  var
    i: Integer;
  begin
    SourceEditorManagerIntf.ActiveEditor := nil;
    for i := 0 to SourceEditorManagerIntf.UniqueSourceEditorCount - 1 do
      if Form.Form.Designer = SourceEditorManagerIntf.UniqueSourceEditors[i].GetDesigner(True) then
      begin
        SourceEditorManagerIntf.ActiveEditor := SourceEditorManagerIntf.UniqueSourceEditors[i];
        Break;
      end;
  end;

begin
{$IFDEF USE_POPUP_PARENT_DESIGNER}
  if isIdeDestroyed then
    FWinD:=-1;

  if FWinD <> -1 then
    if (TheMessage.msg = WM_ERASEBKGND)
      and (not IsWindowVisible(TCustomForm(LazarusIDE.GetMainBar).Handle)) then
    begin
      if Form.LastActiveSourceWindow <> nil then
      begin
        if Form.LastActiveSourceWindow.ActiveEditor.GetDesigner(True) = Form.Form.Designer then
        begin
          LPageCtrl := FindModulePageControl(Form.LastActiveSourceWindow.ActiveEditor);
          if LPageCtrl.PageIndex = 1 then
          begin
            LPageCtrl.PageIndex := 0;
          end;
        end;
      end;
    end;
{$ENDIF}

  if TheMessage.msg = WM_SETNOFRAME then
  begin
    ShowWindow(Form.Form.Handle, SW_HIDE);
    FHiding := False;

    FixF12_ActiveEditor;

    if Form.Form is TFakeForm then
      RepaintFormImages;
  end;

  // during docking, form position was in wrong place... we need to delay changing position :)
  if TheMessage.msg = WM_BoundToDesignTabSheet then
    if Form.LastActiveSourceWindow <> nil then
      SourceEditorWindows[Form.LastActiveSourceWindow].OnChangeBounds(nil);

   // we need to correct ActiveEditor to right form
   case TheMessage.msg of
     LM_LBUTTONDOWN, LM_RBUTTONDOWN, LM_MBUTTONDOWN, LM_XBUTTONDOWN:
       if Form.LastActiveSourceWindow <> nil then
       begin
         SourceEditorManagerIntf.ActiveSourceWindow := Form.LastActiveSourceWindow;
         SourceEditorManagerIntf.ActiveEditor := Form.LastActiveSourceWindow.ActiveEditor;
       end;
   end;

   FWndMethod(TheMessage);
end;

procedure TDesignFormData.SetPopupParent(AVal: TSourceEditorWindowInterface);
begin
  FPopupParent := AVal;
  Form.RealPopupParent := FPopupParent;
end;

class constructor TDesignFormData.Init;
begin
{$IFDEF USE_GENERICS_COLLECTIONS}
  AddFormEvents := TList<TNotifyEvent>.Create;
{$ELSE}
  AddFormEvents := TVector<TNotifyEvent>.Create;
{$ENDIF}
end;

class destructor TDesignFormData.Finit;
begin
  AddFormEvents.Free;
end;

procedure TDesignFormData.AddFormImage(AImage: TImage);
begin
  if FFormImages <> nil then
    FFormImages.Add(AImage);
end;

procedure TDesignFormData.RemoveFormImage(AImage: TImage);
begin
  if FFormImages <> nil then
    FFormImages.Remove(AImage);
end;

procedure TDesignFormData.RepaintFormImages;
var
  LImage: TImage;
begin
  if FFormImages <> nil then
  begin
    for LImage in FFormImages do
      LImage.OnResize(LImage);
  end;
end;

procedure TDesignFormData.DoAddForm;
var
{$IFDEF USE_GENERICS_COLLECTIONS}
  ne: TNotifyEvent;
{$ELSE}
  i: Integer;
{$ENDIF}
begin
{$IFDEF USE_GENERICS_COLLECTIONS}
  for ne in AddFormEvents do
    ne(Self);
{$ELSE}
  for i := 0 to AddFormEvents.Size - 1 do
    AddFormEvents[i](Self);
{$ENDIF}

end;

constructor TDesignFormData.Create(AForm: TCustomForm);
begin
  FForm := AForm as IDesignedForm;

  FLastScreenshot := TBitmap.Create;
  FWndMethod := FForm.Form.WindowProc;
  FForm.Form.WindowProc := WndMethod;

  if FForm.Form is TFakeForm then
  begin
{$IFDEF USE_GENERICS_COLLECTIONS}
    FFormImages := TList<TImage>.Create;
{$ELSE}
    FFormImages := TList.Create;
{$ENDIF}
    DoAddForm;
  end;
end;

destructor TDesignFormData.Destroy;
var
  LImage: TImage;
begin
  FForm.Form.WindowProc := FWndMethod; // ! important risky point :P

  if FFormImages <> nil then
  begin
    for LImage in FFormImages do
      LImage.Free;

    FreeAndNil(FFormImages);
  end;
  FLastScreenshot.Free;

  inherited Destroy;
end;

{ TModulePageControl }

procedure TModulePageControl.SetDesignFormData(const AValue: TDesignFormData);
begin
  if (AValue = FDesignFormData) then
    // for show lfm code, if we want after editing lfm go back to form without any error (error can be raised even when we restart IDE)
    if Assigned(FResizer) then
    begin
      if FResizer.DesignedForm = AValue as IDesignedForm then
        Exit;
    end
    else
      Exit;

  FDesignFormData := AValue;
  if AValue = nil then
  begin
    //find
    if Assigned(FResizer) then
      FResizer.DesignedForm := nil;
  end
  else
  begin
    AValue.Form.LastActiveSourceWindow := Owner as TSourceEditorWindowInterface;
    if Assigned(FResizer) then
      FResizer.DesignedForm := AValue;
    BoundToDesignTabSheet;
  end;
end;

destructor TModulePageControl.Destroy;
begin
  DesignFormData := nil;
  inherited Destroy;
end;

procedure TModulePageControl.ShowDesignPage;
begin
  Pages[1].TabVisible := True;
end;

procedure TModulePageControl.HideDesignPage;
begin
  Pages[1].TabVisible:=False;
end;

procedure TModulePageControl.BoundToDesignTabSheet;
begin
  if (ActivePageIndex <> 1) then
    Exit;

  if Assigned(FResizer) then
    FResizer.TryBoundSizerToDesignedForm(nil);
end;

{ TSourceEditorWindowData }

procedure TSourceEditorWindowData.SetActiveDesignFormData(
  const AValue: TDesignFormData);
var
  LPageCtrl: TModulePageControl;
begin
  if FActiveDesignFormData = AValue then
    Exit;

  // ukrywanie jest też w resize. To zdarzenie jest potrzebne gdyż nie zachodzi OnChange ModulePageCtrl
  // tylko można przelaczyc kod z zakladkami z kodem i window jest still active :)
  if FActiveDesignFormData <> nil then
    // nie chowaj jesli forma ma byc schowana podczas przechwytywania na starcie
    if not FActiveDesignFormData.FHiding then
    begin
      FActiveDesignFormData.FForm.HideWindow;
    end;
  FActiveDesignFormData := AValue;

  LPageCtrl := FindModulePageControl(FForm);
  if (AValue <> nil) then
  begin
    with AValue as IDesignedForm do
    if not AValue.FHiding and (RealBorderStyle <> bsNone) then
    begin
      BeginUpdate;
      RealBorderIcons := [];
      RealBorderStyle := bsNone;
      Form.Show;
      EndUpdate;
    end;
    // important when we want back to tab where was oppened form :<
    LazarusIDE.DoShowDesignerFormOfSrc(FForm.ActiveEditor);
  end;

  // gdy DestroyEditor - z tego miejsca nie jestesmy w stanie ustalic pagecontrol po FForm (trzebabyprzechowywac lastactiveeditor)
  if LPageCtrl = nil then
    Exit;

  LPageCtrl.DesignFormData := AValue;
  // for USE_POPUP_PARENT_DESIGNER to eliminate form over code
  LPageCtrl.OnChange(LPageCtrl);
end;

procedure TSourceEditorWindowData.WndMethod(var TheMessage: TLMessage);
begin
  FWndMethod(TheMessage);
end;

constructor TSourceEditorWindowData.Create(AForm: TSourceEditorWindowInterface);
begin
  FWndMethod := AForm.WindowProc;
  AForm.WindowProc := WndMethod;
  FForm := AForm;
{$IFDEF USE_GENERICS_COLLECTIONS}
  FPageCtrlList := TDictionary<TSourceEditorInterface, TModulePageControl>.Create;
{$ELSE}
  FPageCtrlList := THashmap<TSourceEditorInterface, TModulePageControl, THash_TObject>.Create;
{$ENDIF}
end;

destructor TSourceEditorWindowData.Destroy;
begin
  FForm.WindowProc := FWndMethod;
  FPageCtrlList.Free;
  inherited Destroy;
end;

procedure TSourceEditorWindowData.OnChangeBounds(Sender: TObject);
var
  LPageCtrl: TModulePageControl;
begin
  LPageCtrl := FindModulePageControl(FForm);
  if LPageCtrl <> nil then
    LPageCtrl.BoundToDesignTabSheet;
end;

procedure TSourceEditorWindowData.AddPageCtrl(ASrcEditor: TSourceEditorInterface; APage: TModulePageControl);
begin
{$IFDEF USE_GENERICS_COLLECTIONS}
  FPageCtrlList.Add(ASrcEditor, APage);
{$ELSE}
  FPageCtrlList.insert(ASrcEditor, APage);
{$ENDIF}
  APage.Pages[1].OnChangeBounds:=OnChangeBounds;
end;

procedure TSourceEditorWindowData.RemovePageCtrl(ASrcEditor: TSourceEditorInterface);
begin
{$IFDEF USE_GENERICS_COLLECTIONS}
  FPageCtrlList.Remove(ASrcEditor);
{$ELSE}
  FPageCtrlList.Delete(ASrcEditor);
{$ENDIF}
end;

{ TDTXTabMaster }

function TDTXTabMaster.GetTabDisplayState: TTabDisplayState;
begin
  Result := GetTabDisplayStateEditor(SourceEditorManagerIntf.ActiveEditor);
end;

function TDTXTabMaster.GetTabDisplayStateEditor(Index: TSourceEditorInterface
  ): TTabDisplayState;
var
  LPageCtrl: TModulePageControl;
begin
  if Index = nil then
    Exit(tdsNone);

  LPageCtrl := FindModulePageControl(Index);
  if LPageCtrl = nil then
    Exit(tdsNone);

  case LPageCtrl.PageIndex of
    0: Exit(tdsCode);
    1: Exit(tdsDesign);
  else
    Exit(tdsOther);
  end;
end;

procedure TDTXTabMaster.ToggleFormUnit;
begin
  case TabDisplayState of
    tdsCode:
      ShowDesigner(SourceEditorManagerIntf.ActiveEditor);
    tdsDesign:
      ShowCode(SourceEditorManagerIntf.ActiveEditor);
  end;
end;

procedure TDTXTabMaster.JumpToCompilerMessage(
  ASourceEditor: TSourceEditorInterface);
begin
  SourceEditorManagerIntf.ActiveEditor := ASourceEditor;

  ShowCode(ASourceEditor);
end;

procedure TDTXTabMaster.ShowCode(ASourceEditor: TSourceEditorInterface);
begin
  if ASourceEditor = nil then
    Exit;

  FindModulePageControl(ASourceEditor).PageIndex := 0;
end;

procedure TDTXTabMaster.ShowDesigner(ASourceEditor: TSourceEditorInterface; AIndex: Integer);
var
  LPageCtrl: TModulePageControl;
begin
  if ASourceEditor = nil then
    Exit;

  LPageCtrl := FindModulePageControl(ASourceEditor);

  if not LPageCtrl.Pages[1].TabVisible then
    Exit;

  LPageCtrl.PageIndex := 1;
end;

procedure TDTXTabMaster.ShowForm(AForm: TCustomForm);
var
  LEditor: TSourceEditorInterface;
begin
  LEditor := FindSourceEditorForDesigner(AForm.Designer);

  SourceEditorManagerIntf.ActiveEditor := LEditor;

  ShowDesigner(LEditor);
end;

{ TDTXComponentsMaster }

function TDTXComponentsMaster.DrawNonVisualComponents(ALookupRoot: TComponent
  ): Boolean;
var
  LFormData: TDesignFormData;
  LPageCtrl: TModulePageControl;
begin
  Result := True;

  LFormData := FindDesignFormData(FormEditingHook.GetDesignerForm(ALookupRoot){ALookupRoot as TCustomForm});
  if LFormData = nil then
    Exit;

  LPageCtrl := FindModulePageControl(LFormData.Form.LastActiveSourceWindow);
  if (LPageCtrl = nil) or (LPageCtrl.Resizer = nil) or (LPageCtrl.Resizer.FMainDTU = nil) then
    Exit;

  Result := LPageCtrl.Resizer.FMainDTU.ShowNonVisualComponents;
end;

{ TSpartaMainIDE }

procedure TSpartaMainIDE.Screen_FormAdded(Sender: TObject; Form: TCustomForm);
var
  LSourceEditor: TSourceEditorInterface;
  LFormData: TDesignFormData;
  i: Integer;
  LPageCtrl: TModulePageControl;
begin
  if IsFormDesign(Form) then
  begin
    // Form like TForm1 etc...
    if (csDesignInstance in Form.ComponentState) or (Form is TNonFormProxyDesignerForm) then
    begin
      LFormData := TDesignFormData.Create(Form);
      LFormData.FHiding:=True;
      dsgForms.Add(LFormData);

      LSourceEditor := FindSourceEditorForDesigner(Form.Designer);

      if LSourceEditor <> nil then
      begin
        LPageCtrl := FindModulePageControl(LSourceEditor);
        if LPageCtrl <> nil then
        begin
          LPageCtrl.ShowDesignPage;
          LPageCtrl.DesignFormData := LFormData;
        end;
      end;

      PostMessage(Form.Handle, WM_SETNOFRAME, 0, 0);
    end;
  end
  else
  begin
    if not BoundInitialized then
    begin
      for i := 0 to Screen.FormCount - 1 do
        if Screen.Forms[i] = Form then
          Continue
        else
        begin
          Screen.Forms[i].AddHandlerOnChangeBounds(spartaIDE.GlobalOnChangeBounds);

          // if POPUP_WINDOWS is defined then show all forms on main form
          if (LazarusIDE.GetMainBar = Screen.Forms[i]) then
            Continue;

{$IFDEF POPUP_WINDOWS}
          Screen.Forms[i].PopupMode := pmExplicit;
          Screen.Forms[i].PopupParent := LazarusIDE.GetMainBar as TCustomForm;
{$ENDIF}
        end;
      BoundInitialized := True;
    end;

    if Form is TSourceEditorWindowInterface then
    begin
      Form.AddHandlerOnChangeBounds(spartaIDE.GlobalSNOnChangeBounds);
      Form.PopupMode := pmExplicit;
    end
    else
    begin
      Form.AddHandlerOnChangeBounds(spartaIDE.GlobalOnChangeBounds);
{$IFDEF POPUP_WINDOWS}
      Form.PopupMode := pmExplicit;
{$ENDIF}
    end;

{$IFDEF POPUP_WINDOWS}
    Form.PopupParent := LazarusIDE.GetMainBar as TCustomForm;
{$ENDIF}

    Forms.Add(Form);
  end;
end;

procedure TSpartaMainIDE.Screen_FormDel(Sender: TObject; Form: TCustomForm);
var
  LSEWD: TSourceEditorWindowData;
  mpc: TModulePageControl;
  LFormData: TDesignFormData;
{$IFNDEF USE_GENERICS_COLLECTIONS}
  LIterator: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.TIterator;
  LIterator2: THashmap<TSourceEditorInterface, TModulePageControl, THash_TObject>.TIterator;
{$ENDIF}
begin
  if not IsFormDesign(Form) then
  begin
    if Form is TSourceEditorWindowInterface then
      Form.RemoveHandlerOnChangeBounds(spartaIDE.GlobalSNOnChangeBounds)
    else
      Form.RemoveHandlerOnChangeBounds(spartaIDE.GlobalOnChangeBounds)
  end
  else
  begin
{$IFNDEF USE_POPUP_PARENT_DESIGNER}
    Form.ParentWindow := 0;
    Application.ProcessMessages; // For TFrame - System Error. Code: 1400. Invalid window handle.
{$ENDIF}

    LFormData := FindDesignFormData(Form);
    dsgForms.Remove(LFormData);

    if Assigned(MainComponentsPalette) then
      if MainComponentsPalette.Root = LookupRoot(Form) then
        MainComponentsPalette.Root := nil;

{$IFDEF USE_GENERICS_COLLECTIONS}
    for LSEWD in SourceEditorWindows.Values do
    begin
      if LSEWD.ActiveDesignFormData <> nil then
        if LSEWD.ActiveDesignFormData.Form.Form = Form then
          LSEWD.FActiveDesignFormData := nil; // important - we can't call OnChange tab, because tab don't exist anymore

      for mpc in LSEWD.FPageCtrlList.Values do
        if mpc.DesignFormData <> nil then
           if mpc.DesignFormData.Form.Form = Form then
              mpc.DesignFormData := nil;
    end;
{$ELSE}
    LIterator := SourceEditorWindows.Iterator;
    if LIterator <> nil then
    repeat
      LSEWD := LIterator.Value;
      if LSEWD.ActiveDesignFormData <> nil then
        if LSEWD.ActiveDesignFormData.Form.Form = Form then
          LSEWD.FActiveDesignFormData := nil; // important - we can't call OnChange tab, because tab don't exist anymore

      LIterator2 := LSEWD.FPageCtrlList.Iterator;
      if LIterator2 <> nil then
      repeat
        mpc := LIterator2.Value;
        if mpc.DesignFormData <> nil then
           if mpc.DesignFormData.Form.Form = Form then
              mpc.DesignFormData := nil;
      until not LIterator2.next;
      LIterator2.Free;
    until not LIterator.next;
    LIterator.Free;
{$ENDIF}

    LFormData.Free;
  end;
end;

procedure TSpartaMainIDE.WindowCreate(Sender: TObject);
var
  LSourceEditorWindow: TSourceEditorWindowInterface;
begin
  if Sender.ClassNameIs('TSourceNotebook') then
  begin
    LSourceEditorWindow := Sender as TSourceEditorWindowInterface;
{$IFDEF USE_GENERICS_COLLECTIONS}
    SourceEditorWindows.Add(LSourceEditorWindow, TSourceEditorWindowData.Create(LSourceEditorWindow));
{$ELSE}
    SourceEditorWindows.insert(LSourceEditorWindow, TSourceEditorWindowData.Create(LSourceEditorWindow));
{$ENDIF}
  end;
end;

procedure TSpartaMainIDE.WindowDestroy(Sender: TObject);
var
  p: Pointer;
  f: TDesignFormData absolute p;
begin
  for p in dsgForms do
    if f.FForm.LastActiveSourceWindow = Sender then
      f.FForm.LastActiveSourceWindow := nil;
{$IFDEF USE_GENERICS_COLLECTIONS}
  SourceEditorWindows.Remove(Sender as TSourceEditorWindowInterface);
{$ELSE}
  SourceEditorWindows[Sender as TSourceEditorWindowInterface].Free;
  SourceEditorWindows.Delete(Sender as TSourceEditorWindowInterface);
{$ENDIF}
  if LastActiveSourceEditorWindow = Sender then
    LastActiveSourceEditorWindow := nil;
end;

procedure TSpartaMainIDE.WindowShow(Sender: TObject);
var
  LWindow: TSourceEditorWindowInterface;
  LWindowData: TSourceEditorWindowData;
  LDesignedForm: IDesignedForm;
begin
  LWindow := Sender as TSourceEditorWindowInterface;

{$IFDEF USE_GENERICS_COLLECTIONS}
  if not SourceEditorWindows.TryGetValue(LWindow, LWindowData) or
    (LWindowData.ActiveDesignFormData = nil)
  then
    Exit;
{$ELSE}
  if not SourceEditorWindows.contains(LWindow) then
    Exit;
  if SourceEditorWindows.GetData(LWindow).ActiveDesignFormData = nil then
    Exit;
{$ENDIF}

  LDesignedForm := LWindowData.ActiveDesignFormData as IDesignedForm;
  LDesignedForm.ShowWindow;
end;

procedure TSpartaMainIDE.WindowHide(Sender: TObject);
var
  LWindow: TSourceEditorWindowInterface;
  LWindowData: TSourceEditorWindowData;
  LDesignedForm: IDesignedForm;
begin
  LWindow := Sender as TSourceEditorWindowInterface;

{$IFDEF USE_GENERICS_COLLECTIONS}
  if not SourceEditorWindows.TryGetValue(LWindow, LWindowData) or
    (LWindowData.ActiveDesignFormData = nil)
  then
    Exit;
{$ELSE}
  if not SourceEditorWindows.contains(LWindow) then
    Exit;
  LWindowData := SourceEditorWindows[LWindow];
  if LWindowData.ActiveDesignFormData = nil then
    Exit;
{$ENDIF}

  LDesignedForm := LWindowData.ActiveDesignFormData as IDesignedForm;
  LDesignedForm.HideWindow;
end;

procedure TSpartaMainIDE.EditorActivated(Sender: TObject);
var
  LDesigner: TIDesigner;
  LSourceEditor: TSourceEditorInterface;
{$IFNDEF USE_GENERICS_COLLECTIONS}
  LIterator: THashmap<TSourceEditorInterface, TModulePageControl, THash_TObject>.TIterator;
{$ENDIF}

  function LastSourceEditorNotFound: boolean;
  var
    i: Integer;
    se: TSourceEditorInterface;
  begin
    if (LastActiveSourceEditorWindow = nil) or (LastActiveSourceEditor = nil) then
      Exit(False);

{$IFDEF USE_GENERICS_COLLECTIONS}
    for se in SourceEditorWindows[LastActiveSourceEditorWindow].FPageCtrlList.Keys do
    begin
      Result := True;
      for i := 0 to LastActiveSourceEditorWindow.Count - 1 do
        if se = LastActiveSourceEditorWindow.Items[i] then
        begin
          Result := False;
          Break;
        end;

      if Result then
      begin
        LastActiveSourceEditor := se; // after moving code editor into other window, sometimes IDE switch to other tab :\ damn... this line prevent this.
        Exit;
      end;
    end;
{$ELSE}
    LIterator := SourceEditorWindows[LastActiveSourceEditorWindow].FPageCtrlList.Iterator;
    if LIterator <> nil then
    repeat
      se := LIterator.Key;
      Result := True;
      for i := 0 to LastActiveSourceEditorWindow.Count - 1 do
        if se = LastActiveSourceEditorWindow.Items[i] then
        begin
          Result := False;
          Break;
        end;
      if Result then
      begin
        LastActiveSourceEditor := se; // after moving code editor into other window, sometimes IDE switch to other tab :\ damn... this line prevent this.
        LIterator.Free;
        Exit;
      end;
    until not LIterator.next;
    LIterator.Free;
{$ENDIF}

    Result := False;
  end;

var
  LPageCtrl: TModulePageControl;
  LSourceEditorWindow: TSourceEditorWindowInterface;
  LDesignFormData: TDesignFormData;
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
    if LPageCtrl = nil then
      Exit;

    if LDesigner = nil then
      LPageCtrl.HideDesignPage
    else
    begin
      if LPageCtrl.Resizer = nil then
        LPageCtrl.FResizer := TResizer.Create(LPageCtrl.Pages[1]);

      LPageCtrl.ShowDesignPage;
    end;

    LSourceEditorWindow := TSourceEditorWindowInterface(LPageCtrl.Owner);

    LastActiveSourceEditorWindow := LSourceEditorWindow;
    LastActiveSourceEditor := LSourceEditor;

    LDesignFormData := FindDesignFormData(LPageCtrl);

    // thanks this when we switch tab, design form is hidden
    if (LDesigner = nil) or (LDesignFormData = nil) then
      SourceEditorWindows[LSourceEditorWindow].ActiveDesignFormData := nil
    else
    begin
      // podczas ladowania formy np. z paczki przypisanie ActiveDesignFormData
      // blokuje kolejke komunikatow chowajacych forme i nie zachodzi zdarzenie
      // ktore ustawia hiding na false. Nie mozna od razu sprawdzic tego bo istnieja formy
      // (np. nieobsluzone design - frame na wczesnym etapie) ktore zwracaja puste designformdata
      // moze w przyszlosci to nie bedzie potrzebne
      if not LDesignFormData.FHiding then
        // zapobiega nadawaniu captiona formy jako eFormCaption z kliknięcia :)
        if (LDesignFormData.Form.LastActiveSourceWindow = LSourceEditorWindow)
        // important!!! for many error - przelaczanie sie miedzy edytorami...
        and (LPageCtrl.PageIndex = 1) then
          SourceEditorWindows[LSourceEditorWindow].ActiveDesignFormData := LDesignFormData
        else
          SourceEditorWindows[LSourceEditorWindow].ActiveDesignFormData := nil;
    end;

    case LPageCtrl.PageIndex of
      0: if LDesignFormData <> nil then {$IFNDEF USE_POPUP_PARENT_DESIGNER}LDesignFormData.Form.HideWindow{$ENDIF};
      1:
        begin
          LazarusIDE.DoShowDesignerFormOfSrc(LSourceEditorWindow.ActiveEditor);

          // for lfm edition...
          with LDesignFormData as IDesignedForm do
          if not LDesignFormData.FHiding and (RealBorderStyle <> bsNone) then
          begin
              BeginUpdate;
              RealBorderIcons := [];
              RealBorderStyle := bsNone;
              Form.Show;
              EndUpdate;
              LPageCtrl.BoundToDesignTabSheet;

              PostMessage(Form.Handle, WM_BoundToDesignTabSheet, 0, 0);
          end;
        end;
    end;
  end
  else
  begin
    RefreshAllSourceWindowsModulePageControl;
  end;
end;

procedure TSpartaMainIDE.EditorDestroyed(Sender: TObject);
var
  LSourceEditor: TSourceEditorInterface;
  LPageCtrl: TModulePageControl;
  LSourceEditorWindow: TSourceEditorWindowInterface;
  LFormData: TDesignFormData;
begin
  // sender jest specjalnym parametrem. mozliwa jest sytuacja podczas przenoszenia eytora
  // ze nie zostalo wywolane EditorDestroy - patrz editoractivate
  if Sender = nil then
    LSourceEditor := LastActiveSourceEditor
  else
    LSourceEditor := TSourceEditorInterface(Sender);

  // parent już nie istnieje i musimy szukać po wszystkich oknach ...
  if Sender = nil then // ale nie dla tej sytuacji
    LPageCtrl := SourceEditorWindows[LastActiveSourceEditorWindow].FPageCtrlList[LastActiveSourceEditor]
  else
    LPageCtrl := AbsoluteFindModulePageControl(LSourceEditor);

  if LPageCtrl = nil then
    Exit;

  LFormData := FindDesignFormData(LSourceEditor.GetDesigner(False));

  // patrz komentarz na poczatku (wymuszone destroy)
  if Sender = nil then
    LSourceEditorWindow := LastActiveSourceEditorWindow
  else
    LSourceEditorWindow := TSourceEditorWindowInterface(LPageCtrl.Owner);

  if LFormData <> nil then
  begin
    SourceEditorWindows[LSourceEditorWindow].ActiveDesignFormData := nil;
    LFormData.Form.LastActiveSourceWindow := nil;
  end;

  SourceEditorWindows[LSourceEditorWindow].RemovePageCtrl(LSourceEditor);
  LPageCtrl.Free;

  if LastActiveSourceEditor = LSourceEditor then
    LastActiveSourceEditor := nil;
end;

procedure TSpartaMainIDE.EditorCreate(Sender: TObject);

var
  LSourceEditor: TSourceEditorInterface;

  function CreateModulePageControl: TModulePageControl;
  var
    LNewTabSheet: TTabSheet;
    LSourceEditorWindow: TSourceEditorWindowInterface;
    LParent: TWinControl;
  begin
    Result := TModulePageControl.Create(LSourceEditor.EditorControl.Owner);

    Result.TabPosition := tpBottom;
    Result.Align:=alClient;
    LParent := LSourceEditor.EditorControl.Parent;

    LNewTabSheet := TTabSheet.Create(Result);
    LNewTabSheet.PageControl := Result;
    LNewTabSheet.Caption := 'Code';
    LSourceEditor.EditorControl.Parent := LNewTabSheet;  // ! SynEdit :)

    LNewTabSheet := TTabSheet.Create(Result);
    LNewTabSheet.PageControl := Result;
    LNewTabSheet.Caption := 'Designer';

    Result.OnChange := spartaIDE.TabChange;

    Result.Parent := LParent;

    LSourceEditorWindow := TSourceEditorWindowInterface(Result.Owner);
    SourceEditorWindows[LSourceEditorWindow].AddPageCtrl(LSourceEditor, Result)
  end;

begin
  LSourceEditor := Sender as TSourceEditorInterface;
  if not (LSourceEditor.EditorControl.Parent.Parent is TModulePageControl) then
    CreateModulePageControl;
end;

procedure TSpartaMainIDE.TabChange(Sender: TObject);
var
  LActiveSourceWindow: TSourceEditorWindowInterface;
  w: TSourceEditorWindowInterface;
{$IFDEF USE_GENERICS_COLLECTIONS}
  p: TPair<TSourceEditorInterface, TModulePageControl>;
{$ELSE}
  p: THashmap<TSourceEditorInterface, TModulePageControl, THash_TObject>.TPair;
  LIterator: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.TIterator;
  LIterator2: THashmap<TSourceEditorInterface, TModulePageControl, THash_TObject>.TIterator;
{$ENDIF}
  LDesigner: TIDesigner;
  LFormData: TDesignFormData;
  LPageCtrl: TModulePageControl;
  LSourceWndData: TSourceEditorWindowData;
begin
  // activate right source editor window when user is clicking on page.
  // (at clicking time can be active other source window)
  LActiveSourceWindow := TComponent(Sender).Owner as TSourceEditorWindowInterface;
  if LActiveSourceWindow <> SourceEditorManagerIntf.ActiveSourceWindow then
    SourceEditorManagerIntf.ActiveSourceWindow := LActiveSourceWindow;

  LPageCtrl := TModulePageControl(Sender);
  // if in some key there is no module and is visible page other than code page.
  if (LActiveSourceWindow.ActiveEditor <> nil) and (LPageCtrl <> nil) then
  begin
    LDesigner := LActiveSourceWindow.ActiveEditor.GetDesigner(True);
    LFormData := FindDesignFormData(LDesigner);

{$IFDEF USE_GENERICS_COLLECTIONS}
    if (LFormData <> nil) and SourceEditorWindows.TryGetValue(LActiveSourceWindow, LSourceWndData) then
    begin
      case LPageCtrl.ActivePageIndex of
        0:
          begin
            LSourceWndData.ActiveDesignFormData := nil;
          end;
        1:
          begin
            // deaktywuj zakladke design w pozostalych page control :)
            for w in SourceEditorWindows.Keys do
              if w = LActiveSourceWindow then
                Continue
              else
                for p in SourceEditorWindows[w].FPageCtrlList do
                  if (p.Value.DesignFormData = LFormData) and (p.Value <> Sender) then
                  begin
                    IDETabMaster.ShowCode(p.Key);
                  end;

            LSourceWndData.ActiveDesignFormData := LFormData;
            // rozne okna moga miec rozne rozmiary
            LPageCtrl.BoundToDesignTabSheet;
          end;
      end;
    end;
{$ELSE}
    if (LFormData <> nil) and SourceEditorWindows.contains(LActiveSourceWindow) then
    begin
      LSourceWndData := SourceEditorWindows[LActiveSourceWindow];
      case LPageCtrl.ActivePageIndex of
        0:
          begin
            LSourceWndData.ActiveDesignFormData := nil;
          end;
        1:
          begin
            // deaktywuj zakladke design w pozostalych page control :)
            LIterator := SourceEditorWindows.Iterator;
            if LIterator <> nil then
            repeat
              w := LIterator.Key;
              if w = LActiveSourceWindow then
                Continue
              else
              begin
                LIterator2 := SourceEditorWindows[w].FPageCtrlList.Iterator;
                if LIterator2 <> nil then
                repeat
                  p := LIterator2.Data;
                  if (p.Value.DesignFormData = LFormData) and (p.Value <> Sender) then
                  begin
                    IDETabMaster.ShowCode(p.Key);
                  end;
                until not LIterator2.next;
                LIterator2.Free;
              end;
            until not LIterator.next;
            LIterator.Free;

            LSourceWndData.ActiveDesignFormData := LFormData;
            // rozne okna moga miec rozne rozmiary
            LPageCtrl.BoundToDesignTabSheet;
          end;
      end;
    end;
{$ENDIF}
  end;
end;

procedure TSpartaMainIDE.GlobalOnChangeBounds(Sender: TObject);
var
  sewd: TSourceEditorWindowData;
{$IFNDEF USE_GENERICS_COLLECTIONS}
  LIterator: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.TIterator;
{$ENDIF}
begin
{$IFDEF USE_GENERICS_COLLECTIONS}
  for sewd in SourceEditorWindows.Values do
  begin
    sewd.OnChangeBounds(Sender);
  end;
{$ELSE}
  LIterator := SourceEditorWindows.Iterator;
  if LIterator <> nil then
  repeat
    sewd := LIterator.Value;
    sewd.OnChangeBounds(Sender)
  until not LIterator.next;
  LIterator.Free;
{$ENDIF}
end;

procedure TSpartaMainIDE.GlobalSNOnChangeBounds(Sender: TObject);
var
  LWindow: TSourceEditorWindowInterface;
  LWindowData: TSourceEditorWindowData;
  LDesignForm: TDesignFormData;
begin
  // sprawdź rodzica czy siezmienil. jesli tak to znaczy ze oddokowalismy zadokowalismy okno i trzeba wykonac pewne akcje
  LWindow := Sender as TSourceEditorWindowInterface;

  // dock/undock event :)
{$IFDEF USE_GENERICS_COLLECTIONS}
  if not SourceEditorWindows.TryGetValue(LWindow, LWindowData) then
    Exit;
{$ELSE}
  if not SourceEditorWindows.contains(LWindow) then
    Exit;
  LWindowData := SourceEditorWindows[LWindow];
{$ENDIF}
  if LWindowData.FLastTopParent <> LWindow.GetTopParent then
  begin
    LWindowData.FLastTopParent := LWindow.GetTopParent;
    // trzeba zrobic refresh w popupparent
    LDesignForm := LWindowData.ActiveDesignFormData;
    LWindowData.ActiveDesignFormData := nil;
    LWindowData.ActiveDesignFormData := LDesignForm;
    // ten postmessage cos rozwala w dokowaniu, trzeba wydelegowac to do design form ...
    //PostMessage(LWindow.Handle, WM_BoundToDesignTabSheet, 0, 0);
    if LDesignForm <> nil then
    begin
{$IFNDEF USE_POPUP_PARENT_DESIGNER}
      LDesignForm.Form.Form.ParentWindow := FindModulePageControl(LWindow).Resizer.FResizerFrame.pClient.Handle;
{$ENDIF}
      PostMessage(LDesignForm.Form.Form.Handle, WM_BoundToDesignTabSheet, 0, 0);
    end;
  end;

  LWindowData.OnChangeBounds(Sender);
end;

{$IFDEF USE_POPUP_PARENT_DESIGNER}
procedure TDesignerIDEBoss.OnBeforeClose(Sender: TObject);
begin
  isIdeDestroyed := True;
end;
{$ENDIF}

procedure TSpartaMainIDE.OnShowDesignerForm(Sender: TObject; AEditor: TSourceEditorInterface;
                                 AComponentPaletteClassSelected: Boolean);
var
  LForm: TDesignFormData;
  LPageCtrl, p: TModulePageControl;
  w: TSourceEditorWindowInterface;
  e: TSourceEditorInterface;
{$IFNDEF USE_GENERICS_COLLECTIONS}
  LIterator: THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.TIterator;
{$ENDIF}
begin
  LForm := FindDesignFormData(TCustomForm(Sender).Designer);
  if LForm = nil then
    Exit;

  if LForm.FHiding then
    Exit;

  LPageCtrl := FindModulePageControl(SourceEditorManagerIntf.ActiveEditor);

  if LPageCtrl = nil then
    Exit; // nie powinno sie zdarzyc ale kto wie :P to IDE bywa zlosliwe

  // lekarstwo na buga gdy pomimo otwartej zakladki design, klikniemy poza obszarem roboczym formy (np.
  // w zakladke designer, IDE aktywowac usiluje forme co jest bledem. musimy przezucic sie na "Code"
  {case LPageCtrl.PageIndex of
    0: LPageCtrl.PageIndex := 1;
    1: LPageCtrl.PageIndex := 0;
  end;}

  // powyzsze jest jednak falszywe. dodawanie komponentow na forme wszystko papralo i na przemian pokazywal sie kod
  // albo forma. jedyne logiczne rozwiazanie to uzycie OnUserInputHandler i sprawdzanie ActiveEditor i przypisanego
  // mu ModulePage ktory ciagle ustawia stan na Designer dla F12.

  // przy double widoku (forma + clon z kodem) w chwili wybierania componentow nie mozna sie przelaczyc...
  //  przemyslec ... (dodatkowy parametr dla tej metody? Show for components?)

  if AComponentPaletteClassSelected then
  begin
    // sprawdź czy forma juz jest gdzies otwarta w edytorze. jesli nie jesli sie da
    // pokaz dla obecnie aktywnego modulu.
{$IFDEF USE_GENERICS_COLLECTIONS}
    for w in SourceEditorWindows.Keys do
    begin
      e := w.ActiveEditor;
      // zakomentowano, bo wkurwiajace bylo ze dzialalo to dla kazdego editora...
      if (e = nil) or (e.GetDesigner(True) <> LForm.Form.Form.Designer) then  // mozna to wyelminowac tworzac event zwracajacy ze
      // nie mozna pokazac formy... albo propert
        Continue;

      p := FindModulePageControl(e);
      if p.PageIndex = 1 then
        Exit;
    end;
{$ELSE}
    LIterator := SourceEditorWindows.Iterator;
    if LIterator <> nil then
    repeat
      w := LIterator.Key;
      e := w.ActiveEditor;
      // zakomentowano, bo wkurwiajace bylo ze dzialalo to dla kazdego editora...
      if (e = nil) or (e.GetDesigner(True) <> LForm.Form.Form.Designer) then  // mozna to wyelminowac tworzac event zwracajacy ze
      // nie mozna pokazac formy... albo propert
        Continue;

      p := FindModulePageControl(e);
      if p.PageIndex = 1 then
        Exit;
    until not LIterator.next;
    LIterator.Free;
{$ENDIF}
  end;

  IDETabMaster.ShowDesigner(SourceEditorManagerIntf.ActiveEditor);
end;

procedure TSpartaMainIDE.OnShowSrcEditor(Sender: TObject);
begin
  IDETabMaster.ShowCode(Sender as TSourceEditorInterface);
end;

procedure TSpartaMainIDE.OnShowMethod(const Name: String);
var
  LForm: TDesignFormData;
  LSecondEditor: TSourceEditorInterface = nil;
  i: Integer;
  LSourceWindow: TSourceEditorWindowInterface;
begin
  LForm := FindDesignFormData(FormEditingHook.GetCurrentDesigner);
  if LForm = nil then
    Exit;

  for i := 0 to SourceEditorManagerIntf.SourceWindowCount - 1 do
  begin
    LSourceWindow := SourceEditorManagerIntf.SourceWindows[i];
    if LForm.Form.LastActiveSourceWindow = LSourceWindow then
      Continue;

    if LSourceWindow.ActiveEditor <> nil then
      if LSourceWindow.ActiveEditor.GetDesigner(True) = LForm.Form.Form.Designer then
      begin
        LSecondEditor := LSourceWindow.ActiveEditor;
        Break;
      end;
  end;

  if LSecondEditor = nil then
  begin
    if LForm.Form.LastActiveSourceWindow <> nil then
    begin
      IDETabMaster.ShowCode(LForm.Form.LastActiveSourceWindow.ActiveEditor);
    end;
  end
  else
  begin
    IDETabMaster.ShowCode(LSecondEditor);
  end;

  if LSecondEditor <> nil then
  begin
    LazarusIDE.DoShowMethod(LSecondEditor, Name);
  end;
end;

procedure TSpartaMainIDE.OnDesignRefreshPropertyValues;
var
  LForm: TCustomForm;
  LSourceWindow: TSourceEditorWindowInterface;
  LFormData: TDesignFormData;
  LPageCtrl: TModulePageControl;

  function RootIsSelected: Boolean;
  var
    LSelection: TPersistentSelectionList;
    i: integer;
  begin
    Result := False;
    LSelection := TPersistentSelectionList.Create;
    GlobalDesignHook.GetSelection(LSelection);
    for i := 0 to LSelection.Count - 1 do
      if LSelection.Items[i] = GlobalDesignHook.LookupRoot then
      begin
        Result := True;
        Break;
      end;
    LSelection.Free;
  end;

begin
  if (GlobalDesignHook.LookupRoot is TCustomFrame) then
  begin
    if not RootIsSelected then
      Exit;

    LForm := FormEditingHook.GetDesignerForm(GlobalDesignHook.LookupRoot);
    LFormData := FindDesignFormData(LForm);
    LSourceWindow := (LFormData as IDesignedForm).LastActiveSourceWindow;
    LPageCtrl := FindModulePageControl(LSourceWindow);
    TFakeFrame(LForm).SetBounds(LForm.Left-1,LForm.Top-1,TFakeFrame(LForm).Width,TFakeFrame(LForm).Height);
    LPageCtrl.BoundToDesignTabSheet;
  end
  else
  if (GlobalDesignHook.LookupRoot is TCustomForm) then
  begin
    if not RootIsSelected then
      Exit;

    LForm := TCustomForm(GlobalDesignHook.LookupRoot);
    LFormData := FindDesignFormData(LForm);
    LFormData.RepaintFormImages;
  end;
end;

class function TSpartaMainIDE.ComponentPageControl: TPageControl;
begin
  Result := TPageControl(LazarusIDE.OwningComponent.FindComponent('ComponentPageControl'));
end;

procedure TSpartaMainIDE.eFilterChange(Sender: TObject);
begin
  if not MainComponentsPalette.IsEmpty then
    MainComponentsPalette.Filter := TEditButton(Sender).Text;
end;

procedure TSpartaMainIDE.eFilterClear(Sender: TObject);
begin
  TEditButton(Sender).Text:='';
end;

procedure TSpartaMainIDE.mnuHideHideComponentPageControlClicked(
  Sender: TObject);
var
  AMenuHeight, AHeight: Integer;
  LChildSite: TWinControl;
begin
  if TCustomForm(LazarusIDE.GetMainBar).DockManager = nil then
    Exit;

  // LChildSite := TCustomForm(LazarusIDE.GetMainBar).DockManager.GetChildSite; TODO

  if LChildSite = nil then
    Exit;

  AMenuHeight := LCLIntf.GetSystemMetrics(SM_CYMENU);

  // Hide
  if (TCustomForm(LazarusIDE.GetMainBar).Height - LChildSite.Height) >= 85 then
  begin
    if AMenuHeight > 0 then
      AHeight := AMenuHeight + TToolBar(LazarusIDE.OwningComponent.FindComponent('tbStandard')).Height + 4 {splitter width}
    else
      AHeight:=85;
  end
  // show
  else
  begin
      if AMenuHeight > 0 then
        AHeight := AMenuHeight + 85
      else
        AHeight:=85;

    MainComponentsPaletteFilter.SetFocus;
  end;

  LChildSite.Height := TCustomForm(LazarusIDE.GetMainBar).Height - AHeight;
end;

initialization
  dsgForms := Classes.TList.Create;
{$IFDEF USE_GENERICS_COLLECTIONS}
  SourceEditorWindows := TObjectDictionary<TSourceEditorWindowInterface, TSourceEditorWindowData>.Create([doOwnsValues]);
{$ELSE}
  SourceEditorWindows := THashmap<TSourceEditorWindowInterface, TSourceEditorWindowData, THash_TObject>.Create();
{$ENDIF}
  Forms := Classes.TList.Create;
finalization
  Forms.Free;
  SourceEditorWindows.Free;
  FreeAndNil(dsgForms);
end.

