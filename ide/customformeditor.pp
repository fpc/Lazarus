{
 /***************************************************************************
                             CustomFormEditor.pp
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
}
unit CustomFormEditor;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

{ $DEFINE VerboseFormEditor}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  // LCL+FCL
  Classes, SysUtils, TypInfo, Math, LCLIntf, LCLType, LResources,
  AVL_Tree, LCLMemManager, FileUtil,
  LCLProc, Graphics, Controls, Forms, Menus, Dialogs,
  // IDEIntf
  PropEdits, PropEditUtils, ObjectInspector, IDECommands, FormEditingIntf,
  // IDE
  LazarusIDEStrConsts, Project, JITForms, MainIntf,
  CustomNonFormDesigner, NonControlDesigner, FrameDesigner,
  ComponentReg, IDEProcs, ComponentEditors, KeyMapping, EditorOptions,
  EnvironmentOpts, DesignerProcs;

const
  OrdinalTypes = [tkInteger,tkChar,tkEnumeration,tkbool];

const
  LRSStreamChunkSize = 4096; // allocating mem in 4k chunks helps many mem managers

type
  TSelectFrameEvent = procedure(Sender: TObject; var AComponentClass: TComponentClass) of Object;

  { TCustomFormEditor }

  TCustomFormEditor = class(TAbstractFormEditor)
  private
    FOnSelectFrame: TSelectFrameEvent;
    FSelection: TPersistentSelectionList;
    FObj_Inspector: TObjectInspectorDlg;
    FDefineProperties: TAVLTree;// tree of TDefinePropertiesCacheItem
    FStandardDefinePropertiesRegistered: Boolean;
    FDesignerBaseClasses: TFPList; // list of TComponentClass
    FDesignerMediatorClasses: TFPList;// list of TDesignerMediatorClass
    function GetPropertyEditorHook: TPropertyEditorHook;
    function FindDefinePropertyNode(const APersistentClassName: string
                                    ): TAVLTreeNode;
    procedure FrameCompGetCreationClass(Sender: TObject;
      var NewComponentClass: TComponentClass);
  protected
    FNonFormForms: TAVLTree; // tree of TNonControlDesignerForm sorted for LookupRoot
    procedure SetSelection(const ASelection: TPersistentSelectionList);
    procedure OnObjectInspectorModified(Sender: TObject);
    procedure SetObj_Inspector(AnObjectInspector: TObjectInspectorDlg); virtual;
    procedure JITListReaderError(Sender: TObject; Reader: TReader;
          ErrorType: TJITFormError; var Action: TModalResult); virtual;
    procedure JITListBeforeCreate(Sender: TObject; Instance: TPersistent);
    procedure JITListException(Sender: TObject; E: Exception;
                               var Action: TModalResult);
    procedure JITListPropertyNotFound(Sender: TObject; Reader: TReader;
      Instance: TPersistent; var PropName: string; IsPath: boolean;
      var Handled, Skip: Boolean);
    procedure JITListFindAncestors(Sender: TObject; AClass: TClass;
      var Ancestors: TFPList;// list of TComponent
      var BinStreams: TFPList;// list of TExtMemoryStream;
      var Abort: boolean);
    procedure JITListFindClass(Sender: TObject;
                               const ComponentClassName: string;
                               var ComponentClass: TComponentClass);

    function GetDesignerBaseClasses(Index: integer): TComponentClass; override;
    procedure OnDesignerMenuItemClick(Sender: TObject); virtual;
    function FindNonFormFormNode(LookupRoot: TComponent): TAVLTreeNode;
  public
    JITFormList: TJITForms;// designed forms
    JITNonFormList: TJITNonFormComponents;// designed custom components like data modules

    constructor Create;
    destructor Destroy; override;
    procedure RegisterFrame;

    // selection
    function AddSelected(Value: TComponent) : Integer;
    procedure DeleteComponent(AComponent: TComponent; FreeComponent: boolean);
    function FindComponentByName(const Name: ShortString): TComponent; override;
    function SaveSelectionToStream(s: TStream): Boolean; override;
    function InsertFromStream(s: TStream; Parent: TWinControl;
                              Flags: TComponentPasteSelectionFlags): Boolean; override;
    function ClearSelection: Boolean; override;
    function DeleteSelection: Boolean; override;
    function CopySelectionToClipboard: Boolean; override;
    function CutSelectionToClipboard: Boolean; override;
    function PasteSelectionFromClipboard(Flags: TComponentPasteSelectionFlags
                                         ): Boolean; override;
    function GetCurrentObjectInspector: TObjectInspectorDlg; override;

    // JIT components
    function IsJITComponent(AComponent: TComponent): boolean;
    function GetJITListOfType(AncestorType: TComponentClass): TJITComponentList;
    function FindJITList(AComponent: TComponent): TJITComponentList;
    function FindJITListByClassName(const AComponentClassName: string
                                    ): TJITComponentList;
    function FindJITListByClass(AComponentClass: TComponentClass
                                ): TJITComponentList;
    function GetDesignerForm(APersistent: TPersistent): TCustomForm; override;

    function FindNonFormForm(LookupRoot: TComponent): TCustomNonFormDesignerForm;

    function CreateNonFormForm(LookupRoot: TComponent): TCustomNonFormDesignerForm;

    procedure RenameJITComponent(AComponent: TComponent;
                                 const NewClassName: shortstring);
    procedure RenameJITComponentUnitname(AComponent: TComponent;
                                         const NewUnitName: shortstring);
    procedure UpdateDesignerFormName(AComponent: TComponent);
    procedure UpdateComponentName(AComponent: TComponent);
    function CreateNewJITMethod(ALookupRoot: TComponent;
                                const AMethodName: shortstring): TMethod;
    procedure RenameJITMethod(AComponent: TComponent;
                           const OldMethodName, NewMethodName: shortstring);
    procedure SaveHiddenDesignerFormProperties(AComponent: TComponent);
    function FindJITComponentByClassName(const AComponentClassName: string
                                         ): TComponent;
    function FindJITComponentByClass(AComponentClass: TComponentClass
                                     ): TComponent;
    procedure WriteMethodPropertyEvent(Writer: TWriter; Instance: TPersistent;
      PropInfo: PPropInfo; const MethodValue, DefMethodValue: TMethod;
      var Handled: boolean);
    function SaveUnitComponentToBinStream(AnUnitInfo: TUnitInfo;
      var BinCompStream: TExtMemoryStream): TModalResult;
    function OnGetDanglingMethodName(const AMethod: TMethod;
                                     aRootComponent: TObject): string;

    // ancestors
    function GetAncestorLookupRoot(AComponent: TComponent): TComponent; override;
    function GetAncestorInstance(AComponent: TComponent): TComponent; override;
    function RegisterDesignerBaseClass(AClass: TComponentClass): integer; override;
    function DesignerBaseClassCount: Integer; override;
    procedure UnregisterDesignerBaseClass(AClass: TComponentClass); override;
    function IndexOfDesignerBaseClass(AClass: TComponentClass): integer; override;
    function DescendFromDesignerBaseClass(AClass: TComponentClass): integer; override;
    function FindDesignerBaseClassByName(const AClassName: shortstring; WithDefaults: boolean): TComponentClass; override;

    // designers
    function DesignerCount: integer; override;
    function GetDesigner(Index: integer): TIDesigner; override;
    function GetCurrentDesigner: TIDesigner; override;
    function GetDesignerByComponent(AComponent: TComponent): TIDesigner; override;

    // designer mediators
    function GetDesignerMediators(Index: integer): TDesignerMediatorClass; override;
    procedure RegisterDesignerMediator(MediatorClass: TDesignerMediatorClass); override;
    procedure UnregisterDesignerMediator(MediatorClass: TDesignerMediatorClass); override;
    function DesignerMediatorCount: integer; override;
    function GetDesignerMediatorClass(ComponentClass: TComponentClass): TDesignerMediatorClass;

    // component editors
    function GetComponentEditor(AComponent: TComponent): TBaseComponentEditor;
    
    // component creation
    function CreateUniqueComponentName(AComponent: TComponent): string; override;
    function CreateUniqueComponentName(const AClassName: string;
                                       OwnerComponent: TComponent): string; override;
    function GetDefaultComponentParent(TypeClass: TComponentClass
                                       ): TComponent; override;
    function GetDefaultComponentPosition(TypeClass: TComponentClass;
                                         ParentComponent: TComponent;
                                         var X,Y: integer): boolean; override;
    function CreateComponent(ParentComponent: TComponent;
                             TypeClass: TComponentClass;
                             const AUnitName: shortstring;
                             NewLeft,NewTop,NewWidth,NewHeight: Integer;
                             DisableAutoSize: boolean): TComponent; override;
    function CreateComponentFromStream(BinStream: TStream;
                      AncestorType: TComponentClass;
                      const NewUnitName: ShortString;
                      Interactive: boolean;
                      Visible: boolean = true;
                      DisableAutoSize: boolean = false;
                      ContextObj: TObject = nil): TComponent; override;
    function CreateRawComponentFromStream(BinStream: TStream;
                      AncestorType: TComponentClass;
                      const NewUnitName: ShortString;
                      Interactive: boolean;
                      Visible: boolean = true;
                      DisableAutoSize: boolean = false;
                      ContextObj: TObject = nil): TComponent;
    function CreateChildComponentFromStream(BinStream: TStream;
                       ComponentClass: TComponentClass; Root: TComponent;
                       ParentControl: TWinControl): TComponent; override;
    function FixupReferences(AComponent: TComponent): TModalResult;
    procedure WriterFindAncestor(Writer: TWriter; Component: TComponent;
                                 const Name: string;
                                 var Ancestor, RootAncestor: TComponent);
    procedure SetComponentNameAndClass(AComponent: TComponent;
                                       const NewName, NewClassName: shortstring);
    function ClassDependsOnComponent(AClass: TComponentClass;
                                     AComponent: TComponent): Boolean;
    function ComponentDependsOnClass(AComponent: TComponent;
                                     AClass: TComponentClass): Boolean;

    // define properties
    procedure FindDefineProperty(const APersistentClassName,
                                 AncestorClassName, Identifier: string;
                                 var IsDefined: boolean);
    procedure RegisterDefineProperty(const APersistentClassName,
                                     Identifier: string);
    procedure RegisterStandardDefineProperties;

    // keys
    function TranslateKeyToDesignerCommand(Key: word; Shift: TShiftState): word;
  public
    property Selection: TPersistentSelectionList read FSelection
                                                 write SetSelection;
    property Obj_Inspector: TObjectInspectorDlg
                                     read FObj_Inspector write SetObj_Inspector;
    property PropertyEditorHook: TPropertyEditorHook read GetPropertyEditorHook;
    property OnSelectFrame: TSelectFrameEvent read FOnSelectFrame write FOnSelectFrame;
  end;
  
  
  { TDefinePropertiesCacheItem }
  
  TDefinePropertiesCacheItem = class
  public
    PersistentClassname: string;
    RegisteredComponent: TRegisteredComponent;
    DefineProperties: TStrings;
    destructor Destroy; override;
  end;
  
  
  { TDefinePropertiesReader }
  
  TDefinePropertiesReader = class(TFiler)
  private
    FDefinePropertyNames: TStrings;
  protected
    procedure AddPropertyName(const Name: string);
  public
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    property DefinePropertyNames: TStrings read FDefinePropertyNames;
  end;
  
  
  { TDefinePropertiesPersistent
    Wrapper/Friend class, to call the protected method 'DefineProperties' }
  
  TDefinePropertiesPersistent = class(TPersistent)
  private
    FTarget: TPersistent;
  public
    constructor Create(TargetPersistent: TPersistent);
    procedure PublicDefineProperties(Filer: TFiler);
    property Target: TPersistent read FTarget;
  end;
  

const
  StandardDesignerBaseClasses: array[1..3] of TComponentClass = 
  (
    Forms.TForm,
    TDataModule,
    Forms.TFrame
  );
  
  
function CompareDefPropCacheItems(Item1, Item2: TDefinePropertiesCacheItem): integer;
function ComparePersClassNameAndDefPropCacheItem(Key: Pointer;
                                     Item: TDefinePropertiesCacheItem): integer;

function TryFreeComponent(var AComponent: TComponent): boolean;

procedure RegisterStandardClasses;

var
  BaseFormEditor1: TCustomFormEditor = nil;

implementation


function CompareDefPropCacheItems(Item1, Item2: TDefinePropertiesCacheItem
  ): integer;
begin
  Result:=CompareText(Item1.PersistentClassname,Item2.PersistentClassname);
end;

function ComparePersClassNameAndDefPropCacheItem(Key: Pointer;
                                     Item: TDefinePropertiesCacheItem): integer;
begin
  Result:=CompareText(AnsiString(Key),Item.PersistentClassname);
end;

procedure RegisterStandardClasses;
begin
  RegisterClasses([TStringList]);
end;

function TryFreeComponent(var AComponent: TComponent): boolean;
var
  OldName, OldClassName: string;
Begin
  Result := False;
  //debugln(['TryFreeComponent ',DbgSName(AComponent)]);
  {$IFNDEF NoCompCatch}
  try
  {$ENDIF}
    OldName := AComponent.Name;
    OldClassName := AComponent.ClassName;
    AComponent.Free;
    //debugln(['TryFreeComponent ',OldName,':',OldClassName,' success']);
    Result := True;
  {$IFNDEF NoCompCatch}
  except
    on E: Exception do begin
      DebugLn('TryFreeComponent ERROR:',
        ' "'+OldName+':'+OldClassName+'" ',E.Message);
      DumpExceptionBackTrace;
      MessageDlg(lisCCOErrorCaption,
        Format(lisCFEAnExceptionOccuredDuringDeletionOf, [#13, OldName,
          OldClassName, #13, E.Message]),
        mtError,[mbOk],0);
    end;
  end;
  {$ENDIF}
  if not Result then begin
    // maybe some references can be removed
    try
      if AComponent is TControl then begin
        TControl(AComponent).Parent:=nil;
      end;
    except
      on e: Exception do begin
        DebugLn('TryFreeComponent manual clean up failed also for ',
          ' "'+OldName+':'+OldClassName+'". This is likely, nothing to worry about. ',E.Message);
      end;
    end;
  end;
  AComponent := nil;
end;

{ TCustomFormEditor }

constructor TCustomFormEditor.Create;

  procedure InitJITList(List: TJITComponentList);
  begin
    List.OnReaderError:=@JITListReaderError;
    List.OnBeforeCreate:=@JITListBeforeCreate;
    List.OnException:=@JITListException;
    List.OnPropertyNotFound:=@JITListPropertyNotFound;
    List.OnFindAncestors:=@JITListFindAncestors;
    List.OnFindClass:=@JITListFindClass;
  end;

var
  l: Integer;
begin
  inherited Create;
  FNonFormForms := TAVLTree.Create(@CompareNonFormDesignerForms);
  FSelection := TPersistentSelectionList.Create;
  FDesignerBaseClasses:=TFPList.Create;
  FDesignerMediatorClasses:=TFPList.Create;
  for l:=Low(StandardDesignerBaseClasses) to High(StandardDesignerBaseClasses) do
    FDesignerBaseClasses.Add(StandardDesignerBaseClasses[l]);

  JITFormList := TJITForms.Create(nil);
  InitJITList(JITFormList);

  JITNonFormList := TJITNonFormComponents.Create(nil);
  InitJITList(JITNonFormList);

  DesignerMenuItemClick:=@OnDesignerMenuItemClick;
  OnGetDesignerForm:=@GetDesignerForm;
  FormEditingHook:=Self;
end;

destructor TCustomFormEditor.Destroy;
begin
  FormEditingHook:=nil;
  DesignerMenuItemClick:=nil;
  if FDefineProperties<>nil then begin
    FDefineProperties.FreeAndClear;
    FreeAndNil(FDefineProperties);
  end;
  FreeAndNil(JITFormList);
  FreeAndNil(JITNonFormList);
  FreeAndNil(FDesignerMediatorClasses);
  FreeAndNil(FDesignerBaseClasses);
  FreeAndNil(FSelection);
  FreeAndNil(FNonFormForms);
  inherited Destroy;
end;

procedure TCustomFormEditor.RegisterFrame;
var
  FrameComp: TRegisteredComponent;
begin
  FrameComp:=IDEComponentPalette.FindComponent('TFrame');
  if FrameComp <> nil then
    FrameComp.OnGetCreationClass:=@FrameCompGetCreationClass;
end;

procedure TCustomFormEditor.SetSelection(
  const ASelection: TPersistentSelectionList);
begin
  FSelection.Assign(ASelection);
  if Obj_Inspector=nil then exit;
  if FSelection.Count>0 then begin
      Obj_Inspector.PropertyEditorHook.LookupRoot:=
        GetLookupRootForComponent(FSelection[0]);
  end;
  Obj_Inspector.Selection := FSelection;
end;

Function TCustomFormEditor.AddSelected(Value : TComponent) : Integer;
Begin
  Result := FSelection.Add(Value) + 1;
  if Obj_Inspector<>nil then
    Obj_Inspector.Selection := FSelection;
end;

Procedure TCustomFormEditor.DeleteComponent(AComponent: TComponent;
  FreeComponent: boolean);
var
  AForm: TCustomForm;
  AWinControl: TWinControl;
  IsJIT: Boolean;
Begin
  {$IFDEF IDE_DEBUG}
  DebugLn(['TCustomFormEditor.DeleteComponent ',DbgSName(AComponent),' IsJITComponent=',IsJITComponent(AComponent),' FreeComponent=',FreeComponent]);
  {$ENDIF}
  if PropertyEditorHook.LookupRoot=AComponent then
    PropertyEditorHook.LookupRoot:=nil;
  IsJIT:=IsJITComponent(AComponent);
  if IsJIT then begin
    // value is a top level component
    if JITFormList.IsJITForm(AComponent) then begin
      // free/unbind a form component
      if FreeComponent then
        JITFormList.DestroyJITComponent(AComponent);
    end else if JITNonFormList.IsJITNonForm(AComponent) then begin
      // free/unbind a non form component and its designer form
      AForm:=GetDesignerForm(AComponent);
      if (AForm<>nil) and (not (AForm is TCustomNonFormDesignerForm)) then
        RaiseException(Format(
          lisCFETCustomFormEditorDeleteComponentWhereIsTheTCustomN, [AComponent.
          ClassName]));

      if (AForm <> nil) and (AForm is TCustomNonFormDesignerForm) then
      begin
        FNonFormForms.Remove(AForm);
        TCustomNonFormDesignerForm(AForm).LookupRoot := nil;
        Application.ReleaseComponent(AForm);
      end;

      if FreeComponent then
        JITNonFormList.DestroyJITComponent(AComponent);
    end else
      RaiseException('TCustomFormEditor.DeleteComponent '+AComponent.ClassName);
  end else if FreeComponent then begin
    if (AComponent.Owner=nil) then
      DebugLn(['WARNING: TCustomFormEditor.DeleteComponent freeing orphaned component ',DbgSName(AComponent)]);
    TryFreeComponent(AComponent);
  end;
  // if not free, then hide it
  if (not FreeComponent) and (AComponent is TWinControl) then begin
    AWinControl:=TWinControl(AComponent);
    if AWinControl.HandleAllocated and (AWinControl.Parent=nil) then begin
      AWinControl.ControlStyle:=AWinControl.ControlStyle+[csNoDesignVisible];
      LCLIntf.ShowWindow(AWinControl.Handle,SW_HIDE);
      DebugLn(['TCustomFormEditor.DeleteComponent Hiding: ',dbgsName(AWinControl)]);
    end;
  end;
end;

Function TCustomFormEditor.FindComponentByName(const Name: ShortString
  ): TComponent;
var
  i: longint;
Begin
  if JITFormList<>nil then begin
    i:=JITFormList.FindComponentByName(Name);
    if i>=0 then begin
      Result:=JITFormList[i];
      exit;
    end;
  end;
  if JITNonFormList<>nil then begin
    i:=JITNonFormList.FindComponentByName(Name);
    if i>=0 then begin
      Result:=JITNonFormList[i];
      exit;
    end;
  end;
  Result:=nil;
end;

function TCustomFormEditor.SaveSelectionToStream(s: TStream): boolean;
var
  ADesigner: TIDesigner;
begin
  ADesigner:=GetCurrentDesigner;
  if ADesigner is TComponentEditorDesigner then
    Result:=TComponentEditorDesigner(ADesigner).CopySelectionToStream(s)
  else
    Result:=false;
end;

function TCustomFormEditor.InsertFromStream(s: TStream; Parent: TWinControl;
  Flags: TComponentPasteSelectionFlags): Boolean;
var
  ADesigner: TIDesigner;
begin
  ADesigner:=GetCurrentDesigner;
  if ADesigner is TComponentEditorDesigner then
    Result:=TComponentEditorDesigner(ADesigner).InsertFromStream(s,Parent,Flags)
  else
    Result:=false;
end;

function TCustomFormEditor.ClearSelection: Boolean;
var
  ASelection: TPersistentSelectionList;
begin
  if Selection.Count=0 then exit;
  ASelection:=TPersistentSelectionList.Create;
  try
    Selection:=ASelection;
  except
    on E: Exception do begin
      MessageDlg(lisCCOErrorCaption,
        Format(lisCFEUnableToClearTheFormEditingSelection, [#13, E.Message]),
          mtError, [mbCancel], 0);
    end;
  end;
  ASelection.Free;
  Result:=(Selection=nil) or (Selection.Count=0);
end;

function TCustomFormEditor.DeleteSelection: Boolean;
var
  ADesigner: TIDesigner;
begin
  if (Selection.Count=0) then begin
    Result:=true;
    exit;
  end;
  if Selection[0] is TComponent then begin
    ADesigner:=FindRootDesigner(TComponent(Selection[0]));
    if ADesigner is TComponentEditorDesigner then begin
      TComponentEditorDesigner(ADesigner).DeleteSelection;
    end;
  end;
  Result:=Selection.Count=0;
  if Selection.Count>0 then begin
    MessageDlg(lisCCOErrorCaption,
      lisCFEDoNotKnowHowToDeleteThisFormEditingSelection,
      mtError,[mbCancel],0);
  end;
end;

function TCustomFormEditor.CopySelectionToClipboard: Boolean;
var
  ADesigner: TIDesigner;
begin
  if (Selection.Count=0) then begin
    Result:=false;
    exit;
  end;
  if Selection[0] is TComponent then begin
    ADesigner:=FindRootDesigner(TComponent(Selection[0]));
    if ADesigner is TComponentEditorDesigner then begin
      TComponentEditorDesigner(ADesigner).CopySelection;
    end;
  end;
  Result:=Selection.Count=0;
  if Selection.Count>0 then begin
    MessageDlg(lisCCOErrorCaption,
      lisCFEDoNotKnowHowToCopyThisFormEditingSelection,
      mtError,[mbCancel],0);
  end;
end;

function TCustomFormEditor.CutSelectionToClipboard: Boolean;
var
  ADesigner: TIDesigner;
begin
  if (Selection.Count=0) then begin
    Result:=false;
    exit;
  end;
  if Selection[0] is TComponent then begin
    ADesigner:=FindRootDesigner(TComponent(Selection[0]));
    if ADesigner is TComponentEditorDesigner then begin
      TComponentEditorDesigner(ADesigner).CutSelection;
    end;
  end;
  Result:=Selection.Count=0;
  if Selection.Count>0 then begin
    MessageDlg(lisCCOErrorCaption,
      lisCFEDoNotKnowHowToCutThisFormEditingSelection,
      mtError,[mbCancel],0);
  end;
end;

function TCustomFormEditor.PasteSelectionFromClipboard(
  Flags: TComponentPasteSelectionFlags): Boolean;
var
  ADesigner: TIDesigner;
begin
  ADesigner:=GetCurrentDesigner;
  if ADesigner is TComponentEditorDesigner then begin
    Result:=TComponentEditorDesigner(ADesigner).PasteSelection(Flags);
  end else
    Result:=false;
end;

function TCustomFormEditor.GetCurrentObjectInspector: TObjectInspectorDlg;
begin
  Result:=FObj_Inspector;
end;

function TCustomFormEditor.IsJITComponent(AComponent: TComponent): boolean;
begin
  Result:=JITFormList.IsJITForm(AComponent)
          or JITNonFormList.IsJITNonForm(AComponent);
end;

function TCustomFormEditor.GetJITListOfType(AncestorType: TComponentClass): TJITComponentList;
begin
  if AncestorType.InheritsFrom(TCustomForm) then
    Result := JITFormList
  else 
  if AncestorType.InheritsFrom(TComponent) then
    Result := JITNonFormList
  else
    Result := nil;
end;

function TCustomFormEditor.FindJITList(AComponent: TComponent): TJITComponentList;
begin
  if JITFormList.IndexOf(AComponent) >= 0 then
    Result := JITFormList
  else 
  if JITNonFormList.IndexOf(AComponent) >= 0 then
    Result := JITNonFormList
  else
    Result := nil;
end;

function TCustomFormEditor.FindJITListByClassName(const AComponentClassName: string): TJITComponentList;
begin
  if JITFormList.FindComponentByClassName(AComponentClassName) >= 0 then
    Result := JITFormList
  else 
  if JITNonFormList.FindComponentByClassName(AComponentClassName) >= 0 then
    Result := JITNonFormList
  else
    Result := nil;
end;

function TCustomFormEditor.FindJITListByClass(AComponentClass: TComponentClass): TJITComponentList;
begin
  if JITFormList.FindComponentByClass(AComponentClass) >= 0 then
    Result := JITFormList
  else 
  if JITNonFormList.FindComponentByClass(AComponentClass) >= 0 then
    Result := JITNonFormList
  else
    Result := nil;
end;

function TCustomFormEditor.GetDesignerForm(APersistent: TPersistent): TCustomForm;
var
  TheOwner: TPersistent;
begin
  Result:=nil;
  TheOwner := GetLookupRootForComponent(APersistent);
  if TheOwner = nil then
    exit;
  if TheOwner is TCustomForm then
    Result := TCustomForm(TheOwner)
  else if TheOwner is TComponent then
    Result := FindNonFormForm(TComponent(TheOwner))
  else
    exit;
end;

function TCustomFormEditor.FindNonFormForm(LookupRoot: TComponent): TCustomNonFormDesignerForm;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode := FindNonFormFormNode(LookupRoot);
  if AVLNode <> nil then
    Result := TCustomNonFormDesignerForm(AVLNode.Data)
  else
    Result := nil;
end;

function TCustomFormEditor.CreateNonFormForm(LookupRoot: TComponent): TCustomNonFormDesignerForm;
var
  MediatorClass: TDesignerMediatorClass;
begin
  if FindNonFormFormNode(LookupRoot) <> nil then
    RaiseException(lisCFETCustomFormEditorCreateNonFormFormAlreadyExists);
  if LookupRoot is TComponent then
  begin
    if LookupRoot is TCustomFrame then
      Result := TFrameDesignerForm.Create(nil)
    else
      Result := TNonControlDesignerForm.Create(nil);
    Result.Name:='_Designer_'+LookupRoot.Name;
    Result.LookupRoot := LookupRoot;
    FNonFormForms.Add(Result);

    if Result is TNonControlDesignerForm then begin
      // create the mediator
      MediatorClass:=GetDesignerMediatorClass(TComponentClass(LookupRoot.ClassType));
      if MediatorClass<>nil then
        TNonControlDesignerForm(Result).Mediator:=MediatorClass.CreateMediator(nil,LookupRoot);
    end;
  end else
    RaiseException(Format(lisCFETCustomFormEditorCreateNonFormFormUnknownType, [
      LookupRoot.ClassName]));
end;

procedure TCustomFormEditor.RenameJITComponent(AComponent: TComponent;
  const NewClassName: shortstring);
var
  JITComponentList: TJITComponentList;
begin
  JITComponentList:=FindJITList(AComponent);
  if JITComponentList=nil then
    RaiseException('TCustomFormEditor.RenameJITComponent');
  JITComponentList.RenameComponentClass(AComponent,NewClassName);
end;

procedure TCustomFormEditor.RenameJITComponentUnitname(AComponent: TComponent;
  const NewUnitName: shortstring);
var
  JITComponentList: TJITComponentList;
begin
  JITComponentList:=FindJITList(AComponent);
  if JITComponentList=nil then
    RaiseException('TCustomFormEditor.RenameJITComponent');
  JITComponentList.RenameComponentUnitname(AComponent,NewUnitName);
end;

procedure TCustomFormEditor.UpdateDesignerFormName(AComponent: TComponent);
var
  ANonFormForm: TCustomNonFormDesignerForm;
begin
  ANonFormForm := FindNonFormForm(AComponent);
  //DebugLn(['TCustomFormEditor.UpdateDesignerFormName ',ANonFormForm<>nil, ' ',AComponent.Name]);
  if ANonFormForm <> nil then
    ANonFormForm.Caption := AComponent.Name;
end;

procedure TCustomFormEditor.UpdateComponentName(AComponent: TComponent);
var
  DesignerForm: TCustomForm;
begin
  if AComponent.Owner = nil then
    UpdateDesignerFormName(AComponent)
  else
  begin
    DesignerForm := GetDesignerForm(AComponent);
    if (DesignerForm <> nil) and (DesignerForm.Designer <> nil) and
       EnvironmentOptions.ShowComponentCaptions then
      DesignerForm.Invalidate;
  end;
end;

function TCustomFormEditor.CreateNewJITMethod(ALookupRoot: TComponent;
  const AMethodName: shortstring): TMethod;
var
  JITComponentList: TJITComponentList;
begin
  JITComponentList:=FindJITList(ALookupRoot);
  if JITComponentList=nil then
    RaiseException('TCustomFormEditor.CreateNewJITMethod');
  Result:=JITComponentList.CreateNewMethod(ALookupRoot,AMethodName);
end;

procedure TCustomFormEditor.RenameJITMethod(AComponent: TComponent;
  const OldMethodName, NewMethodName: shortstring);
var
  JITComponentList: TJITComponentList;
begin
  JITComponentList:=FindJITList(AComponent);
  if JITComponentList=nil then
    RaiseException('TCustomFormEditor.RenameJITMethod');
  JITComponentList.RenameMethod(AComponent,OldMethodName,NewMethodName);
end;

procedure TCustomFormEditor.SaveHiddenDesignerFormProperties(AComponent: TComponent);
var
  NonFormForm: TCustomNonFormDesignerForm;
begin
  NonFormForm := FindNonFormForm(AComponent);
  if NonFormForm <> nil then
    NonFormForm.DoSaveBounds;
end;

function TCustomFormEditor.FindJITComponentByClassName(
  const AComponentClassName: string): TComponent;
var
  i: LongInt;
begin
  Result := nil;
  i := JITFormList.FindComponentByClassName(AComponentClassName);
  if i >= 0 then 
  begin
    Result := JITFormList[i];
    exit;
  end;
  i := JITNonFormList.FindComponentByClassName(AComponentClassName);
  if i >= 0 then 
  begin
    Result := JITNonFormList[i];
    exit;
  end;
end;

function TCustomFormEditor.FindJITComponentByClass(
  AComponentClass: TComponentClass): TComponent;
var
  i: LongInt;
begin
  Result := nil;
  i := JITFormList.FindComponentByClass(AComponentClass);
  if i >= 0 then 
  begin
    Result := JITFormList[i];
    exit;
  end;
  i := JITNonFormList.FindComponentByClass(AComponentClass);
  if i >= 0 then 
  begin
    Result := JITNonFormList[i];
    exit;
  end;
end;

procedure TCustomFormEditor.WriteMethodPropertyEvent(Writer: TWriter;
  Instance: TPersistent; PropInfo: PPropInfo;
  const MethodValue, DefMethodValue: TMethod; var Handled: boolean);
var
  CurName: String;
begin
  Handled:=true;

  //DebugLn(['TCustomFormEditor.WriteMethodPropertyEvent ',GlobalDesignHook.GetMethodName(MethodValue,nil)]);

  // find ancestor method value
  if (DefMethodValue.Data=MethodValue.Data)
  and (DefMethodValue.Code=MethodValue.Code) then
    exit;
  if IsJITMethod(MethodValue) then
    CurName:=TJITMethod(MethodValue.Data).TheMethodName
  else if MethodValue.Code<>nil then begin
    CurName:=Writer.LookupRoot.MethodName(MethodValue.Code);
    if CurName='' then begin
      // this event was not set by the IDE
      // for Delphi compatibility, do not write this property
      // see bug 13846
      exit;
    end;
  end else
    CurName:='';
  Writer.Driver.BeginProperty(Writer.PropertyPath + PPropInfo(PropInfo)^.Name);
  Writer.Driver.WriteMethodName(CurName);
  Writer.Driver.EndProperty;
end;

function TCustomFormEditor.SaveUnitComponentToBinStream(AnUnitInfo: TUnitInfo;
  var BinCompStream: TExtMemoryStream): TModalResult;
var
  Writer: TWriter;
  DestroyDriver: Boolean;
  AncestorUnit: TUnitInfo;
  Ancestor: TComponent;
  {$IFDEF VerboseSaveUnitComponent}
  memStream: TMemoryStream;
  s: string;
  {$ENDIF}
begin
  // save designer form properties to the component
  SaveHiddenDesignerFormProperties(AnUnitInfo.Component);

  // stream component to binary stream
  if BinCompStream=nil then
    BinCompStream:=TExtMemoryStream.Create;
  if AnUnitInfo.ComponentLastBinStreamSize>0 then
    BinCompStream.Capacity:=Max(BinCompStream.Capacity,BinCompStream.Position+
                      AnUnitInfo.ComponentLastBinStreamSize+LRSStreamChunkSize);
  Writer:=nil;
  DestroyDriver:=false;
  try
    Result:=mrOk;
    try
      BinCompStream.Position:=0;
      Writer:=CreateLRSWriter(BinCompStream,DestroyDriver);
      Writer.OnWriteMethodProperty:=@WriteMethodPropertyEvent;
      Writer.OnFindAncestor:=@WriterFindAncestor;
      AncestorUnit:=AnUnitInfo.FindAncestorUnit;
      Ancestor:=nil;
      if AncestorUnit<>nil then
        Ancestor:=AncestorUnit.Component;
      Writer.WriteDescendent(AnUnitInfo.Component,Ancestor);
      if DestroyDriver then Writer.Driver.Free;
      FreeAndNil(Writer);
      AnUnitInfo.ComponentLastBinStreamSize:=BinCompStream.Size;

      {$IFDEF VerboseSaveUnitComponent}
      BinCompStream.Position:=0;
      memStream:=TMemoryStream.Create;
      LRSObjectBinaryToText(BinCompStream,memStream);
      memStream.Position:=0;
      SetLength(s,memStream.Size);
      memStream.Read(s[1],length(s));
      DebugLn(['TCustomFormEditor.SaveUnitComponentToBinStream START ==================']);
      debugln(s);
      DebugLn(['TCustomFormEditor.SaveUnitComponentToBinStream END ==================']);
      memStream.Free;
      {$ENDIF}
    except
      on E: Exception do begin
        DebugLn(['TCustomFormEditor.SaveUnitComponentToBinStream ',E.Message]);
        DumpExceptionBackTrace;
        Result:=MessageDlg(lisStreamingError,
            Format(lisUnableToStreamT, [AnUnitInfo.ComponentName,
                              AnUnitInfo.ComponentName])+#13
                              +E.Message,
            mtError,[mbAbort, mbRetry, mbIgnore], 0);
        if Result=mrAbort then exit;
        if Result=mrIgnore then Result:=mrOk;
      end;
    end;
  finally
    try
      if DestroyDriver and (Writer<>nil) then Writer.Driver.Free;
      Writer.Free;
    except
      on E: Exception do begin
        debugln('TCustomFormEditor.SaveUnitComponentToBinStream Error cleaning up: ',E.Message);
      end;
    end;
  end;
end;

function TCustomFormEditor.OnGetDanglingMethodName(const AMethod: TMethod;
  aRootComponent: TObject): string;
// check if event is a JITMethod of aRootComponent
var
  JITMethod: TJITMethod;
begin
  Result:='';
  if IsJITMethod(aMethod) then begin
    JITMethod:=TJITMethod(aMethod.Data);
    if aRootComponent.ClassType=JITMethod.TheClass then
      Result:=JITMethod.TheMethodName;
  end;
end;

function TCustomFormEditor.DesignerCount: integer;
begin
  Result:=JITFormList.Count+JITNonFormList.Count;
end;

function TCustomFormEditor.GetDesigner(Index: integer): TIDesigner;
var
  AForm: TCustomForm;
begin
  if Index < JITFormList.Count then
    Result := JITFormList[Index].Designer
  else 
  begin
    AForm := GetDesignerForm(JITNonFormList[Index-JITFormList.Count]);
    Result := TIDesigner(AForm.Designer);
  end;
end;

function TCustomFormEditor.GetCurrentDesigner: TIDesigner;
begin
  Result:=nil;
  if (Selection<>nil) and (Selection.Count>0) and (Selection[0] is TComponent)
  then
    Result:=GetDesignerByComponent(TComponent(Selection[0]));
end;

function TCustomFormEditor.GetDesignerByComponent(AComponent: TComponent
  ): TIDesigner;
var
  AForm: TCustomForm;
begin
  AForm:=GetDesignerForm(AComponent);
  if AForm=nil then
    Result:=nil
  else
    Result:=AForm.Designer;
end;

function TCustomFormEditor.GetDesignerMediators(Index: integer
  ): TDesignerMediatorClass;
begin
  Result:=TDesignerMediatorClass(FDesignerMediatorClasses[Index]);
end;

procedure TCustomFormEditor.RegisterDesignerMediator(
  MediatorClass: TDesignerMediatorClass);
begin
  if FDesignerMediatorClasses.IndexOf(MediatorClass)>=0 then
    raise Exception.Create(Format(
      lisCFETCustomFormEditorRegisterDesignerMediatorAlreadyRe, [DbgSName(
      MediatorClass)]));
  FDesignerMediatorClasses.Add(MediatorClass);
  RegisterDesignerBaseClass(MediatorClass.FormClass);
end;

procedure TCustomFormEditor.UnregisterDesignerMediator(
  MediatorClass: TDesignerMediatorClass);
begin
  UnregisterDesignerBaseClass(MediatorClass.FormClass);
  FDesignerMediatorClasses.Remove(MediatorClass);
end;

function TCustomFormEditor.DesignerMediatorCount: integer;
begin
  Result:=FDesignerMediatorClasses.Count;
end;

function TCustomFormEditor.GetDesignerMediatorClass(
  ComponentClass: TComponentClass): TDesignerMediatorClass;
var
  i: Integer;
  Candidate: TDesignerMediatorClass;
begin
  Result:=nil;
  for i:=0 to DesignerMediatorCount-1 do begin
    Candidate:=DesignerMediators[i];
    if not (ComponentClass.InheritsFrom(Candidate.FormClass)) then continue;
    if (Result<>nil) and Result.InheritsFrom(Candidate.FormClass) then continue;
    Result:=Candidate;
  end;
end;

function TCustomFormEditor.GetComponentEditor(AComponent: TComponent
  ): TBaseComponentEditor;
var
  ADesigner: TIDesigner;
begin
  Result:=nil;
  if AComponent=nil then exit;
  ADesigner:=GetDesignerByComponent(AComponent);
  if ADesigner is TComponentEditorDesigner then
    Result:=ComponentEditors.GetComponentEditor(AComponent,
                                           TComponentEditorDesigner(ADesigner));
end;

function TCustomFormEditor.CreateComponent(ParentComponent: TComponent;
  TypeClass: TComponentClass; const AUnitName: shortstring;
  NewLeft, NewTop, NewWidth, NewHeight: Integer;
  DisableAutoSize: boolean): TComponent;
const
  PreferredDistanceMin = 30;
  PreferredDistanceMax = 250;
var
  NewJITIndex: Integer;
  CompLeft, CompTop, CompWidth, CompHeight: integer;
  NewComponent: TComponent;
  OwnerComponent: TComponent;
  JITList: TJITComponentList;
  AControl: TControl;
  AParent: TWinControl;
  NewComponentName: String;
  DesignForm: TCustomForm;
  NewUnitName: String;
  s: String;
  MonitorBounds: TRect;
  Mediator: TDesignerMediator;
  FreeMediator: Boolean;
  MediatorClass: TDesignerMediatorClass;

  function ActiveMonitor: TMonitor;
  begin
    if Screen.ActiveCustomForm <> nil then
      Result := Screen.ActiveCustomForm.Monitor
    else
    if Application.MainForm <> nil then
      Result := Application.MainForm.Monitor
    else
      Result := Screen.PrimaryMonitor;
  end;

begin
  Result:=nil;
  AParent:=nil;
  NewComponent:=nil;
  Mediator:=nil;
  FreeMediator:=false;
  try
    //DebugLn(['[TCustomFormEditor.CreateComponent] Class="'+TypeClass.ClassName+'" ',NewLeft,',',NewTop,',',NewWidth,'x',NewHeight]);
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent A');{$ENDIF}

    OwnerComponent:=nil;
    if Assigned(ParentComponent) then
    begin
      // add as child component
      Mediator:=GetDesignerMediatorByComponent(ParentComponent);
      OwnerComponent := ParentComponent;
      if OwnerComponent.Owner <> nil then
        OwnerComponent := OwnerComponent.Owner;
      try
        NewComponent := TComponent(TypeClass.newinstance);
        if DisableAutoSize and (NewComponent is TControl) then
          TControl(NewComponent).DisableAutoSizing;
        SetComponentDesignMode(NewComponent,true);
        if DescendFromDesignerBaseClass(TypeClass)>=0 then begin
          // this class can have its own lfm streams (e.g. a TFrame)
          //  => set csInline
          DebugLn(['TCustomFormEditor.CreateComponent Inline ',DbgSName(TypeClass)]);
          SetComponentInlineMode(NewComponent,true);
        end;
        NewComponent.Create(OwnerComponent);
      except
        on e: Exception do begin
          DumpExceptionBackTrace;
          MessageDlg(lisCFEErrorCreatingComponent,
            Format(lisCFEErrorCreatingComponent2, [TypeClass.ClassName, #13, E.
              Message]),
            mtError,[mbCancel],0);
          exit;
        end;
      end;
      // check if Owner was properly set
      if NewComponent.Owner <> OwnerComponent then begin
        MessageDlg(lisCFEInvalidComponentOwner,
          Format(lisCFETheComponentOfTypeFailedToSetItsOwnerTo, [NewComponent.
            ClassName, OwnerComponent.Name, OwnerComponent.ClassName]),
          mtError,[mbCancel],0);
        exit;
      end;
      
      // read inline streams
      if csInline in NewComponent.ComponentState then begin
        JITList:=FindJITList(OwnerComponent);
        if JITList=nil then
          RaiseException('TCustomFormEditor.CreateComponent '+TypeClass.ClassName);
        JITList.ReadInlineJITChildComponent(NewComponent);
      end;

      // calc parent
      AParent:=nil;
      if ParentComponent is TControl then begin
        if (ParentComponent is TWinControl) then
          AParent:=TWinControl(ParentComponent)
        else
          AParent:=TControl(ParentComponent).Parent;
        while (AParent<>nil) do begin
          if (AParent is TWinControl)
          and (csAcceptsControls in AParent.ControlStyle) then
            break;
          AParent:=AParent.Parent;
        end;
      end;
      DebugLn('Parent is '''+dbgsName(AParent)+'''');
    end else begin
      // create a toplevel component
      // -> a form or a datamodule or a custom component
      if AUnitName='' then
        NewUnitName:=DefaultJITUnitName
      else
        NewUnitName:=AUnitName;
      JITList:=GetJITListOfType(TypeClass);
      if JITList=nil then
        RaiseException('TCustomFormEditor.CreateComponent '+TypeClass.ClassName);
      NewJITIndex := JITList.AddNewJITComponent(NewUnitName,TypeClass,DisableAutoSize);
      if NewJITIndex < 0 then
        exit;
      // create component interface
      NewComponent:=JITList[NewJITIndex];
    end;
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent D ');{$ENDIF}
    try
      NewComponentName := CreateUniqueComponentName(NewComponent);
      NewComponent.Name := NewComponentName;
    except
      on e: Exception do begin
        MessageDlg(lisErrorNamingComponent,
          Format(lisErrorSettingTheNameOfAComponentTo, [dbgsName(NewComponent),
            NewComponentName]),
          mtError,[mbCancel],0);
        exit;
      end;
    end;

    try
      // set bounds
      CompLeft:=NewLeft;
      CompTop:=NewTop;
      CompWidth:=NewWidth;
      CompHeight:=NewHeight;
      if NewComponent is TControl then
      begin
        AControl := TControl(NewComponent);
        // calc bounds
        if CompWidth <= 0 then CompWidth := Max(5, AControl.Width);
        if CompHeight <= 0 then CompHeight := Max(5, AControl.Height);
        MonitorBounds := ActiveMonitor.BoundsRect;
        if (CompLeft < 0) and (AParent <> nil) then
          CompLeft := (AParent.Width - CompWidth) div 2
        else
        if (AControl is TCustomForm) and (CompLeft < MonitorBounds.Left + PreferredDistanceMin) then
          with MonitorBounds do
            CompLeft := Max(Left + PreferredDistanceMin, Min(Left + PreferredDistanceMax, Right - CompWidth - PreferredDistanceMin))
        else
        if CompLeft < 0 then
          CompLeft := 0;
        if (CompTop < 0) and (AParent <> nil) then
          CompTop := (AParent.Height - CompHeight) div 2
        else
        if (AControl is TCustomForm) and (CompTop < MonitorBounds.Top + PreferredDistanceMin) then
          with MonitorBounds do
            CompTop := Max(Top + PreferredDistanceMin, Min(Top + PreferredDistanceMax, Bottom - CompWidth - PreferredDistanceMin))
        else
        if CompTop < 0 then
          CompTop := 0;

        if (AParent <> nil) or (AControl is TCustomForm) then
        begin
          // set parent after placing control to prevent display at (0,0)
          AControl.SetBounds(CompLeft,CompTop,CompWidth,CompHeight);
          AControl.Parent := AParent;
        end else
        begin
          // no parent and not a form
          AControl.SetBounds(0,0,CompWidth,CompHeight);
          AControl.DesignInfo := LeftTopToDesignInfo(CompLeft, CompTop);
          //DebugLn(['TCustomFormEditor.CreateComponent ',dbgsName(AControl),' ',LongRec(AControl.DesignInfo).Lo,',',LongRec(AControl.DesignInfo).Hi]);
        end;
      end
      else
      if (NewComponent is TDataModule) then
      begin
        // data module
        with TDataModule(NewComponent) do
        begin
          if CompWidth <= 0 then CompWidth := Max(50, DesignSize.X);
          if CompHeight <= 0 then CompHeight := Max(50, DesignSize.Y);
          MonitorBounds := ActiveMonitor.BoundsRect;
          if CompLeft < MonitorBounds.Left + PreferredDistanceMin then
            with MonitorBounds do
              CompLeft := Max(Left + PreferredDistanceMin, Min(Left + PreferredDistanceMax, Right - CompWidth - PreferredDistanceMin));
          if CompTop < MonitorBounds.Top + PreferredDistanceMin then
            with MonitorBounds do
              CompTop := Max(Top + PreferredDistanceMin, Min(Top + PreferredDistanceMax, Bottom - CompWidth - PreferredDistanceMin));
          DesignOffset := Point(CompLeft, CompTop);
          DesignSize := Point(CompWidth, CompHeight);
          //debugln('TCustomFormEditor.CreateComponent TDataModule Bounds ',dbgsName(NewComponent),' ',dbgs(DesignOffset.X),',',dbgs(DesignOffset.Y),' ',DbgS(NewComponent),8),' ',DbgS(Cardinal(@DesignOffset));
        end;
      end
      else begin
        // non TControl
        if CompWidth <= 0 then CompWidth := 50;
        if CompHeight <= 0 then CompHeight := 50;

        CompLeft := Max(Low(SmallInt), Min(High(SmallInt), CompLeft));
        CompTop := Max(Low(SmallInt), Min(High(SmallInt), CompTop));

        SetComponentLeftTopOrDesignInfo(NewComponent,CompLeft,CompTop);
        if ParentComponent <> nil then
        begin
          DesignForm := GetDesignerForm(ParentComponent);
          if DesignForm <> nil then DesignForm.Invalidate;
        end;
        if Mediator=nil then begin
          MediatorClass:=GetDesignerMediatorClass(TComponentClass(NewComponent.ClassType));
          if MediatorClass<>nil then begin
            Mediator:=MediatorClass.CreateMediator(nil,NewComponent);
            FreeMediator:=Mediator<>nil;
          end;
        end;
        //DebugLn(['TCustomFormEditor.CreateComponent ',DbgSName(NewComponent),' ',dbgs(Bounds(CompLeft,CompTop,CompWidth,CompHeight)),' ',Mediator<>nil]);
        if Mediator<>nil then begin
          Mediator.InitComponent(NewComponent,ParentComponent,
            Bounds(CompLeft,CompTop,CompWidth,CompHeight));
        end;

      end;
    except
      on e: Exception do begin
        DebugLn(e.Message);
        DumpExceptionBackTrace;
        MessageDlg(lisErrorMovingComponent,
          Format(lisErrorMovingComponent2, [NewComponent.Name,
            NewComponent.ClassName]),
          mtError,[mbCancel],0);
        exit;
      end;
    end;

    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent F ');{$ENDIF}
    //DebugLn(['TCustomFormEditor.CreateComponent ',dbgsName(NewComponent),' ',FindComponent(NewComponent)<>nil]);

    Result := NewComponent;
  finally
    // clean up carefully
    if FreeMediator and (Mediator<>nil) then begin
      try
        FreeAndNil(Mediator);
      except
        on E: Exception do begin
          s:=Format(lisCFEErrorDestroyingMediatorOfUnit, [Mediator.ClassName,
            AUnitName, #13, E.Message]);
          DebugLn(['TCustomFormEditor.CreateComponent ',UTF8ToConsole(s)]);
          DumpExceptionBackTrace;
          MessageDlg(lisCFEErrorDestroyingMediator, s, mtError, [mbCancel], 0);
        end;
      end;
    end;
    if Result=nil then begin
      if NewComponent<>nil then begin
        try
          NewComponent.Free;
          NewComponent:=nil;
        except
          on E: Exception do begin
            s:=Format(lisCFEErrorDestroyingComponentOfTypeOfUnit, [TypeClass.
              ClassName, AUnitName, #13, E.Message]);
            DebugLn(['TCustomFormEditor.CreateComponent ',UTF8ToConsole(s)]);
            DumpExceptionBackTrace;
            MessageDlg(lisCFEErrorDestroyingComponent, s, mtError, [mbCancel], 0
              );
          end;
        end;
      end;
    end;
  end;
end;

function TCustomFormEditor.CreateComponentFromStream(
  BinStream: TStream;
  AncestorType: TComponentClass;
  const NewUnitName: ShortString;
  Interactive: boolean; Visible: boolean; DisableAutoSize: boolean;
  ContextObj: TObject): TComponent;
begin
  Result:=CreateRawComponentFromStream(BinStream,
       AncestorType,NewUnitName,Interactive,Visible,DisableAutoSize,ContextObj);
end;

function TCustomFormEditor.CreateRawComponentFromStream(BinStream: TStream;
  AncestorType: TComponentClass;
  const NewUnitName: ShortString;
  Interactive: boolean; Visible: boolean; DisableAutoSize: boolean;
  ContextObj: TObject): TComponent;
var
  NewJITIndex: integer;
  JITList: TJITComponentList;
begin
  // create JIT Component
  JITList:=GetJITListOfType(AncestorType);
  if JITList=nil then
    RaiseException('TCustomFormEditor.CreateComponentFromStream ClassName='+
                   AncestorType.ClassName);
  NewJITIndex := JITList.AddJITComponentFromStream(BinStream,
              AncestorType,NewUnitName,Interactive,Visible,DisableAutoSize,
              ContextObj);
  if NewJITIndex < 0 then begin
    Result:=nil;
    exit;
  end;
  Result:=JITList[NewJITIndex];
end;

function TCustomFormEditor.CreateChildComponentFromStream(BinStream: TStream;
  ComponentClass: TComponentClass; Root: TComponent;
  ParentControl: TWinControl): TComponent;
var
  JITList: TJITComponentList;
begin
  Result:=nil;
  
  JITList:=FindJITList(Root);
  if JITList=nil then
    RaiseException('TCustomFormEditor.CreateChildComponentFromStream ClassName='+
                   Root.ClassName);

  Result:=JITList.AddJITChildComponentFromStream(
                                   Root,BinStream,ComponentClass,ParentControl);
end;

function TCustomFormEditor.FixupReferences(AComponent: TComponent): TModalResult;
begin
  Result:=MainIDEInterface.DoFixupComponentReferences(AComponent,[]);
end;

procedure TCustomFormEditor.WriterFindAncestor(Writer: TWriter;
  Component: TComponent; const Name: string; var Ancestor,
  RootAncestor: TComponent);
// Note: TWriter wants the stream ancestor, which is not always the class ancestor
var
  AnUnitInfo: TUnitInfo;
begin
  {$IFDEF VerboseFormEditor}
  DebugLn(['TCustomFormEditor.WriterFindAncestor START Component=',DbgSName(Component)]);
  {$ENDIF}
  AnUnitInfo:=Project1.UnitWithComponentClass(TComponentClass(Component.ClassType));
  if (AnUnitInfo<>nil) then begin
    if (AnUnitInfo.Component=Component) then begin
      // Component is a root component (e.g. not nested, inline)
      // the stream ancestor is the component of the ClassParent
      AnUnitInfo:=AnUnitInfo.FindAncestorUnit;
    end else begin
      // Component is a nested, inline component
      // the stream ancestor is the component of the class
    end;
    if (AnUnitInfo<>nil) and (AnUnitInfo.Component<>nil) then begin
      Ancestor:=AnUnitInfo.Component;
      RootAncestor:=AnUnitInfo.Component;
    end;
    {$IFDEF VerboseFormEditor}
    DebugLn(['TCustomFormEditor.WriterFindAncestor Component=',DbgSName(Component),' Ancestor=',DbgSName(Ancestor),' RootAncestor=',DbgSName(RootAncestor)]);
    {$ENDIF}
  end;
end;

procedure TCustomFormEditor.SetComponentNameAndClass(
  AComponent: TComponent;
  const NewName, NewClassName: shortstring);
var
  JITList: TJITComponentList;
begin
  JITList:=GetJITListOfType(TComponentClass(AComponent.ClassType));
  JITList.RenameComponentClass(AComponent,NewClassName);
  AComponent.Name:=NewName;
end;

function TCustomFormEditor.ClassDependsOnComponent(AClass: TComponentClass;
  AComponent: TComponent): Boolean;
{ Check if AClass uses AComponent.

  For example:
    Add frame2 to frame1 ( frame1 uses frame2 )
    Add frame3 to frame2 ( frame2 uses frame3 => frame 2 uses frame1)
    Add frame1 to frame3 => circle
}
var
  AnUnitInfo: TUnitInfo;
begin
  if AClass.InheritsFrom(AComponent.ClassType) then exit(true);
  AnUnitInfo := Project1.UnitWithComponentClass(AClass);
  if AnUnitInfo = nil then Exit(false);
  Result := ComponentDependsOnClass(AnUnitInfo.Component,
                                    TComponentClass(AComponent.ClassType));
end;

function TCustomFormEditor.ComponentDependsOnClass(AComponent: TComponent;
  AClass: TComponentClass): Boolean;
var
  i: Integer;
begin
  if AComponent is AClass then exit(true);
  if AComponent<>nil then
    for i:=0 to AComponent.ComponentCount-1 do
      if ComponentDependsOnClass(AComponent.Components[i],AClass) then
        exit(true);
  Result:=false;
end;

function TCustomFormEditor.GetAncestorLookupRoot(AComponent: TComponent
  ): TComponent;
var
  CurRoot: TComponent;
  AncestorRoot: TComponent;
begin
  Result:=nil;
  if AComponent=nil then exit;
  CurRoot:=AComponent.Owner;
  if CurRoot=nil then exit;
  
  if csInline in CurRoot.ComponentState then begin
    // inline/embedded components (e.g. nested frame)
    CurRoot:=FindJITComponentByClass(TComponentClass(CurRoot.ClassType));
    if CurRoot=nil then exit;
    if CurRoot.FindComponent(AComponent.Name)=nil then exit;
    Result:=CurRoot;
  end;

  repeat
    // search in next ancestor
    AncestorRoot:=GetAncestorInstance(CurRoot);
    if AncestorRoot=nil then break;
    if AncestorRoot.FindComponent(AComponent.Name)=nil then break;
    // a better ancestor was found
    Result:=AncestorRoot;
    CurRoot:=AncestorRoot;
  until false;
  {$IFDEF VerboseFormEditor}
  DebugLn(['TCustomFormEditor.GetAncestorLookupRoot AComponent=',DbgSName(AComponent),' Result=',DbgSName(Result)]);
  {$ENDIF}
end;

function TCustomFormEditor.GetAncestorInstance(AComponent: TComponent
  ): TComponent;
begin
  Result:=nil;
  if (AComponent=nil) or (AComponent.ClassType=TComponent) then exit;
  Result:=FindJITComponentByClass(TComponentClass(AComponent.ClassParent));
end;

function TCustomFormEditor.RegisterDesignerBaseClass(AClass: TComponentClass
  ): integer;
begin
  if AClass=nil then
    RaiseGDBException('TCustomFormEditor.RegisterDesignerBaseClass');
  Result:=FDesignerBaseClasses.IndexOf(AClass);
  if Result<0 then
    Result:=FDesignerBaseClasses.Add(AClass)
end;

function TCustomFormEditor.DesignerBaseClassCount: Integer;
begin
  Result:=FDesignerBaseClasses.Count;
end;

procedure TCustomFormEditor.UnregisterDesignerBaseClass(AClass: TComponentClass);
var
  l: Integer;
begin
  for l:=Low(StandardDesignerBaseClasses) to High(StandardDesignerBaseClasses)
  do
    if StandardDesignerBaseClasses[l]=AClass then
      RaiseGDBException('TCustomFormEditor.UnregisterDesignerBaseClass');
  FDesignerBaseClasses.Remove(AClass);
end;

function TCustomFormEditor.IndexOfDesignerBaseClass(AClass: TComponentClass
  ): integer;
begin
  Result:=FDesignerBaseClasses.IndexOf(AClass);
end;

function TCustomFormEditor.DescendFromDesignerBaseClass(AClass: TComponentClass
  ): integer;
begin
  Result:=FDesignerBaseClasses.Count-1;
  while (Result>=0)
  and (not AClass.InheritsFrom(TClass(FDesignerBaseClasses[Result]))) do
    dec(Result);
end;

function TCustomFormEditor.FindDesignerBaseClassByName(
  const AClassName: shortstring; WithDefaults: boolean): TComponentClass;
var
  i: Integer;
begin
  if WithDefaults then begin
    for i:=Low(StandardDesignerBaseClasses) to high(StandardDesignerBaseClasses)
    do begin
      if CompareText(AClassName,StandardDesignerBaseClasses[i].ClassName)=0 then
      begin
        Result:=StandardDesignerBaseClasses[i];
        exit;
      end;
    end;
  end;
  for i:=FDesignerBaseClasses.Count-1 downto 0 do begin
    Result:=DesignerBaseClasses[i];
    if CompareText(Result.ClassName,AClassName)=0 then exit;
  end;
  Result:=nil;
end;

procedure TCustomFormEditor.FindDefineProperty(
  const APersistentClassName, AncestorClassName, Identifier: string;
  var IsDefined: boolean);
var
  AutoFreePersistent: Boolean;
  APersistent: TPersistent;
  CacheItem: TDefinePropertiesCacheItem;
  DefinePropertiesReader: TDefinePropertiesReader;
  ANode: TAVLTreeNode;
  OldClassName: String;
  DefinePropertiesPersistent: TDefinePropertiesPersistent;

  function CreateTempPersistent(
    const APersistentClass: TPersistentClass): boolean;
  begin
    Result:=false;
    if APersistent<>nil then
      RaiseGDBException('TCustomFormEditor.FindDefineProperty.CreateTempPersistent Inconsistency');
    try
      if APersistentClass.InheritsFrom(TComponent) then
        APersistent:=TComponentClass(APersistentClass).Create(nil)
      else if APersistentClass.InheritsFrom(TGraphic) then
        APersistent:=TGraphicClass(APersistentClass).Create
      else
        APersistent:=APersistentClass.Create;
      Result:=true;
      AutoFreePersistent:=true;
    except
      on E: Exception do begin
        debugln('TCustomFormEditor.GetDefineProperties Error creating ',
          APersistentClass.Classname,
          ': ',E.Message);
      end;
    end;
  end;
  
  function GetDefinePersistent(const AClassName: string): Boolean;
  var
    APersistentClass: TPersistentClass;
    AncestorClass: TComponentClass;
  begin
    Result:=false;
    
    // try to find the AClassName in the registered components
    if APersistent=nil then begin
      CacheItem.RegisteredComponent:=IDEComponentPalette.FindComponent(AClassname);
      if (CacheItem.RegisteredComponent<>nil)
      and (CacheItem.RegisteredComponent.ComponentClass<>nil) then begin
        debugln('TCustomFormEditor.GetDefineProperties ComponentClass ',AClassName,' is registered');
        if not CreateTempPersistent(CacheItem.RegisteredComponent.ComponentClass)
        then exit;
      end;
    end;
    
    // try to find the AClassName in the registered TPersistent classes
    if APersistent=nil then begin
      APersistentClass:=Classes.GetClass(AClassName);
      if APersistentClass<>nil then begin
        debugln('TCustomFormEditor.GetDefineProperties PersistentClass ',AClassName,' is registered');
        if not CreateTempPersistent(APersistentClass) then exit;
      end;
    end;

    if APersistent=nil then begin
      // try to find the AClassName in the open forms/datamodules
      APersistent:=FindJITComponentByClassName(AClassName);
      if APersistent<>nil then
        debugln('TCustomFormEditor.GetDefineProperties ComponentClass ',
          AClassName,' is a resource,'
          +' but inheriting design properties is not yet implemented');
    end;

    // try default classes
    if (APersistent=nil) then begin
      AncestorClass:=FindDesignerBaseClassByName(AClassName,true);
      if AncestorClass<>nil then begin
        if not CreateTempPersistent(AncestorClass) then exit;
      end;
    end;

    Result:=true;
  end;
  
begin
  //debugln('TCustomFormEditor.GetDefineProperties ',
  //  ' APersistentClassName="',APersistentClassName,'"',
  // ' AncestorClassName="',AncestorClassName,'"',
  //  ' Identifier="',Identifier,'"');
  IsDefined:=false;
  RegisterStandardDefineProperties;
  ANode:=FindDefinePropertyNode(APersistentClassName);
  if ANode=nil then begin
    // cache component class, try to retrieve the define properties
    CacheItem:=TDefinePropertiesCacheItem.Create;
    CacheItem.PersistentClassname:=APersistentClassName;
    FDefineProperties.Add(CacheItem);
    debugln('TCustomFormEditor.GetDefineProperties APersistentClassName="',APersistentClassName,'" AncestorClassName="',AncestorClassName,'"');

    APersistent:=nil;
    AutoFreePersistent:=false;

    if not GetDefinePersistent(APersistentClassName) then exit;
    if (APersistent=nil) then begin
      if not GetDefinePersistent(AncestorClassName) then exit;
    end;

    if APersistent<>nil then begin
      debugln('TCustomFormEditor.GetDefineProperties Getting define properties for ',APersistent.ClassName);

      // try creating a component class and call DefineProperties
      DefinePropertiesReader:=nil;
      DefinePropertiesPersistent:=nil;
      try
        try
          DefinePropertiesReader:=TDefinePropertiesReader.Create;
          DefinePropertiesPersistent:=
                                TDefinePropertiesPersistent.Create(APersistent);
          DefinePropertiesPersistent.PublicDefineProperties(
                                                        DefinePropertiesReader);
        except
          on E: Exception do begin
            DbgOut('TCustomFormEditor.GetDefineProperties Error calling DefineProperties for ');
            if (CacheItem.RegisteredComponent<>nil) then begin
              DbgOut(CacheItem.RegisteredComponent.ComponentClass.Classname);
            end;
            DebugLn(' : ',E.Message);
          end;
        end;
        // free component
        if AutoFreePersistent then begin
          try
            OldClassName:=APersistent.ClassName;
            APersistent.Free;
          except
            on E: Exception do begin
              debugln('TCustomFormEditor.GetDefineProperties Error freeing ',
                OldClassName,': ',E.Message);
            end;
          end;
        end;
      finally
        // cache defined properties
        if (DefinePropertiesReader<>nil)
        and (DefinePropertiesReader.DefinePropertyNames<>nil) then begin
          CacheItem.DefineProperties:=TStringList.Create;
          CacheItem.DefineProperties.Assign(
                                    DefinePropertiesReader.DefinePropertyNames);
          debugln('TCustomFormEditor.GetDefineProperties Class=',APersistentClassName,
            ' DefineProps="',CacheItem.DefineProperties.Text,'"');
        end;
        DefinePropertiesReader.Free;
        DefinePropertiesPersistent.Free;
      end;
    end else begin
      debugln('TCustomFormEditor.GetDefineProperties Persistent is NOT registered');
    end;
    //debugln('TCustomFormEditor.GetDefineProperties END APersistentClassName="',APersistentClassName,'" AncestorClassName="',AncestorClassName,'"');
  end else begin
    CacheItem:=TDefinePropertiesCacheItem(ANode.Data);
  end;
  if CacheItem.DefineProperties<>nil then
    IsDefined:=CacheItem.DefineProperties.IndexOf(Identifier)>=0;
end;

procedure TCustomFormEditor.RegisterDefineProperty(const APersistentClassName,
  Identifier: string);
var
  ANode: TAVLTreeNode;
  CacheItem: TDefinePropertiesCacheItem;
begin
  //DebugLn('TCustomFormEditor.RegisterDefineProperty ',APersistentClassName,' ',Identifier);
  ANode:=FindDefinePropertyNode(APersistentClassName);
  if ANode=nil then begin
    CacheItem:=TDefinePropertiesCacheItem.Create;
    CacheItem.PersistentClassname:=APersistentClassName;
    FDefineProperties.Add(CacheItem);
  end else begin
    CacheItem:=TDefinePropertiesCacheItem(ANode.Data);
  end;
  if (CacheItem.DefineProperties=nil) then
    CacheItem.DefineProperties:=TStringList.Create;
  if (CacheItem.DefineProperties.IndexOf(Identifier)<0) then
    CacheItem.DefineProperties.Add(Identifier);
end;

procedure TCustomFormEditor.RegisterStandardDefineProperties;
begin
  if FStandardDefinePropertiesRegistered then exit;
  FStandardDefinePropertiesRegistered:=true;
  RegisterDefineProperty('TStrings','Strings');
end;

procedure TCustomFormEditor.JITListReaderError(Sender: TObject;
  Reader: TReader; ErrorType: TJITFormError; var Action: TModalResult);
var
  aCaption, aMsg: string;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  HelpCtx: Longint;
  JITComponentList: TJITComponentList;
  StreamClass: TComponentClass;
  AnUnitInfo: TUnitInfo;
  LFMFilename: String;
  ErrorBinPos: Int64;
begin
  JITComponentList:=TJITComponentList(Sender);
  aCaption:='Read error';
  aMsg:='';
  DlgType:=mtError;
  Buttons:=[mbCancel];
  HelpCtx:=0;
  
  // get current lfm filename
  LFMFilename:='';
  if (JITComponentList.CurReadStreamClass<>nil)
  and (JITComponentList.CurReadStreamClass.InheritsFrom(TComponent)) then begin
    StreamClass:=TComponentClass(JITComponentList.CurReadStreamClass);
    AnUnitInfo:=Project1.UnitWithComponentClass(StreamClass);
    if AnUnitInfo<>nil then begin
      LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
    end;
  end;
  if LFMFilename<>'' then
    aCaption:=Format(lisCFEErrorReading, [ExtractFilename(LFMFilename)]);
  
  with JITComponentList do begin
    if LFMFilename<>'' then
      aMsg:=aMsg+LFMFilename
    else if CurReadStreamClass<>nil then
      aMsg:=Format(lisCFEStream, [aMsg, CurReadStreamClass.ClassName])
    else
      aMsg:=aMsg+'JITList='+ClassName;
    aMsg:=aMsg+': ';
    if CurReadJITComponent<>nil then
      aMsg:=Format(lisCFERoot, [aMsg, CurReadJITComponent.Name,
        CurReadJITComponent.ClassName]);
    if CurReadChild<>nil then
      aMsg:=Format(lisCFEComponent, [aMsg, #13, CurReadChild.Name, CurReadChild.
        ClassName])
    else if CurReadChildClass<>nil then
      aMsg:=Format(lisCFEComponentClass, [aMsg, #13, CurReadChildClass.ClassName
        ]);
    aMsg:=aMsg+#13+CurReadErrorMsg;
  end;
  if (Reader<>nil) and (Reader.Driver is TLRSObjectReader) then begin
    ErrorBinPos:=TLRSObjectReader(Reader.Driver).Stream.Position;
    aMsg:=Format(lisCFEStreamPosition, [aMsg, #13, dbgs(ErrorBinPos)]);
  end;

  case ErrorType of
    jfeUnknownProperty, jfeReaderError:
      begin
        Buttons:=[mbIgnore,mbCancel];
      end;
    jfeUnknownComponentClass:
      begin
        aMsg:=Format(lisCFEClassNotFound, [aMsg, #13, JITComponentList.
          CurUnknownClass]);
      end;
  end;
  if Buttons=[mbIgnore,mbCancel] then begin
    Action:=QuestionDlg(aCaption,aMsg,DlgType,
      [mrIgnore, lisCFEContinueLoading,
       mrCancel, lisCFECancelLoadingThisResource,
       mrAbort, lisCFEStopAllLoading], HelpCtx);
  end else begin
    Action:=QuestionDlg(aCaption,aMsg,DlgType,
      [mrCancel, lisCFECancelLoadingThisResource,
       mrAbort, lisCFEStopAllLoading], HelpCtx);
  end;
end;

procedure TCustomFormEditor.JITListBeforeCreate(Sender: TObject;
  Instance: TPersistent);
var
  MediatorClass: TDesignerMediatorClass;
begin
  if Instance is TComponent then begin
    MediatorClass:=GetDesignerMediatorClass(TComponentClass(Instance.ClassType));
    if MediatorClass<>nil then
      MediatorClass.InitFormInstance(TComponent(Instance));
  end;
end;

procedure TCustomFormEditor.JITListException(Sender: TObject; E: Exception;
  var Action: TModalResult);
var
  List: TJITComponentList;
  AnUnitInfo: TUnitInfo;
  LFMFilename: String;
  Msg: String;
begin
  List:=TJITComponentList(Sender);
  LFMFilename:='';
  Msg:='';
  DebugLn(['TCustomFormEditor.JITListException List.CurReadStreamClass=',DbgSName(List.CurReadStreamClass),' ',DbgSName(List.ContextObject)]);
  if (List.CurReadStreamClass<>nil) and (Project1<>nil)
  and (List.CurReadStreamClass.InheritsFrom(TComponent)) then begin
    AnUnitInfo:=Project1.UnitWithComponentClass(TComponentClass(List.CurReadStreamClass));
    if AnUnitInfo<>nil then begin
      LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
    end;
  end;
  if (LFMFilename='') and (List.ContextObject is TUnitInfo) then begin
    LFMFilename:=ChangeFileExt(TUnitInfo(List.ContextObject).Filename,'.lfm');
  end;
  if LFMFilename<>'' then
    Msg:=Format(lisCFEInFile, [Msg, LFMFilename, #13]);

  if List.CurReadErrorMsg<>'' then
    Msg:=Msg+List.CurReadErrorMsg+#13;
  if E is EReadError then;
  MessageDlg(lisCodeToolsDefsReadError, Msg, mtError, [mbCancel], 0);
end;

procedure TCustomFormEditor.OnDesignerMenuItemClick(Sender: TObject);
var
  CompEditor: TBaseComponentEditor;
  MenuItem: TMenuItem;
  CompClassName: String;
begin
  if (Sender=nil) or (not (Sender is TMenuItem)) then exit;
  MenuItem:=TMenuItem(Sender);
  if (MenuItem.Count>0) or MenuItem.IsInMenuBar then exit;

  CompEditor:=GetComponentEditor(TComponent(Sender));
  if CompEditor=nil then exit;
  CompClassName:=CompEditor.ClassName;
  try
    CompEditor.Edit;
  except
    on E: Exception do begin
      DebugLn('TCustomFormEditor.OnDesignerMenuItemClick ERROR on CompEditor.Edit: ',E.Message);
      MessageDlg(Format(lisErrorIn, [CompClassName]),
        Format(lisCFETheComponentEditorOfClassHasCreatedTheError, [CompClassName, #13, E.Message]),
        mtError,[mbOk],0);
    end;
  end;
  try
    CompEditor.Free;
  except
    on E: Exception do begin
      DebugLn('TCustomFormEditor.OnDesignerMenuItemClick ERROR on CompEditor.Free: ',E.Message);
      MessageDlg(Format(lisErrorIn, [CompClassName]),
        Format(lisCFETheComponentEditorOfClassHasCreatedTheError, [CompClassName, #13, E.Message]),
        mtError,[mbOk],0);
    end;
  end;
end;

function TCustomFormEditor.FindNonFormFormNode(LookupRoot: TComponent): TAVLTreeNode;
begin
  Result := FNonFormForms.FindKey(Pointer(LookupRoot),
                                   @CompareLookupRootAndNonFormDesignerForm);
end;

procedure TCustomFormEditor.JITListPropertyNotFound(Sender: TObject;
  Reader: TReader; Instance: TPersistent; var PropName: string;
  IsPath: boolean; var Handled, Skip: Boolean);
var
  Index: Integer;
begin
  Index := PropertiesToSkip.IndexOf(Instance, PropName);
  if Index >= 0 then
  begin
    Skip := True;
    Handled := True;
  end
  else
    DebugLn(['TCustomFormEditor.JITListPropertyNotFound ',Sender.ClassName,
      ' Instance=',Instance.ClassName,' PropName="',PropName,
      '" IsPath=',IsPath]);
end;

procedure TCustomFormEditor.JITListFindAncestors(Sender: TObject;
  AClass: TClass;
  var Ancestors: TFPList;// list of TComponent
  var BinStreams: TFPList;// list of TExtMemoryStream;
  var Abort: boolean);
var
  AnUnitInfo: TUnitInfo;
  Ancestor: TComponent;
  BinStream: TExtMemoryStream;
begin
  Ancestors:=nil;
  BinStreams:=nil;
  if Project1=nil then exit;
  if (AClass=nil) or (AClass=TComponent)
  or (AClass=TForm) or (AClass=TCustomForm)
  or (AClass=TDataModule)
  or (not AClass.InheritsFrom(TComponent))
  or (IndexOfDesignerBaseClass(TComponentClass(AClass))>=0) then begin
    exit;
  end;
  //DebugLn(['TCustomFormEditor.JITListFindAncestors Class=',DbgSName(AClass)]);
  AnUnitInfo:=Project1.UnitWithComponentClass(TComponentClass(AClass));
  while AnUnitInfo<>nil do begin
    {$IFDEF VerboseFormEditor}
    DebugLn(['TCustomFormEditor.JITListFindAncestors FOUND ancestor ',DbgSName(AnUnitInfo.Component),', streaming ...']);
    {$ENDIF}
    Ancestor:=AnUnitInfo.Component;
    BinStream:=nil;
    if SaveUnitComponentToBinStream(AnUnitInfo,BinStream)<>mrOk then begin
      Abort:=true;
      exit;
    end;
    BinStream.Position:=0;
    if Ancestors=nil then begin
      Ancestors:=TFPList.Create;
      BinStreams:=TFPList.Create;
    end;
    Ancestors.Add(Ancestor);
    BinStreams.Add(BinStream);
    AnUnitInfo:=AnUnitInfo.FindAncestorUnit;
  end;
end;

procedure TCustomFormEditor.JITListFindClass(Sender: TObject;
  const ComponentClassName: string; var ComponentClass: TComponentClass);
var
  AnUnitInfo: TUnitInfo;
  Component: TComponent;
  RegComp: TRegisteredComponent;
begin
  //DebugLn(['TCustomFormEditor.JITListFindClass ',ComponentClassName]);
  RegComp:=IDEComponentPalette.FindComponent(ComponentClassName);
  if RegComp<>nil then begin
    //DebugLn(['TCustomFormEditor.JITListFindClass ',ComponentClassName,' is registered as ',DbgSName(RegComp.ComponentClass)]);
    ComponentClass:=RegComp.ComponentClass;
  end else begin
    // ToDo: search only in reachable forms
    AnUnitInfo:=Project1.FirstUnitWithComponent;
    while AnUnitInfo<>nil do begin
      Component:=AnUnitInfo.Component;
      if SysUtils.CompareText(Component.ClassName,ComponentClassName)=0 then
      begin
        DebugLn(['TCustomFormEditor.JITListFindClass found nested class '+DbgSName(Component)+' in unit '+AnUnitInfo.Filename]);
        ComponentClass:=TComponentClass(Component.ClassType);
        break;
      end;
      AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
    end;
  end;
  //DebugLn(['TCustomFormEditor.JITListFindClass Searched=',ComponentClassName,' Found=',DbgSName(ComponentClass)]);
end;

function TCustomFormEditor.GetDesignerBaseClasses(Index: integer
  ): TComponentClass;
begin
  Result:=TComponentClass(FDesignerBaseClasses[Index]);
end;

procedure TCustomFormEditor.FrameCompGetCreationClass(Sender: TObject;
  var NewComponentClass: TComponentClass);
begin
  if Assigned(OnSelectFrame) then
    OnSelectFrame(Sender,NewComponentClass);
end;

function TCustomFormEditor.GetPropertyEditorHook: TPropertyEditorHook;
begin
  Result:=nil;
  if Obj_Inspector<>nil then
    Result:=Obj_Inspector.PropertyEditorHook;
end;

function TCustomFormEditor.FindDefinePropertyNode(
  const APersistentClassName: string): TAVLTreeNode;
begin
  if FDefineProperties=nil then
    FDefineProperties:=
                   TAVLTree.Create(TListSortCompare(@CompareDefPropCacheItems));
  Result:=FDefineProperties.FindKey(PChar(APersistentClassName),
                    TListSortCompare(@ComparePersClassNameAndDefPropCacheItem));
end;

function TCustomFormEditor.CreateUniqueComponentName(AComponent: TComponent
  ): string;
begin
  Result:='';
  if (AComponent=nil) then exit;
  Result:=AComponent.Name;
  if (AComponent.Owner=nil) or (Result<>'') then exit;
  Result:=CreateUniqueComponentName(AComponent.ClassName,AComponent.Owner);
end;

function TCustomFormEditor.CreateUniqueComponentName(const AClassName: string;
  OwnerComponent: TComponent): string;
var
  i, j: integer;
begin
  Result:=AClassName;
  if (OwnerComponent=nil) or (Result='') then exit;
  i:=1;
  while true do begin
    j:=OwnerComponent.ComponentCount-1;
    Result:=ClassNameToComponentName(AClassName);
    if Result[length(Result)] in ['0'..'9'] then
      Result:=Result+'_';
    Result:=Result+IntToStr(i);
    while (j>=0)
    and (CompareText(Result,OwnerComponent.Components[j].Name)<>0) do
      dec(j);
    if j<0 then exit;
    inc(i);
  end;
end;

function TCustomFormEditor.TranslateKeyToDesignerCommand(Key: word;
  Shift: TShiftState): word;
begin
  //debugln(['TCustomFormEditor.TranslateKeyToDesignerCommand ',DbgSName(TDesignerIDECommandForm),' ',Key,' ',dbgs(Shift)]);
  Result:=EditorOpts.KeyMap.TranslateKey(Key,Shift,TDesignerIDECommandForm);
end;

function TCustomFormEditor.GetDefaultComponentParent(TypeClass: TComponentClass
  ): TComponent;
var
  NewParent: TComponent;
  Root: TPersistent;
  Mediator: TDesignerMediator;
begin
  Result:=nil;
  // find selected component
  if (FSelection = nil) or (FSelection.Count <= 0) then Exit;
  NewParent:=TComponent(FSelection[0]);
  //Debugln('TCustomFormEditor.GetDefaultComponentParent A:', DbgSName(NewParent));
  if not (NewParent is TComponent) then exit;
  if TypeClass<>nil then begin
    if TypeClass.InheritsFrom(TControl) and (NewParent is TControl) then begin
      // New TypeClass is a TControl and selected component is TControl =>
      // use only a TWinControl as parent
      while (NewParent<>nil) do begin
        if (NewParent is TWinControl)
        and (csAcceptsControls in TWinControl(NewParent).ControlStyle) then
          break;
        NewParent:=TControl(NewParent).Parent;
        //Debugln('TCustomFormEditor.GetDefaultComponentParent B:', DbgSName(NewParent));
      end;
    end else begin
      // New TypeClass or selected component is not a TControl =>
      // use Root component as parent
      Root:=GetLookupRootForComponent(NewParent);
      if Root is TComponent then begin
        Mediator:=GetDesignerMediatorByComponent(TComponent(Root));
        if (Mediator<>nil) then begin
          while (NewParent<>nil) do begin
            if Mediator.ParentAcceptsChild(NewParent,TypeClass) then
              break;
            NewParent:=NewParent.GetParentComponent;
          end;
          if NewParent=nil then
            NewParent:=TComponent(Root);
        end else
          NewParent:=TComponent(Root);
      end;
    end;
  end;
  Result:=NewParent;
end;

function TCustomFormEditor.GetDefaultComponentPosition(
  TypeClass: TComponentClass; ParentComponent: TComponent; var X, Y: integer
  ): boolean;
var
  i: Integer;
  CurComponent: TComponent;
  P: TPoint;
  AForm: TCustomNonFormDesignerForm;
  MinX: Integer;
  MinY: Integer;
  MaxX: Integer;
  MaxY: Integer;
begin
  Result:=true;
  X:=10;
  Y:=10;
  if ParentComponent=nil then
    ParentComponent:=GetDefaultComponentParent(TypeClass);
  if (ParentComponent=nil) or (TypeClass=nil) then exit;
  if (TypeClass.InheritsFrom(TControl)) then exit;
  // a non visual component
  // put it somewhere right or below the other non visual components
  MinX:=-1;
  MinY:=-1;
  if (ParentComponent is TWinControl) then
  begin
    MaxX:=TWinControl(ParentComponent).ClientWidth-ComponentPaletteBtnWidth;
    MaxY:=TWinControl(ParentComponent).ClientHeight-ComponentPaletteBtnHeight;
  end else
  begin
    AForm:=FindNonFormForm(ParentComponent);
    if AForm<>nil then begin
      MaxX:=AForm.ClientWidth-ComponentPaletteBtnWidth;
      MaxY:=AForm.ClientHeight-ComponentPaletteBtnHeight;
    end else begin
      MaxX:=300;
      MaxY:=0;
    end;
  end;
  // find top left most non visual component
  for i:=0 to ParentComponent.ComponentCount-1 do begin
    CurComponent:=ParentComponent.Components[i];
    if ComponentIsNonVisual(CurComponent) then begin
      P:=GetParentFormRelativeTopLeft(CurComponent);
      if (P.X>=0) and (P.Y>=0) then begin
        if (MinX<0) or (P.Y<MinY) or ((P.Y=MinY) and (P.X<MinX)) then begin
          MinX:=P.X;
          MinY:=P.Y;
        end;
      end;
    end;
  end;
  if MinX<0 then begin
    MinX:=10;
    MinY:=10;
  end;
  // find a position without intersection
  X:=MinX;
  Y:=MinY;
  //debugln('TCustomFormEditor.GetDefaultComponentPosition Min=',dbgs(MinX),',',dbgs(MinY));
  i:=0;
  while i<ParentComponent.ComponentCount do begin
    CurComponent:=ParentComponent.Components[i];
    inc(i);
    if ComponentIsNonVisual(CurComponent) then begin
      P:=GetParentFormRelativeTopLeft(CurComponent);
      //debugln('TCustomFormEditor.GetDefaultComponentPosition ',dbgsName(CurComponent),' P=',dbgs(P));
      if (P.X>=0) and (P.Y>=0) then begin
        if (X+ComponentPaletteBtnWidth>=P.X)
        and (X<=P.X+ComponentPaletteBtnWidth)
        and (Y+ComponentPaletteBtnHeight>=P.Y)
        and (Y<=P.Y+ComponentPaletteBtnHeight) then begin
          // intersection found
          // move position
          inc(X,ComponentPaletteBtnWidth+2);
          if X>MaxX then begin
            inc(Y,ComponentPaletteBtnHeight+2);
            X:=MinX;
          end;
          // restart intersection test
          i:=0;
        end;
      end;
    end;
  end;
  // keep it visible
  if X>MaxX then X:=MaxX;
  if Y>MaxY then Y:=MaxY;
end;

procedure TCustomFormEditor.OnObjectInspectorModified(Sender: TObject);
var
  CustomForm: TCustomForm;
  Instance: TPersistent;
begin
  if (FSelection = nil)
  or (FSelection.Count <= 0) then Exit;
  
  Instance := FSelection[0];
  CustomForm:=GetDesignerForm(Instance);
  if (CustomForm<>nil) and (CustomForm.Designer<>nil) then
    CustomForm.Designer.Modified;
end;

procedure TCustomFormEditor.SetObj_Inspector(
  AnObjectInspector: TObjectInspectorDlg);
begin
  if AnObjectInspector=FObj_Inspector then exit;
  if FObj_Inspector<>nil then begin
    FObj_Inspector.OnModified:=nil;
  end;

  FObj_Inspector:=AnObjectInspector;
  
  if FObj_Inspector<>nil then begin
    FObj_Inspector.OnModified:=@OnObjectInspectorModified;
  end;
end;


{ TDefinePropertiesCacheItem }

destructor TDefinePropertiesCacheItem.Destroy;
begin
  DefineProperties.Free;
  inherited Destroy;
end;

{ TDefinePropertiesReader }

procedure TDefinePropertiesReader.AddPropertyName(const Name: string);
begin
  debugln('TDefinePropertiesReader.AddPropertyName Name="',Name,'"');
  if FDefinePropertyNames=nil then FDefinePropertyNames:=TStringList.Create;
  if FDefinePropertyNames.IndexOf(Name)<=0 then
    FDefinePropertyNames.Add(Name);
end;

destructor TDefinePropertiesReader.Destroy;
begin
  FDefinePropertyNames.Free;
  inherited Destroy;
end;

procedure TDefinePropertiesReader.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
begin
  AddPropertyName(Name);
end;

procedure TDefinePropertiesReader.DefineBinaryProperty(const Name: string;
  ReadData, WriteData: TStreamProc; HasData: Boolean);
begin
  AddPropertyName(Name);
end;

{ TDefinePropertiesPersistent }

constructor TDefinePropertiesPersistent.Create(TargetPersistent: TPersistent);
begin
  FTarget:=TargetPersistent;
end;

procedure TDefinePropertiesPersistent.PublicDefineProperties(Filer: TFiler);
begin
  debugln('TDefinePropertiesPersistent.PublicDefineProperties START ',ClassName,' ',dbgsName(FTarget));
  {$IFOPT R+}{$DEFINE RangeCheckOn}{$ENDIF}
  {$R-}
  TDefinePropertiesPersistent(Target).DefineProperties(Filer);
  {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
  debugln('TDefinePropertiesPersistent.PublicDefineProperties END ',ClassName,' ',dbgsName(FTarget));
end;

initialization
  RegisterStandardClasses;

end.

