unit DesignEditors;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, IniFiles,
  Menus,
  ComponentEditors, PropEdits,
{$IF FPC_FULLVERSION>=30200}
  Generics.Defaults,
{$ELSE}
  sparta_Generics.Defaults,
{$ENDIF}
  DesignIntf, DesignMenus;

type

  { TComponentEditor }

  TComponentEditor = class(TBaseComponentEditor, IComponentEditor)
  private
    //FLazaComponentEditors: ComponentEditors.TComponentEditor;
    FComponent: TComponent;
    FDesigner: IDesigner;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    procedure Edit; virtual;
    procedure ExecuteVerb(Index: Integer); virtual;
    function GetComponent: TComponent;
    function GetDesigner: IDesigner;
    function GetVerb(Index: Integer): string; virtual;
    function GetVerbCount: Integer; virtual;
    function IsInInlined: Boolean;
    procedure Copy; virtual;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); virtual;
    property Component: TComponent read FComponent;
    property Designer: IDesigner read GetDesigner;
  end;

implementation

type

  { TIDesignerProxy }

  TIDesignerProxy = class(TInterfacedObject, IDesigner60, IDesigner70, IDesigner80,
    IDesigner100, IDesigner)
  private
    FLazarusDesigner: TComponentEditorDesigner;
  public
    constructor Create(ADesigner: TComponentEditorDesigner);

    // IDesigner60
    procedure Activate;
    procedure Modified;
    function CreateMethod(const Name: string; TypeData: PTypeData): TMethod; overload;
    function GetMethodName(const Method: TMethod): string;
    procedure GetMethods(TypeData: PTypeData; Proc: TGetStrProc); overload;
    function GetPathAndBaseExeName: string;
    function GetPrivateDirectory: string;
    function GetBaseRegKey: string;
    function GetIDEOptions: TCustomIniFile;
    procedure GetSelections(const List: IDesignerSelections);
    function MethodExists(const Name: string): Boolean;
    procedure RenameMethod(const CurName, NewName: string);
    procedure SelectComponent(Instance: TPersistent); overload;
    procedure SetSelections(const List: IDesignerSelections);
    procedure ShowMethod(const Name: string);
    procedure GetComponentNames(TypeData: PTypeData; Proc: TGetStrProc);
    function GetComponent(const Name: string): TComponent;
    function GetComponentName(Component: TComponent): string;
    function GetObject(const Name: string): TPersistent;
    function GetObjectName(Instance: TPersistent): string;
    procedure GetObjectNames(TypeData: PTypeData; Proc: TGetStrProc);
    function MethodFromAncestor(const Method: TMethod): Boolean;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent;
    function CreateCurrentComponent(Parent: TComponent; const Rect: TRect): TComponent;
    function IsComponentLinkable(Component: TComponent): Boolean;
    function IsComponentHidden(Component: TComponent): Boolean;
    procedure MakeComponentLinkable(Component: TComponent);
    procedure Revert(Instance: TPersistent; PropInfo: PPropInfo);
    function GetIsDormant: Boolean;
    procedure GetProjectModules(Proc: TGetModuleProc);
    function GetAncestorDesigner: IDesigner;
    function IsSourceReadOnly: Boolean;
    function GetScrollRanges(const ScrollPosition: TPoint): TPoint;
    procedure Edit(const Component: TComponent);
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string;
      TypeData: PTypeData); overload;
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string;
      const AEventInfo: IEventInfo); overload;
    procedure CopySelection;
    procedure CutSelection;
    function CanPaste: Boolean;
    procedure PasteSelection;
    procedure DeleteSelection(ADoAll: Boolean = False);
    procedure ClearSelection;
    procedure NoSelection;
    procedure ModuleFileNames(var ImplFileName, IntfFileName, FormFileName: string);
    function GetRootClassName: string;
    function UniqueName(const BaseName: string): string;
    function GetRoot: TComponent;
    function GetShiftState: TShiftState;
    procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);
    procedure SelectItemName(const PropertyName: string);
    procedure Resurrect;

    // IDesigner70
    function GetActiveClassGroup: TPersistentClass;
    function FindRootAncestor(const AClassName: string): TComponent;

    // IDesigner80
    function CreateMethod(const Name: string; const AEventInfo: IEventInfo): TMethod; overload;
    procedure GetMethods(const AEventInfo: IEventInfo; Proc: TGetStrProc); overload;
    procedure SelectComponent(const ADesignObject: IDesignObject); overload;

    // IDesigner100 = interface(IDesigner80)
    function GetDesignerExtension: string;

    // IDesigner
    function GetAppDataDirectory(Local: Boolean = False): string;
  end;


  { TComponentEditorProxy }

  TComponentEditorProxy = class(ComponentEditors.TComponentEditor)
  private
    FDelphiComponentEditor: TComponentEditor;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetComponent: TComponent; override;
    function GetCustomHint: String; override;
    function GetDesigner: TComponentEditorDesigner; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function IsInInlined: Boolean; override;
    procedure Copy; override;
    procedure PrepareItem({%H-}Index: Integer; const {%H-}AnItem: TMenuItem); override;
    function GetHook(out Hook: TPropertyEditorHook): boolean; override;
    procedure Modified; override;
  end;

{ TIDesignerProxy }

constructor TIDesignerProxy.Create(ADesigner: TComponentEditorDesigner);
begin
  inherited Create;
  FLazarusDesigner := ADesigner;
end;

procedure TIDesignerProxy.Activate;
begin

end;

procedure TIDesignerProxy.Modified;
begin
  FLazarusDesigner.Modified;
end;

function TIDesignerProxy.CreateMethod(const Name: string; TypeData: PTypeData
  ): TMethod;
begin
  FLazarusDesigner.PropertyEditorHook.CreateMethod(Name, TypeData.BaseType,
    FLazarusDesigner.LookupRoot, '');
end;

function TIDesignerProxy.GetMethodName(const Method: TMethod): string;
begin

end;

procedure TIDesignerProxy.GetMethods(TypeData: PTypeData; Proc: TGetStrProc);
begin

end;

function TIDesignerProxy.GetPathAndBaseExeName: string;
begin

end;

function TIDesignerProxy.GetPrivateDirectory: string;
begin

end;

function TIDesignerProxy.GetBaseRegKey: string;
begin

end;

function TIDesignerProxy.GetIDEOptions: TCustomIniFile;
begin

end;

procedure TIDesignerProxy.GetSelections(const List: IDesignerSelections);
begin

end;

function TIDesignerProxy.MethodExists(const Name: string): Boolean;
begin

end;

procedure TIDesignerProxy.RenameMethod(const CurName, NewName: string);
begin

end;

procedure TIDesignerProxy.SelectComponent(Instance: TPersistent);
begin

end;

procedure TIDesignerProxy.SetSelections(const List: IDesignerSelections);
begin

end;

procedure TIDesignerProxy.ShowMethod(const Name: string);
begin

end;

procedure TIDesignerProxy.GetComponentNames(TypeData: PTypeData;
  Proc: TGetStrProc);
begin

end;

function TIDesignerProxy.GetComponent(const Name: string): TComponent;
begin

end;

function TIDesignerProxy.GetComponentName(Component: TComponent): string;
begin

end;

function TIDesignerProxy.GetObject(const Name: string): TPersistent;
begin

end;

function TIDesignerProxy.GetObjectName(Instance: TPersistent): string;
begin

end;

procedure TIDesignerProxy.GetObjectNames(TypeData: PTypeData; Proc: TGetStrProc
  );
begin

end;

function TIDesignerProxy.MethodFromAncestor(const Method: TMethod): Boolean;
begin

end;

function TIDesignerProxy.CreateComponent(ComponentClass: TComponentClass;
  Parent: TComponent; Left, Top, Width, Height: Integer): TComponent;
begin

end;

function TIDesignerProxy.CreateCurrentComponent(Parent: TComponent;
  const Rect: TRect): TComponent;
begin

end;

function TIDesignerProxy.IsComponentLinkable(Component: TComponent): Boolean;
begin

end;

function TIDesignerProxy.IsComponentHidden(Component: TComponent): Boolean;
begin

end;

procedure TIDesignerProxy.MakeComponentLinkable(Component: TComponent);
begin

end;

procedure TIDesignerProxy.Revert(Instance: TPersistent; PropInfo: PPropInfo);
begin

end;

function TIDesignerProxy.GetIsDormant: Boolean;
begin

end;

procedure TIDesignerProxy.GetProjectModules(Proc: TGetModuleProc);
begin

end;

function TIDesignerProxy.GetAncestorDesigner: IDesigner;
begin

end;

function TIDesignerProxy.IsSourceReadOnly: Boolean;
begin

end;

function TIDesignerProxy.GetScrollRanges(const ScrollPosition: TPoint): TPoint;
begin

end;

procedure TIDesignerProxy.Edit(const Component: TComponent);
begin

end;

procedure TIDesignerProxy.ChainCall(const MethodName, InstanceName,
  InstanceMethod: string; TypeData: PTypeData);
begin

end;

procedure TIDesignerProxy.ChainCall(const MethodName, InstanceName,
  InstanceMethod: string; const AEventInfo: IEventInfo);
begin

end;

procedure TIDesignerProxy.CopySelection;
begin

end;

procedure TIDesignerProxy.CutSelection;
begin

end;

function TIDesignerProxy.CanPaste: Boolean;
begin

end;

procedure TIDesignerProxy.PasteSelection;
begin

end;

procedure TIDesignerProxy.DeleteSelection(ADoAll: Boolean);
begin

end;

procedure TIDesignerProxy.ClearSelection;
begin

end;

procedure TIDesignerProxy.NoSelection;
begin

end;

procedure TIDesignerProxy.ModuleFileNames(var ImplFileName, IntfFileName,
  FormFileName: string);
begin

end;

function TIDesignerProxy.GetRootClassName: string;
begin

end;

function TIDesignerProxy.UniqueName(const BaseName: string): string;
begin

end;

function TIDesignerProxy.GetRoot: TComponent;
begin

end;

function TIDesignerProxy.GetShiftState: TShiftState;
begin

end;

procedure TIDesignerProxy.ModalEdit(EditKey: Char;
  const ReturnWindow: IActivatable);
begin

end;

procedure TIDesignerProxy.SelectItemName(const PropertyName: string);
begin

end;

procedure TIDesignerProxy.Resurrect;
begin

end;

function TIDesignerProxy.GetActiveClassGroup: TPersistentClass;
begin

end;

function TIDesignerProxy.FindRootAncestor(const AClassName: string): TComponent;
begin

end;

function TIDesignerProxy.CreateMethod(const Name: string;
  const AEventInfo: IEventInfo): TMethod;
begin

end;

procedure TIDesignerProxy.GetMethods(const AEventInfo: IEventInfo;
  Proc: TGetStrProc);
begin

end;

procedure TIDesignerProxy.SelectComponent(const ADesignObject: IDesignObject);
begin

end;

function TIDesignerProxy.GetDesignerExtension: string;
begin

end;

function TIDesignerProxy.GetAppDataDirectory(Local: Boolean): string;
begin

end;

constructor TComponentEditorProxy.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  //TComponentEditor.Create();
end;

procedure TComponentEditorProxy.Edit;
begin
  inherited Edit;
end;

procedure TComponentEditorProxy.ExecuteVerb(Index: Integer);
begin
  inherited ExecuteVerb(Index);
end;

function TComponentEditorProxy.GetComponent: TComponent;
begin
  Result:=inherited GetComponent;
end;

function TComponentEditorProxy.GetCustomHint: String;
begin
  Result:=inherited GetCustomHint;
end;

function TComponentEditorProxy.GetDesigner: TComponentEditorDesigner;
begin
  Result:=inherited GetDesigner;
end;

function TComponentEditorProxy.GetVerb(Index: Integer): string;
begin
  Result:=inherited GetVerb(Index);
end;

function TComponentEditorProxy.GetVerbCount: Integer;
begin
  Result:=inherited GetVerbCount;
end;

function TComponentEditorProxy.IsInInlined: Boolean;
begin
  Result:=inherited IsInInlined;
end;

procedure TComponentEditorProxy.Copy;
begin
  inherited Copy;
end;

procedure TComponentEditorProxy.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
end;

function TComponentEditorProxy.GetHook(out Hook: TPropertyEditorHook): boolean;
begin
  Result:=inherited GetHook(Hook);
end;

procedure TComponentEditorProxy.Modified;
begin
  inherited Modified;
end;

{ TComponentEditor }

constructor TComponentEditor.Create(AComponent: TComponent; ADesigner: IDesigner
  );
begin
  inherited Create(AComponent, ADesigner);

  //FLazComponentEditors := ComponentEditors.TComponentEditor.Create(AComponent);
  FComponent := AComponent;
  FDesigner := ADesigner;
end;

procedure TComponentEditor.Edit;
begin

end;

procedure TComponentEditor.ExecuteVerb(Index: Integer);
begin

end;

function TComponentEditor.GetComponent: TComponent;
begin

end;

function TComponentEditor.GetDesigner: IDesigner;
begin

end;

function TComponentEditor.GetVerb(Index: Integer): string;
begin

end;

function TComponentEditor.GetVerbCount: Integer;
begin

end;

function TComponentEditor.IsInInlined: Boolean;
begin

end;

procedure TComponentEditor.Copy;
begin

end;

procedure TComponentEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin

end;

procedure LazRegisterComponentEditor(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);
begin

end;

initialization
  DesignIntf.RegisterComponentEditorProc := LazRegisterComponentEditor;
finalization
  DesignIntf.RegisterComponentEditorProc := nil;
end.

