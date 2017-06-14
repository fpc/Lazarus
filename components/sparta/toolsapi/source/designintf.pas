unit DesignIntf;

{$mode objfpc}{$H+}

interface

uses
  TypInfo, Classes, SysUtils, IniFiles, DesignMenus;

type
  // this one should be moved to Classes
  TGetModuleProc = procedure(const FileName, UnitName, FormName,
    DesignClass: string; CoClasses: TStrings) of object;

  IEventInfo = interface
    ['{C3A5B0FD-37C6-486B-AD29-642C51928787}']
    function GetMethodKind: TMethodKind;
    function GetParamCount: Integer;
    function GetParamName(Index: Integer): string;
    function GetParamType(Index: Integer): string;
    function GetParamFlags(Index: Integer): TParamFlags;
    function GetResultType: string;
  end;

  IClass = interface
    ['{94CD802C-3E83-4C38-AB36-1CD9DB196519}']
    function ClassNameIs(const AClassName: string): Boolean;
    function GetClassName: string;
    function GetUnitName: string;
    function GetClassParent: IClass;

    property ClassName: string read GetClassName;
    property ClassParent: IClass read GetClassParent;
    property UnitName: string read GetUnitName;
  end;

  IActivatable = interface
    ['{F00AA4BD-3459-43E9-ACB2-97DBD1663AFF}']
    procedure Activate;
  end;

  IDesignObject = interface
    ['{B1648433-D671-4D5E-B49F-26740D4EB360}']
    function Equals(Obj: TObject): Boolean; overload;
    function Equals(const ADesignObject: IDesignObject): Boolean; overload;
    function GetClassType: IClass;
    function GetClassName: string;
    function GetComponentIndex: Integer;
    function GetComponentName: string;
    function GetIsComponent: Boolean;
    function GetNamePath: string;

    property ClassType: IClass read GetClassType;
    property ClassName: string read GetClassName;
    property ComponentIndex: Integer read GetComponentIndex;
    property ComponentName: string read GetComponentName;
    property IsComponent: Boolean read GetIsComponent;
    property NamePath: string read GetNamePath;
  end;

  IDesignPersistent = interface(IDesignObject)
    ['{8858E03D-5B6A-427A-BFFC-4A9B8198FB13}']
    function GetPersistent: TPersistent;

    property Persistent: TPersistent read GetPersistent;
  end;

  IDesignerSelections = interface
    ['{7ED7BF30-E349-11D3-AB4A-00C04FB17A72}']
    function Add(const Item: TPersistent): Integer;
    function Equals(const List: IDesignerSelections): Boolean;
    function Get(Index: Integer): TPersistent;
    function GetDesignObject(Index: Integer): IDesignObject;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPersistent read Get; default;
    property DesignObjects[Index: Integer]: IDesignObject read GetDesignObject;
  end;

  IDesigner = interface;
  IDesigner60 = interface
    ['{A29C6480-D4AF-11D3-BA96-0080C78ADCDB}']
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

    property Root: TComponent read GetRoot;
    property IsDormant: Boolean read GetIsDormant;
    property AncestorDesigner: IDesigner read GetAncestorDesigner;
  end;

  IDesigner70 = interface(IDesigner60)
    ['{2F704CE2-7614-4AAF-B177-357D00D9634B}']
    function GetActiveClassGroup: TPersistentClass;

    function FindRootAncestor(const AClassName: string): TComponent;
    property ActiveClassGroup: TPersistentClass read GetActiveClassGroup;
  end;

  IDesigner80 = interface(IDesigner70)
    ['{BCE34322-B22A-4494-BEA5-5B2B9754DE36}']
    function CreateMethod(const Name: string; const AEventInfo: IEventInfo): TMethod; overload;
    procedure GetMethods(const AEventInfo: IEventInfo; Proc: TGetStrProc); overload;
    procedure SelectComponent(const ADesignObject: IDesignObject); overload;
  end;

  IDesigner100 = interface(IDesigner80)
    ['{55501C77-FE8D-4844-A407-A7F90F7D5303}']
    function GetDesignerExtension: string;

    property DesignerExtention: string read GetDesignerExtension;
  end;

  IDesigner = interface(IDesigner100)
    ['{17ACD4A3-ED00-483E-8480-B5FBD4589440}']
    function GetAppDataDirectory(Local: Boolean = False): string;
  end;

  IComponentEditor = interface
    ['{ECACBA34-DCDF-4BE2-A645-E4404BC06106}']
    procedure Edit;
    procedure ExecuteVerb(Index: Integer);
    function GetVerb(Index: Integer): string;
    function GetVerbCount: Integer;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem);
    procedure Copy;
    function IsInInlined: Boolean;
    function GetComponent: TComponent;
    function GetDesigner: IDesigner;
  end;

  { TBaseComponentEditor }

  TBaseComponentEditor = class(TInterfacedObject)
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); virtual;
  end;

  TComponentEditorClass = class of TBaseComponentEditor;
  TRegisterComponentEditorProc = procedure (ComponentClass: TComponentClass;
    ComponentEditor: TComponentEditorClass);

var
  RegisterComponentEditorProc: TRegisterComponentEditorProc;

procedure RegisterComponentEditor(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);

implementation

procedure RegisterComponentEditor(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);
begin
  if Assigned(RegisterComponentEditorProc) then
    RegisterComponentEditorProc(ComponentClass, ComponentEditor);
end;

{ TBaseComponentEditor }

constructor TBaseComponentEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
begin
  inherited Create;
end;

end.

