{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Abstract:
   Under construction by Mattias
 
 
   Interface unit for IDE commands.
   IDE commands are functions like open file, save, build, ... .
   
   Every command can have up to two shortcuts. For example:
     ecCopy: two shortcuts: Ctrl+C and Ctrl+Insert
     ecDeleteChar: one shortcut: Delete
     ecInsertDateTime: no shortcut
   
   Commands are sorted into categories. For example:
     ecCopy is in the category 'Selection'.
     This is only to help the user find commands.
     
   Scopes:
     A command can work globally or only in some IDE windows.
     For example: When the user presses a key in the source editor, the IDE
       first searches in all commands with the Scope IDECmdScopeSrcEdit.
       Then it will search in all commands without scope.
}
unit IDECommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, LCLType, Menus, TextTools;
  
type
  TIDECommand = class;
  TIDECommandCategory = class;

  { TIDECommandScope
    A TIDECommandScope defines a set of IDE windows that will share the same
    IDE commands. An IDE command can be valid in several scopes at the same
    time. }
    
  { TIDECommandScope }

  TIDECommandScope = class(TPersistent)
  private
    FName: string;
    FIDEWindowClasses: TFPList;// list of TCustomFormClass
    FCategories: TFPList;
    function GetCategories(Index: integer): TIDECommandCategory;
    function GetIDEWindowClasses(Index: integer): TCustomFormClass;
  public
    constructor Create(const TheName: string);
    destructor Destroy; override;
    procedure AddWindowClass(AWindowClass: TCustomFormClass);
    procedure RemoveWindowClass(AWindowClass: TCustomFormClass);
    function IDEWindowClassCount: integer;
    function CategoryCount: integer;
    function HasIDEWindowClass(AWindowClass: TCustomFormClass): boolean;
    function Intersects(AScope: TIDECommandScope): boolean;
    procedure WriteDebugReport;
  public
    property Name: string read FName;
    property IDEWindowClasses[Index: integer]: TCustomFormClass read GetIDEWindowClasses;
    property Categories[Index: integer]: TIDECommandCategory read GetCategories;
  end;

  { TIDECommandScopes }

  TIDECommandScopes = class(TPersistent)
  private
    FItems: TFPList;
    function GetItems(Index: integer): TIDECommandScope;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(NewItem: TIDECommandScope);
    function IndexOf(AnItem: TIDECommandScope): Integer;
    function IndexByName(const AName: string): Integer;
    function FindByName(const AName: string): TIDECommandScope;
    function CreateUniqueName(const AName: string): string;
    function Count: integer;
  public
    property Items[Index: integer]: TIDECommandScope read GetItems;
  end;
  

  { TIDEShortCut }

  TIDEShortCut = record
    Key1: word;
    Shift1: TShiftState;
    Key2: word;
    Shift2: TShiftState;
  end;
  PIDEShortCut = ^TIDEShortCut;

  { TIDECommandCategory
    TIDECommandCategory is used to divide the commands in handy packets }
    
  { TIDECommandCategory }

  TIDECommandCategory = class(TList)
  protected
    FDescription: string;
    FName: string;
    FParent: TIDECommandCategory;
    FScope: TIDECommandScope;
    procedure SetScope(const AValue: TIDECommandScope);
  public
    destructor Destroy; override;
    function ScopeIntersects(AScope: TIDECommandScope): boolean;
    procedure WriteScopeDebugReport;
  public
    property Name: string read FName;
    property Description: string read FDescription;
    property Parent: TIDECommandCategory read FParent;
    procedure Delete(Index: Integer); virtual;
    property Scope: TIDECommandScope read FScope write SetScope;
  end;
  
  
  { TIDECommand }
  { class for storing the keys of a single command
    (shortcut-command relationship) }
  TIDECommand = class
  private
    FCategory: TIDECommandCategory;
    FCommand: word;
    FLocalizedName: string;
    FName: String;
    FShortcutA: TIDEShortCut;
    FShortcutB: TIDEShortCut;
  protected
    function GetLocalizedName: string; virtual;
    procedure SetLocalizedName(const AValue: string); virtual;
    procedure SetCategory(const AValue: TIDECommandCategory); virtual;
    procedure SetShortcutA(const AValue: TIDEShortCut); virtual;
    procedure SetShortcutB(const AValue: TIDEShortCut); virtual;
  public
    function AsShortCut: TShortCut; virtual;
    constructor Create(TheCategory: TIDECommandCategory; const TheName: String;
      TheCommand: word; const TheShortcutA, TheShortcutB: TIDEShortCut);
  public
    DefaultShortcutA: TIDEShortCut;
    DefaultShortcutB: TIDEShortCut;
    procedure ClearShortcutA;
    procedure ClearShortcutB;
    function GetCategoryAndName: string;
  public
    property Name: String read FName;
    property Command: word read FCommand;// see the ecXXX constants in ../ide/keymapping.pp
    property LocalizedName: string read GetLocalizedName write SetLocalizedName;
    property Category: TIDECommandCategory read FCategory write SetCategory;
    property ShortcutA: TIDEShortCut read FShortcutA write SetShortcutA;
    property ShortcutB: TIDEShortCut read FShortcutB write SetShortcutB;
  end;


  { TIDECommands }

  TIDECommands = class
  protected
    function GetCategory(Index: integer): TIDECommandCategory; virtual; abstract;
  public
    function FindIDECommand(ACommand: word): TIDECommand; virtual; abstract;
    function CreateCategory(Parent: TIDECommandCategory;
                            const Name, Description: string;
                            Scope: TIDECommandScope = nil): TIDECommandCategory; virtual; abstract;
    function CreateCommand(Category: TIDECommandCategory;
                           const Name, Description: string;
                           const TheShortcutA, TheShortcutB: TIDEShortCut
                           ): TIDECommand; virtual; abstract;
    function CategoryCount: integer; virtual; abstract;
  public
    property Categories[Index: integer]: TIDECommandCategory read GetCategory;
  end;

const
  CleanIDEShortCut: TIDEShortCut =
    (Key1: VK_UNKNOWN; Shift1: []; Key2: VK_UNKNOWN; Shift2: []);

function IDEShortCut(Key1: word; Shift1: TShiftState;
  Key2: word = VK_UNKNOWN; Shift2: TShiftState = []): TIDEShortCut;


type
  TExecuteIDEShortCut =
    procedure(Sender: TObject; var Key: word; Shift: TShiftState;
              IDEWindowClass: TCustomFormClass) of object;
  TExecuteIDECommand = procedure(Sender: TObject; Command: word) of object;

var
  OnExecuteIDEShortCut: TExecuteIDEShortCut;
  OnExecuteIDECommand: TExecuteIDECommand;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState;
  IDEWindowClass: TCustomFormClass);
procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState);
procedure ExecuteIDECommand(Sender: TObject; Command: word);

function IDEShortCutToMenuShortCut(const IDEShortCut: TIDEShortCut): TShortCut;

var
  // will be set by the IDE
  IDECommandList: TIDECommands;
  IDECommandScopes: TIDECommandScopes = nil;
var
  IDECmdScopeSrcEdit: TIDECommandScope;
  IDECmdScopeSrcEditOnly: TIDECommandScope;
  IDECmdScopeDesignerOnly: TIDECommandScope;

// register a new IDE command category (i.e. set of commands)
function RegisterIDECommandCategory(Parent: TIDECommandCategory;
                          const Name, Description: string): TIDECommandCategory;

// register a new IDE command (i.e. a shortcut, IDE function)
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string): TIDECommand;
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string; Key1: word; Shift1: TShiftState): TIDECommand;
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string; const ShortCut1: TIDEShortCut): TIDECommand;
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  const ShortCut1, ShortCut2: TIDEShortCut): TIDECommand;

// register a new IDE command scope (i.e. a set of windows)
function RegisterIDECommandScope(const Name: string): TIDECommandScope;

procedure CreateStandardIDECommandScopes;


function CompareIDEShortCuts(Data1, Data2: Pointer): integer;
function CompareIDEShortCutKey1s(Data1, Data2: Pointer): integer;

implementation


function IDEShortCut(Key1: word; Shift1: TShiftState;
  Key2: word; Shift2: TShiftState): TIDEShortCut;
begin
  Result.Key1:=Key1;
  Result.Shift1:=Shift1;
  Result.Key2:=Key2;
  Result.Shift2:=Shift2;
end;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState;
  IDEWindowClass: TCustomFormClass);
begin
  if (OnExecuteIDECommand<>nil) and (Key<>VK_UNKNOWN) then
    OnExecuteIDEShortCut(Sender,Key,Shift,IDEWindowClass);
end;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  OnExecuteIDEShortCut(Sender,Key,Shift,nil);
end;

procedure ExecuteIDECommand(Sender: TObject; Command: word);
begin
  if (OnExecuteIDECommand<>nil) and (Command<>0) then
    OnExecuteIDECommand(Sender,Command);
end;

function IDEShortCutToMenuShortCut(const IDEShortCut: TIDEShortCut): TShortCut;
begin
  if IDEShortCut.Key2=VK_UNKNOWN then
    Result:=ShortCut(IDEShortCut.Key1,IDEShortCut.Shift1)
  else
    Result:=ShortCut(VK_UNKNOWN,[]);
end;

procedure CreateStandardIDECommandScopes;
begin
  IDECommandScopes:=TIDECommandScopes.Create;
  IDECmdScopeSrcEdit:=RegisterIDECommandScope('SourceEditor');
  IDECmdScopeSrcEditOnly:=RegisterIDECommandScope('SourceEditorOnly');
  IDECmdScopeDesignerOnly:=RegisterIDECommandScope('DesignerOnly');
end;

function CompareIDEShortCuts(Data1, Data2: Pointer): integer;
var
  ShortCut1: PIDEShortCut;
  ShortCut2: PIDEShortCut;
begin
  ShortCut1:=PIDEShortCut(Data1);
  ShortCut2:=PIDEShortCut(Data2);
  if ShortCut1^.Key1>ShortCut2^.Key1 then
    Result:=1
  else if ShortCut1^.Key1<ShortCut2^.Key1 then
    Result:=-1
  else if integer(ShortCut1^.Shift1)>integer(ShortCut2^.Shift1) then
    Result:=1
  else if integer(ShortCut1^.Shift1)<integer(ShortCut2^.Shift1) then
    Result:=-1
  else if ShortCut1^.Key2>ShortCut2^.Key2 then
    Result:=1
  else if ShortCut1^.Key2<ShortCut2^.Key2 then
    Result:=-1
  else if integer(ShortCut1^.Shift2)>integer(ShortCut2^.Shift2) then
    Result:=1
  else if integer(ShortCut1^.Shift2)<integer(ShortCut2^.Shift2) then
    Result:=-1
  else
    Result:=0;
end;

function CompareIDEShortCutKey1s(Data1, Data2: Pointer): integer;
var
  ShortCut1: PIDEShortCut;
  ShortCut2: PIDEShortCut;
begin
  ShortCut1:=PIDEShortCut(Data1);
  ShortCut2:=PIDEShortCut(Data2);
  if ShortCut1^.Key1>ShortCut2^.Key1 then
    Result:=1
  else if ShortCut1^.Key1<ShortCut2^.Key1 then
    Result:=-1
  else if integer(ShortCut1^.Shift1)>integer(ShortCut2^.Shift1) then
    Result:=1
  else if integer(ShortCut1^.Shift1)<integer(ShortCut2^.Shift1) then
    Result:=-1
  else
    Result:=0;
end;

function RegisterIDECommandCategory(Parent: TIDECommandCategory;
  const Name, Description: string): TIDECommandCategory;
begin
  Result:=IDECommandList.CreateCategory(Parent,Name,Description);
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string): TIDECommand;
begin
  Result:=RegisterIDECommand(Category,Name,Description,IDEShortCut(VK_UNKNOWN,[]));
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  Key1: word; Shift1: TShiftState): TIDECommand;
begin
  Result:=RegisterIDECommand(Category,Name,Description,IDEShortCut(Key1,Shift1));
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;  const ShortCut1: TIDEShortCut): TIDECommand;
begin
  Result:=RegisterIDECommand(Category,Name,Description,
                             ShortCut1,IDEShortCut(VK_UNKNOWN,[]));
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  const ShortCut1, ShortCut2: TIDEShortCut): TIDECommand;
begin
  Result:=IDECommandList.CreateCommand(Category,Name,Description,
                                       ShortCut1,ShortCut2);
end;

function RegisterIDECommandScope(const Name: string): TIDECommandScope;
begin
  Result:=TIDECommandScope.Create(Name);
  IDECommandScopes.Add(Result);
end;

{ TIDECommand }

procedure TIDECommand.SetShortcutA(const AValue: TIDEShortCut);
begin
  if CompareIDEShortCuts(@FShortcutA,@AValue)=0 then exit;
  FShortcutA:=AValue;
end;

procedure TIDECommand.SetShortcutB(const AValue: TIDEShortCut);
begin
  if CompareIDEShortCuts(@FShortcutB,@AValue)=0 then exit;
  FShortcutB:=AValue;
end;

function TIDECommand.GetLocalizedName: string;
begin
  if FLocalizedName<>'' then
    Result:=FLocalizedName
  else
    Result:=Name;
end;

procedure TIDECommand.SetLocalizedName(const AValue: string);
begin
  if FLocalizedName=AValue then exit;
  FLocalizedName:=AValue;
end;

procedure TIDECommand.SetCategory(const AValue: TIDECommandCategory);
begin
  if FCategory=AValue then exit;
  // unbind
  if Category<>nil then
    Category.Remove(Self);
  // bind
  fCategory:=AValue;
  if Category<>nil then
    Category.Add(Self);
end;

function TIDECommand.AsShortCut: TShortCut;
var
  CurKey: TIDEShortCut;
begin
  if (ShortcutA.Key1<>VK_UNKNOWN) and (ShortcutA.Key2=VK_UNKNOWN) then
    CurKey:=ShortcutA
  else if (ShortcutB.Key1<>VK_UNKNOWN) and (ShortcutB.Key2=VK_UNKNOWN) then
    CurKey:=ShortcutB
  else
    CurKey:=CleanIDEShortCut;
  Result:=CurKey.Key1;
  if ssCtrl in CurKey.Shift1 then
    Result:=Result+scCtrl;
  if ssShift in CurKey.Shift1 then
    Result:=Result+scShift;
  if ssAlt in CurKey.Shift1 then
    Result:=Result+scAlt;
end;

constructor TIDECommand.Create(TheCategory: TIDECommandCategory;
  const TheName: String; TheCommand: word;
  const TheShortcutA, TheShortcutB: TIDEShortCut);
begin
  fCommand:=TheCommand;
  fName:=TheName;
  ShortcutA:=TheShortcutA;
  ShortcutB:=TheShortcutB;
  DefaultShortcutA:=ShortcutA;
  DefaultShortcutB:=ShortcutB;
  Category:=TheCategory;
end;

procedure TIDECommand.ClearShortcutA;
begin
  ShortcutA:=CleanIDEShortCut;
end;

procedure TIDECommand.ClearShortcutB;
begin
  ShortcutB:=CleanIDEShortCut;
end;

function TIDECommand.GetCategoryAndName: string;
begin
  Result:='"'+GetLocalizedName+'"';
  if Category<>nil then
    Result:=Result+' in "'+Category.Description+'"';
end;

{ TIDECommandScopes }

function TIDECommandScopes.GetItems(Index: integer): TIDECommandScope;
begin
  Result:=TIDECommandScope(FItems[Index]);
end;

constructor TIDECommandScopes.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TIDECommandScopes.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TIDECommandScopes.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TIDECommandScopes.Add(NewItem: TIDECommandScope);
begin
  NewItem.fName:=CreateUniqueName(NewItem.Name);
  FItems.Add(NewItem);
end;

function TIDECommandScopes.IndexOf(AnItem: TIDECommandScope): Integer;
begin
  Result:=FItems.IndexOf(AnItem);
end;

function TIDECommandScopes.IndexByName(const AName: string): Integer;
begin
  Result:=Count-1;
  while (Result>=0) and (CompareText(AName,Items[Result].Name)<>0) do
    dec(Result);
end;

function TIDECommandScopes.FindByName(const AName: string): TIDECommandScope;
var
  i: LongInt;
begin
  i:=IndexByName(AName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDECommandScopes.CreateUniqueName(const AName: string): string;
begin
  Result:=AName;
  if IndexByName(Result)<0 then exit;
  Result:=CreateFirstIdentifier(Result);
  while IndexByName(Result)>=0 do
    Result:=CreateNextIdentifier(Result);
end;

function TIDECommandScopes.Count: integer;
begin
  Result:=FItems.Count;
end;

{ TIDECommandCategory }

procedure TIDECommandCategory.SetScope(const AValue: TIDECommandScope);
begin
  if FScope=AValue then exit;
  if FScope<>nil then
    FScope.FCategories.Remove(Self);
  FScope:=AValue;
  if FScope<>nil then
    FScope.FCategories.Add(Self);
end;

destructor TIDECommandCategory.Destroy;
begin
  Scope:=nil;
  inherited Destroy;
end;

function TIDECommandCategory.ScopeIntersects(AScope: TIDECommandScope
  ): boolean;
begin
  if (Scope=nil) or (AScope=nil) then
    Result:=true
  else
    Result:=Scope.Intersects(AScope);
end;

procedure TIDECommandCategory.WriteScopeDebugReport;
begin
  debugln('TIDECommandCategory.WriteScopeDebugReport ',Name,'=',Description);
  if Scope<>nil then
    Scope.WriteDebugReport
  else
    debugln('  Scope=nil');
end;

procedure TIDECommandCategory.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

{ TIDECommandScope }

function TIDECommandScope.GetCategories(Index: integer): TIDECommandCategory;
begin
  Result:=TIDECommandCategory(FCategories[Index]);
end;

function TIDECommandScope.GetIDEWindowClasses(Index: integer): TCustomFormClass;
begin
  Result:=TCustomFormClass(FIDEWindowClasses[Index]);
end;

constructor TIDECommandScope.Create(const TheName: string);
begin
  FName:=TheName;
  FIDEWindowClasses:=TFPList.Create;
  FCategories:=TFPList.Create;
end;

destructor TIDECommandScope.Destroy;
var
  i: Integer;
begin
  for i:=FCategories.Count-1 downto 0 do
    Categories[i].Scope:=nil;
  FreeAndNil(FIDEWindowClasses);
  FreeAndNil(FCategories);
  inherited Destroy;
end;

procedure TIDECommandScope.AddWindowClass(AWindowClass: TCustomFormClass);
begin
  if FIDEWindowClasses.IndexOf(AWindowClass)>=0 then
    RaiseGDBException('TIDECommandScope.AddWindowClass');
  FIDEWindowClasses.Add(AWindowClass);
end;

procedure TIDECommandScope.RemoveWindowClass(AWindowClass: TCustomFormClass);
begin
  FIDEWindowClasses.Remove(AWindowClass);
end;

function TIDECommandScope.IDEWindowClassCount: integer;
begin
  Result:=FIDEWindowClasses.Count;
end;

function TIDECommandScope.CategoryCount: integer;
begin
  Result:=FCategories.Count;
end;

function TIDECommandScope.HasIDEWindowClass(AWindowClass: TCustomFormClass
  ): boolean;
var
  i: Integer;
begin
  if AWindowClass<>nil then begin
    for i:=0 to FIDEWindowClasses.Count-1 do begin
      if (FIDEWindowClasses[i]=nil)
      or AWindowClass.InheritsFrom(TCustomFormClass(FIDEWindowClasses[i])) then
        exit(true);
    end;
  end else begin
    if FIDEWindowClasses.IndexOf(nil)>=0 then
      exit(true);
  end;
  Result:=false;
end;

function TIDECommandScope.Intersects(AScope: TIDECommandScope): boolean;
var
  i: Integer;
  CurClass: TCustomFormClass;
begin
  if AScope=nil then
    Result:=true
  else begin
    for i:=0 to FIDEWindowClasses.Count-1 do begin
      CurClass:=TCustomFormClass(FIDEWindowClasses[i]);
      if (CurClass=nil)
      or (AScope.FIDEWindowClasses.IndexOf(FIDEWindowClasses[i])>=0) then
        exit(true);
    end;
    Result:=false;
  end;
end;

procedure TIDECommandScope.WriteDebugReport;
var
  i: Integer;
begin
  debugln('TIDECommandScope.WriteDebugReport ',Name);
  for i:=0 to FIDEWindowClasses.Count-1 do begin
    if FIDEWindowClasses[i]=nil then
      debugln('  ',dbgs(i),'/',dbgs(FIDEWindowClasses.Count),' nil')
    else
      debugln('  ',dbgs(i),'/',dbgs(FIDEWindowClasses.Count),' ',TClass(FIDEWindowClasses[i]).ClassName);
  end;
end;

end.

