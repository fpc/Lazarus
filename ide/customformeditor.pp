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
  Classes, SysUtils, TypInfo, Math, Controls, Forms, Menus, Dialogs,
  // components
  AVL_Tree, PropEdits, ObjectInspector, IDECommands,
  // IDE
  JITForms, NonControlForms, FormEditingIntf, ComponentReg, IDEProcs,
  ComponentEditors, KeyMapping, EditorOptions, DesignerProcs;

Const OrdinalTypes = [tkInteger,tkChar,tkENumeration,tkbool];

type
{
TComponentInterface is derived from TIComponentInterface.  It gives access to
each control that's dropped onto the form
}

  TCustomFormEditor = class; //forward declaration


  TComponentInterface = class(TIComponentInterface)
  private
    FComponent : TComponent;
    FComponentEditor: TBaseComponentEditor;
    FDesigner: TComponentEditorDesigner;
    FFormEditor : TCustomFormEditor;  //used to call it's functions
    Function FSetProp(PRI : PPropInfo; const Value) : Boolean;
    Function FGetProp(PRI : PPropInfo; var Value) : Boolean;
    function GetDesigner: TComponentEditorDesigner;

  protected
    Function GetPPropInfobyIndex(Index : Integer) : PPropInfo;
    Function GetPPropInfobyName(Name : ShortString) : PPropInfo;

  public
    constructor Create;
    constructor Create(AComponent: TComponent);
    destructor Destroy; override;

    Function GetComponentType: ShortString; override;
    Function GetComponentHandle: LongInt; override;
    Function GetParent: TIComponentInterface; override;
    Function IsTControl: Boolean; override;
    Function GetPropCount: Integer; override;
    Function GetPropType(Index: Integer): TTypeKind; override;
    Function GetPropTypeInfo(Index: Integer): PTypeInfo;
    Function GetPropName(Index: Integer): ShortString; override;
    Function GetPropTypeName(Index: Integer): ShortString; override;
    Function GetPropTypebyName(Name: ShortString): TTypeKind; override;

    Function GetPropValue(Index : Integer; var Value) : Boolean; override;
    Function GetPropValuebyName(Name: ShortString; var Value) : Boolean; override;
    Function SetProp(Index : Integer; const Value) : Boolean; override;
    Function SetPropbyName(Name : ShortString; const Value) : Boolean; override;

    Function GetControlCount: Integer; override;
    Function GetControl(Index : Integer): TIComponentInterface; override;

    Function GetComponentCount: Integer; override;
    Function GetComponent(Index : Integer): TIComponentInterface; override;

    Function Select : Boolean; override;
    Function Focus : Boolean; override;
    Function Delete : Boolean; override;
    
    function GetComponentEditor: TBaseComponentEditor;
    property Designer: TComponentEditorDesigner read GetDesigner write FDesigner;
    
    property Component : TComponent read FComponent;
  end;


  { TCustomFormEditor }

  TControlClass = class of TControl;

  TCustomFormEditor = class(TAbstractFormEditor)
  private
    FComponentInterfaces: TAVLTree; // tree of TComponentInterface sorted for
                                    // component
    FSelection: TPersistentSelectionList;
    FObj_Inspector: TObjectInspector;
    function GetPropertyEditorHook: TPropertyEditorHook;
  protected
    FNonControlForms: TAVLTree; // tree of TNonControlForm sorted for LookupRoot
    procedure SetSelection(const ASelection: TPersistentSelectionList);
    procedure OnObjectInspectorModified(Sender: TObject);
    procedure SetObj_Inspector(AnObjectInspector: TObjectInspector); virtual;
    procedure JITListReaderError(Sender: TObject; ErrorType: TJITFormError;
          var Action: TModalResult); virtual;
    procedure JITListPropertyNotFound(Sender: TObject; Reader: TReader;
      Instance: TPersistent; var PropName: string; IsPath: boolean;
      var Handled, Skip: Boolean);

    procedure OnDesignerMenuItemClick(Sender: TObject); virtual;
    function FindNonControlFormNode(LookupRoot: TComponent): TAVLTreeNode;
  public
    JITFormList: TJITForms;// designed forms
    JITDataModuleList: TJITDataModules;// designed data modules

    constructor Create;
    destructor Destroy; override;

    Function AddSelected(Value : TComponent) : Integer;
    Procedure DeleteComponent(AComponent: TComponent; FreeComponent: boolean);
    Function FindComponentByName(const Name : ShortString) : TIComponentInterface; override;
    Function FindComponent(AComponent: TComponent): TIComponentInterface; override;
    
    function IsJITComponent(AComponent: TComponent): boolean;
    function GetJITListOfType(AncestorType: TComponentClass): TJITComponentList;
    function FindJITList(AComponent: TComponent): TJITComponentList;
    function GetDesignerForm(AComponent: TComponent): TCustomForm;
    function FindNonControlForm(LookupRoot: TComponent): TNonControlForm;
    function CreateNonControlForm(LookupRoot: TComponent): TNonControlForm;
    procedure RenameJITComponent(AComponent: TComponent;
                                 const NewName: shortstring);
    procedure UpdateDesignerFormName(AComponent: TComponent);
    function CreateNewJITMethod(AComponent: TComponent;
                                const AMethodName: shortstring): TMethod;
    procedure RenameJITMethod(AComponent: TComponent;
                           const OldMethodName, NewMethodName: shortstring);
    procedure SaveHiddenDesignerFormProperties(AComponent: TComponent);
    
    function DesignerCount: integer; override;
    function GetDesigner(Index: integer): TIDesigner; override;

    function GetComponentEditor(AComponent: TComponent): TBaseComponentEditor;
    function CreateUniqueComponentName(AComponent: TComponent): string;
    function CreateUniqueComponentName(const AClassName: string;
                                       OwnerComponent: TComponent): string;
    Function CreateComponentInterface(AComponent: TComponent): TIComponentInterface;
    procedure CreateChildComponentInterfaces(AComponent: TComponent);
    Function CreateComponent(ParentCI : TIComponentInterface;
                             TypeClass: TComponentClass;
                             X,Y,W,H : Integer): TIComponentInterface; override;
    Function CreateComponentFromStream(BinStream: TStream;
                       AncestorType: TComponentClass;
                       Interactive: boolean): TIComponentInterface; override;
    Function CreateChildComponentFromStream(BinStream: TStream;
                       ComponentClass: TComponentClass; Root: TComponent;
                       ParentControl: TWinControl): TIComponentInterface; override;
    Procedure SetComponentNameAndClass(CI: TIComponentInterface;
      const NewName, NewClassName: shortstring);
    Procedure ClearSelected;
    
    function TranslateKeyToDesignerCommand(Key: word; Shift: TShiftState): word;
  public
    property Selection: TPersistentSelectionList read FSelection
                                                 write SetSelection;
    property Obj_Inspector: TObjectInspector
                                     read FObj_Inspector write SetObj_Inspector;
    property PropertyEditorHook: TPropertyEditorHook read GetPropertyEditorHook;
  end;
  
  
function CompareComponentInterfaces(Data1, Data2: Pointer): integer;
function CompareComponentAndInterface(Key, Data: Pointer): integer;

implementation


function CompareComponentInterfaces(Data1, Data2: Pointer): integer;
var
  CompIntf1: TComponentInterface;
  CompIntf2: TComponentInterface;
begin
  CompIntf1:=TComponentInterface(Data1);
  CompIntf2:=TComponentInterface(Data2);
  Result:=integer(CompIntf1.Component)-integer(CompIntf2.Component);
end;

function CompareComponentAndInterface(Key, Data: Pointer): integer;
var
  AComponent: TComponent;
  CompIntf: TComponentInterface;
begin
  AComponent:=TComponent(Key);
  CompIntf:=TComponentInterface(Data);
  Result:=integer(AComponent)-integer(CompIntf.Component);
end;

{TComponentInterface}

constructor TComponentInterface.Create;
begin
  inherited Create;
end;

constructor TComponentInterface.Create(AComponent: TComponent);
begin
  inherited Create;
  FComponent:=AComponent;
end;

destructor TComponentInterface.Destroy;
begin
  FreeAndNil(FComponentEditor);
  inherited Destroy;
end;

Function TComponentInterface.FSetProp(PRI : PPropInfo;
const Value) : Boolean;
Begin
//writeln('Index = '+inttostr(PRI^.index));
  case PRI^.PropType^.Kind of
  tkBool: Begin
             //Writeln('Boolean....');
             SetOrdProp(FComponent,PRI,longint(Value));
             Result := True;
             end;
  tkSString,
  tkLString,
  tkAString,
  tkWString : Begin
              //Writeln('String...');
              SetStrProp(FComponent,PRI,ShortString(Value));
              Result := True;
             end;
  tkInteger,
  tkInt64   : Begin
              //Writeln('Int64...');
              SetInt64Prop(FComponent,PRI,Int64(Value));
              Result := True;
             end;
  tkFloat  : Begin
              //Writeln('Float...');
              SetFloatProp(FComponent,PRI,Extended(Value));
              Result := True;
             end;
  tkVariant  : Begin
              //Writeln('Variant...');
              SetVariantProp(FComponent,PRI,Variant(Value));
              Result := True;
             end;
  tkMethod  : Begin
              //Writeln('Method...');
              SetMethodProp(FComponent,PRI,TMethod(value));
              Result := True;
             end;
  else
    Result := False;
  end;//case
end;

Function TComponentInterface.FGetProp(PRI : PPropInfo; var Value) : Boolean;
Begin
Result := True;
       case PRI^.PropType^.Kind of
       tkBool    : Longint(Value) := GetOrdProp(FComponent,PRI);
       tkSString,
       tkLString,
       tkAString,
       tkWString : Begin
                    //Writeln('Get String...');
                    ShortString(Value) := GetStrProp(FComponent,PRI);
                    Writeln('The string returned is '+String(value));
                    Writeln('*Get String...');
                   end;
       tkInteger,
       tkInt64   : Begin
                    //Writeln('Get Int64...');
                    Int64(Value) := GetInt64Prop(FComponent,PRI);
                   end;
       tkFloat  : Begin
                    //Writeln('Get Float...');
                    Extended(Value) := GetFloatProp(FComponent,PRI);
                   end;
       tkVariant  : Begin
                    //Writeln('Get Variant...');
                    Variant(Value) := GetVariantProp(FComponent,PRI);
                   end;
       tkMethod  : Begin
                    //Writeln('Get Method...');
                    TMethod(Value) := GetMethodProp(FComponent,PRI);
                   end;
         else
          Result := False;
       end;//case
end;

function TComponentInterface.GetDesigner: TComponentEditorDesigner;
var
  DesignerForm: TCustomForm;
begin
  if FDesigner=nil then begin
    DesignerForm:=GetDesignerForm(Component);
    if DesignerForm=nil then begin
      raise Exception.Create('TComponentInterface.GetDesigner: '
        +Component.Name+' DesignerForm=nil');
    end;
    FDesigner:=TComponentEditorDesigner(DesignerForm.Designer);
    if FDesigner=nil then begin
      raise Exception.Create('TComponentInterface.GetDesigner: '
        +Component.Name+' Designer=nil');
    end;
    if not (FDesigner is TComponentEditorDesigner) then begin
      raise Exception.Create('TComponentInterface.GetDesigner: '
         +Component.Name+' Designer='+FDesigner.ClassName);
    end;
  end;
  Result:=FDesigner;
end;

Function TComponentInterface.GetPPropInfoByIndex(Index:Integer): PPropInfo;
var
  PT : PTypeData;
  PP : PPropList;
  PI : PTypeInfo;
Begin
  PI := FComponent.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
    Result:=PP^[index]
    else
    Result := nil;

   Freemem(PP);
end;

Function TComponentInterface.GetPPropInfoByName(Name:ShortString): PPropInfo;
var
  PT : PTypeData;
  PP : PPropList;
  PI : PTypeInfo;
  I  : Longint;
Begin
  Name := Uppercase(name);
  PI := FComponent.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  I := -1;
  repeat
    inc(i);
  until (PP^[i]^.Name = Name) or (i = PT^.PropCount-1);

  if PP^[i]^.Name = Name then
    Result:=PP^[i]
  else
    Result := nil;

   Freemem(PP);
end;

Function TComponentInterface.GetComponentType    : ShortString;
Begin
  Result:=FComponent.ClassName;
end;

Function TComponentInterface.GetComponentHandle  : LongInt;
Begin
//return the TWinControl handle?
  if (Component is TWinControl) then
  Result := TWinControl(Component).Handle;
end;

Function TComponentInterface.GetParent : TIComponentInterface;
Begin
  result := nil;
  if (FComponent is TControl) then
  if TControl(FComponent).Parent <> nil then
  begin
     Result := FFormEditor.FindComponent(TControl(FComponent).Parent);
  end;
end;

Function TComponentInterface.IsTControl : Boolean;
Begin
  Result := (FComponent is TControl);
end;

Function TComponentInterface.GetPropCount : Integer;
var
  PT : PTypeData;
Begin
  PT:=GetTypeData(FComponent.ClassInfo);
  Result := PT^.PropCount;
end;

Function TComponentInterface.GetPropType(Index : Integer) : TTypeKind;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PI:=FComponent.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType^.Kind
      else
      Result := tkUnknown;

  freemem(PP);
end;

Function TComponentInterface.GetPropTypeInfo(Index : Integer) : PTypeInfo;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PI:=FComponent.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType
      else
      Result := nil;
  freemem(PP);
end;


{This returns "Integer" or "Boolean"}
Function TComponentInterface.GetPropTypeName(Index : Integer) : ShortString;
var
  PT : PTypeData;
  PP : PPropList;
  PI : PTypeInfo;
Begin
  PI:=FComponent.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType^.Name
      else
      Result := '';
  freemem(PP);
end;


{This returns "Left" "Align" "Visible"}
Function TComponentInterface.GetPropName(Index : Integer) : ShortString;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PI:=FComponent.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
//      Result := PP^[Index]^.PropType^.Name
      Result := PP^[Index]^.Name
      else
      Result := '';
  freemem(PP);
end;

Function TComponentInterface.GetPropTypebyName(Name : ShortString) : TTypeKind;
var
  PT  : PTypeData;
  PP  : PPropList;
  PI  : PTypeInfo;
  I   : Longint;
Begin
  PI:=FComponent.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);

  Result := tkUnknown;
  For I:=0 to PT^.PropCount-1 do
  If PP^[i]<>Nil then
  begin
    if PP^[i]^.Name = Name then
    begin
      Result := PP^[i]^.PropType^.Kind;
      Break;
    end;
  end;

  freemem(PP);
end;

Function TComponentInterface.GetPropValue(Index : Integer; var Value) : Boolean;
var
PP : PPropInfo;
Begin
PP := GetPPropInfoByIndex(Index);
Result := FGetProp(PP,Value);
end;

Function TComponentInterface.GetPropValuebyName(Name: ShortString; var Value) : Boolean;
var
PRI : PPropInfo;
Begin
Result := False;
PRI := GetPPropInfoByName(Name);

if PRI <> nil then
Result := FGetProp(PRI,Value);
end;

Function TComponentInterface.SetProp(Index : Integer; const Value) : Boolean;
var
PRI : PPropInfo;
Begin
  Result := False;
  PRI := GetPPropInfoByIndex(Index);
  if PRI <> nil then
      Begin
        Result := FSetProp(PRI,Value);
      end;
end;


Function TComponentInterface.SetPropbyName(Name : ShortString; const Value) : Boolean;
var
  PRI : PPropInfo;
Begin
  //Writeln('SetPropByName Name='''+Name+'''');
  Result := False;

  PRI := GetPropInfo(FComponent.ClassInfo,Name);
  if PRI <> nil then
  Begin
    Result :=FSetProp(PRI,Value);
  end;
end;

Function TComponentInterface.GetControlCount: Integer;
Begin
  // XXX Todo:
  Result := -1;
end;

Function TComponentInterface.GetControl(Index : Integer): TIComponentInterface;
Begin
  // XXX Todo:
  Result := nil;
end;

Function TComponentInterface.GetComponentCount: Integer;
Begin
  // XXX Todo:
   Result := -1;
end;

Function TComponentInterface.GetComponent(Index : Integer): TIComponentInterface;
Begin
  // XXX Todo:
  Result := nil;
end;

Function TComponentInterface.Select : Boolean;
Begin
  // XXX Todo:
  Result := False;
end;

Function TComponentInterface.Focus : Boolean;
Begin
  Result := False;
  if (FComponent is TWinControl) and (TWinControl(FComponent).CanFocus) then
  Begin
    TWinControl(FComponent).SetFocus;
    Result := True;
  end;
end;

Function TComponentInterface.Delete: Boolean;
var
  OldName, OldClassName: string;
Begin
  {$IFDEF VerboseFormEditor}
  writeln('TComponentInterface.Delete A ',Component.Name,':',Component.ClassName);
  {$ENDIF}
  {$IFNDEF NoCompCatch}
  try
  {$ENDIF}
    OldName:=Component.Name;
    OldClassName:=Component.ClassName;
    Component.Free;
  {$IFNDEF NoCompCatch}
  except
    on E: Exception do begin
      writeln('TComponentInterface.Delete ERROR:',
        ' "'+OldName+':'+OldClassName+'" ',E.Message);
      MessageDlg('Error',
        'An exception occured during deletion of'#13
        +'"'+OldName+':'+OldClassName+'"'#13
        +E.Message,
        mtError,[mbOk],0);
    end;
  end;
  {$ENDIF}
  FComponent:=nil;
  {$IFDEF VerboseFormEditor}
  writeln('TComponentInterface.Delete B ');
  {$ENDIF}
  Free;
  Result := True;
end;

function TComponentInterface.GetComponentEditor: TBaseComponentEditor;
begin
  if FComponentEditor=nil then begin
    FComponentEditor:=ComponentEditors.GetComponentEditor(Component,Designer);
  end;
  Result:=FComponentEditor;
end;


{ TCustomFormEditor }

constructor TCustomFormEditor.Create;
begin
  inherited Create;
  FComponentInterfaces := TAVLTree.Create(@CompareComponentInterfaces);
  FNonControlForms:=TAVLTree.Create(@CompareNonControlForms);
  FSelection := TPersistentSelectionList.Create;
  
  JITFormList := TJITForms.Create;
  JITFormList.OnReaderError:=@JITListReaderError;
  JITFormList.OnPropertyNotFound:=@JITListPropertyNotFound;

  JITDataModuleList := TJITDataModules.Create;
  JITDataModuleList.OnReaderError:=@JITListReaderError;
  JITDataModuleList.OnPropertyNotFound:=@JITListPropertyNotFound;

  DesignerMenuItemClick:=@OnDesignerMenuItemClick;
  OnGetDesignerForm:=@GetDesignerForm;
end;

destructor TCustomFormEditor.Destroy;
begin
  DesignerMenuItemClick:=nil;
  FreeAndNil(JITFormList);
  FreeAndNil(JITDataModuleList);
  FreeAndNil(FComponentInterfaces);
  FreeAndNil(FSelection);
  FreeAndNil(FNonControlForms);
  inherited Destroy;
end;

procedure TCustomFormEditor.SetSelection(
  const ASelection: TPersistentSelectionList);
begin
  FSelection.Assign(ASelection);
  if FSelection.Count>0 then begin
    Obj_Inspector.PropertyEditorHook.LookupRoot:=
      GetLookupRootForComponent(FSelection[0]);
  end;
  Obj_Inspector.Selection := FSelection;
end;

Function TCustomFormEditor.AddSelected(Value : TComponent) : Integer;
Begin
  Result := FSelection.Add(Value) + 1;
  Obj_Inspector.Selection := FSelection;
end;

Procedure TCustomFormEditor.DeleteComponent(AComponent: TComponent;
  FreeComponent: boolean);
var
  Temp : TComponentInterface;
  i: integer;
  AForm: TCustomForm;
Begin
  Temp := TComponentInterface(FindComponent(AComponent));
  if Temp <> nil then
  begin
    FComponentInterfaces.Remove(Temp);

    writeln('TCustomFormEditor.DeleteControl ',
            AComponent.ClassName,' ',IsJITComponent(AComponent));
    if IsJITComponent(AComponent) then begin
      // value is a top level component
      if FreeComponent then begin
        i:=AComponent.ComponentCount-1;
        while i>=0 do begin
          DeleteComponent(AComponent.Components[i],true);
          dec(i);
          if i>AComponent.ComponentCount-1 then
            i:=AComponent.ComponentCount-1;
        end;
        if PropertyEditorHook.LookupRoot=AComponent then
          PropertyEditorHook.LookupRoot:=nil;
        if JITFormList.IsJITForm(AComponent) then
          // free a form component
          JITFormList.DestroyJITComponent(AComponent)
        else if JITDataModuleList.IsJITDataModule(AComponent) then begin
          // free a datamodule and its designer form
          AForm:=GetDesignerForm(AComponent);
          if not (AForm is TNonControlForm) then
            RaiseException('TCustomFormEditor.DeleteControl  Where is the TNonControlForm? '+AComponent.ClassName);
          FNonControlForms.Remove(AForm);
          TNonControlForm(AForm).LookupRoot:=nil;
          AForm.Free;
          JITDataModuleList.DestroyJITComponent(AComponent);
        end else
          RaiseException('TCustomFormEditor.DeleteControl '+AComponent.ClassName);
      end;
      Temp.Free;
    end
    else begin
      // value is a normal child component
      if FreeComponent then
        Temp.Delete
      else
        Temp.Free;
    end;
  end;
end;

Function TCustomFormEditor.FindComponentByName(
  const Name: ShortString) : TIComponentInterface;
Var
  ANode: TAVLTreeNode;
Begin
  ANode:=FComponentInterfaces.FindLowest;
  while ANode<>nil do begin
    Result := TIComponentInterface(ANode.Data);
    if AnsiCompareText(TComponentInterface(Result).Component.Name,Name)=0 then
      exit;
    ANode:=FComponentInterfaces.FindSuccessor(ANode);
  end;
  Result:=nil;
end;

Function TCustomFormEditor.FindComponent(AComponent: TComponent
  ): TIComponentInterface;
Var
  ANode: TAVLTreeNode;
Begin
  ANode:=FComponentInterfaces.FindKey(Pointer(AComponent),
                                      @CompareComponentAndInterface);
  if ANode<>nil then
    Result:=TIComponentInterface(ANode.Data)
  else
    Result:=nil;
end;

function TCustomFormEditor.IsJITComponent(AComponent: TComponent): boolean;
begin
  Result:=JITFormList.IsJITForm(AComponent)
          or JITDataModuleList.IsJITDataModule(AComponent);
end;

function TCustomFormEditor.GetJITListOfType(AncestorType: TComponentClass
  ): TJITComponentList;
begin
  if AncestorType.InheritsFrom(TForm) then
    Result:=JITFormList
  else if AncestorType.InheritsFrom(TDataModule) then
    Result:=JITDataModuleList
  else
    Result:=nil;
end;

function TCustomFormEditor.FindJITList(AComponent: TComponent
  ): TJITComponentList;
begin
  if JITFormList.IndexOf(AComponent)>=0 then
    Result:=JITFormList
  else if JITDataModuleList.IndexOf(AComponent)>=0 then
    Result:=JITDataModuleList
  else
    Result:=nil;
end;

function TCustomFormEditor.GetDesignerForm(AComponent: TComponent
  ): TCustomForm;
var
  OwnerComponent: TComponent;
begin
  Result:=nil;
  OwnerComponent:=AComponent.Owner;
  if OwnerComponent=nil then
    OwnerComponent:=AComponent;
  if OwnerComponent is TCustomForm then
    Result:=TCustomForm(OwnerComponent)
  else
    Result:=FindNonControlForm(OwnerComponent);
end;

function TCustomFormEditor.FindNonControlForm(LookupRoot: TComponent
  ): TNonControlForm;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FindNonControlFormNode(LookupRoot);
  if AVLNode<>nil then
    Result:=TNonControlForm(AVLNode.Data)
  else
    Result:=nil;
end;

function TCustomFormEditor.CreateNonControlForm(LookupRoot: TComponent
  ): TNonControlForm;
begin
  if FindNonControlFormNode(LookupRoot)<>nil then
    RaiseException('TCustomFormEditor.CreateNonControlForm exists already');
  if LookupRoot is TDataModule then begin
    Result:=TDataModuleForm.Create(nil);
    Result.LookupRoot:=LookupRoot;
    FNonControlForms.Add(Result);
  end else
    RaiseException('TCustomFormEditor.CreateNonControlForm Unknown type '
      +LookupRoot.ClassName);
end;

procedure TCustomFormEditor.RenameJITComponent(AComponent: TComponent;
  const NewName: shortstring);
var
  JITComponentList: TJITComponentList;
begin
  JITComponentList:=FindJITList(AComponent);
  if JITComponentList=nil then
    RaiseException('TCustomFormEditor.RenameJITComponent');
  JITComponentList.RenameComponentClass(AComponent,NewName);
end;

procedure TCustomFormEditor.UpdateDesignerFormName(AComponent: TComponent);
var
  ANonControlForm: TNonControlForm;
begin
  ANonControlForm:=FindNonControlForm(AComponent);
writeln('TCustomFormEditor.UpdateDesignerFormName ',ANonControlForm<>nil,' ',AComponent.Name);
  if ANonControlForm<>nil then
    ANonControlForm.Caption:=AComponent.Name;
end;

function TCustomFormEditor.CreateNewJITMethod(AComponent: TComponent;
  const AMethodName: shortstring): TMethod;
var
  JITComponentList: TJITComponentList;
begin
  JITComponentList:=FindJITList(AComponent);
  if JITComponentList=nil then
    RaiseException('TCustomFormEditor.CreateNewJITMethod');
  Result:=JITComponentList.CreateNewMethod(AComponent,AMethodName);
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

procedure TCustomFormEditor.SaveHiddenDesignerFormProperties(
  AComponent: TComponent);
var
  NonControlForm: TNonControlForm;
begin
  NonControlForm:=FindNonControlForm(AComponent);
  if NonControlForm<>nil then
    NonControlForm.DoSaveBounds;
end;

function TCustomFormEditor.DesignerCount: integer;
begin
  Result:=JITFormList.Count+JITDataModuleList.Count;
end;

function TCustomFormEditor.GetDesigner(Index: integer): TIDesigner;
var
  AForm: TCustomForm;
begin
  if Index<JITFormList.Count then
    Result:=JITFormList[Index].Designer
  else begin
    AForm:=GetDesignerForm(JITDataModuleList[Index-JITFormList.Count]);
    Result:=TIDesigner(AForm.Designer);
  end;
end;

function TCustomFormEditor.GetComponentEditor(AComponent: TComponent
  ): TBaseComponentEditor;
var
  ACompIntf: TComponentInterface;
begin
  Result:=nil;
  if AComponent=nil then exit;
  ACompIntf:=TComponentInterface(FindComponent(AComponent));
  if ACompIntf=nil then exit;
  Result:=ACompIntf.GetComponentEditor;
end;

Function TCustomFormEditor.CreateComponent(ParentCI: TIComponentInterface;
  TypeClass: TComponentClass;  X,Y,W,H: Integer): TIComponentInterface;
Var
  Temp: TComponentInterface;
  NewJITIndex: Integer;
  CompLeft, CompTop, CompWidth, CompHeight: integer;
  NewComponent: TComponent;
  OwnerComponent: TComponent;
  ParentComponent: TComponent;
  JITList: TJITComponentList;
  AControl: TControl;
  NewComponentName: String;
Begin
  Result:=nil;
  Temp:=nil;
  try
    writeln('[TCustomFormEditor.CreateComponent] Class='''+TypeClass.ClassName+'''');
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent A');{$ENDIF}

    OwnerComponent:=nil;
    if Assigned(ParentCI) then
    begin
      // add as child component
      ParentComponent:=TComponentInterface(ParentCI).Component;
      OwnerComponent:=ParentComponent;
      if OwnerComponent.Owner<>nil then
        OwnerComponent:=OwnerComponent.Owner;
      try
        NewComponent := TypeClass.Create(OwnerComponent);
      except
        on e: Exception do begin
          MessageDlg('Error creating component',
            'Error creating component: '+TypeClass.ClassName,
            mtError,[mbCancel],0);
          exit;
        end;
      end;
      // check if Owner was properly set
      if NewComponent.Owner<>OwnerComponent then begin
        MessageDlg('Invalid component owner',
          'The component of type '+NewComponent.ClassName
          +' failed to set its owner to '
          +OwnerComponent.Name+':'+OwnerComponent.ClassName,
          mtError,[mbCancel],0);
        exit;
      end;
      
      // create component interface
      Temp := TComponentInterface.Create;
      Temp.FComponent:=NewComponent;
      
      // set parent
      if Temp.IsTControl then begin
        if (ParentComponent is TWinControl)
        and (csAcceptsControls in TWinControl(ParentComponent).ControlStyle) then
        begin
          TWinControl(Temp.Component).Parent :=
            TWinControl(ParentComponent);
          writeln('Parent is '''+TWinControl(Temp.Component).Parent.Name+'''');
        end
        else begin
          TControl(Temp.Component).Parent :=
            TControl(ParentComponent).Parent;
          writeln('Parent is '''+TControl(Temp.Component).Parent.Name+'''');
        end;
      end;
    end else begin
      // create a toplevel control -> a form or a datamodule
      ParentComponent:=nil;
      JITList:=GetJITListOfType(TypeClass);
      if JITList=nil then
        RaiseException('TCustomFormEditor.CreateComponent '+TypeClass.ClassName);
      NewJITIndex := JITList.AddNewJITComponent;
      if NewJITIndex >= 0 then begin
        // create component interface
        Temp := TComponentInterface.Create;
        Temp.FComponent := JITList[NewJITIndex]
      end else begin
        exit;
      end;
    end;
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent D ');{$ENDIF}
    try
      NewComponentName := CreateUniqueComponentName(Temp.Component);
      Temp.Component.Name := NewComponentName;
    except
      on e: Exception do begin
        MessageDlg('Error naming component',
          'Error setting the name of a component '
          +Temp.Component.Name+':'+Temp.Component.ClassName
          +' to '+NewComponentName,
          mtError,[mbCancel],0);
        exit;
      end;
    end;

    try
      // set bounds
      CompLeft:=X;
      CompTop:=Y;
      CompWidth:=W;
      CompHeight:=H;
      if (Temp.Component is TControl) then
      Begin
        AControl:=TControl(Temp.Component);
        if CompWidth<=0 then CompWidth:=Max(5,AControl.Width);
        if CompHeight<=0 then CompHeight:=Max(5,AControl.Height);
        if CompLeft<0 then begin
          if AControl.Parent<>nil then
            CompLeft:=(AControl.Parent.Width - CompWidth) div 2
          else if AControl is TCustomForm then
            CompLeft:=Max(1,Min(250,Screen.Width-CompWidth-50))
          else
            CompLeft:=0;
        end;
        if CompTop<0 then begin
          if AControl.Parent<>nil then
            CompTop:=(AControl.Parent.Height - CompHeight) div 2
          else if AControl is TCustomForm then
            CompTop:=Max(1,Min(250,Screen.Height-CompHeight-50))
          else
            CompTop:=0;
        end;
        AControl.SetBounds(CompLeft,CompTop,CompWidth,CompHeight);
      end
      else if (Temp.Component is TDataModule) then begin
        // data module
        with TDataModule(Temp.Component) do begin
          if CompWidth<=0 then CompWidth:=Max(50,DesignSize.X);
          if CompHeight<=0 then CompHeight:=Max(50,DesignSize.Y);
          if CompLeft<0 then
            CompLeft:=Max(1,Min(250,Screen.Width-CompWidth-50));
          if CompTop<0 then
            CompTop:=Max(1,Min(250,Screen.Height-CompHeight-50));
          DesignOffset.X:=CompLeft;
          DesignOffset.Y:=CompTop;
          DesignSize.X:=CompWidth;
          DesignSize.Y:=CompHeight;
        end;
      end
      else begin
        // non TControl
        with LongRec(Temp.Component.DesignInfo) do begin
          Lo:=word(Min(32000,CompLeft));
          Hi:=word(Min(32000,CompTop));
        end;
      end;
    except
      on e: Exception do begin
        MessageDlg('Error moving component',
          'Error moving component '
          +Temp.Component.Name+':'+Temp.Component.ClassName,
          mtError,[mbCancel],0);
        exit;
      end;
    end;

    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent F ');{$ENDIF}
    // add to component list
    FComponentInterfaces.Add(Temp);

    if Temp.Component.Owner<>nil then
      CreateChildComponentInterfaces(Temp.Component.Owner);

    Result := Temp;
  finally
    // clean up carefully
    if Result=nil then begin
      if Temp=nil then begin
        if NewComponent<>nil then begin
          try
            NewComponent.Free;
            NewComponent:=nil;
          except
            MessageDlg('Error destroying component',
              'Error destroying component of type '+TypeClass.ClassName,
              mtError,[mbCancel],0);
          end;
        end;
      end;
      if (Result<>Temp) then begin
        Temp.Free;
        Temp:=nil;
      end;
    end;
  end;
end;

Function TCustomFormEditor.CreateComponentFromStream(
  BinStream: TStream; AncestorType: TComponentClass;
  Interactive: boolean): TIComponentInterface;
var
  NewJITIndex: integer;
  NewComponent: TComponent;
  JITList: TJITComponentList;
begin
  // create JIT Component
  JITList:=GetJITListOfType(AncestorType);
  if JITList=nil then
    RaiseException('TCustomFormEditor.CreateComponentFromStream ClassName='+
                   AncestorType.ClassName);
  NewJITIndex := JITList.AddJITComponentFromStream(BinStream,Interactive);
  if NewJITIndex < 0 then begin
    Result:=nil;
    exit;
  end;
  NewComponent:=JITList[NewJITIndex];
  
  // create a component interface for the form
  Result:=CreateComponentInterface(NewComponent);

  CreateChildComponentInterfaces(NewComponent);
end;

function TCustomFormEditor.CreateChildComponentFromStream(BinStream: TStream;
  ComponentClass: TComponentClass; Root: TComponent;
  ParentControl: TWinControl): TIComponentInterface;
var
  NewComponent: TComponent;
  JITList: TJITComponentList;
  i: Integer;
begin
  Result:=nil;
  
  JITList:=FindJITList(Root);
  if JITList=nil then
    RaiseException('TCustomFormEditor.CreateChildComponentFromStream ClassName='+
                   Root.ClassName);

  NewComponent:=JITList.AddJITChildComponentFromStream(
                                   Root,BinStream,ComponentClass,ParentControl);
                                                 
  // create a component interface for the new child component
  Result:=CreateComponentInterface(NewComponent);

  // create a component interface for each new child component
  for i:=0 to Root.ComponentCount-1 do
    if FindComponent(Root.Components[i])=nil then
      CreateComponentInterface(Root.Components[i]);
end;

Procedure TCustomFormEditor.SetComponentNameAndClass(CI: TIComponentInterface;
  const NewName, NewClassName: shortstring);
var
  AComponent: TComponent;
  JITList: TJITComponentList;
begin
  AComponent:=TComponentInterface(CI).Component;
  JITList:=GetJITListOfType(TComponentClass(AComponent.ClassType));
  JITList.RenameComponentClass(AComponent,NewClassName);
  AComponent.Name:=NewName;
end;

procedure TCustomFormEditor.JITListReaderError(Sender: TObject;
  ErrorType: TJITFormError; var Action: TModalResult);
var
  aCaption, aMsg: string;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  HelpCtx: Longint;
  JITComponentList: TJITComponentList;
begin
  JITComponentList:=TJITComponentList(Sender);
  aCaption:='Error reading '+JITComponentList.ComponentPrefix;
  aMsg:='';
  DlgType:=mtError;
  Buttons:=[mbCancel];
  HelpCtx:=0;
  
  with JITComponentList do begin
    aMsg:=aMsg+ComponentPrefix+': ';
    if CurReadJITComponent<>nil then
      aMsg:=aMsg+CurReadJITComponent.Name+':'+CurReadJITComponent.ClassName
    else
      aMsg:=aMsg+'?';
    if CurReadChild<>nil then
      aMsg:=aMsg+#13'Component: '
        +CurReadChild.Name+':'+CurReadChild.ClassName
    else if CurReadChildClass<>nil then
      aMsg:=aMsg+#13'Component Class: '+CurReadChildClass.ClassName;
    aMsg:=aMsg+#13+CurReadErrorMsg;
  end;

  case ErrorType of
    jfeUnknownProperty, jfeReaderError:
      begin
        Buttons:=[mbIgnore,mbCancel];
      end;
    jfeUnknownComponentClass:
      begin
        aMsg:=aMsg+#13+'Class "'+JITComponentList.CurUnknownClass+'" not found.';
      end;
  end;
  Action:=MessageDlg(aCaption,aMsg,DlgType,Buttons,HelpCtx);
end;

procedure TCustomFormEditor.OnDesignerMenuItemClick(Sender: TObject);
var
  CompEditor: TBaseComponentEditor;
  MenuItem: TMenuItem;
begin
  if (Sender=nil) or (not (Sender is TMenuItem)) then exit;
  MenuItem:=TMenuItem(Sender);
  if (MenuItem.Count>0) or MenuItem.IsInMenuBar then exit;

  CompEditor:=GetComponentEditor(TComponent(Sender));
  if CompEditor=nil then exit;
  try
    CompEditor.Edit;
  except
    on E: Exception do begin
      writeln('TCustomFormEditor.OnDesignerMenuItemClick ERROR: ',E.Message);
      MessageDlg('Error in '+CompEditor.ClassName,
        'The component editor of class "'+CompEditor.ClassName+'"'
        +'has created the error:'#13
        +'"'+E.Message+'"',
        mtError,[mbOk],0);
    end;
  end;
end;

function TCustomFormEditor.FindNonControlFormNode(LookupRoot: TComponent
  ): TAVLTreeNode;
begin
  Result:=FNonControlForms.FindKey(Pointer(LookupRoot),
                                   @CompareLookupRootAndNonControlForm);
end;

procedure TCustomFormEditor.JITListPropertyNotFound(Sender: TObject;
  Reader: TReader; Instance: TPersistent; var PropName: string;
  IsPath: boolean; var Handled, Skip: Boolean);
begin
  writeln('TCustomFormEditor.JITListPropertyNotFound ',Sender.ClassName,
    ' Instance=',Instance.ClassName,' PropName="',PropName,'" IsPath=',IsPath);
end;

function TCustomFormEditor.GetPropertyEditorHook: TPropertyEditorHook;
begin
  Result:=Obj_Inspector.PropertyEditorHook;
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
    Result:=AClassName;
    if (length(Result)>1) and (Result[1]='T') then
      Result:=RightStr(Result,length(Result)-1);
    {$IfDef VER1_0}
    //make it more presentable
    Result := Result[1] + lowercase(Copy(Result,2,length(Result)));
    {$EndIf}
    Result:=Result+IntToStr(i);
    while (j>=0)
    and (AnsiCompareText(Result,OwnerComponent.Components[j].Name)<>0) do
      dec(j);
    if j<0 then exit;
    inc(i);
  end;
end;

Procedure TCustomFormEditor.ClearSelected;
Begin
  FSelection.Clear;
end;

function TCustomFormEditor.TranslateKeyToDesignerCommand(Key: word;
  Shift: TShiftState): word;
begin
  Result:=EditorOpts.KeyMap.TranslateKey(Key,Shift,[caDesigner]);
end;

Function TCustomFormEditor.CreateComponentInterface(
  AComponent: TComponent): TIComponentInterface;
Begin
  if FindComponent(AComponent)<>nil then exit;
  Result := TComponentInterface.Create(AComponent);
  FComponentInterfaces.Add(Result);
end;

procedure TCustomFormEditor.CreateChildComponentInterfaces(
  AComponent: TComponent);
var
  i: Integer;
begin
  // create a component interface for each component owned by the new component
  for i:=0 to AComponent.ComponentCount-1 do
    CreateComponentInterface(AComponent.Components[i]);
end;

procedure TCustomFormEditor.OnObjectInspectorModified(Sender: TObject);
var
  CustomForm: TCustomForm;
  Instance: TPersistent;
begin
  if (FSelection = nil)
  or (FSelection.Count <= 0) then Exit;
  
  Instance := FSelection[0];
  if Instance is TCustomForm
  then CustomForm:=TCustomForm(Instance)
  else if (Instance is TComponent)
      and (TComponent(Instance).Owner <> nil)
      and (TComponent(Instance).Owner is TCustomForm)
      then CustomForm:=TCustomForm(TComponent(Instance).Owner)
      else CustomForm:=nil;
      
  if (CustomForm<>nil) and (CustomForm.Designer<>nil) then
    CustomForm.Designer.Modified;
end;

procedure TCustomFormEditor.SetObj_Inspector(
  AnObjectInspector: TObjectInspector);
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


end.

