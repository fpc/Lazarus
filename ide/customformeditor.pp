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

{$DEFINE VerboseFormEditor}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, AbstractFormeditor, Controls, PropEdits, TypInfo, ObjectInspector,
  Forms, IDEComp, JITForms, Compreg, ComponentEditors, KeyMapping,
  EditorOptions, Dialogs;

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

    Function GetComponentType    : ShortString; override;
    Function GetComponentHandle  : LongInt; override;
    Function GetParent           : TIComponentInterface; override;
    Function IsTControl          : Boolean; override;
    Function GetPropCount	   : Integer; override;
    Function GetPropType(Index : Integer) : TTypeKind; override;
    Function GetPropTypeInfo(Index : Integer) : PTypeInfo;
    Function GetPropName(Index : Integer) : ShortString; override;
    Function GetPropTypeName(Index : Integer) : ShortString; override;
    Function GetPropTypebyName(Name : ShortString) : TTypeKind; override;

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

{
TCustomFormEditor

}

  TControlClass = class of TControl;

  TCustomFormEditor = class(TAbstractFormEditor)
  private
    FModified     : Boolean;
    FComponentInterfaceList : TList; //used to track and find controls
    FSelectedComponents : TComponentSelectionList;
    FObj_Inspector : TObjectInspector;
    function GetPropertyEditorHook: TPropertyEditorHook;
  protected
    Procedure RemoveFromComponentInterfaceList(Value :TIComponentInterface);
    procedure SetSelectedComponents(TheSelectedComponents : TComponentSelectionList);
    procedure OnObjectInspectorModified(Sender: TObject);
    procedure SetObj_Inspector(AnObjectInspector: TObjectInspector); virtual;
    procedure JITFormListReaderError(Sender: TObject; ErrorType: TJITFormError;
          var Action: TModalResult); virtual;
  public
    JITFormList : TJITForms;
    constructor Create;
    destructor Destroy; override;

    Function AddSelected(Value : TComponent) : Integer;
    Procedure DeleteControl(Value : TComponent);
    Function FormModified : Boolean; override;
    Function FindComponentByName(const Name : ShortString) : TIComponentInterface; override;
    Function FindComponent(AComponent: TComponent): TIComponentInterface; override;
    
    function GetComponentEditor(AComponent: TComponent): TBaseComponentEditor;
    Function GetFormComponent: TIComponentInterface; override;
    function CreateUniqueComponentName(AComponent: TComponent): string;
    function CreateUniqueComponentName(const AClassName: string;
      OwnerComponent: TComponent): string;
//    Function CreateComponent(CI : TIComponentInterface; TypeName : String;
    Function CreateComponentInterface(AComponent: TComponent): TIComponentInterface;
    Function CreateComponent(ParentCI : TIComponentInterface;
      TypeClass : TComponentClass;  X,Y,W,H : Integer): TIComponentInterface; override;
    Function CreateFormFromStream(BinStream: TStream): TIComponentInterface; override;
    Procedure SetFormNameAndClass(CI: TIComponentInterface; 
      const NewFormName, NewClassName: shortstring);
    Procedure ClearSelected;
    
    function TranslateKeyToDesignerCommand(Key: word; Shift: TShiftState): integer;
    
    property SelectedComponents: TComponentSelectionList
      read FSelectedComponents write SetSelectedComponents;
    property Obj_Inspector : TObjectInspector
      read FObj_Inspector write SetObj_Inspector;
    property PropertyEditorHook: TPropertyEditorHook read GetPropertyEditorHook;
  end;


implementation


uses
  SysUtils;

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
  OwnerForm: TCustomForm;
begin
  if FDesigner=nil then begin
    if (Component is TCustomForm) and (TCustomForm(Component).Parent=nil) then
      OwnerForm:=TCustomForm(Component)
    else begin
      OwnerForm:=TCustomForm(Component.Owner);
      if OwnerForm=nil then begin
        raise Exception.Create('TComponentInterface.GetDesigner: '
          +Component.Name+' Owner=nil');
      end;
      if not (OwnerForm is TCustomForm) then begin
        raise Exception.Create('TComponentInterface.GetDesigner: '
          +Component.Name+' OwnerForm='+OwnerForm.ClassName);
      end;
    end;
    FDesigner:=TComponentEditorDesigner(OwnerForm.Designer);
    if FDesigner=nil then begin
      raise Exception.Create('TComponentInterface.GetDesigner: '
        +Component.Name+' Designer=nil');
    end;
    if not (FDesigner is TComponentEditorDesigner) then begin
      raise Exception.Create('TComponentInterface.GetDesigner: '
         +Component.Name+' Designer='+
         +FDesigner.ClassName);
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
Begin
  {$IFDEF VerboseFormEditor}
  writeln('TComponentInterface.Delete A ',Component.Name,':',Component.ClassName);
  {$ENDIF}
  Component.Free;
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
  FComponentInterfaceList := TList.Create;
  FSelectedComponents := TComponentSelectionList.Create;
  JITFormList := TJITForms.Create;
  JITFormList.RegCompList := RegCompList;
  JITFormList.OnReaderError:=@JITFormListReaderError;
end;

destructor TCustomFormEditor.Destroy;
begin
  JITFormList.Free;
  FComponentInterfaceList.Free;
  FSelectedComponents.Free;
  inherited;
end;

procedure TCustomFormEditor.SetSelectedComponents(
  TheSelectedComponents : TComponentSelectionList);
begin
  FSelectedComponents.Assign(TheSelectedComponents);
  if FSelectedComponents.Count>0 then
  begin
    if FSelectedComponents[0].Owner<>nil then
    begin
      Obj_Inspector.PropertyEditorHook.LookupRoot:=FSelectedComponents[0].Owner;
    end
    else
    begin
      Obj_Inspector.PropertyEditorHook.LookupRoot:=FSelectedComponents[0];
    end;
  end;
  Obj_Inspector.Selections := FSelectedComponents;
end;

Function TCustomFormEditor.AddSelected(Value : TComponent) : Integer;
Begin
  FSelectedComponents.Add(Value);
  Result := FSelectedComponents.Count;
  Obj_Inspector.Selections := FSelectedComponents;
end;

Procedure TCustomFormEditor.DeleteControl(Value : TComponent);
var
  Temp : TComponentInterface;
  i: integer;
  AForm: TCustomForm;
Begin
  Temp := TComponentInterface(FindComponent(Value));
  if Temp <> nil then
  begin
    RemoveFromComponentInterfaceList(Temp);
    if (Value is TCustomForm) then begin
      AForm:=TCustomForm(Value);
      i:=AForm.ComponentCount-1;
      while i>=0 do begin
        DeleteControl(AForm.Components[i]);
        dec(i);
        if i>AForm.ComponentCount-1 then
          i:=AForm.ComponentCount-1;
      end;
      if not (AForm is TForm) then
        writeln('WARNING: TCustomFormEditor.DeleteControl ',AForm.ClassName);
      JITFormList.DestroyJITForm(TForm(AForm));
      Temp.Free;
    end
    else
      Temp.Delete;
  end;
end;

Function TCustomFormEditor.FormModified : Boolean;
Begin
  Result := FModified;
end;

Function TCustomFormEditor.FindComponentByName(
  const Name : ShortString) : TIComponentInterface;
Var
  Num : Integer;
Begin
  Num := 0;
  While Num < FComponentInterfaceList.Count do
  Begin
    Result := TIComponentInterface(FComponentInterfaceList.Items[Num]);
    if AnsiCompareText(TComponentInterface(Result).Component.Name,Name)=0 then
      exit;
    inc(num);
  end;
  Result:=nil;
end;

Function TCustomFormEditor.FindComponent(AComponent:TComponent): TIComponentInterface;
Var
  Num : Integer;
Begin
  Num := 0;
  While Num < FComponentInterfaceList.Count do
  Begin
    Result := TIComponentInterface(FComponentInterfaceList.Items[Num]);
    if TComponentInterface(Result).Component = AComponent then exit;
    inc(num);
  end;
  Result:=nil;
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

Function TCustomFormEditor.CreateComponent(ParentCI : TIComponentInterface;
  TypeClass : TComponentClass;  X,Y,W,H : Integer): TIComponentInterface;
Var
  Temp: TComponentInterface;
  NewFormIndex: Integer;
  CompLeft, CompTop, CompWidth, CompHeight: integer;
  OwnerComponent: TComponent;
  ParentComponent: TComponent;
Begin
  writeln('[TCustomFormEditor.CreateComponent] Class='''+TypeClass.ClassName+'''');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent A');{$ENDIF}
  Temp := TComponentInterface.Create;

  OwnerComponent:=nil;
  if Assigned(ParentCI) and (ParentCI.IsTControl) then
  begin
    ParentComponent:=TComponentInterface(ParentCI).Component;
    OwnerComponent:=GetParentForm(TControl(ParentComponent));
    if OwnerComponent=nil then
      OwnerComponent:=ParentComponent;
    Temp.FComponent := TypeClass.Create(OwnerComponent);
  end else begin
    //this should be a form
    ParentComponent:=nil;
    NewFormIndex := JITFormList.AddNewJITForm;
    if NewFormIndex >= 0 then
      Temp.FComponent := JITFormList[NewFormIndex]
    else begin
      Result:=nil;
      exit;
    end;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent C ');{$ENDIF}

  if Assigned(ParentCI) and (Temp.Component is TControl) then
    Begin
      if (ParentComponent is TWinControl)
      and (csAcceptsControls in
        TWinControl(ParentComponent).ControlStyle) then
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

  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent D ');{$ENDIF}
  Temp.Component.Name := CreateUniqueComponentName(Temp.Component);

  if (Temp.Component is TControl) then
  Begin
    CompLeft:=X;
    CompTop:=Y;
    CompWidth:=W;
    CompHeight:=H;
    if CompWidth<=0 then CompWidth:=TControl(Temp.Component).Width;
    if CompHeight<=0 then CompHeight:=TControl(Temp.Component).Height;
    if CompLeft<0 then
      CompLeft:=(TControl(Temp.Component).Parent.Width + CompWidth) div 2;
    if CompTop<0 then
      CompTop:=(TControl(Temp.Component).Parent.Height+ CompHeight) div 2;
    TControl(Temp.Component).SetBounds(CompLeft,CompTop,CompWidth,CompHeight);
  end else begin
    with LongRec(Temp.Component.DesignInfo) do begin
      Lo:=X;
      Hi:=Y;
    end;
  end;

  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TCustomFormEditor.CreateComponent F ');{$ENDIF}
  FComponentInterfaceList.Add(Temp);

  Result := Temp;
end;

Function TCustomFormEditor.CreateFormFromStream(
  BinStream: TStream): TIComponentInterface;
var
  NewFormIndex: integer;
  i: integer;
  NewForm: TCustomForm;
begin
  // create JITForm
  NewFormIndex := JITFormList.AddJITFormFromStream(BinStream);
  if NewFormIndex < 0 then begin
    Result:=nil;
    exit;
  end;
  NewForm:=JITFormList[NewFormIndex];
  
  // create a component interface for the form
  Result:=CreateComponentInterface(NewForm);

  // create component interfaces for the form components
  for i:=0 to NewForm.ComponentCount-1 do
    CreateComponentInterface(NewForm.Components[i]);
end;

Procedure TCustomFormEditor.SetFormNameAndClass(CI: TIComponentInterface;
  const NewFormName, NewClassName: shortstring);
var AComponent: TComponent;
begin
  AComponent:=TComponentInterface(CI).Component;
  if (AComponent<>nil) and (AComponent is TForm) then begin
    JITFormList.RenameFormClass(TForm(AComponent),NewClassName);
    TForm(AComponent).Name:=NewFormName;
  end;
end;

procedure TCustomFormEditor.JITFormListReaderError(Sender: TObject;
  ErrorType: TJITFormError; var Action: TModalResult);
var
  aCaption, aMsg: string;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  HelpCtx: Longint;
begin
  aCaption:='Error reading form';
  aMsg:='';
  DlgType:=mtError;
  Buttons:=[mbCancel];
  HelpCtx:=0;
  
  with JITFormList do begin
    aMsg:=aMsg+'Form: ';
    if CurReadForm<>nil then
      aMsg:=aMsg+CurReadForm.Name+':'+CurReadForm.ClassName
    else
      aMsg:=aMsg+'?';
    if CurReadComponent<>nil then
      aMsg:=aMsg+#13'Component: '
        +CurReadComponent.Name+':'+CurReadComponent.ClassName
    else if CurReadComponentClass<>nil then
      aMsg:=aMsg+#13'Component Class: '+CurReadComponentClass.ClassName;
  end;
  aMsg:=aMsg+#13+JITFormList.CurReadErrorMsg;
  
  case ErrorType of
    jfeUnknownProperty, jfeReaderError:
      begin
        Buttons:=[mbIgnore,mbCancel];
      end;
  end;
  Action:=MessageDlg(aCaption,aMsg,DlgType,Buttons,HelpCtx);
end;

function TCustomFormEditor.GetPropertyEditorHook: TPropertyEditorHook;
begin
  Result:=Obj_Inspector.PropertyEditorHook;
end;

Procedure TCustomFormEditor.RemoveFromComponentInterfaceList(
  Value :TIComponentInterface);
Begin
  FComponentInterfaceList.Remove(Value);
end;

Function TCustomFormEditor.GetFormComponent : TIComponentInterface;
Begin
  //this can only be used IF you have one FormEditor per form.  I currently don't
  Result := nil;
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
    {$IfNDef VER1_1}
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
  FSelectedComponents.Clear;
end;

function TCustomFormEditor.TranslateKeyToDesignerCommand(Key: word;
  Shift: TShiftState): integer;
begin
  Result:=EditorOpts.KeyMap.TranslateKey(Key,Shift,[caDesigner]);
end;

Function TCustomFormEditor.CreateComponentInterface(
  AComponent: TComponent): TIComponentInterface;
Begin
  Result := TComponentInterface.Create(AComponent);
  FComponentInterfaceList.Add(Result);
end;

procedure TCustomFormEditor.OnObjectInspectorModified(Sender: TObject);
var CustomForm: TCustomForm;
begin
  if (FSelectedComponents<>nil) and (FSelectedComponents.Count>0) then begin
    if FSelectedComponents[0] is TCustomForm then
      CustomForm:=TCustomForm(FSelectedComponents[0])
    else if (FSelectedComponents[0].Owner<>nil)
    and (FSelectedComponents[0].Owner is TCustomForm) then
      CustomForm:=TCustomForm(FSelectedComponents[0].Owner)
    else
      CustomForm:=nil;
    if (CustomForm<>nil) and (CustomForm.Designer<>nil) then
      CustomForm.Designer.Modified;
  end;
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

