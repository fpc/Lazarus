{
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
 
  Author: Mattias Gaertner

  Abstract:
    This unit defines a list of forms descendents. The forms are normal TForm
    descendents with one exception: Every form has its own class. These classes
    are changeable at runtime, so that IDEs can add, remove or rename methods
    and such stuff. Also these forms can be loaded from streams and missing
    components and methods are added just-in-time to the class definition.
    Hence the name for the class: TJITForms.
    Subcomponents are looked up in the list of registered components
    (TJITForms.RegCompList).

  ToDo:
    -Add recursion needed for frames.
}
unit JITForms;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, CompReg, Forms, Controls, LCLLinux, Dialogs;

type
  //----------------------------------------------------------------------------
  TJITFormError = (
      jfeNone,
      jfeUnknown,
      jfeUnknownProperty,
      jfeUnknownComponentClass,
      jfeReaderError
    );
  TJITFormErrors = set of TJITFormError;
  
  TJITReaderErrorEvent = procedure(Sender: TObject; ErrorType: TJITFormError;
    var Action: TModalResult) of object;
  
  
  // Just-In-Time-Form List
  TJITForms = class(TPersistent)
  private
    FCurReadErrorMsg: string;
    FCurReadForm:TForm;
    FCurReadClass:TClass;
    FCurReadComponent: TComponent;
    FCurReadComponentClass: TComponentClass;
    FForms: TList; // list of TJITForm
    FOnReaderError: TJITReaderErrorEvent;
    FRegCompList:TRegisteredComponentList;
    // jit procedures
    function CreateVMTCopy(SourceClass:TClass; const NewClassName:ShortString):Pointer;
    procedure FreevmtCopy(vmtCopy:Pointer);
    procedure DoAddNewMethod(JITClass:TClass; AName:ShortString; ACode:Pointer);
      // AddNewMethod does not check if method already exists
    procedure DoRemoveMethod(JITClass:TClass; AName:ShortString; var OldCode:Pointer);
      // RemoveMethod does not free code memory
    procedure DoRenameMethod(JITClass:TClass; OldName,NewName:ShortString);
    procedure DoRenameClass(JITClass:TClass; NewName:ShortString);
    // TReader events
    procedure ReaderFindMethod(Reader: TReader; const FindMethodName: Ansistring;
      var Address: Pointer; var Error: Boolean);
    procedure ReaderSetName(Reader: TReader; Component: TComponent;
      var NewName: Ansistring);
    procedure ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
    procedure ReaderAncestorNotFound(Reader: TReader; const ComponentName: Ansistring;
      ComponentClass: TPersistentClass; var Component: TComponent);
    procedure ReaderError(Reader: TReader; const ErrorMsg: Ansistring;
      var Handled: Boolean);
    procedure ReaderFindComponentClass(Reader: TReader; const FindClassName: Ansistring;
      var ComponentClass: TComponentClass);
    procedure ReaderCreateComponent(Reader: TReader;
      ComponentClass: TComponentClass; var Component: TComponent);
    // some useful functions
    function GetItem(Index:integer):TForm;
    function GetClassNameFromStream(s:TStream):shortstring;
    function DoCreateJITForm(NewFormName,NewClassName:shortstring):integer;
    function OnFindGlobalComponent(const AName:AnsiString):TComponent;
    procedure InitReading;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index:integer]:TForm read GetItem; default;
    function Count:integer;
    property RegCompList:TRegisteredComponentList read FRegCompList write FRegCompList;
    function AddNewJITForm:integer;
    function AddJITFormFromStream(BinStream:TStream):integer;
    procedure DestroyJITForm(JITForm:TForm);
    procedure DestroyJITForm(Index:integer);
    function IndexOf(JITForm:TForm):integer;
    function FindFormByClassName(AClassName:shortstring):integer;
    function FindFormByName(AName:shortstring):integer;
    procedure GetUnusedNames(var FormName,FormClassName:shortstring);
    procedure AddNewMethod(JITForm:TForm; AName:ShortString);
    function CreateNewMethod(JITForm:TForm; AName:ShortString): TMethod;
    procedure RemoveMethod(JITForm:TForm; AName:ShortString);
    procedure RenameMethod(JITForm:TForm; OldName,NewName:ShortString);
    procedure RenameFormClass(JITForm:TForm; NewName:ShortString);
    property OnReaderError: TJITReaderErrorEvent
      read FOnReaderError write FOnReaderError;
    property CurReadForm:TForm read FCurReadForm;
    property CurReadClass:TClass read FCurReadClass;
    property CurReadComponent: TComponent read FCurReadComponent;
    property CurReadComponentClass: TComponentClass read FCurReadComponentClass;
    property CurReadErrorMsg: string read FCurReadErrorMsg;
  published
    // the dummy template 'procedure of object' for all events
    procedure DoNothing;
  end;

implementation

type
  //----------------------------------------------------------------------------
  // TJITForm is a template TForm descendent class that can be altered at
  // runtime
  TJITForm = class (TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJITFormClass = class of TJITForm;
  //----------------------------------------------------------------------------

var
  MyFindGlobalComponentProc:function(const AName:AnsiString):TComponent of object;

function MyFindGlobalComponent(const AName:AnsiString):TComponent;
begin
  Result:=MyFindGlobalComponentProc(AName);
end;

{ TJITForm }

constructor TJITForm.Create(AOwner: TComponent);
begin
  SetDesigning(true);
  inherited Create(AOwner);
end;

{ TJITForms }

constructor TJITForms.Create;
begin
  inherited Create;
  FForms:=TList.Create;
end;

destructor TJITForms.Destroy;
begin
  while FForms.Count>0 do DestroyJITForm(FForms.Count-1);
  FForms.Free;
  inherited Destroy;
end;

function TJITForms.GetItem(Index:integer):TForm;
begin
  Result:=TForm(FForms[Index]);
end;

function TJITForms.Count:integer;
begin
  Result:=FForms.Count;
end;

function TJITForms.IndexOf(JITForm:TForm):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>JITForm) do dec(Result);
end;

procedure TJITForms.DestroyJITForm(JITForm:TForm);
var a:integer;
begin
  if JITForm=nil then
    raise Exception.Create('TJITForms.DestroyJITForm JITForm=nil');
  a:=IndexOf(JITForm);
  if a<0 then
    raise Exception.Create('TJITForms.DestroyJITForm JITForm.ClassName='+
      JITForm.ClassName);
  if a>=0 then DestroyJITForm(a);
end;

procedure TJITForms.DestroyJITForm(Index:integer);
var OldClass:TClass;
begin
  OldClass:=Items[Index].ClassType;
  Items[Index].Free;
  FreevmtCopy(OldClass);
  FForms.Delete(Index);
end;

function TJITForms.FindFormByClassName(AClassName:shortstring):integer;
begin
  AClassName:=uppercase(AClassName);
  Result:=FForms.Count-1;
  while (Result>=0) and (uppercase(Items[Result].ClassName)<>AClassName) do
    dec(Result);
end;

function TJITForms.FindFormByName(AName:shortstring):integer;
begin
  AName:=uppercase(AName);
  Result:=FForms.Count-1;
  while (Result>=0) and (uppercase(Items[Result].Name)<>AName) do
    dec(Result);
end;

procedure TJITForms.GetUnusedNames(var FormName,FormClassName:shortstring);
var a:integer;
begin
  a:=1;
  repeat
    FormName:='Form'+IntToStr(a);
    FormClassName:='TForm'+IntToStr(a);
    inc(a);
  until (FindFormByName(FormName)<0) and (FindFormByClassName(FormClassName)<0);
end;

function TJITForms.DoCreateJITForm(
  NewFormName,NewClassName:shortstring):integer;
var
  Instance:TComponent;
begin
  Result:=-1;
  // create new class and an instance
  //writeln('[TJITForms.DoCreateJITForm] Creating new JIT class '''+NewClassName+''' ...');
  Pointer(FCurReadClass):=CreateVMTCopy(TJITForm,'TJITForm');
  //writeln('[TJITForms.DoCreateJITForm] Creating an instance of JIT class '''+NewClassName+''' ...');
  Instance:=TComponent(FCurReadClass.NewInstance);
  //writeln('[TJITForms.DoCreateJITForm] Initializing new instance ...');
  TComponent(FCurReadForm):=Instance;
  try
    Instance.Create(nil);
    if NewFormName<>'' then
      Instance.Name:=NewFormName;
    DoRenameClass(FCurReadClass,NewClassName);
  //writeln('[TJITForms.DoCreateJITForm] Initialization was successful! FormName="',NewFormName,'"');
  except
    TComponent(FCurReadForm):=nil;
    writeln('[TJITForms.DoCreateJITForm] Error while creating instance');
    raise;
  end;
  Result:=FForms.Add(FCurReadForm);
end;

function TJITForms.AddNewJITForm:integer;
var NewFormName,NewClassName:shortstring;
begin
  {$IFDEF IDE_VERBOSE}
  Writeln('[TJITForms] AddNewJITForm');
  {$ENDIF}
  GetUnusedNames(NewFormName,NewClassName);
  {$IFDEF IDE_VERBOSE}
  Writeln('NewFormName is ',NewFormname,', NewClassName is ',NewClassName);
  {$ENDIF}
  Result:=DoCreateJITForm(NewFormName,NewClassName);
end;

function TJITForms.GetClassNameFromStream(s:TStream):shortstring;
var Signature:shortstring;
  NameLen:byte;
begin
  Result:='';
  // read signature
  Signature:='1234';
  s.Read(Signature[1],length(Signature));
  if Signature<>'TPF0' then exit;
  // read classname length
  NameLen:=0;
  s.Read(NameLen,1);
  // read classname
  if NameLen>0 then begin
    SetLength(Result,NameLen);
    s.Read(Result[1],NameLen);
  end;
  s.Position:=0;
end;

function TJITForms.AddJITFormFromStream(BinStream:TStream):integer;
//  returns new index
// -1 = invalid stream

  procedure ApplyVisible;
  var
    i: integer;
    AControl: TControl;
  begin
    // The LCL has as default Visible=false. But for Delphi compatbility
    // loading control defaults to true.
    for i:=0 to FCurReadForm.ComponentCount-1 do begin
      AControl:=TControl(FCurReadForm.Components[i]);
      if (AControl is TControl) then begin
        if (not (csVisibleSetInLoading in AControl.ControlState)) then
          AControl.Visible:=true
        else
          AControl.ControlState:=
            AControl.ControlState-[csVisibleSetInLoading];
      end;
    end;
  end;

var
  Reader:TReader;
  NewClassName:shortstring;
  NewName: string;
begin
  Result:=-1;
  NewClassName:=GetClassNameFromStream(BinStream);
  if NewClassName='' then begin

    // Application.MessageBox('No classname in form stream found.','',mb_OK);
    MessageDlg('No classname in form stream found.',mterror,[mbOK],0);

    exit;
  end;
  {$IFDEF IDE_VERBOSE}
  writeln('[TJITForms.AddJITFormFromStream] 1');
  {$ENDIF}
  try
    Result:=DoCreateJITForm('',NewClassName);
    {$IFDEF IDE_VERBOSE}
    writeln('[TJITForms.AddJITFormFromStream] 2');
    {$ENDIF}

    Reader:=TReader.Create(BinStream,4096);
    MyFindGlobalComponentProc:=@OnFindGlobalComponent;
    FindGlobalComponent:=@MyFindGlobalComponent;

    {$IFDEF IDE_VERBOSE}
    writeln('[TJITForms.AddJITFormFromStream] 3');
    {$ENDIF}
    try
      // connect TReader events
      Reader.OnError:=@ReaderError;
      Reader.OnFindMethod:=@ReaderFindMethod;
      Reader.OnSetName:=@ReaderSetName;
      Reader.OnReferenceName:=@ReaderReferenceName;
      Reader.OnAncestorNotFound:=@ReaderAncestorNotFound;
      Reader.OnCreateComponent:=@ReaderCreateComponent;
      Reader.OnFindComponentClass:=@ReaderFindComponentClass;

      {$IFDEF IDE_VERBOSE}
      writeln('[TJITForms.AddJITFormFromStream] 4');
      {$ENDIF}
      InitReading;
      Reader.ReadRootComponent(FCurReadForm);
      if FCurReadForm.Name='' then begin
        NewName:=FCurReadForm.ClassName;
        if NewName[1] in ['T','t'] then
          System.Delete(NewName,1,1);
        FCurReadForm.Name:=NewName;
      end;

      {$IFDEF IDE_VERBOSE}
      writeln('[TJITForms.AddJITFormFromStream] 5');
      {$ENDIF}
      ApplyVisible;

      {$IFDEF IDE_VERBOSE}
      writeln('[TJITForms.AddJITFormFromStream] 6');
      {$ENDIF}
      FCurReadForm.Show;
    finally
      FindGlobalComponent:=nil;
      Reader.Free;
    end;
  except
    writeln('[TJITForms.AddJITFormFromStream] ERROR reading form stream'
       +' of Class ''',NewClassName,'''');
    Result:=-1;
  end;
end;

function TJITForms.OnFindGlobalComponent(const AName:AnsiString):TComponent;
begin
  Result:=Application.FindComponent(AName);
end;

procedure TJITForms.InitReading;
begin
  FCurReadComponentClass:=nil;
  FCurReadComponent:=nil;
  FCurReadErrorMsg:='';
end;

procedure TJITForms.AddNewMethod(JITForm:TForm; AName:ShortString);
begin
  CreateNewmethod(JITForm,AName);
end;

procedure TJITForms.RemoveMethod(JITForm:TForm; AName:ShortString);
var OldCode:Pointer;
begin
  if JITForm=nil then
    raise Exception.Create('TJITForms.RemoveMethod JITForm=nil');
  if IndexOf(JITForm)<0 then
    raise Exception.Create('TJITForms.RemoveMethod JITForm.ClassName='+
      JITForm.ClassName);
  OldCode:=nil;
  DoRemoveMethod(JITForm.ClassType,AName,OldCode);
  FreeMem(OldCode);
end;

procedure TJITForms.RenameMethod(JITForm:TForm; OldName,NewName:ShortString);
begin
  if JITForm=nil then
    raise Exception.Create('TJITForms.RenameMethod JITForm=nil');
  if IndexOf(JITForm)<0 then
    raise Exception.Create('TJITForms.RenameMethod JITForm.ClassName='+
      JITForm.ClassName);
  DoRenameMethod(JITForm.ClassType,OldName,NewName);
end;

procedure TJITForms.RenameFormClass(JITForm:TForm; NewName:ShortString);
begin
  if JITForm=nil then
    raise Exception.Create('TJITForms.RenameFormClass JITForm=nil');
  if IndexOf(JITForm)<0 then
    raise Exception.Create('TJITForms.RenameFormClass JITForm.ClassName='+
      JITForm.ClassName);
  DoRenameClass(JITForm.ClassType,NewName);
end;

function TJITForms.CreateNewMethod(JITForm: TForm; AName: ShortString): TMethod;
var CodeTemplate,NewCode:Pointer;
  CodeSize:integer;
  OldCode: Pointer;
begin
  if JITForm=nil then
    raise Exception.Create('TJITForms.CreateNewMethod JITForm=nil');
  if IndexOf(JITForm)<0 then
    raise Exception.Create('TJITForms.CreateNewMethod JITForm.ClassName='+
      JITForm.ClassName);
  OldCode:=JITForm.MethodAddress(AName);
  if OldCode<>nil then begin
    Result.Data:=JITForm;
    Result.Code:=OldCode;
    exit;
  end;
  CodeTemplate:=MethodAddress('DoNothing');
  CodeSize:=100; // !!! what is the real codesize of DoNothing? !!!
  GetMem(NewCode,CodeSize);
  Move(CodeTemplate^,NewCode^,CodeSize);
  DoAddNewMethod(JITForm.ClassType,AName,NewCode);
  Result.Data:=JITForm;
  Result.Code:=NewCode;
end;

//------------------------------------------------------------------------------
// adding, removing and renaming of classes and methods at runtime

type
   // these definitions are copied from objpas.inc

   TMethodNameRec = packed record
      Name : PShortString;
      Addr : Pointer;
   end;

   TMethodNameTable = packed record
     Count : DWord;
     Entries : packed array[0..0] of TMethodNameRec;
   end;

   PMethodNameTable =  ^TMethodNameTable;


function TJITForms.CreateVMTCopy(SourceClass:TClass;
  const NewClassName:ShortString):Pointer;
const
  vmtSize:integer=2000; //XXX how big is the vmt of class TJITForm ?
var MethodTable, NewMethodTable : PMethodNameTable;
  MethodTableSize: integer;
  ClassNamePtr, ClassNamePShortString: Pointer;
begin
//writeln('[TJITForms.CreatevmtCopy] SourceClass='''+SourceClass.ClassName+''''
// +' NewClassName='''+NewClassName+'''');
  // create copy of vmt
  GetMem(Result,vmtSize);
  // type of self is class of TJITForm => it points to the vmt
  Move(Pointer(SourceClass)^,Result^,vmtSize);
  // create copy of methodtable
  MethodTable:=PMethodNameTable((Pointer(SourceClass)+vmtMethodTable)^);
  if Assigned(MethodTable) then begin
    MethodTableSize:=SizeOf(DWord)+
                     MethodTable^.Count*SizeOf(TMethodNameRec);
    GetMem(NewMethodTable,MethodTableSize);
    Move(MethodTable^,NewMethodTable^,MethodTableSize);
    PPointer(Result+vmtMethodTable)^:=NewMethodTable;
  end;
  // create pointer to classname
  // set ClassNamePtr to point to the PShortString of ClassName 
  ClassNamePtr:=Pointer(Result)+vmtClassName;
  GetMem(ClassNamePShortString,SizeOf(ShortString));
  Pointer(ClassNamePtr^):=ClassNamePShortString;
  Move(NewClassName[0],ClassNamePShortString^,SizeOf(ShortString));
end;

procedure TJITForms.FreevmtCopy(vmtCopy:Pointer);

  procedure FreeNewMethods(MethodTable: PMethodNameTable);
  var
    CurCount, BaseCount, i: integer;
    BaseMethodTable: PMethodNameTable;
    CurMethod: TMethodNameRec;
  begin
    if MethodTable=nil then exit;
    BaseMethodTable:=PMethodNameTable((Pointer(TJITForm)+vmtMethodTable)^);
    if Assigned(BaseMethodTable) then
      BaseCount:=BaseMethodTable^.Count
    else
      BaseCount:=0;
    CurCount:=MethodTable^.Count;
    if CurCount=BaseCount then exit;
    i:=CurCount;
    while i>BaseCount do begin
      CurMethod:=MethodTable^.Entries[i-1];
      if CurMethod.Name<>nil then
        FreeMem(CurMethod.Name);
      if CurMethod.Addr<>nil then
        FreeMem(CurMethod.Addr);
      dec(i);
    end;
  end;

var
  MethodTable : PMethodNameTable;
  ClassNamePtr: Pointer;
begin
  //writeln('[TJITForms.FreevmtCopy] ClassName='''+TClass(vmtCopy).ClassName+'''');
  if vmtCopy=nil then exit;
  // free copy of methodtable
  MethodTable:=PMethodNameTable((Pointer(vmtCopy)+vmtMethodTable)^);
  if (Assigned(MethodTable)) then begin
    FreeNewMethods(MethodTable);
    FreeMem(MethodTable);
  end;
  // free pointer to classname
  ClassNamePtr:=Pointer(vmtCopy)+vmtClassName;
  FreeMem(Pointer(ClassNamePtr^));
  // free copy of VMT
  FreeMem(vmtCopy);
end;

procedure TJITForms.DoAddNewMethod(JITClass:TClass;
  AName:ShortString;  ACode:Pointer);
var OldMethodTable,NewMethodTable: PMethodNameTable;
  NewMethodTableSize:integer;
begin
  //writeln('[TJITForms.AddNewMethod] '''+JITClass.ClassName+'.'+AName+'''');
  OldMethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  if Assigned(OldMethodTable) then begin
    NewMethodTableSize:=SizeOf(DWord)+
                     (OldMethodTable^.Count + 1)*SizeOf(TMethodNameRec);
  end else begin
    NewMethodTableSize:=SizeOf(DWord)+SizeOf(TMethodNameRec);
  end;
  GetMem(NewMethodTable,NewMethodTableSize);
  if Assigned(OldMethodTable) then begin
    Move(OldMethodTable^,NewMethodTable^,
      NewMethodTableSize-SizeOf(TMethodNameRec));
    NewMethodTable^.Count:=NewMethodTable^.Count+1;
  end else begin
    NewMethodTable^.Count:=1;
  end;
  {$R-}
  //for a:=0 to NewMethodTable^.Count-2 do
  //  writeln(a,'=',NewMethodTable^.Entries[a].Name^,' $'
  //    ,HexStr(Integer(NewMethodTable^.Entries[a].Name),8));
  with NewMethodTable^.Entries[NewMethodTable^.Count-1] do begin
    GetMem(Name,256);
    Name^:=AName;
    Addr:=ACode;
  end;
  //for a:=0 to NewMethodTable^.Count-1 do
  //  writeln(a,'=',NewMethodTable^.Entries[a].Name^,' $'
  //    ,HexStr(Integer(NewMethodTable^.Entries[a].Name),8));
  {$R+}
  PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^):=NewMethodTable;
  if Assigned(OldMethodTable) then
    FreeMem(OldMethodTable);
end;

procedure TJITForms.DoRemoveMethod(JITClass:TClass;  AName:ShortString;
          var OldCode:Pointer);
var OldMethodTable, NewMethodTable: PMethodNameTable;
  NewMethodTableSize:integer;
  a:cardinal;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('[TJITForms.DoRemoveMethod] '''+JITClass.ClassName+'.'+AName+'''');
  {$ENDIF}
  AName:=uppercase(AName);
  OldMethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  OldCode:=nil;
  if Assigned(OldMethodTable) then begin
    a:=0;
    while a<OldMethodTable^.Count do begin
      {$R-}
      if uppercase(OldMethodTable^.Entries[a].Name^)=AName then begin
        OldCode:=OldMethodTable^.Entries[a].Addr;
        FreeMem(OldMethodTable^.Entries[a].Name);
        if OldMethodTable^.Count>0 then begin
          NewMethodTableSize:=SizeOf(DWord)+
                     OldMethodTable^.Count*SizeOf(TMethodNameRec);
          GetMem(NewMethodTable,NewMethodTableSize);
          NewMethodTable^.Count:=OldMethodTable^.Count-1;
          Move(OldMethodTable^,NewMethodTable^,SizeOf(DWord)+
                                                     a*SizeOf(TMethodNameRec));
          Move(OldMethodTable^.Entries[a],NewMethodTable^.Entries[a+1],
                 SizeOf(DWord)+a*SizeOf(TMethodNameRec));
        end else begin
          NewMethodTable:=nil;
        end;
        PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^):=NewMethodTable;
        FreeMem(OldMethodTable);
        break;
      end;
      {$R+}
      inc(a);
    end;
  end;
end;

procedure TJITForms.DoRenameMethod(JITClass:TClass;
  OldName,NewName:ShortString);
var MethodTable: PMethodNameTable;
  a:integer;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('[TJITForms.DoRenameMethod] ClassName='''+JITClass.ClassName+''''
    +' OldName='''+OldName+''' NewName='''+OldName+'''');
  {$ENDIF}
  OldName:=uppercase(OldName);
  MethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  if Assigned(MethodTable) then begin
    for a:=0 to MethodTable^.Count-1 do begin
      if uppercase(MethodTable^.Entries[a].Name^)=OldName then
        MethodTable^.Entries[a].Name^:=NewName;
    end;
  end;
end;

procedure TJITForms.DoRenameClass(JITClass:TClass; NewName:ShortString);
begin
  {$IFDEF IDE_VERBOSE}
  writeln('[TJITForms.DoRenameClass] OldName='''+JITClass.ClassName
    +''' NewName='''+NewName+''' ');
  {$ENDIF}
  PShortString((Pointer(JITClass)+vmtClassName)^)^:=NewName;
end;

//------------------------------------------------------------------------------

{
  TReader events.
  If a LFM is streamed back into the corresponfing TForm descendent, all methods
  and components are published members and TReader can set these values.
  But at design time we do not have the corresponding TForm descendent. And
  there is no compiled code, thus it must be produced it at runtime
  (just-in-time).
}

procedure TJITForms.DoNothing;
// this is the template procedure for all unknown procedures
begin
  // !!! do not write any code in here !!!
end;

procedure TJITForms.ReaderFindMethod(Reader: TReader;
  const FindMethodName: Ansistring;  var Address: Pointer; var Error: Boolean);
var NewMethod: TMethod;
begin
  {$IFDEF IDE_DEBUG}
  writeln('[TJITForms.ReaderFindMethod] A "'+FindMethodName+'" Address=',HexStr(Cardinal(Address),8));
  {$ENDIF}
  if Address=nil then begin
    // there is no method in the ancestor class with this name
    // => add a JIT method with this name to the JITForm
    NewMethod:=CreateNewMethod(FCurReadForm,FindMethodName);
    Address:=NewMethod.Code;
    Error:=false;
  end;
end;

procedure TJITForms.ReaderSetName(Reader: TReader; Component: TComponent;
  var NewName: Ansistring);
begin
//  writeln('[TJITForms.ReaderSetName] OldName="'+Component.Name+'" NewName="'+NewName+'"');
end;

procedure TJITForms.ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
begin
//  writeln('[TJITForms.ReaderReferenceName] Name='''+RefName+'''');
end;

procedure TJITForms.ReaderAncestorNotFound(Reader: TReader;
  const ComponentName: Ansistring;  ComponentClass: TPersistentClass;
  var Component: TComponent);
begin
// ToDo: this is for custom form templates
//  writeln('[TJITForms.ReaderAncestorNotFound] ComponentName='''+ComponentName
//    +''' Component='''+Component.Name+'''');
end;

procedure TJITForms.ReaderError(Reader: TReader; const ErrorMsg: Ansistring;
  var Handled: Boolean);
// ToDo: use SUnknownProperty when it is published by the fpc team
const
  SUnknownProperty = 'Unknown property';
var
  ErrorType: TJITFormError;
  Action: TModalResult;
begin
  ErrorType:=jfeReaderError;
  Action:=mrCancel;
  FCurReadErrorMsg:=ErrorMsg;
  // find out, what error occured
  if RightStr(ErrorMsg,length(SUnknownProperty))=SUnknownProperty then begin
    ErrorType:=jfeUnknownProperty;
    Action:=mrIgnore;
  end;
  if Assigned(OnReaderError) then
    OnReaderError(Self,ErrorType,Action);
  Handled:=Action in [mrIgnore];
  writeln('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  writeln('[TJITForms.ReaderError] "'+ErrorMsg+'" ignoring=',Handled);
  writeln('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
end;

procedure TJITForms.ReaderFindComponentClass(Reader: TReader;
  const FindClassName: Ansistring;  var ComponentClass: TComponentClass);
var
  RegComp:TRegisteredComponent;
  Action: TModalResult;
  ErrorType: TJITFormError;
begin
  fCurReadComponent:=nil;
  fCurReadComponentClass:=ComponentClass;
  if ComponentClass=nil then begin
    RegComp:=FRegCompList.FindComponentClassByName(FindClassName);
    if RegComp<>nil then begin
      //writeln('[TJITForms.ReaderFindComponentClass] '''+FindClassName
      //   +''' is registered');
      ComponentClass:=RegComp.ComponentClass;
    end else begin
      writeln('[TJITForms.ReaderFindComponentClass] '''+FindClassName
         +''' is unregistered');
      Action:=mrCancel;
      ErrorType:=jfeUnknownComponentClass;
      if Assigned(OnReaderError) then
        OnReaderError(Self,ErrorType,Action);
    end;
  end;
end;

procedure TJITForms.ReaderCreateComponent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
  fCurReadComponent:=Component;
  fCurReadComponentClass:=ComponentClass;
//  writeln('[TJITForms.ReaderCreateComponent] Class='''+ComponentClass.ClassName+'''');
end;

//==============================================================================


end.

