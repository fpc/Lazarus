unit jitforms;
{
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
    -activate SetDesigning in TJITForm.Create when LCL is ready for components
      in designing state
}

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, CompReg, Forms, Controls, LCLLinux;

type
  //----------------------------------------------------------------------------
  // Just-In-Time-Form List
  TJITForms = class(TPersistent)
  private
    FForms: TList;
    FCurReadForm:TForm;
    FCurReadClass:TClass;
    FRegCompList:TRegisteredComponentList;
    // jit procedures
    function CreatevmtCopy(SourceClass:TClass; const NewClassName:ShortString):Pointer;
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
    procedure ReaderError(Reader: TReader; const Message: Ansistring;
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
  public
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
    procedure RemoveMethod(JITForm:TForm; AName:ShortString);
    procedure RenameMethod(JITForm:TForm; OldName,NewName:ShortString);
    procedure RenameFormAndClass(JITForm:TForm; NewName:ShortString);
    constructor Create;
    destructor Destroy; override;
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
// XXX ToDo: uncomment this when LCL is ready for csDesigning
  //SetDesigning(true);
  inherited Create(AOwner);
end;

{ TJITForms }

constructor TJITForms.Create;
begin
  inherited Create;
  FForms:=TList.Create;
end;

destructor TJITForms.Destroy;
var a:integer;
begin
  for a:=0 to FForms.Count-1 do
    DestroyJITForm(a);
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
  a:=IndexOf(JITForm);
  if a>=0 then DestroyJITForm(a);
end;

procedure TJITForms.DestroyJITForm(Index:integer);
var OldClass:TClass;
begin
  OldClass:=Items[Index].ClassType;
  Items[Index].Free;
  FreevmtCopy(OldClass);
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
writeln('[TJITForms.DoCreateJITForm] Creating new JIT class '''+NewClassName+''' ...');
  Pointer(FCurReadClass):=CreatevmtCopy(TJITForm,'TJITForm');
writeln('[TJITForms.DoCreateJITForm] Creating an instance of JIT class '''+NewClassName+''' ...');
  Instance:=TComponent(FCurReadClass.NewInstance);
writeln('[TJITForms.DoCreateJITForm] Initializing new instance ...');
  TComponent(FCurReadForm):=Instance;
  try
    Instance.Create(nil);
    Writeln('----------------------------------');
    Writeln('New form name is '+NewFormName);
    Writeln('----------------------------------');
    Writeln('----------------------------------');
    if NewFormName<>'' then
      Instance.Name:=NewFormName;
    DoRenameClass(FCurReadClass,NewClassName);
writeln('[TJITForms.DoCreateJITForm] Initialization was successful!');
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
  Writeln('[TJITForms] AddNewJITForm');
  GetUnusedNames(NewFormName,NewClassName);
  Writeln('Newformname is '+NewFormname);
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
//  0 = ok
// -1 = invalid stream
var
  Reader:TReader;
  NewClassName:shortstring;
  a:integer;
begin
  Result:=0;
  NewClassName:=GetClassNameFromStream(BinStream);
  if NewClassName='' then begin
    Application.MessageBox('No classname in form stream found.','',mb_OK);
    Result:=-1;  exit;
  end;
writeln('[TJITForms.AddJITFormFromStream] 1');
  try
    Result:=DoCreateJITForm('',NewClassName);
writeln('[TJITForms.AddJITFormFromStream] 2');

    Reader:=TReader.Create(BinStream,4096);
    MyFindGlobalComponentProc:=@OnFindGlobalComponent;
    FindGlobalComponent:=@MyFindGlobalComponent;

writeln('[TJITForms.AddJITFormFromStream] 3');
    try
      // connect TReader events
      Reader.OnError:=@ReaderError;
      Reader.OnFindMethod:=@ReaderFindMethod;
      Reader.OnSetName:=@ReaderSetName;
      Reader.OnReferenceName:=@ReaderReferenceName;
      Reader.OnAncestorNotFound:=@ReaderAncestorNotFound;
      Reader.OnCreateComponent:=@ReaderCreateComponent;
      Reader.OnFindComponentClass:=@ReaderFindComponentClass;

writeln('[TJITForms.AddJITFormFromStream] 4');
      Reader.ReadRootComponent(FCurReadForm);

writeln('[TJITForms.AddJITFormFromStream] 5');
      // MG: workaround til visible=true is default
      for a:=0 to FCurReadForm.ComponentCount-1 do begin
        if FCurReadForm.Components[a] is TControl then
          TControl(FCurReadForm.Components[a]).Visible:=true;
      end;
      // MG: end of workaround

writeln('[TJITForms.AddJITFormFromStream] 6');
      FCurReadForm.Show;
    finally
      FindGlobalComponent:=nil;
      Reader.Free;
    end;
    Result:=0;
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

procedure TJITForms.AddNewMethod(JITForm:TForm; AName:ShortString);
var CodeTemplate,NewCode:Pointer;
  CodeSize:integer;
begin
  if JITForm.MethodAddress(AName)<>nil then exit;
  CodeTemplate:=MethodAddress('DoNothing');
  CodeSize:=100; // !!! what is the real codesize of DoNothing? !!!
  GetMem(NewCode,CodeSize);
  Move(CodeTemplate^,NewCode^,CodeSize);
  DoAddNewMethod(JITForm.ClassType,AName,NewCode);
end;

procedure TJITForms.RemoveMethod(JITForm:TForm; AName:ShortString);
var OldCode:Pointer;
begin
  OldCode:=nil;
  DoRemoveMethod(JITForm.ClassType,AName,OldCode);
  FreeMem(OldCode);
end;

procedure TJITForms.RenameMethod(JITForm:TForm; OldName,NewName:ShortString);
begin
  DoRenameMethod(JITForm.ClassType,OldName,NewName);
end;

procedure TJITForms.RenameFormAndClass(JITForm:TForm; NewName:ShortString);
begin
  DoRenameClass(JITForm.ClassType,NewName);
  JITForm.Name:=NewName;
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


function TJITForms.CreatevmtCopy(SourceClass:TClass;
  const NewClassName:ShortString):Pointer;
const
  vmtSize:integer=2000; //XXX how big is the vmt of class TJITForm ?
var MethodTable, NewMethodTable : PMethodNameTable;
  MethodTableSize: integer;
begin
//writeln('[TJITForms.CreatevmtCopy] SourceClass='''+SourceClass.ClassName+''''
//   +' NewClassName='''+NewClassName+'''');
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
  PShortString((Pointer(Result)+vmtClassName)^)^:=NewClassName;
end;

procedure TJITForms.FreevmtCopy(vmtCopy:Pointer);
var MethodTable : PMethodNameTable;
begin
//writeln('[TJITForms.FreevmtCopy] ClassName='''+TClass(vmtCopy).ClassName+'''');
  if vmtCopy=nil then exit;
  MethodTable:=PMethodNameTable((Pointer(vmtCopy)+vmtMethodTable)^);
  if (Assigned(MethodTable)) then
    FreeMem(MethodTable);
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
writeln('[TJITForms.RemoveMethod] '''+JITClass.ClassName+'.'+AName+'''');
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
writeln('[TJITForms.RenameMethod] ClassName='''+JITClass.ClassName+''''
    +' OldName='''+OldName+''' NewName='''+OldName+'''');
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
writeln('[TJITForms.RenameClass] OldName='''+JITClass.ClassName
 +''' NewName='''+NewName+''' ');
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
begin
//  writeln('[TJITForms.ReaderFindMethod] '''+FindMethodName+'''');
  if Address=nil then begin
    AddNewMethod(FCurReadForm,FindMethodName);
    Error:=false;
  end;
end;

procedure TJITForms.ReaderSetName(Reader: TReader; Component: TComponent;
  var NewName: Ansistring);
begin
//  writeln('[TJITForms.ReaderSetName] OldName='''+Component.Name+''' NewName='''+NewName+'''');
end;

procedure TJITForms.ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
begin
//  writeln('[TJITForms.ReaderReferenceName] Name='''+RefName+'''');
end;

procedure TJITForms.ReaderAncestorNotFound(Reader: TReader;
  const ComponentName: Ansistring;  ComponentClass: TPersistentClass;
  var Component: TComponent);
begin
//  writeln('[TJITForms.ReaderAncestorNotFound] ComponentName='''+ComponentName
//    +''' Component='''+Component.Name+'''');
end;

procedure TJITForms.ReaderError(Reader: TReader; const Message: Ansistring;
  var Handled: Boolean);
begin
  writeln('[TJITForms.ReaderError] '''+Message+'''');
end;

procedure TJITForms.ReaderFindComponentClass(Reader: TReader;
  const FindClassName: Ansistring;  var ComponentClass: TComponentClass);
var
  RegComp:TRegisteredComponent;
begin
  if ComponentClass=nil then begin
    RegComp:=FRegCompList.FindComponentClassByName(FindClassName);
    if RegComp<>nil then begin
      //write('[TJITForms.ReaderFindComponentClass] '''+FindClassName
      //   +''' is registered');
      ComponentClass:=RegComp.ComponentClass;
    end else begin
      write('[TJITForms.ReaderFindComponentClass] '''+FindClassName
         +''' is unregistered');
    end;
  end;
  writeln('');
end;

procedure TJITForms.ReaderCreateComponent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
//  writeln('[TJITForms.ReaderCreateComponent] Class='''+ComponentClass.ClassName+'''');
end;


//==============================================================================


end.
