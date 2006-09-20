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

  Author: Mattias Gaertner

  Abstract:
    Defines a converter and tools to modify a Text. The Text can be a file,
    a string or a TStrings.
    Packages can register extra tools, which the IDE user can then be put
    together to define a series of changes. For example several Find&Replace
    tools can be added and then executed automatically.
    For an extensive example, see the package in components/h2pas/.
}
unit IDETextConverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, Forms, FileUtil, SrcEditorIntf,
  PropEdits;
  
type
  TCustomTextConverterTool = class;

  TTextConverterType = (
    tctSource,
    tctFile,
    tctStrings
    );

  { TIDETextConverter
    A component to hold a Text and tools to change the Text.
    For example to do several find and replace operations on the text.
    The Text can be a file, a string or TStrings.
    The Text is converted on the fly, whenever someone reads/write one of the
    formats.
    The tools are decendants of TCustomTextConverterTool. }

  TIDETextConverter = class(TComponent)
  private
    FFilename: string;
    FSource: string;
    FStrings: TStrings;
    FCurrentType: TTextConverterType;
    FFileIsTemporary: boolean;
    FStringsIsTemporary: Boolean;
    procedure CreateTempFilename;
    function GetFilename: string;
    function GetSource: string;
    function GetStrings: TStrings;
    procedure RemoveStrings;
    procedure SaveToFile(const NewFilename: string);
    procedure SetFilename(const AValue: string);
    procedure SetSource(const AValue: string);
    procedure SetStrings(const AValue: TStrings);
    procedure SetCurrentType(const AValue: TTextConverterType);
    procedure SetFileIsTemporary(const AValue: boolean);
    procedure SetStringsIsTemporary(const AValue: Boolean);
  protected
    function GetTempFilename: string; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Execute(ToolList: TComponent): TModalResult;// run the tools
    function LoadFromFile(const AFilename: string;
                          UseIDECache: Boolean = true;
                          UpdateFromDisk: Boolean = true;
                          Revert: Boolean = false
                          ): Boolean; virtual;
    procedure InitWithFilename(const AFilename: string);
    procedure InitWithSource(const ASource: string);
    procedure InitWithStrings(const aStrings: TStrings);
    property CurrentType: TTextConverterType read FCurrentType write SetCurrentType;
    property Source: string read GetSource write SetSource;
    property Filename: string read GetFilename write SetFilename;
    property Strings: TStrings read GetStrings write SetStrings;
    property FileIsTemporary: boolean read FFileIsTemporary write SetFileIsTemporary;
    property StringsIsTemporary: Boolean read FStringsIsTemporary write SetStringsIsTemporary;
  end;

  { TCustomTextConverterTool
    An abstract component to change a Text (TIDETextConverter). }

  TCustomTextConverterTool = class(TComponent)
  private
    FCaption: string;
    FDescription: string;
    FEnabled: boolean;
    procedure SetCaption(const AValue: string);
    procedure SetDescription(const AValue: string);
  public
    class function ClassDescription: string; virtual; abstract;//the first line should be a short title
    class function FirstLineOfClassDescription: string;
    constructor Create(TheOwner: TComponent); override;
    function Execute(aText: TIDETextConverter): TModalResult; virtual; abstract;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Description: string read FDescription write SetDescription;
    property Enabled: boolean read FEnabled write FEnabled default True;
  end;
  TCustomTextConverterToolClass = class of TCustomTextConverterTool;

  { TCustomTextReplaceTool
    A tool to do a 'find and replace' in a text. }

  TTextReplaceToolOption = (
    trtMatchCase,       // search case sensitive
    trtWholeWord,       // search at word boundaries
    trtRegExpr,         // use regular expressions for find and replace
    trtMultiLine        // ignore type of line endings in pattern (e.g. #10 = #13#10)
    //TODO trtSearchInReplacement,// when replaced, continue search at start of replacement, instead of end of replacement
    //TODO trtReplaceUntilNotFound// restart replace until pattern not found
    );
  TTextReplaceToolOptions = set of TTextReplaceToolOption;
  
  TCustomTextReplaceTool = class(TCustomTextConverterTool)
  private
    FOptions: TTextReplaceToolOptions;
    FReplaceWith: string;
    FSearchFor: string;
    procedure SetOptions(const AValue: TTextReplaceToolOptions);
    procedure SetReplaceWith(const AValue: string);
    procedure SetSearchFor(const AValue: string);
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
    procedure Assign(Source: TPersistent); override;
    property SearchFor: string read FSearchFor write SetSearchFor;
    property ReplaceWith: string read FReplaceWith write SetReplaceWith;
    property Options: TTextReplaceToolOptions read FOptions write SetOptions;
  end;
  
  { TTextReplaceTool }

  TTextReplaceTool = class(TCustomTextReplaceTool)
  published
    property SearchFor;
    property ReplaceWith;
    property Options;
  end;
  
  { TTextConverterToolClasses
    A list to hold the registered TCustomTextConverterToolClass(es) }

  TTextConverterToolClasses = class
  private
    FItems: TFPList;// list of TCustomTextConverterToolClass
    function GetCount: integer;
    function GetItems(Index: integer): TCustomTextConverterToolClass;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterClass(AClass: TCustomTextConverterToolClass);
    procedure UnregisterClass(AClass: TCustomTextConverterToolClass);
    function FindByName(const aClassName: string): TCustomTextConverterToolClass;
    function FindByFirstLineOfClassDescription(const Line: string
                                               ): TCustomTextConverterToolClass;
    procedure FindClass(Reader: TReader; const aClassName: string;
                        var ComponentClass: TComponentClass);
    property Items[Index: integer]: TCustomTextConverterToolClass read GetItems; default;
    property Count: integer read GetCount;
    function GetTempFilename: string; virtual; abstract;
    function LoadFromFile(Converter: TIDETextConverter; const AFilename: string;
                          UpdateFromDisk, Revert: Boolean): Boolean; virtual; abstract;
  end;
  
var
  TextConverterToolClasses: TTextConverterToolClasses = nil;// set by the IDE
  
procedure ClearTextConverterToolList(List: TComponent);
procedure CopyTextConverterToolList(Src, Dest: TComponent;
                                    ClearDest: boolean = true);
  
procedure MakeToolNameUnique(List: TComponent;
                             NewTool: TCustomTextConverterTool);
procedure MakeToolCaptionUnique(List: TComponent;
                                NewTool: TCustomTextConverterTool);
procedure MakeToolCaptionAndNameUnique(List: TComponent;
                                       NewTool: TCustomTextConverterTool);
function AddNewTextConverterTool(List: TComponent;
             NewClass: TCustomTextConverterToolClass): TCustomTextConverterTool;
                                       

implementation


procedure MakeToolNameUnique(List: TComponent;
  NewTool: TCustomTextConverterTool);
var
  NewName: String;

  procedure MakeValidIdentifier;
  var
    i: Integer;
  begin
    for i:=length(NewName) downto 1 do
      if not (NewName[i] in ['0'..'9','_','a'..'z','A'..'Z']) then
        System.Delete(NewName,i,1);
    if (NewName<>'') and (NewName[1] in ['0'..'9']) then
      NewName:='_'+NewName;
  end;

  function NameIsUnique: Boolean;
  var
    i: Integer;
    CurTool: TCustomTextConverterTool;
  begin
    MakeValidIdentifier;
    if NewName='' then exit(false);
    for i:=0 to List.ComponentCount-1 do begin
      CurTool:=TCustomTextConverterTool(List.Components[i]);
      if CurTool=NewTool then continue;
      if CompareText(CurTool.Name,NewName)=0 then exit(false);
    end;
    Result:=true;
    NewTool.Name:=NewName;
  end;

begin
  NewName:=NewTool.Name;
  if NameIsUnique then exit;
  NewName:=NewTool.FirstLineOfClassDescription;
  if NewName='' then NewName:=NewTool.ClassName;
  while not NameIsUnique do
    NewName:=CreateNextIdentifier(NewName);
end;

procedure MakeToolCaptionUnique(List: TComponent;
  NewTool: TCustomTextConverterTool);
var
  NewCaption: String;

  function CaptionIsUnique: Boolean;
  var
    i: Integer;
    CurTool: TCustomTextConverterTool;
  begin
    if NewCaption='' then exit(false);
    for i:=0 to List.ComponentCount-1 do begin
      CurTool:=TCustomTextConverterTool(List.Components[i]);
      if CurTool=NewTool then continue;
      if CompareText(CurTool.Caption,NewCaption)=0 then exit(false);
    end;
    Result:=true;
    NewTool.Caption:=NewCaption;
  end;

begin
  NewCaption:=NewTool.Caption;
  if CaptionIsUnique then exit;
  NewCaption:=NewTool.FirstLineOfClassDescription;
  if NewCaption='' then NewCaption:=NewTool.ClassName;
  while not CaptionIsUnique do
    NewCaption:=CreateNextIdentifier(NewCaption);
end;

procedure MakeToolCaptionAndNameUnique(List: TComponent;
  NewTool: TCustomTextConverterTool);
begin
  MakeToolNameUnique(List,NewTool);
  MakeToolCaptionUnique(List,NewTool);
end;

function AddNewTextConverterTool(List: TComponent;
  NewClass: TCustomTextConverterToolClass): TCustomTextConverterTool;
begin
  Result:=NewClass.Create(List);
  MakeToolCaptionAndNameUnique(List,Result);
  if Result.Caption='' then RaiseGDBException('');
end;

procedure ClearTextConverterToolList(List: TComponent);
begin
  if List=nil then exit;
  while List.ComponentCount>0 do
    List.Components[List.ComponentCount-1].Free;
end;

procedure CopyTextConverterToolList(Src, Dest: TComponent; ClearDest: boolean);
var
  i: Integer;
  SrcTool: TCustomTextConverterTool;
  NewTool: TCustomTextConverterTool;
begin
  if ClearDest then
    ClearTextConverterToolList(Dest);
  for i:=0 to Src.ComponentCount-1 do begin
    SrcTool:=Src.Components[i] as TCustomTextConverterTool;
    NewTool:=TCustomTextConverterToolClass(SrcTool.ClassType).Create(Dest);
    NewTool.Assign(SrcTool);
    NewTool.Name:=SrcTool.Name;
  end;
end;

{ TIDETextConverter }

procedure TIDETextConverter.SetFilename(const AValue: string);
begin
  SaveToFile(AValue);
end;

function TIDETextConverter.GetFilename: string;
begin
  CurrentType:=tctFile;
  Result:=FFilename;
end;

function TIDETextConverter.GetSource: string;
begin
  CurrentType:=tctSource;
  Result:=FSource;
end;

function TIDETextConverter.GetStrings: TStrings;
begin
  CurrentType:=tctStrings;
  Result:=FStrings;
end;

procedure TIDETextConverter.RemoveStrings;
begin
  if StringsIsTemporary then
    FStrings.Free;
  FStrings:=nil;
  FStringsIsTemporary:=false;
end;

procedure TIDETextConverter.SetSource(const AValue: string);
begin
  FCurrentType:=tctSource;
  RemoveStrings;
  FSource:=AValue;
end;

procedure TIDETextConverter.SetStrings(const AValue: TStrings);
begin
  FCurrentType:=tctStrings;
  if (AValue<>FStrings) and StringsIsTemporary then
    FreeAndNil(FStrings);
  FStrings:=AValue;
  FStringsIsTemporary:=false;
end;

procedure TIDETextConverter.SetCurrentType(const AValue: TTextConverterType);
var
  fs: TFileStream;
begin
  if FCurrentType=AValue then exit;
  //DebugLn(['TIDETextConverter.SetCurrentType ',ord(FCurrentType),' ',ord(AValue)]);
  case AValue of
  tctSource:
    // convert to Source
    begin
      FSource:='';
      case FCurrentType of
      tctStrings:
        if FStrings<>nil then begin
          FSource:=FStrings.Text;
          RemoveStrings;
        end;
      tctFile:
        if FileExists(FFilename) then begin
          fs:=TFileStream.Create(FFilename,fmOpenRead);
          try
            SetLength(FSource,fs.Size);
            fs.Read(FSource[1],length(FSource));
          finally
            fs.Free;
          end;
          if FileIsTemporary then begin
            DeleteFile(FFilename);
          end;
        end;
      end;
    end;
  tctStrings:
    // convert to TStrings
    begin
      if FStrings<>nil then
        RaiseGDBException('TTextConverterText.SetCurrentType FStrings<>nil');
      FStrings:=TStringList.Create;
      fStringsIsTemporary:=true;
      case FCurrentType of
      tctSource:
        begin
          FStrings.Text:=FSource;
          FSource:='';
        end;
      tctFile:
        if FileExists(FFilename) then begin
          FStrings.LoadFromFile(FFilename);
          if FileIsTemporary then begin
            DeleteFile(FFilename);
          end;
        end;
      end;
    end;
  tctFile:
    // convert to File
    begin
      // keep old Filename, so that a Filename, Source, Filename combination
      // uses the same Filename
      if FFilename='' then
        CreateTempFilename;
      case FCurrentType of
      tctSource:
        begin
          fs:=TFileStream.Create(FFilename,fmCreate);
          try
            if FSource<>'' then begin
              fs.Write(FSource[1],length(FSource));
              FSource:='';
            end;
          finally
            fs.Free;
          end;
        end;
      tctStrings:
        if FStrings<>nil then begin
          FStrings.SaveToFile(FFilename);
          RemoveStrings;
        end;
      end;
    end;
  end;
  FCurrentType:=AValue;
end;

procedure TIDETextConverter.SetFileIsTemporary(const AValue: boolean);
begin
  if FFileIsTemporary=AValue then exit;
  FFileIsTemporary:=AValue;
end;

procedure TIDETextConverter.SaveToFile(const NewFilename: string);
var
  fs: TFileStream;
  TrimmedFilename: String;
begin
  TrimmedFilename:=TrimFilename(NewFilename);
  case CurrentType of
  tctFile:
    if (FFilename<>'') and (FFilename<>TrimmedFilename)
    and (FileExists(FFilename)) then
      RenameFile(FFilename,TrimmedFilename);
  tctSource:
    begin
      fs:=TFileStream.Create(TrimmedFilename,fmCreate);
      try
        if FSource<>'' then
          fs.Write(FSource[1],length(FSource));
      finally
        fs.Free;
      end;
    end;
  tctStrings:
    begin
      fStrings.SaveToFile(TrimmedFilename);
      RemoveStrings;
    end;
  end;
  FCurrentType:=tctFile;
  FFilename:=TrimmedFilename;
end;

procedure TIDETextConverter.CreateTempFilename;
begin
  FFilename:=GetTempFilename;
  FFileIsTemporary:=true;
end;

procedure TIDETextConverter.SetStringsIsTemporary(const AValue: Boolean);
begin
  if FStringsIsTemporary=AValue then exit;
  FStringsIsTemporary:=AValue;
end;

function TIDETextConverter.GetTempFilename: string;
begin
  if TextConverterToolClasses<>nil then
    Result:=TextConverterToolClasses.GetTempFilename;
  if Result='' then
    Result:='temp.txt';
end;

constructor TIDETextConverter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCurrentType:=tctSource;
end;

destructor TIDETextConverter.Destroy;
begin
  RemoveStrings;
  inherited Destroy;
end;

procedure TIDETextConverter.Clear;
begin
  FFilename:='';
  FSource:='';
  RemoveStrings;
  FCurrentType:=tctSource;
end;

function TIDETextConverter.Execute(ToolList: TComponent): TModalResult;
var
  i: Integer;
  Tool: TCustomTextConverterTool;
  CurResult: TModalResult;
begin
  Result:=mrOk;
  for i:=0 to ToolList.ComponentCount-1 do begin
    if ToolList.Components[i] is TCustomTextConverterTool then begin
      Tool:=TCustomTextConverterTool(ToolList.Components[i]);
      if Tool.Enabled then begin
        CurResult:=Tool.Execute(Self);
        if CurResult=mrIgnore then
          Result:=mrCancel
        else if CurResult<>mrOk then
          exit(mrAbort);
      end;
    end;
  end;
end;

function TIDETextConverter.LoadFromFile(const AFilename: string;
  UseIDECache: Boolean; UpdateFromDisk: Boolean; Revert: Boolean): Boolean;
var
  fs: TFileStream;
begin
  if UseIDECache and (TextConverterToolClasses<>nil) then begin
    //DebugLn(['TIDETextConverter.LoadFromFile using IDE cache']);
    Result:=TextConverterToolClasses.LoadFromFile(Self,AFilename,
                                                  UpdateFromDisk,Revert);
  end else begin
    //DebugLn(['TIDETextConverter.LoadFromFile loading directly CurrentType=',ord(CurrentType),' FFilename="',FFilename,'"']);
    Result:=false;
    try
      case CurrentType of
      tctSource:
        begin
          fs:=TFileStream.Create(AFilename,fmOpenRead);
          try
            SetLength(FSource,fs.Size);
            if fSource<>'' then
              fs.Read(fSource[1],length(fSource));
          finally
            fs.Free;
          end;
        end;
      tctFile:
        CopyFile(AFilename,FFilename);
      tctStrings:
        FStrings.LoadFromFile(AFilename);
      end;
      Result:=true;
    except
    end;
  end;
end;

procedure TIDETextConverter.InitWithFilename(const AFilename: string);
begin
  Clear;
  FCurrentType:=tctFile;
  FFilename:=AFilename;
end;

procedure TIDETextConverter.InitWithSource(const ASource: string);
begin
  Clear;
  FCurrentType:=tctSource;
  FSource:=ASource;
end;

procedure TIDETextConverter.InitWithStrings(const aStrings: TStrings);
begin
  Clear;
  FCurrentType:=tctStrings;
  FStrings:=aStrings;
end;

{ TCustomTextConverterTool }

procedure TCustomTextConverterTool.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

procedure TCustomTextConverterTool.SetDescription(const AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
end;

constructor TCustomTextConverterTool.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Enabled:=true;
end;

procedure TCustomTextConverterTool.Assign(Source: TPersistent);
var
  Src: TCustomTextConverterTool;
begin
  if Source is TCustomTextConverterTool then begin
    Src:=TCustomTextConverterTool(Source);
    Caption:=Src.Caption;
    Description:=Src.Description;
  end else
    inherited Assign(Source);
end;

class function TCustomTextConverterTool.FirstLineOfClassDescription: string;
var
  p: Integer;
begin
  Result:=ClassDescription;
  p:=1;
  while (p<=length(Result)) do begin
    if Result[p] in [#10,#13] then begin
      Result:=copy(Result,1,p-1);
      exit;
    end;
    inc(p);
  end;
end;

{ TCustomTextReplaceTool }

procedure TCustomTextReplaceTool.SetOptions(
  const AValue: TTextReplaceToolOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TCustomTextReplaceTool.SetReplaceWith(const AValue: string);
begin
  if FReplaceWith=AValue then exit;
  FReplaceWith:=AValue;
end;

procedure TCustomTextReplaceTool.SetSearchFor(const AValue: string);
begin
  if FSearchFor=AValue then exit;
  FSearchFor:=AValue;
end;

function TCustomTextReplaceTool.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Source: String;
  Flags: TSrcEditSearchOptions;
  Prompt: Boolean;
begin
  //DebugLn(['TCustomTextReplaceTool.Execute ',dbgsName(Self),' aText=',dbgsName(aText),' SearchFor="',dbgstr(SearchFor),'"']);
  Result:=mrCancel;
  if aText=nil then exit;
  if SearchFor='' then exit(mrOk);
  Source:=aText.Source;
  Flags:=[sesoReplace,sesoReplaceAll];
  if trtMatchCase in Options then Include(Flags,sesoMatchCase);
  if trtWholeWord in Options then Include(Flags,sesoWholeWord);
  if trtRegExpr in Options then Include(Flags,sesoRegExpr);
  if trtMultiLine in Options then Include(Flags,sesoMultiLine);
  Prompt:=false;
  Result:=IDESearchInText('',Source,SearchFor,ReplaceWith,Flags,Prompt,nil);
  if Result=mrOk then
    aText.Source:=Source;
  //DebugLn(['TCustomTextReplaceTool.Execute END Result=',Result=mrOk]);
end;

procedure TCustomTextReplaceTool.Assign(Source: TPersistent);
var
  Src: TCustomTextReplaceTool;
begin
  if Source is TCustomTextReplaceTool then begin
    Src:=TCustomTextReplaceTool(Source);
    SearchFor:=Src.SearchFor;
    ReplaceWith:=Src.ReplaceWith;
    Options:=Src.Options;
  end;
  inherited Assign(Source);
end;

class function TCustomTextReplaceTool.ClassDescription: string;
begin
  Result:='Search and replace';
end;

{ TTextConverterToolClasses }

function TTextConverterToolClasses.GetCount: integer;
begin
  if Self<>nil then
    Result:=FItems.Count
  else
    Result:=0;
end;

function TTextConverterToolClasses.GetItems(Index: integer
  ): TCustomTextConverterToolClass;
begin
  Result:=TCustomTextConverterToolClass(FItems[Index]);
end;

constructor TTextConverterToolClasses.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TTextConverterToolClasses.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TTextConverterToolClasses.RegisterClass(
  AClass: TCustomTextConverterToolClass);
begin
  if Self=nil then exit;
  if FItems.IndexOf(AClass)<0 then
    FItems.Add(AClass);
end;

procedure TTextConverterToolClasses.UnregisterClass(
  AClass: TCustomTextConverterToolClass);
begin
  if Self=nil then exit;
  FItems.Remove(AClass);
end;

function TTextConverterToolClasses.FindByName(const aClassName: string
  ): TCustomTextConverterToolClass;
var
  i: Integer;
begin
  if Self<>nil then
    for i:=0 to FItems.Count-1 do begin
      Result:=Items[i];
      if CompareText(Result.ClassName,aClassName)=0 then exit;
    end;
  Result:=nil;
end;

function TTextConverterToolClasses.FindByFirstLineOfClassDescription(
  const Line: string): TCustomTextConverterToolClass;
var
  i: Integer;
begin
  if Self<>nil then
    for i:=0 to FItems.Count-1 do begin
      Result:=Items[i];
      if Result.FirstLineOfClassDescription=Line then exit;
    end;
  Result:=nil;
end;

procedure TTextConverterToolClasses.FindClass(Reader: TReader;
  const aClassName: string; var ComponentClass: TComponentClass);
begin
  if Reader=nil then ;
  ComponentClass:=FindByName(aClassName);
end;

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TCustomTextReplaceTool, 'SearchFor', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TCustomTextReplaceTool, 'ReplaceWith', TStringMultilinePropertyEditor);

end.

