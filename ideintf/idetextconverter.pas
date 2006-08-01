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
  Classes, SysUtils, LCLProc, Controls, Forms, FileUtil, SrcEditorIntf;
  
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
    function GetFilename: string;
    function GetSource: string;
    function GetStrings: TStrings;
    procedure SetFilename(const AValue: string);
    procedure SetSource(const AValue: string);
    procedure SetStrings(const AValue: TStrings);
    procedure SetCurrentType(const AValue: TTextConverterType);
    procedure SetFileIsTemporary(const AValue: boolean);
    procedure SaveToFile(const NewFilename: string);
    procedure CreateTempFilename;
  protected
    function GetTempFilename: string; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ToolList: TComponent): TModalResult;// run the tools
    property CurrentType: TTextConverterType read FCurrentType write SetCurrentType;
    property Source: string read GetSource write SetSource;
    property Filename: string read GetFilename write SetFilename;
    property Strings: TStrings read GetStrings write SetStrings;
    property FileIsTemporary: boolean read FFileIsTemporary write SetFileIsTemporary;
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
    function Execute(aText: TIDETextConverter): TModalResult; virtual; abstract;
    procedure Assign(Source: TPersistent); override;
    class function ClassDescription: string; virtual; abstract;
    class function FirstLineOfClassDescription: string;
  published
    property Name;
    property Caption: string read FCaption write SetCaption;
    property Description: string read FDescription write SetDescription;
    property Enabled: boolean read FEnabled write FEnabled;
  end;
  TCustomTextConverterToolClass = class of TCustomTextConverterTool;

  { TCustomTextReplaceTool
    A tool to do a 'find and replace' in a text. }

  TTextReplaceToolOption = (
    trtMatchCase,       // search case sensitive
    trtWholeWord,       // search at word boundaries
    trtRegExpr,         // use regular expressions for find and replace
    trtMultiLine        // ignore line boundaries. The expression can span multiple lines.
    //TODO trtSearchInReplacement,// when replaced, continue search at start of replacement, instead of end of replacement
    //TODO trtReplaceUntilNotFound// restart replace until the pattern not found
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
    function Execute(aText: TIDETextConverter): TModalResult; override;
    procedure Assign(Source: TPersistent); override;
    class function ClassDescription: string; override;
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
  end;
  
var
  TextConverterToolClasses: TTextConverterToolClasses = nil;// set by the IDE
  
procedure ClearTextConverterToolList(List: TComponent);
procedure CopyTextConverterToolList(Src, Dest: TComponent;
                                    ClearDest: boolean = true);
  
implementation

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

procedure TIDETextConverter.SetSource(const AValue: string);
begin
  FCurrentType:=tctSource;
  FSource:=AValue;
end;

procedure TIDETextConverter.SetStrings(const AValue: TStrings);
begin
  FCurrentType:=tctStrings;
  FStrings:=AValue;
end;

procedure TIDETextConverter.SetCurrentType(const AValue: TTextConverterType);
var
  fs: TFileStream;
begin
  if FCurrentType=AValue then exit;
  case AValue of
  tctSource:
    // convert to Source
    begin
      FSource:='';
      case FCurrentType of
      tctStrings:
        if FStrings<>nil then begin
          FSource:=FStrings.Text;
          FreeAndNil(FStrings);
        end;
      tctFile:
        if FileExists(FFilename) then begin
          fs:=TFileStream.Create(FFilename,fmCreate);
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
          FreeAndNil(FStrings);
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
  inherited Destroy;
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

procedure TCustomTextConverterTool.Assign(Source: TPersistent);
var
  Src: TCustomTextConverterTool;
begin
  if Source is TCustomTextConverterTool then begin
    Src:=TCustomTextConverterTool(Source);
    Name:=Src.Name;
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
  Result:=mrCancel;
  if aText=nil then exit;
  Source:=aText.Source;
  Flags:=[];
  if trtMatchCase in Options then Include(Flags,sesoMatchCase);
  if trtWholeWord in Options then Include(Flags,sesoWholeWord);
  if trtRegExpr in Options then Include(Flags,sesoRegExpr);
  if trtMultiLine in Options then Include(Flags,sesoMultiLine);
  Prompt:=false;
  Result:=IDESearchInText('',Source,SearchFor,ReplaceWith,Flags,Prompt,nil);
  if Result=mrOk then
    aText.Source:=Source;
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
  Result:=FItems.Count;
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
  if FItems.IndexOf(AClass)<0 then
    FItems.Add(AClass);
end;

procedure TTextConverterToolClasses.UnregisterClass(
  AClass: TCustomTextConverterToolClass);
begin
  FItems.Remove(AClass);
end;

function TTextConverterToolClasses.FindByName(const aClassName: string
  ): TCustomTextConverterToolClass;
var
  i: Integer;
begin
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

end.

