unit datasettoform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

Type
  TInputType = (itText, itPassword, itNumber, itTel, itTime, itURL, itImage, itMonth, itRange, itHidden, itCheckBox, itDate, itEmail, itColor, itFile, itRadio, itTextArea, itSelect);

  TEntryData = Record
    FieldName : String;
    FieldLabel: String;
    DataType : TFieldType;
    Size : integer;
    Required : Boolean;
  end;
  TEntryDataArray = array of TEntryData;

  { TDataFieldEntryItem }

  TDataFieldEntryItem = class(TCollectionItem)
  private
    FClasses: String;
    FEnabled: Boolean;
    FEntryLabel: string;
    FFieldName: String;
    FInputID: String;
    FInputName: String;
    FInPutType: TInputType;
    FItems: TStrings;
    FMaxLength: Integer;
    FMaxValue: Integer;
    FMinValue: Integer;
    FPattern: String;
    FPlaceHolder: String;
    FRequired: Boolean;
    function GetInputID: String;
    function GetInputName: String;
    procedure SetItems(AValue: TStrings);
  Public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function GetDisplayName: string; override;
  Published
    Property Enabled : Boolean Read FEnabled Write FEnabled;
    Property FieldName : String Read FFieldName Write FFieldName;
    Property InputName : String Read GetInputName Write FInputName;
    Property InputID : String Read GetInputID Write FInputID;
    Property InputType : TInputType Read FInPutType Write FInputType;
    Property Items : TStrings Read FItems Write SetItems;
    Property MinValue : Integer Read FMinValue Write FMinValue;
    Property MaxValue : Integer Read FMaxValue Write FMaxValue;
    Property MaxLength : Integer Read FMaxLength Write FMaxLength;
    Property Classes : String Read FClasses Write FClasses;
    Property Required : Boolean Read FRequired Write FRequired;
    Property Pattern : String Read FPattern Write FPattern;
    Property PlaceHolder : String Read FPlaceHolder Write FPlaceHolder;
    Property EntryLabel : string Read FEntryLabel Write FEntryLabel;
  end;
  TDataFieldEntryItemClass = Class of TDataFieldEntryItem;

  { TDataFieldEntryList }

  TDataFieldEntryList = class(TCollection)
  private
    function GetEntry(aIndex: Integer): TDataFieldEntryItem;
    procedure SetEntry(aIndex: Integer; AValue: TDataFieldEntryItem);
  Public
    Property Entries[aIndex: Integer] : TDataFieldEntryItem Read GetEntry Write SetEntry; default;
  end;

  { TDataFormGenerator }

  TDataFormGenerator = class(TComponent)
  private
    FAddFormTag: Boolean;
    FCurrIndent: Integer;
    FFormAction: String;
    FFormMethod: String;
    FFormTarget: String;
  Protected
    function GetBasicInputTag(aEntry: TDataFieldEntryItem): string; virtual;
    function GetExtraAttributes(aEntry: TDataFieldEntryItem): string; virtual;
    function GetInputClasses(aEntry: TDataFieldEntryItem): string; virtual;
    function GetOptionTag(aEntry: TDataFieldEntryItem; const aItem: String): String; virtual;
    function GetSelectTag(aEntry: TDataFieldEntryItem; aBaseAttributes, aClasses, aExtraAttributes: string): string; virtual;
    function GetTextAreaTag(aEntry: TDataFieldEntryItem; aBaseAttributes, aClasses, aExtraAttributes: string): string; virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Class Function EntryClass : TDataFieldEntryItemClass; virtual;
    Procedure IncIndent;
    Procedure DecIndent;
    Function Indent : String;
    Function HeaderHTML : String; virtual;
    Function GenerateFieldHTML(aEntry : TDataFieldEntryItem) : String; virtual;
    Function FooterHTML : String; virtual;
    Class procedure Register;
    Class procedure UnRegister;
    Class Function RegisterName : string;
  Published
    Property AddFormTag : Boolean Read FAddFormTag Write FAddFormTag;
    Property FormAction : String Read FFormAction Write FFormAction;
    Property FormMethod : String Read FFormMethod Write FFormMethod;
    Property FormTarget: String Read FFormTarget Write FFormTarget;
    Property CurrIndent : Integer Read FCurrIndent Write FCurrIndent;
  end;
  TDataFormGeneratorClass = class of TDataFormGenerator;

  TSeparatorStyle = (ssParagraph,ssDiv,ssTable);

  { TSimpleFormGenerator }

  TSimpleFormGenerator = class(TDataFormGenerator)
  private
    FEntryCellClass: String;
    FLabelCellClass: String;
    FSeparatorClasses: String;
    FSeparatorStyle: TSeparatorStyle;
    FTableClasses: String;
    FTableTagID: String;
    function GetCellattributes(aEntry: TDataFieldEntryItem; IsLabel: boolean): String;
    function GetSeparatorAttributes(aEntry: TDataFieldEntryItem): String;
  protected
    Function GetTableAttributes : string;
  Public
    Function HeaderHTML : String; override;
    Function GenerateFieldHTML(aEntry : TDataFieldEntryItem) : String; override;
    Function FooterHTML : String; override;
  Published
    Property SeparatorStyle : TSeparatorStyle Read FSeparatorStyle Write FSeparatorStyle;
    Property SeparatorClasses : String Read FSeparatorClasses Write FSeparatorClasses;
    Property LabelCellClass : String Read FLabelCellClass Write FLabelCellClass;
    Property EntryCellClass : String Read FEntryCellClass Write FEntryCellClass;
    Property TableTagID : String Read FTableTagID Write FTableTagID;
    Property TableClasses : String Read FTableClasses Write FTableClasses;
  end;

  { TDatasetHTMLGenerator }

  TDatasetHTMLGenerator = class(TComponent)
  Private
    Type
       TGeneratorDef = Record
         aName: String;
         aClass : TDataFormGeneratorClass;
       end;
     Class var  FGenerators : Array of TGeneratorDef;
     procedure CheckRecreateEntries(aEntryClass: TDataFieldEntryItemClass);
     function GetInputType(fieldname: string; aFieldType: TFieldType): TInputType;
  private
    FDatasource: TDatasource;
    FFieldEntries: TDataFieldEntryList;
    FFormGenerator: TDataFormGenerator;
    procedure SetDatasource(AValue: TDatasource);
    procedure SetFieldEntries(AValue: TDataFieldEntryList);
    procedure SetFormGenerator(AValue: TDataFormGenerator);
  protected
    function CreateEntries(aEntryClass : TDataFieldEntryItemClass) :  TDataFieldEntryList; virtual;
    Function GetEntryData : TEntryDataArray; virtual;
    function GenerateFieldHTML(Def: TDataFieldEntryItem): String; virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  Published
    Class procedure RegisterGenerator(aName : string; aGeneratorClass: TDataFormGeneratorClass);
    Class procedure UnRegisterGenerator(aName : string);
    Class Procedure GetGeneratorNames(aList : TStrings);
    Class function GetGeneratorClass(aName : string) : TDataFormGeneratorClass;
    Procedure PopulateEntries;
    Procedure GetHTML(aHTML : TStrings);
    Function GetHTML : String;
    Property FieldEntries : TDataFieldEntryList Read FFieldEntries Write SetFieldEntries;
    Property Datasource : TDatasource Read FDatasource Write SetDatasource;
    Property FormGenerator : TDataFormGenerator Read FFormGenerator Write SetFormGenerator;
  end;

Function  GetInputNamePrefix(aType : TInPutType): string;

implementation

Const
  LF = SLineBreak;
  InputTypeNames : Array[TInputType] of string
    = ('text', 'password', 'number', 'tel', 'time', 'url', 'image', 'month', 'range', 'hidden', 'checkBox', 'date', 'email', 'color', 'file', 'radio', '', '');

Function  GetInputNamePrefix(aType : TInputType): string;

begin
  Case aType of
    itCheckBox : Result:='cbx';
    itSelect : Result:='sel';
    itTextArea : Result:='mem';
    itHidden : Result:='hid';
    itRadio : Result:='rb';
  else
    Result:='edt';
  end;
end;

{ TDataFieldEntryItem }


procedure TDataFieldEntryItem.SetItems(AValue: TStrings);
begin
  if FItems=AValue then Exit;
  FItems.Assign(AValue);
end;

function TDataFieldEntryItem.GetInputID: String;
begin
  Result:=FInputID;
  if Result='' then
    Result:=GetInputNamePrefix(FInPutType)+FieldName;
end;

function TDataFieldEntryItem.GetInputName: String;
begin
  Result:=FInputName;
  if (Result='') then
    Result:=FieldName;
end;

procedure TDataFieldEntryItem.Assign(Source: TPersistent);
var
  aSource: TDataFieldEntryItem absolute source;
begin
  if Source is TDataFieldEntryItem then
  begin
    MinValue:=aSource.MinValue;
    MaxValue:=aSource.MaxValue;
    Items:=aSource.Items;
    InputType:=aSource.InputType;
    FInputName:=aSource.FInputName;
    FInputID:=aSource.FInputID;
    FieldName:=aSource.FieldName;
    Enabled:=aSource.Enabled;
    Classes:=aSource.Classes;
    MaxLength:=aSource.MaxLength;
    Required:=aSource.Required;
    Pattern:=aSource.pattern;
    PlaceHolder:=aSource.PlaceHolder;
    EntryLabel:=aSource.EntryLabel;
  end else
    inherited Assign(Source);
end;

constructor TDataFieldEntryItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FItems:=TStringList.Create;
end;

destructor TDataFieldEntryItem.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TDataFieldEntryItem.GetDisplayName: string;
begin
  Result:=FieldName;
  if Result='' then
    Result:=Inherited GetDisplayName;
end;

{ TDataFieldEntryList }

function TDataFieldEntryList.GetEntry(aIndex: Integer): TDataFieldEntryItem;
begin
  Result:=Items[aIndex] as TDataFieldEntryItem;
end;

procedure TDataFieldEntryList.SetEntry(aIndex: Integer; AValue: TDataFieldEntryItem);
begin
  Items[aIndex]:=aValue;
end;

{ TDataFormGenerator }

function TDataFormGenerator.HeaderHTML: String;
begin
  Result:='';
  if AddFormTag then
    begin
    Result:='<form';
    if FormAction<>'' then
      Result:=Result+Format(' action="%s"',[FormAction]);
    if FormMethod<>'' then
      Result:=Result+Format(' method="%s"',[FormMethod]);
    if FormTarget<>'' then
      Result:=Result+Format(' target="%s"',[FormTarget]);
    Result:=Result+' >';
    Result:=Indent+Result;
    IncIndent;
    end;
end;

function TDataFormGenerator.GetOptionTag(aEntry: TDataFieldEntryItem; const aItem : String) : String;

var
  V,D : String;
  P : Integer;

begin
  V:='';
  D:=aItem;
  P:=Pos('=',D);
  if P>0 then
    begin
    V:=Copy(aItem,1,P-1);
    Delete(D,1,P);
    end;
  Result:='<option ';
  if V<>'' then
    Result:=Result+Format(' value="%s"',[V]);
  Result:=Indent+Result+'>'+D+'</option>';

  if aEntry=nil then ;
end;

function TDataFormGenerator.GetInputClasses(aEntry: TDataFieldEntryItem) : string;

begin
  Result:=aEntry.Classes;
end;

function TDataFormGenerator.GetSelectTag(aEntry: TDataFieldEntryItem; aBaseAttributes, aClasses, aExtraAttributes : string) : string;

var
  S : String;
  I : Integer;

begin
  With AEntry do
    Result:=Format('<select %s class="%s" %s>',[aBaseAttributes,aClasses, aExtraAttributes])+LF;
  // For the other lines we must manage indent ourselves.
  IncIndent;
  for I:=0 to aEntry.Items.Count-1 do
    begin
    S:=GetOptionTag(aEntry,aEntry.Items[i]);
    if S<>'' then
      Result:=Result+S+LF;
    end;
  DecIndent;
  Result:=Result+Indent+'</select>'
end;

function TDataFormGenerator.GetTextAreaTag(aEntry: TDataFieldEntryItem; aBaseAttributes, aClasses, aExtraAttributes : string) : string;

begin
  Result:=Format('<textarea %s class="%s" %s></textarea>',[aBaseAttributes,aClasses,aExtraAttributes]);
  if aEntry=nil then ;
end;

constructor TDataFormGenerator.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FormAction:='javascript:void(0)';
end;

class function TDataFormGenerator.EntryClass: TDataFieldEntryItemClass;
begin
  Result:=TDataFieldEntryItem
end;

procedure TDataFormGenerator.IncIndent;
begin
  Inc(FCurrIndent,2);
end;

procedure TDataFormGenerator.DecIndent;
begin
  if FCurrIndent>=2 then
    Dec(FCurrIndent,2);
end;

function TDataFormGenerator.Indent: String;
begin
  Result:=StringOfChar(' ',FCurrIndent);
end;

function TDataFormGenerator.GetExtraAttributes(aEntry: TDataFieldEntryItem) : string;

begin
  Result:='';
  With aEntry do
  Case InputType of
    itText,
    itTextArea:
      begin
      if MaxLength>0 then
        Result:=Result+Format(' maxlen="%d"',[MaxLength]);
      if PlaceHolder<>'' then
        Result:=Result+Format(' placeholder="%d"',[PlaceHolder]);
      end;
    itNumber :
      begin
      if MinValue>0 then
        Result:=Result+Format(' min="%d"',[MinValue]);
      if MaxValue>0 then
        Result:=Result+Format(' min="%d"',[MinValue]);
      end;

  end;
  With aEntry do
    begin
    if Required and (InputType in [itText,itURL,itTel,itEmail,itPassword,itDate,itNumber,itCheckBox,itFile,itRadio]) then
       Result:=Result+' required';
    if (Pattern<>'') and (InputType in [itText,itURL,itTel,itEmail,itPassword]) then
      Result:=Result+Format(' pattern="%s"',[Pattern]);
    end;
end;

function TDataFormGenerator.GetBasicInputTag(aEntry: TDataFieldEntryItem) : string;

var
  aBaseAttributes : String;
  aExtra : String;
  aClasses : String;

begin
  With aEntry do
    aBaseAttributes:=Format('name="%s" id="%s"',[InputName,InputID]);
  aExtra:=GetExtraAttributes(aEntry);
  aClasses:=GetInputClasses(aEntry);
  Case aEntry.InputType of
    itSelect : Result:=GetSelectTag(aEntry, aBaseAttributes, aClasses,aExtra);
    itTextArea : Result:=GetTextAreaTag(aEntry, aBaseAttributes, aClasses, aExtra);
  else
    aExtra:=GetExtraAttributes(aEntry);
    aClasses:=GetInputClasses(aEntry);
    With aEntry do
      Result:=Format('<input type="%s" %s class="%s" %s>',[InputTypeNames[aEntry.InputType],aBaseAttributes,aClasses,aExtra]);
  end;
  Result:=Result+LF;
end;

function TDataFormGenerator.GenerateFieldHTML(aEntry: TDataFieldEntryItem): String;
begin
  Result:=Indent+GetBasicInputTag(aEntry);
end;


function TDataFormGenerator.FooterHTML: String;
begin
  Result:='';
  if AddFormTag then
    begin
    Result:=Indent+'</form>';
    DecIndent;
    end;
end;

class procedure TDataFormGenerator.Register;
begin
  TDatasetHTMLGenerator.RegisterGenerator(RegisterName,Self);
end;

class procedure TDataFormGenerator.UnRegister;
begin
  TDatasetHTMLGenerator.UnRegisterGenerator(RegisterName);
end;

class function TDataFormGenerator.RegisterName: string;
begin
  Result:=ClassName;
  if Result[1]='T' then
    Delete(Result,1,1);
  Result:=StringReplace(Result,'FormGenerator','',[rfIgnoreCase]);
  Result:=StringReplace(Result,'DataGenerator','',[rfIgnoreCase]);
  Result:=StringReplace(Result,'Generator','',[rfIgnoreCase]);
end;

{ TSimpleFormGenerator }

function TSimpleFormGenerator.GetTableAttributes: string;
begin
  Result:='';
  if (TableTagID<>'') then
    Result:=Format(' id="%s" ',[TableTagID]);
  if TableClasses<>'' then
    Result:=Format(' class="%s" ',[TableClasses]);
end;


function TSimpleFormGenerator.HeaderHTML: String;
begin
  Result:=inherited HeaderHTML;
  if SeparatorStyle=ssTable then
    begin
    Result:=Result+Format('<table %s>',[GetTableAttributes]);
    end;
end;

function TSimpleFormGenerator.GetSeparatorAttributes(aEntry: TDataFieldEntryItem): String;

begin
  Result:='';
  if SeparatorClasses<>'' then
    Result:=Result+Format(' class="%s" ',[SeparatorClasses]);
  if aEntry=nil then ;
end;

function TSimpleFormGenerator.GetCellattributes(aEntry: TDataFieldEntryItem; IsLabel : boolean): String;

var
  aClass: String;
begin
  Result:='';
  if IsLabel then
    aClass:=LabelCellClass
  else
    aClass:=EntryCellClass;
  if aClass<>'' then
    Result:=Result+Format(' class="%s"',[aClass]);
  if aEntry=nil then ;
end;

function TSimpleFormGenerator.GenerateFieldHTML(aEntry: TDataFieldEntryItem): String;
begin
  Result:=inherited GenerateFieldHTML(aEntry);
  Case SeparatorStyle of
    ssTable:
      Result:=Format('<tr %s><td %s>%s</td><td %s>%s</td></tr>',[GetSeparatorAttributes(aEntry),GetCellAttributes(aEntry,True),aEntry.EntryLabel,GetCellAttributes(aEntry,False),Result]);
    ssDiv:
      Result:=Format('<div %s>%s</div>',[GetSeparatorAttributes(aEntry),Result]);
    ssParagraph:
      Result:=Format('<p %s>%s</p>',[GetSeparatorAttributes(aEntry),Result]);
  end;
end;

function TSimpleFormGenerator.FooterHTML: String;
begin
  Result:='';
  if SeparatorStyle=ssTable then
    Result:=Result+'</table>';
  Result:=Result+inherited FooterHTML;
end;

{ TDatasetHTMLGenerator }

procedure TDatasetHTMLGenerator.SetDatasource(AValue: TDatasource);
begin
  if FDatasource=AValue then Exit;
  if Assigned(FDatasource) then FDatasource.RemoveFreeNotification(Self);
  FDatasource:=AValue;
  if Assigned(FDatasource) then FDatasource.FreeNotification(Self);
end;

procedure TDatasetHTMLGenerator.SetFieldEntries(AValue: TDataFieldEntryList);
begin
  if FFieldEntries=AValue then Exit;
  FFieldEntries.Assign(AValue);
end;

procedure TDatasetHTMLGenerator.CheckRecreateEntries(aEntryClass : TDataFieldEntryItemClass);

var
  aOld,aEntries : TDataFieldEntryList;

begin
  if (aEntryClass=FFieldEntries.ItemClass) then
    Exit;
  aEntries:=CreateEntries(aEntryClass);
  try
    aEntries.Assign(FFieldEntries);
  except
    aEntries.Free;
    Raise;
  end;
  aOld:=FFieldEntries;
  FFieldEntries:=aEntries;
  FreeAndNil(aOld);
end;

procedure TDatasetHTMLGenerator.SetFormGenerator(AValue: TDataFormGenerator);

var
  aEntryClass : TDataFieldEntryItemClass;

begin
  if FFormGenerator=AValue then Exit;
  if Assigned(FFormGenerator) then FFormGenerator.RemoveFreeNotification(Self);
  FFormGenerator:=AValue;
  if Assigned(FFormGenerator) then
    begin
    FFormGenerator.FreeNotification(Self);
    aEntryClass:=FFormGenerator.EntryClass;
    end
  else
    aEntryClass:=TDataFieldEntryItem;
  CheckRecreateEntries(aEntryClass);
end;

constructor TDatasetHTMLGenerator.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFieldEntries:=CreateEntries(TDataFieldEntryItem);
end;

function TDatasetHTMLGenerator.CreateEntries(aEntryClass : TDataFieldEntryItemClass) : TDataFieldEntryList;

begin
  if aEntryClass=Nil then
    aEntryClass:=TDataFieldEntryItem;
  Result:=TDataFieldEntryList.Create(aEntryClass);
end;

destructor TDatasetHTMLGenerator.Destroy;
begin
  FreeAndNil(FFieldEntries);
  inherited Destroy;
end;

class procedure TDatasetHTMLGenerator.RegisterGenerator(aName: string; aGeneratorClass: TDataFormGeneratorClass);

var
  Len : Integer;

begin
  Len:=Length(FGenerators);
  SetLength(FGenerators,Len+1);
  FGenerators[Len].aName:=aName;
  FGenerators[Len].aClass:=aGeneratorClass;
end;

class procedure TDatasetHTMLGenerator.UnRegisterGenerator(aName: string);
var
  Len : Integer;

begin
  Len:=Length(FGenerators)-1;
  While (Len>=0) and not SameText(FGenerators[Len].aName,aName) do
    Dec(Len);
  if Len<>-1 then
    Delete(FGenerators,Len,1);
end;

class procedure TDatasetHTMLGenerator.GetGeneratorNames(aList: TStrings);

var
  i : Integer;

begin
  For I:=0 to Length(FGenerators)-1 do
    aList.Add(FGenerators[i].aName);
end;

class function TDatasetHTMLGenerator.GetGeneratorClass(aName: string): TDataFormGeneratorClass;
var
  Len : Integer;

begin
  Len:=Length(FGenerators)-1;
  While (Len>=0) and not SameText(FGenerators[Len].aName,aName) do
    Dec(Len);
  if Len<>-1 then
    Result:=FGenerators[Len].aClass
  else
    Result:=Nil;
end;

function TDatasetHTMLGenerator.GetEntryData: TEntryDataArray;

var
  DS : TDataset;
  i : Integer;
  F : TField;
  FD : TFieldDef;

begin
  Result:=[];
  DS:=Nil;
  if (Assigned(Datasource) and Assigned(Datasource.Dataset)) then
    Ds:=Datasource.Dataset;
  if DS=Nil then exit;
  if (DS.Fields.Count>0) then
    begin
    SetLength(Result,DS.Fields.Count);
    For I:=0 to DS.Fields.Count-1 do
      begin
      F:=DS.Fields[i];
      Result[i].DataType:=F.DataType;
      Result[i].FieldName:=F.FieldName;
      Result[i].FieldLabel:=F.DisplayLabel;
      Result[i].Required:=F.Required;
      Result[i].Size:=F.Size;
      end;
    end
  else
    begin
    SetLength(Result,DS.FieldDefs.Count);
    For I:=0 to DS.FieldDefs.Count-1 do
      begin
      FD:=DS.FieldDefs[i];
      Result[i].DataType:=FD.DataType;
      Result[i].FieldName:=FD.Name;
      Result[i].FieldLabel:=FD.Name;
      Result[i].Required:=FD.Required;
      Result[i].Size:=FD.Size;
      end;
    end;

end;


function TDatasetHTMLGenerator.GetInputType(fieldname : string; aFieldType : TFieldType) : TInputType;

begin
  Case aFieldType of
    ftSmallint, ftInteger, ftWord,
    ftLargeint, ftBCD,ftFMTBcd,ftFloat,ftCurrency : Result:=itNumber;
    ftAutoInc : Result:=itHidden;
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    ftIDispatch, ftGuid, ftBytes, ftVarBytes, ftBlob, ftGraphic, ftParadoxOle, ftDBaseOle,
    ftTypedBinary, ftCursor :
       Result:=itHidden;
    ftMemo, ftFmtMemo, ftWideMemo :
      Result:=itTextArea;
    ftDate: result:=itDate;
    ftTime: result:=itTime;
    ftDateTime : Result:=itDate;
    ftBoolean : Result:=itCheckbox;
  else
    if pos('password',lowercase(fieldname))>0 then
      result:=itPassword
    else if pos('email',lowercase(fieldname))>0 then
      Result:=itEmail
    else
      Result:=itText;
  end;
end;

procedure TDatasetHTMLGenerator.PopulateEntries;

Const
  ForbiddenTypes = [ftAutoInc,ftUnknown,ftBytes,ftCursor,ftDataSet,ftDBaseOle,ftGraphic,ftADT, ftArray, ftReference,
                    ftOraBlob, ftOraClob, ftInterface, ftIDispatch]+ftBlobTypes;
var
  Data : TEntryDataArray;
  Entry : TEntryData;
  Def : TDataFieldEntryItem;

begin
  Data:=GetEntryData;
  if Length(Data)=0 then exit;
  FieldEntries.Clear;
  For Entry in Data do
    begin
    Def:=FieldEntries.Add as TDataFieldEntryItem;
    Def.InputType:=GetInputType(Entry.FieldName,Entry.DataType);
    Def.FieldName:=Entry.FieldName;
    Def.EntryLabel:=Entry.FieldLabel;
    Def.Enabled:=not (Entry.DataType in ForbiddenTypes);
    Def.MaxLength:=Entry.Size;
    Def.Required:=Entry.Required;
    end;
end;

procedure TDatasetHTMLGenerator.GetHTML(aHTML: TStrings);
begin
  aHTML.Text:=GetHTML;
end;

function TDatasetHTMLGenerator.GenerateFieldHTML(Def : TDataFieldEntryItem) : String;

begin
  Result:=FormGenerator.GenerateFieldHTML(Def)+LF;
end;

function TDatasetHTMLGenerator.GetHTML: String;

Var
  Def : TDataFieldEntryItem;
  I : integer;

begin
  Result:='';
  if not Assigned(FormGenerator) then exit;
  Result:=FormGenerator.HeaderHTML+LF;
  For I:=0 to FieldEntries.Count-1 do
    begin
    Def:=FieldEntries[I];
    if Def.Enabled then
      Result:=Result+GenerateFieldHTML(Def);
    end;
  Result:=Result+FormGenerator.FooterHTML+LF;
end;

initialization
  TSimpleFormGenerator.Register;
end.

