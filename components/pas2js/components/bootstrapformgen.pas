unit bootstrapformgen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, datasettoform;

Type

  { TBootstrapDataFieldEntryItem }

  TBootstrapDataFieldEntryItem = class(TDataFieldEntryItem)
  private
    FEditColWidth: Integer;
    FUseInputGroup: Boolean;
  Public
    procedure Assign(Source: TPersistent); override;

  Published
    property EditColWidth : Integer Read FEditColWidth Write FEditColWidth default 0;
    Property UseInputGroup : Boolean Read FUseInputGroup Write FUseInputGroup default false;
  end;

  { TBootstrapFormGenerator }

  TBootstrapFormGenerator = class(TDataFormGenerator)
  private
    FBottomMargin: Integer;
    FEditlColWidth: Integer;
    FHorizontalForm: Boolean;
    FLabelColWidth: Integer;
    FUseInputGroup: Boolean;
  Protected
    function GetEditColWidth(aEntry: TDataFieldEntryItem): Integer; virtual;
    function GetEntryEnd(aEntry: TDataFieldEntryItem): String; virtual;
    function GetEntryStart(aEntry: TDataFieldEntryItem): String; virtual;
    function GetFormLabel(aEntry: TDataFieldEntryItem): String; virtual;
    function GetInputClasses(aEntry: TDataFieldEntryItem): string; override;
    function GetInputGroup(aEntry: TDataFieldEntryItem): String; virtual;
    function GetInputGroupEnd(aEntry: TDataFieldEntryItem): String;virtual;
    function GetRowCol(aEntry: TDataFieldEntryItem): String; virtual;
    function GetRowColEnd(aEntry: TDataFieldEntryItem): String; virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Function GenerateFieldHTML(aEntry : TDataFieldEntryItem) : String; override;
    class function EntryClass: TDataFieldEntryItemClass; override;
  Published
    Property HorizontalForm : Boolean Read FHorizontalForm Write FHorizontalForm default true;
    Property UseInputGroup : Boolean Read FUseInputGroup Write FUseInputGroup default false;
    Property LabelColWidth : Integer Read FLabelColWidth Write FLabelColWidth default 2;
    Property BottomMargin : Integer Read FBottomMargin Write FBottomMargin default 2;
  end;

implementation

const
  LF = sLineBreak;

{ TBootstrapDataFieldEntryItem }

procedure TBootstrapDataFieldEntryItem.Assign(Source: TPersistent);
var
  aSource: TBootstrapDataFieldEntryItem;
begin
  if Source is TBootstrapDataFieldEntryItem then
  begin
    aSource:=TBootstrapDataFieldEntryItem(Source);
    EditColWidth:=aSource.EditColWidth;
    UseInputGroup:=aSource.UseInputGroup;
  end;
  inherited Assign(Source);
end;

{ TBootstrapFormGenerator }

function TBootstrapFormGenerator.GetRowCol(aEntry: TDataFieldEntryItem): String;

var
  S : String;

begin
  Result:='';
  if FBottomMargin>0 then
    S:='mb-'+IntToStr(FBottomMargin);
  Result:=Result+Indent+Format('<div class="row %s">',[S])+LF;
  IncIndent;
end;

function TBootstrapFormGenerator.GetRowColEnd(aEntry: TDataFieldEntryItem): String;
begin
  Decindent;
  Result:=Indent+'</div>  <!-- .col -->'+LF;
  Decindent;
  Result:=Indent+'</div> <!-- .row -->'+Lf;
end;

constructor TBootstrapFormGenerator.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLabelColWidth:=2;
  FBottomMargin:=2;
  FHorizontalForm:=True;
end;

function TBootstrapFormGenerator.GetFormLabel(aEntry: TDataFieldEntryItem): String;

var
  lClass : string;

begin
  lClass:='form-label';
  if HorizontalForm then
    lClass:=Format('col-%d col-form-label',[FLabelColWidth]);
  Result:=Indent+Format('<label for="%s" class="%s">%s</label>',[aEntry.InputID,lClass,aEntry.EntryLabel])+LF;
end;

function TBootstrapFormGenerator.GetInputClasses(aEntry: TDataFieldEntryItem): string;

var
  extra : String;
begin
  Result:=Inherited GetInputClasses(aEntry);
  extra:='';
  Case aEntry.InputType of
    itHidden : ;
    itSelect : extra:='form-select';
    itRange : extra:='form-range-input';
    itRadio,
    itCheckBox : extra:='form-check-input';
  else
    extra:='form-control';
  end;
  if Extra<>'' then
    Result:=Result+' '+Extra;
end;

function TBootstrapFormGenerator.GetEntryStart(aEntry: TDataFieldEntryItem): String;

begin
  Result:='';
  if HorizontalForm then
    begin
    Result:=Indent+Format('<div class="col-%d">',[GetEditColWidth(aEntry)])+LF;
    IncIndent;
    end;
end;

function TBootstrapFormGenerator.GetEditColWidth(aEntry : TDataFieldEntryItem): Integer;

var
  be : TBootstrapDataFieldEntryItem absolute aEntry ;

begin
  Result:=12-FLabelColWidth;
  if aEntry is TBootstrapDataFieldEntryItem then
    begin
    if Be.EditColWidth>0 then
      Result:=Be.EditColWidth;
    end;
end;

function TBootstrapFormGenerator.GetEntryEnd(aEntry: TDataFieldEntryItem): String;

begin
  Result:='';
  if HorizontalForm then
    begin
    DecIndent;
    Result:=Indent+Format('</div> <!-- .col-%d -->',[GetEditColWidth(aEntry)])+LF;
    end;
end;


function TBootstrapFormGenerator.GetInputGroup(aEntry : TDataFieldEntryItem): String;
begin
  Result:=Indent+'<div class="input-group">'+LF;
  IncIndent;
end;

function TBootstrapFormGenerator.GetInputGroupEnd(aEntry : TDataFieldEntryItem): String;
begin
  DecIndent;
  Result:=Indent+'</div> <!-- .input-group -->'+LF;
end;

function TBootstrapFormGenerator.GenerateFieldHTML(aEntry: TDataFieldEntryItem): String;

var
  be : TBootstrapDataFieldEntryItem;
  UseIG : Boolean;
begin
  be:=Nil;
  if aEntry is TBootstrapDataFieldEntryItem then
     be:=aEntry as TBootstrapDataFieldEntryItem;
  UseIG:=UseInputGroup or (Assigned(be) and Be.UseInputGroup);
  Result:='';
  if HorizontalForm then
    Result:=Result+GetRowCol(aEntry);
  if not (aEntry.InputType in [itRadio,itCheckBox]) then
    result:=Result+GetFormLabel(aEntry);
  if HorizontalForm then
    result:=Result+GetEntryStart(aEntry);
  if UseIG then
    Result:=Result+GetInputGroup(aEntry);
  Result:=Result+Inherited GenerateFieldHTML(aEntry);
  if UseIG then
    Result:=Result+GetInputGroupEnd(aEntry);
  if (aEntry.InputType in [itRadio,itCheckBox]) then
    result:=Result+GetFormLabel(aEntry);
  if HorizontalForm then
    begin
    result:=Result+GetEntryEnd(aEntry);
    Result:=Result+GetRowColEnd(aEntry);
    end;
end;

class function TBootstrapFormGenerator.EntryClass: TDataFieldEntryItemClass;
begin
  Result:=TBootstrapDataFieldEntryItem;
end;

initialization
  TBootstrapFormGenerator.Register;
end.

