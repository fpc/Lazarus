unit SourceSynWrap;

{$mode objfpc}{$H+}

interface

uses SynEditWrappedView, SynEditViewedLineMap, SynEdit, LazSynEditText, SynEditFoldedView, Classes;

type

  { TLazSynSourceEditLineWrapPlugin }

  TLazSynSourceEditLineWrapPlugin = class(TLazSynEditLineWrapPlugin)
  public
    constructor Create(AnOwner: TComponent); override;
  end;

implementation

{ TLazSynSourceEditLineWrapPlugin }

constructor TLazSynSourceEditLineWrapPlugin.Create(AnOwner: TComponent);
var
  Syn: TSynEdit absolute AnOwner;
  Fld: TSynEditStringsLinked;
  ALineMapView: TSynEditLineMappingView;
  FldIdx: Integer;
begin
  Fld := Syn.TextViewsManager.SynTextViewByClass[TSynEditFoldedView];

  ALineMapView := TSynEditLineMappingView(Syn.TextViewsManager.SynTextViewByClass[TSynEditLineMappingView]);
  if ALineMapView = nil then
    ALineMapView := TSynEditLineMappingView.Create
  else
    Syn.TextViewsManager.RemoveSynTextView(ALineMapView);

  if Fld <> nil then
    FldIdx := Syn.TextViewsManager.IndexOf(Fld)
  else
    FldIdx := Syn.TextViewsManager.Count;

  Syn.TextViewsManager.AddTextView(ALineMapView, FldIdx);

  inherited Create(AnOwner);
end;

end.

