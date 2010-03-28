unit ReplaceNamesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, ExtCtrls, CodeToolsStructs, LazarusIDEStrConsts;

type

  { TGridUpdater }

  TGridUpdater = class
  private
    fGrid: TStringGrid;
    fReplaceMap: TStringToStringTree;
    fSeenName: TStringList;
    i: Integer;
  public
    constructor Create(AGrid: TStringGrid; AReplaceMap: TStringToStringTree);
    destructor Destroy; override;
    procedure AddUnique(AOldIdent: string);
  end;

  { TReplaceNamesForm }

  TReplaceNamesForm = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    BtnPanel: TPanel;
    HelpButton: TBitBtn;
    NamePairGrid: TStringGrid;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end; 

var
  ReplaceNamesForm: TReplaceNamesForm;

procedure CopyFromMapToGrid(AGrid: TStringGrid; AMap: TStringToStringTree);
procedure CopyFromGridToMap(AGrid: TStringGrid; AMap: TStringToStringTree);


implementation

{$R *.lfm}

procedure CopyFromMapToGrid(AGrid: TStringGrid; AMap: TStringToStringTree);
var
  OldIdent, NewIdent: string;
  List: TStringList;
  i: Integer;
begin
  // Collect (maybe edited) properties from StringGrid to NameReplacements.
  List:=TStringList.Create;
  try
    AGrid.BeginUpdate;
    AMap.GetNames(List);
    for i:=0 to List.Count-1 do begin
      OldIdent:=List[i];
      NewIdent:=AMap[OldIdent];
      if AGrid.RowCount<i+1 then
        AGrid.RowCount:=i+1;
      AGrid.Cells[0,i]:=OldIdent;
      AGrid.Cells[1,i]:=NewIdent;
    end;
    AGrid.EndUpdate;
  finally
    List.Free;
  end;
end;

procedure CopyFromGridToMap(AGrid: TStringGrid; AMap: TStringToStringTree);
var
  OldIdent, NewIdent: string;
  i: Integer;
begin
  // Collect (maybe edited) properties from StringGrid to NameReplacements.
  for i:=1 to AGrid.RowCount-1 do begin // Skip the fixed row.
    OldIdent:=AGrid.Cells[0,i];
    NewIdent:=AGrid.Cells[1,i];
    if NewIdent<>'' then
      AMap[OldIdent]:=NewIdent;
  end;
end;

{ TGridUpdater }

constructor TGridUpdater.Create(AGrid: TStringGrid; AReplaceMap: TStringToStringTree);
begin
  fGrid:=AGrid;
  fReplaceMap:=AReplaceMap;
  i:=1;
  fSeenName:=TStringList.Create;
  fGrid.BeginUpdate;
end;

destructor TGridUpdater.Destroy;
begin
  fGrid.EndUpdate;
  fSeenName.Free;
  inherited Destroy;
end;

procedure TGridUpdater.AddUnique(AOldIdent: string);
var
  NewIdent: string;
begin
  // Add only one instance of each property name.
  if fSeenName.IndexOf(AOldIdent)<0 then begin
    fSeenName.Append(AOldIdent);
    NewIdent:=fReplaceMap[AOldIdent];
    if fGrid.RowCount<i+1 then
      fGrid.RowCount:=i+1;
    fGrid.Cells[0,i]:=AOldIdent;
    fGrid.Cells[1,i]:=NewIdent;
    Inc(i);
  end;
end;

{ TReplaceNamesForm }

procedure TReplaceNamesForm.FormCreate(Sender: TObject);
begin
  Caption:=lisReplacementPropTypes;
end;

procedure TReplaceNamesForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

end.

