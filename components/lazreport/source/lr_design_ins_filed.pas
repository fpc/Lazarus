unit lr_design_ins_filed;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, ExtCtrls, StdCtrls, Buttons, ComCtrls,
  IniFiles, LazFileUtils;

type

  { TlrFieldsList }

  TlrFieldsList = class(TFrame)
    cbDSList:TComboBox;
    lbFieldsList: TListBox;
    fPanelHeader: TPanel;
    PageControl1: TPageControl;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ValCombo: TComboBox;
    ValList: TListBox;
    procedure cbDSListChange(Sender: TObject);
    procedure fPanelHeaderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure fPanelHeaderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure fPanelHeaderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ValComboChange(Sender: TObject);
  private
    fDown         : Boolean;
    fPt           : TPoint;
    FLastHeight:integer;
    procedure RestorePos;
    procedure SavePos;
    function IniFileName:string;
    procedure FillValCombo;
    procedure GetVariables;
    procedure GetSpecValues;
    procedure GetFRVariables;
    function CurValSet: String;
    function CurVal: String;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure RefreshDSList;
    function SelectedField:string;
  end;

var
  lrFieldsList:TlrFieldsList = nil;

implementation
uses LR_Utils, LR_Class, LR_DBRel, LR_Desgn, LR_Const;

{$R *.lfm}

{ TlrFieldsList }

procedure TlrFieldsList.fPanelHeaderMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
  begin
    fDown:=True;
    if (x>4) and (x<fPanelHeader.Width-4) and (y<=16) then
    begin
      fPanelHeader.Cursor:=crSize;
      fPt:=Mouse.CursorPos;
    end;
  end;
end;

procedure TlrFieldsList.cbDSListChange(Sender: TObject);
var
  DataSet: TDataSet;
begin
  lbFieldsList.Items.Clear;
  if cbDSList.Items.Count>0 then
  begin
//    DataSet := nil;
//    DataSet := frGetDataSet(cbDSList.Items[cbDSList.ItemIndex]);
    DataSet := frGetDataSet(cbDSList.Text);
    if Assigned(DataSet) then
    begin
      try
        frGetFieldNames(TfrTDataSet(DataSet), lbFieldsList.Items);
      except
      end;
    end;
  end;
end;

procedure TlrFieldsList.fPanelHeaderMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  NewPt: TPoint;
begin
  if fDown then
  begin
    Case fPanelHeader.Cursor of
      crSize :
        begin
          NewPt:=Mouse.CursorPos;
          //DebugLn(['TfrObjectInspector.HeaderMDown ',dbgs(fPt),' New=',dbgs(NewPt)]);
          SetBounds(Left+NewPt.X-fPt.X,Top+NewPt.Y-fPt.Y,Width,Height);
          fPt:=NewPt;
        end;
    end;
  end
end;

procedure TlrFieldsList.fPanelHeaderMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fDown:=False;
  fPanelHeader.Cursor:=crDefault;
end;

procedure TlrFieldsList.SpeedButton1Click(Sender: TObject);
begin
  if SpeedButton1.Caption='-' then
  begin
    FLastHeight:=Height;
    Height:=fPanelHeader.Height + 2*BorderWidth + 3;
    SpeedButton1.Caption:='+';
  end
  else
  begin
    Height:=FLastHeight;
    SpeedButton1.Caption:='-';
  end;
end;

procedure TlrFieldsList.SpeedButton2Click(Sender: TObject);
begin
  TfrDesignerForm(frDesigner).tlsDBFields.Checked:=false;
  Application.ReleaseComponent(Self);
end;

procedure TlrFieldsList.ValComboChange(Sender: TObject);
begin
  if CurValSet = sFRVariables then
    GetFRVariables
  else
    if CurValSet = sSpecVal then
      GetSpecValues
    else
      GetVariables;
end;

procedure TlrFieldsList.RestorePos;
var
  Ini:TIniFile;
begin
  if FileExistsUTF8(IniFileName) then
  begin
    Ini:=TIniFile.Create(IniFileName);
    Left:=Ini.ReadInteger('Position', 'Left', Left);
    Top:=Ini.ReadInteger('Position', 'Top', Top);
    Height:=Ini.ReadInteger('Position', 'Height', Height);
    Width:=Ini.ReadInteger('Position', 'Width', Width);
    Ini.Free;
  end;
end;

procedure TlrFieldsList.SavePos;
var
  Ini:TIniFile;
begin
  Ini:=TIniFile.Create(IniFileName);
  Ini.WriteInteger('Position', 'Left', Left);
  Ini.WriteInteger('Position', 'Top', Top);
  if SpeedButton1.Caption = '+' then
    Ini.WriteInteger('Position', 'Height', FLastHeight)
  else
    Ini.WriteInteger('Position', 'Height', Height);
  Ini.WriteInteger('Position', 'Width', Width);
  Ini.Free;
end;

function TlrFieldsList.IniFileName: string;
begin
  Result:=AppendPathDelim(lrConfigFolderName(false))+'lrFieldsList.cfg';
end;

procedure TlrFieldsList.FillValCombo;
var
  s: TStringList;
begin
  s := TStringList.Create;
  CurReport.GetCategoryList(s);
  s.Add(sSpecVal);
  s.Add(sFRVariables);
  ValCombo.Items.Assign(s);
  s.Free;
end;

procedure TlrFieldsList.GetVariables;
begin
  CurReport.GetVarList(ValCombo.ItemIndex, ValList.Items);
end;

procedure TlrFieldsList.GetSpecValues;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    for i := 0 to frSpecCount-1 do
      if i <> 1 then
        Add(frSpecArr[i]);
  end;
end;

procedure TlrFieldsList.GetFRVariables;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    for i := 0 to frVariables.Count - 1 do
      Add(frVariables.Name[i]);
  end;
end;

function TlrFieldsList.CurValSet: String;
begin
  Result := '';
  if ValCombo.ItemIndex <> -1 then
    Result := ValCombo.Items[ValCombo.ItemIndex];
end;

function TlrFieldsList.CurVal: String;
begin
  Result := '';
  if CurValSet <> sSpecVal then
  begin
    if ValList.ItemIndex <> -1 then
      Result := ValList.Items[ValList.ItemIndex];
  end
  else
  if ValList.ItemIndex > 0 then
    Result := frSpecFuncs[ValList.ItemIndex + 1]
  else
    Result := frSpecFuncs[0];
end;

constructor TlrFieldsList.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  RestorePos;
  Parent :=TWinControl(aOwner);
  RefreshDSList;
  FillValCombo;
  fPanelHeader.Caption:=sFRDesignerDataInsp;
end;

destructor TlrFieldsList.Destroy;
begin
  SavePos;
  lrFieldsList:=nil;
  inherited Destroy;
end;

procedure TlrFieldsList.RefreshDSList;
var
  Lst : TStringList;
begin
  cbDSList.OnChange:=nil;
  Lst := TStringList.Create;
  try
    if CurReport.DataType = dtDataSet then
      frGetComponents(CurReport.Owner, TDataSet, Lst, nil)
    else
      frGetComponents(CurReport.Owner, TDataSource, Lst, nil);
    Lst.Sort;
    cbDSList.Items.Assign(Lst);
    cbDSList.Enabled:=(Lst.Count>0);
  finally
    Lst.Free;
  end;
  cbDSList.OnChange:=@cbDSListChange;
end;

function TlrFieldsList.SelectedField: string;
begin
  Result:='';
  if PageControl1.ActivePageIndex = 0 then
  begin;
    if (lbFieldsList.ItemIndex>-1) and (lbFieldsList.ItemIndex<lbFieldsList.Items.Count) then
      Result:=cbDSList.Text + '."' + lbFieldsList.Items[lbFieldsList.ItemIndex] + '"'
  end
  else
    Result:=CurVal;
end;

end.
