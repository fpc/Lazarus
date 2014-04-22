unit lr_design_ins_filed;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

  { TlrFieldsList }

  TlrFieldsList = class(TFrame)
    cbDSList:TComboBox;
    lbFieldsList: TListBox;
    fPanelHeader: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure cbDSListChange(Sender: TObject);
    procedure fPanelHeaderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure fPanelHeaderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure fPanelHeaderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    fDown         : Boolean;
    fPt           : TPoint;
    FLastHeight:integer;
    procedure RestorePos;
    procedure SavePos;
    function IniFileName:string;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure RefreshDSList;
    function SelectedField:string;
  end;

var
  lrFieldsList:TlrFieldsList = nil;

implementation
uses LR_Utils, LR_Class, LR_DBRel, IniFiles, FileUtil, LR_Desgn, LR_Const;

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

constructor TlrFieldsList.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  RestorePos;
  Parent :=TWinControl(aOwner);
  RefreshDSList;
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
  if (lbFieldsList.ItemIndex>-1) and (lbFieldsList.ItemIndex<lbFieldsList.Items.Count) then
    Result:=cbDSList.Text + '."' + lbFieldsList.Items[lbFieldsList.ItemIndex] + '"'
  else
    Result:='';
end;

end.
