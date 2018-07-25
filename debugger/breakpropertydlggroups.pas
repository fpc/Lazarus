unit BreakPropertyDlgGroups;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  // LCL
  Forms, Controls, ButtonPanel, StdCtrls, CheckLst,
  // IDE
  Debugger, LazarusIDEStrConsts;

type

  TBreakPointGroupAction = (bgaEnable, bgaDisable);

  { TBreakPointGroupDlg }

  TBreakPointGroupDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
  protected
    FBrkPointPoint: TIDEBreakPoint;
    FGroupList: TStringList;
    FAvailableGroups: TIDEBreakPointGroups;
  public
    { public declarations }
    constructor Create(ABrkPointPoint: TIDEBreakPoint;
                       AGroupList: String;
                       AAvailableGroups: TIDEBreakPointGroups;
                       AAction: TBreakPointGroupAction
                      ); reintroduce;
    destructor Destroy; override;
    function ShowModal: Integer; override;
  end; 


function ExecuteBreakPointGroupDlg(ABrkPointPoint: TIDEBreakPoint;
                               var AGroupList: String;
                               AAvailableGroups: TIDEBreakPointGroups;
                               AAction: TBreakPointGroupAction
                              ): TModalResult;


implementation

function ExecuteBreakPointGroupDlg(ABrkPointPoint: TIDEBreakPoint;
  var AGroupList: String; AAvailableGroups: TIDEBreakPointGroups;
  AAction: TBreakPointGroupAction): TModalResult;
var
  dlg: TBreakPointGroupDlg;
begin
  Result := mrAbort;
  dlg := TBreakPointGroupDlg.Create(ABrkPointPoint, AGroupList, AAvailableGroups, AAction);
  try
    Result := dlg.ShowModal;
    if Result = mrOK then
      AGroupList := dlg. FGroupList.DelimitedText;
  finally
    dlg.Free;
  end;
end;

{ TBreakPointGroupDlg }

constructor TBreakPointGroupDlg.Create(ABrkPointPoint: TIDEBreakPoint;
  AGroupList: String; AAvailableGroups: TIDEBreakPointGroups;
  AAction: TBreakPointGroupAction);
var
  g: TIDEBreakPointGroup;
  i, j: Integer;
begin
  inherited Create(nil);
  FBrkPointPoint := ABrkPointPoint;
  FAvailableGroups := AAvailableGroups;
  FGroupList := TStringList.Create;
  FGroupList.Delimiter := ';';
  FGroupList.DelimitedText := AGroupList;

  Caption := dbgBreakGroupDlgCaption;
  case AAction of
    bgaEnable:
      Label1.Caption := dbgBreakGroupDlgHeaderEnable;
    bgaDisable:
      Label1.Caption := dbgBreakGroupDlgHeaderDisable;
  end;

  for i := 0 to FAvailableGroups.Count - 1 do begin
    g := FAvailableGroups[i];
    j := CheckListBox1.Items.Add(g.Name);
    CheckListBox1.Checked[j] := FGroupList.IndexOf(g.Name) >= 0;
  end;
end;

destructor TBreakPointGroupDlg.Destroy;
begin
  inherited Destroy;
  FGroupList.Free;
end;

function TBreakPointGroupDlg.ShowModal: Integer;
var
  i, j: Integer;
  g: TIDEBreakPointGroup;
begin
  Result := inherited ShowModal;

  FGroupList.Clear;
  if Result = mrOK then begin
    for i := 0 to FAvailableGroups.Count - 1 do begin
      g := FAvailableGroups[i];
      j := CheckListBox1.Items.IndexOf(g.Name);
      if j < 0 then continue;

      if CheckListBox1.Checked[j] then begin
        FGroupList.Add(g.Name);
      end;
    end;
  end;
end;

{$R *.lfm}

end.

