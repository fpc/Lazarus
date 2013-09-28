unit AllCompilerOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ButtonPanel, EditBtn,
  Dialogs, contnrs, LCLProc, ComCtrls, ExtCtrls, Compiler, LazarusIDEStrConsts;

type

  { TfrmAllCompilerOptions }

  TfrmAllCompilerOptions = class(TForm)
    btnResetOptionsFilter: TSpeedButton;
    ButtonPanel1: TButtonPanel;
    cbShowModified: TCheckBox;
    cbUseComments: TCheckBox;
    edOptionsFilter: TEdit;
    pnlFilter: TPanel;
    sbAllOptions: TScrollBox;
    procedure btnResetOptionsFilterClick(Sender: TObject);
    procedure cbShowModifiedClick(Sender: TObject);
    procedure edOptionsFilterChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIdleConnected: Boolean;
    FOptionsReader: TCompilerOptReader;
    FOptionsThread: TCompilerOptThread;
    FGeneratedControls: TComponentList;
    FEffectiveFilter: string;
    FEffectiveShowModified: Boolean;
    FRenderedOnce: Boolean;
    procedure SetIdleConnected(AValue: Boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure CheckBoxClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure ComboChange(Sender: TObject);
    procedure RenderAndFilterOptions;
  private
    property IdleConnected: Boolean read FIdleConnected write SetIdleConnected;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ToCustomOptions(aStrings: TStrings): TModalResult;
  public
    property OptionsReader: TCompilerOptReader read FOptionsReader write FOptionsReader;
    property OptionsThread: TCompilerOptThread read FOptionsThread write FOptionsThread;
  end;

var
  frmAllCompilerOptions: TfrmAllCompilerOptions;

implementation

{$R *.lfm}

{ TfrmAllCompilerOptions }

constructor TfrmAllCompilerOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FGeneratedControls := TComponentList.Create;
end;

destructor TfrmAllCompilerOptions.Destroy;
begin
  FGeneratedControls.Clear;
  FreeAndNil(FGeneratedControls);
  inherited Destroy;
end;

function TfrmAllCompilerOptions.ToCustomOptions(aStrings: TStrings): TModalResult;
begin
  Result := FOptionsReader.ToCustomOptions(aStrings, cbUseComments.Checked);
end;

procedure TfrmAllCompilerOptions.FormShow(Sender: TObject);
begin
  Caption:=lisAllOptions;
  edOptionsFilter.Hint := lisFilterTheAvailableOptionsList;
  btnResetOptionsFilter.LoadGlyphFromLazarusResource(ResBtnListFilter);
  btnResetOptionsFilter.Enabled := False;
  btnResetOptionsFilter.Hint := lisClearTheFilterForOptions;
  cbShowModified.Caption:=lisShowOnlyModified;
  cbUseComments.Caption:=lisUseCommentsInCustomOptions;
  FEffectiveFilter:=#1; // Set an impossible value first, makes sure options are filtered.
  FRenderedOnce := False;
  IdleConnected := True;
end;

procedure TfrmAllCompilerOptions.edOptionsFilterChange(Sender: TObject);
begin
  btnResetOptionsFilter.Enabled := edOptionsFilter.Text<>'';
  // Filter the list of options in OnIdle handler
  IdleConnected := True;
end;

procedure TfrmAllCompilerOptions.btnResetOptionsFilterClick(Sender: TObject);
begin
  edOptionsFilter.Text := '';
  btnResetOptionsFilter.Enabled := False;
end;

procedure TfrmAllCompilerOptions.cbShowModifiedClick(Sender: TObject);
begin
  IdleConnected := True;
end;

procedure TfrmAllCompilerOptions.SetIdleConnected(AValue: Boolean);
begin
  if FIdleConnected = AValue then exit;
  FIdleConnected := AValue;
  if FIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TfrmAllCompilerOptions.OnIdle(Sender: TObject; var Done: Boolean);

  function FormatTimeWithMs(aTime: TDateTime): string;
  var
    fs: TFormatSettings;
  begin
    fs.TimeSeparator := ':';
    Result := FormatDateTime('nn:ss', aTime, fs)+'.'+FormatDateTime('zzz', aTime);
  end;

var
  StartTime: TDateTime;
begin
  IdleConnected := False;
  Screen.Cursor := crHourGlass;
  try
    FOptionsThread.WaitFor;            // Make sure the options are read.
    if FOptionsReader.ErrorMsg <> '' then
      DebugLn(FOptionsReader.ErrorMsg)
    else begin
      StartTime := Now;
      RenderAndFilterOptions;
      DebugLn(Format('AllCompilerOptions: Time for reading options: %s, rendering GUI: %s',
                     [FormatTimeWithMs(FOptionsThread.ReadTime),
                      FormatTimeWithMs(Now-StartTime)]));
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  FRenderedOnce := True;
end;

procedure TfrmAllCompilerOptions.CheckBoxClick(Sender: TObject);
var
  cb: TCheckBox;
  Opt: TCompilerOpt;
begin
  cb := Sender as TCheckBox;
  Opt := FOptionsReader.FindOptionById(cb.Tag);
  if Assigned(Opt) then
  begin
    if cb.Checked then
      Opt.Value := 'True'
    else
      Opt.Value := '';
  end;
end;

procedure TfrmAllCompilerOptions.EditChange(Sender: TObject);
var
  ed: TCustomEdit;
  Opt: TCompilerOpt;
begin
  ed := Sender as TCustomEdit;
  Opt := FOptionsReader.FindOptionById(ed.Tag);
  if Assigned(Opt) then
    Opt.Value := ed.Text;
end;

procedure TfrmAllCompilerOptions.ComboChange(Sender: TObject);
var
  cb: TComboBox;
  Opt: TCompilerOpt;
begin
  cb := Sender as TComboBox;
  Opt := FOptionsReader.FindOptionById(cb.Tag);
  if Assigned(Opt) then
    Opt.Value := cb.Text;
end;

procedure TfrmAllCompilerOptions.RenderAndFilterOptions;
const
  LeftEdit = 120;
  LeftDescrEdit = 250;
  LeftDescrBoolean = 140;
  LeftDescrGroup = 110;
var
  Opt: TCompilerOpt;
  yLoc: Integer;
  Container: TCustomControl;

  function MakeOptionCntrl(aCntrlClass: TControlClass; aCaption: string;
    aTopOffs: integer=0): TControl;
  begin
    Result := aCntrlClass.Create(Nil);
    Result.Parent := Container;
    Result.Top := yLoc+aTopOffs;
    Result.Left := Opt.Indentation*4;
    Result.Caption := aCaption;
    Result.Tag := Opt.Id;
    FGeneratedControls.Add(Result);
  end;

  function MakeEditCntrl(aLbl: TControl; aCntrlClass: TControlClass): TControl;
  // TEdit or TComboBox
  begin
    Result := aCntrlClass.Create(Nil);
    Result.Parent := Container;
    Result.AnchorSide[akTop].Control := aLbl;
    Result.AnchorSide[akTop].Side := asrCenter;
    Result.Left := LeftEdit;        // Now use Left instead of anchors
    Result.Width := 125;
    Result.Anchors := [akLeft,akTop];
    Result.Tag := Opt.Id;
    FGeneratedControls.Add(Result);
  end;

  procedure MakeDescrLabel(aCntrl: TControl; aLeft: integer);
  // Description label after CheckBox / Edit control
  var
    Lbl: TControl;
  begin
    Lbl := TLabel.Create(Nil);
    Lbl.Parent := Container;
    Lbl.Caption := Opt.Description;
    Lbl.AnchorSide[akTop].Control := aCntrl;
    Lbl.AnchorSide[akTop].Side := asrCenter;
    Lbl.Left := aLeft;              // Now use Left instead of anchors
    Lbl.Anchors := [akLeft,akTop];
    FGeneratedControls.Add(Lbl);
  end;

  procedure RenderOneLevel(aParentGroup: TCompilerOptGroup);
  var
    Cntrl, Lbl: TControl;
    cb: TComboBox;
    i: Integer;
  begin
    for i := 0 to aParentGroup.CompilerOpts.Count-1 do begin
      Opt := TCompilerOpt(aParentGroup.CompilerOpts[i]);
      if Opt.Ignored or not Opt.Visible then Continue;  // Maybe filtered out
      case Opt.EditKind of
        oeGroup, oeSet: begin                   // Label for group or set
          Cntrl := MakeOptionCntrl(TLabel, Opt.Option+Opt.Suffix);
          MakeDescrLabel(Cntrl, Opt.CalcLeft(LeftDescrGroup, 7));
        end;
        oeBoolean: begin                        // CheckBox
          Cntrl := MakeOptionCntrl(TCheckBox, Opt.Option);
          Assert((Opt.Value='') or (Opt.Value='True'), 'Wrong value in Boolean option '+Opt.Option);
          TCheckBox(Cntrl).Checked := Opt.Value<>'';
          Cntrl.OnClick := @CheckBoxClick;
          MakeDescrLabel(Cntrl, Opt.CalcLeft(LeftDescrBoolean, 11));
        end;
        oeSetElem: begin                        // Sub-item for set, CheckBox
          Cntrl := MakeOptionCntrl(TCheckBox, Opt.Option+Opt.Description);
          Assert((Opt.Value='') or (Opt.Value='True'), 'Wrong value in Boolean option '+Opt.Option);
          TCheckBox(Cntrl).Checked := Opt.Value<>'';
          Cntrl.OnClick := @CheckBoxClick;
        end;
        oeNumber, oeText, oeSetNumber: begin    // Edit
          Lbl := MakeOptionCntrl(TLabel, Opt.Option+Opt.Suffix, 3);
          Cntrl := MakeEditCntrl(Lbl, TEdit);
          if Opt.EditKind <> oeText then
            TCustomEdit(Cntrl).Width := 80;
          TCustomEdit(Cntrl).Text := Opt.Value;
          TCustomEdit(Cntrl).OnChange := @EditChange;
          MakeDescrLabel(Cntrl, LeftDescrEdit);
        end;
        oeList: begin                           // ComboBox
          Lbl := MakeOptionCntrl(TLabel, Opt.Option+Opt.Suffix, 3);
          Cntrl := MakeEditCntrl(Lbl, TComboBox);
          cb := TComboBox(Cntrl);
          cb.Style := csDropDownList;
          if Assigned(Opt.Choices) then
            cb.Items.Assign(Opt.Choices);
          cb.Text := Opt.Value;
          cb.OnChange := @ComboChange;
          MakeDescrLabel(Cntrl, LeftDescrEdit);
        end
        else
          raise Exception.Create('TCompilerOptsRenderer.Render: Unknown EditKind.');
      end;
      Inc(yLoc, Cntrl.Height+2);
      if Opt is TCompilerOptGroup then
        RenderOneLevel(TCompilerOptGroup(Opt));  // Show other levels recursively
    end;
  end;

begin
  if (FEffectiveFilter = edOptionsFilter.Text)
  and (FEffectiveShowModified = cbShowModified.Checked) then Exit;
  Container := sbAllOptions;
  Container.DisableAutoSizing;
  try
    // First filter and set Visible flag.
    FOptionsReader.FilterOptions(UTF8LowerCase(edOptionsFilter.Text),
                                 cbShowModified.Checked);
    // Then create and place new controls in GUI
    FGeneratedControls.Clear;
    yLoc := 0;
    RenderOneLevel(FOptionsReader.RootOptGroup);
    FEffectiveFilter := edOptionsFilter.Text;
    FEffectiveShowModified := cbShowModified.Checked;
    FocusControl(edOptionsFilter);
  finally
    Container.EnableAutoSizing;
    Container.Invalidate;
  end;
end;

end.

