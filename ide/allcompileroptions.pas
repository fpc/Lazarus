unit AllCompilerOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, contnrs, Buttons,
  ButtonPanel, EditBtn, LCLProc, EnvironmentOpts, Compiler, LazarusIDEStrConsts;

type

  { TfrmAllCompilerOptions }

  TfrmAllCompilerOptions = class(TForm)
    btnResetOptionsFilter: TSpeedButton;
    ButtonPanel1: TButtonPanel;
    cbShowModified: TCheckBox;
    cbUseComments: TCheckBox;
    edOptionsFilter: TEdit;
    sbAllOptions: TScrollBox;
    procedure btnResetOptionsFilterClick(Sender: TObject);
    procedure cbShowModifiedClick(Sender: TObject);
    procedure edOptionsFilterChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCustomOptions: TStrings;
    FIdleConnected: Boolean;
    FOptionsReader: TCompilerOptReader;
    FGeneratedControls: TComponentList;
    FEffectiveFilter: string;
    FEffectiveShowModified: Boolean;
    FInitialRender: Boolean;
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
    property CustomOptions: TStrings read FCustomOptions write FCustomOptions;
    property OptionsReader: TCompilerOptReader read FOptionsReader;
  end;

var
  frmAllCompilerOptions: TfrmAllCompilerOptions;

implementation

{$R *.lfm}

{ TfrmAllCompilerOptions }

constructor TfrmAllCompilerOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOptionsReader := TCompilerOptReader.Create;
  FGeneratedControls := TComponentList.Create;
end;

destructor TfrmAllCompilerOptions.Destroy;
begin
  FGeneratedControls.Clear;
  FreeAndNil(FGeneratedControls);
  FreeAndNil(FOptionsReader);
  inherited Destroy;
end;

function TfrmAllCompilerOptions.ToCustomOptions(aStrings: TStrings): TModalResult;
begin
  Result := OptionsReader.ToCustomOptions(aStrings, cbUseComments.Checked);
end;

procedure TfrmAllCompilerOptions.FormShow(Sender: TObject);
begin
  Caption:=lisAllOptions;
  edOptionsFilter.Enabled := False;   // Until the options are read.
  edOptionsFilter.Hint := 'Filter the available options list';
  btnResetOptionsFilter.LoadGlyphFromLazarusResource(ResBtnListFilter);
  btnResetOptionsFilter.Enabled := False;
  btnResetOptionsFilter.Hint := 'Clear the filter for options';
  FEffectiveFilter:=#1; // Set an impossible value first, makes sure options are filtered.
  FInitialRender := True;
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
begin
  IdleConnected := False;
  Screen.Cursor := crHourGlass;
  try
    edOptionsFilter.Enabled := False;
    with FOptionsReader do
      if RootOptGroup.CompilerOpts.Count = 0 then
      begin
        CompilerExecutable := EnvironmentOptions.GetParsedCompilerFilename;
        if ReadAndParseOptions <> mrOK then
          ShowMessage(ErrorMsg);
        FromCustomOptions(FCustomOptions);
      end;
    RenderAndFilterOptions;
    edOptionsFilter.Enabled := True;
  finally
    Screen.Cursor := crDefault;
  end;
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
  LeftDescrBoolean = 120;
  LeftDescrGroup = 100;
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

  procedure AddChoices(aComboBox: TComboBox; aCategory: string);
  // Add selection choices to ComboBox from data originating from "fpc -i".
  var
    i: Integer;
  begin
    with FOptionsReader.SupportedCategories do
      if Find(aCategory, i) then
        aComboBox.Items.Assign(Objects[i] as TStrings)
      else
        raise Exception.CreateFmt('AddChoices: Selection list for "%s" is not found.',
                                  [aCategory]);
  end;

  procedure RenderOneLevel(aParentGroup: TCompilerOptGroup);
  var
    Cntrl, Lbl: TControl;
    cb: TComboBox;
    i, NewLeft: Integer;
  begin
    for i := 0 to aParentGroup.CompilerOpts.Count-1 do begin
      Opt := TCompilerOpt(aParentGroup.CompilerOpts[i]);
      if Opt.Ignored or not Opt.Visible then Continue;  // Maybe filtered out
      case Opt.EditKind of
        oeGroup, oeSet: begin                   // Label for group or set
          Cntrl := MakeOptionCntrl(TLabel, Opt.Option+Opt.Suffix{+#9#9+Opt.Description});
          MakeDescrLabel(Cntrl, LeftDescrGroup);
        end;
        oeBoolean: begin                        // CheckBox
          Cntrl := MakeOptionCntrl(TCheckBox, Opt.Option);
          Assert((Opt.Value='') or (Opt.Value='True'), 'Wrong value in Boolean option '+Opt.Option);
          TCheckBox(Cntrl).Checked := Opt.Value<>'';
          if Length(Opt.Option) > 9 then
            NewLeft := LeftDescrBoolean + (Length(Opt.Option)-9)*8
          else
            NewLeft := LeftDescrBoolean;
          Cntrl.OnClick := @CheckBoxClick;
          MakeDescrLabel(Cntrl, NewLeft);
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
          // ToDo: Move this logic to parser data so values can be validated better.
          case Opt.Option of
            '-Ca':     AddChoices(cb, 'ABI targets:');
            '-Cf':     AddChoices(cb, 'FPU instruction sets:');
            '-Cp':     AddChoices(cb, 'CPU instruction sets:');
            '-Oo[NO]': AddChoices(cb, 'Optimizations:');
            '-Op':     AddChoices(cb, 'CPU instruction sets:');
            '-OW':     AddChoices(cb, 'Whole Program Optimizations:');
            '-Ow':     AddChoices(cb, 'Whole Program Optimizations:');
            else
              raise Exception.Create('AddChoices: Unknown option ' + Opt.Option);
          end;
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
    {$IFDEF AllOptsFocusFilter}
    if not FInitialRender then
      FocusControl(edOptionsFilter);
    {$ENDIF}
    FInitialRender := False;
  finally
    Container.EnableAutoSizing;
    Container.Invalidate;
  end;
end;

end.

