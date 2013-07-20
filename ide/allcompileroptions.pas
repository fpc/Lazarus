unit AllCompilerOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  contnrs, Buttons, ButtonPanel, EditBtn,
  EnvironmentOpts, Compiler, LazarusIDEStrConsts;

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
    procedure ButtonPanel1Click(Sender: TObject);
    procedure edOptionsFilterChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fCustomOptions: TMemo;
    FIdleConnected: Boolean;
    FOptionsReader: TCompilerOptReader;
    FGeneratedControls: TComponentList;
    FEffectiveFilter: string;
    procedure SetIdleConnected(AValue: Boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure RenderAndFilterOptions;
  public
    constructor CreateWithMemo(aCustomOptions: TMemo);
    destructor Destroy; override;
  public
    property IdleConnected: Boolean read FIdleConnected write SetIdleConnected;
  end;

var
  frmAllCompilerOptions: TfrmAllCompilerOptions;

implementation

{$R *.lfm}

{ TfrmAllCompilerOptions }

constructor TfrmAllCompilerOptions.CreateWithMemo(aCustomOptions: TMemo);
begin
  inherited Create(Nil);
  fCustomOptions := aCustomOptions;
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

procedure TfrmAllCompilerOptions.FormShow(Sender: TObject);
begin
  Caption:=lisAllOptions;
  edOptionsFilter.Enabled := False;   // Until the options are read.
  edOptionsFilter.Hint := 'Filter the available options list';
  btnResetOptionsFilter.LoadGlyphFromLazarusResource(ResBtnListFilter);
  btnResetOptionsFilter.Enabled := False;
  btnResetOptionsFilter.Hint := 'Clear the filter for options';
  FEffectiveFilter:=#1; // Set an impossible value first, makes sure options are filtered.
  IdleConnected := True;
  //btnGetAllOptions.Caption := 'Get all options';
  //btnGetAllOptions.Hint := 'Read available options using "fpc -i" and "fpc -h"';
  //lblStatus.Caption := '';
end;

procedure TfrmAllCompilerOptions.ButtonPanel1Click(Sender: TObject);
begin
  // All Options
  //FOptionsReader.CopyNonDefaultOptions(CompOptions.AllOptions);

end;

procedure TfrmAllCompilerOptions.edOptionsFilterChange(Sender: TObject);
begin
  btnResetOptionsFilter.Enabled := edOptionsFilter.Text<>'';
  // ToDo : Filter the list of options
  IdleConnected := True;
end;

procedure TfrmAllCompilerOptions.btnResetOptionsFilterClick(Sender: TObject);
begin
  edOptionsFilter.Text := '';
  btnResetOptionsFilter.Enabled := False;
end;

procedure TfrmAllCompilerOptions.SetIdleConnected(AValue: Boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if FIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TfrmAllCompilerOptions.OnIdle(Sender: TObject; var Done: Boolean);
begin
  IdleConnected := False;
  Screen.Cursor:=crHourGlass;
  try
    edOptionsFilter.Enabled := False;
    FOptionsReader.CompilerExecutable := EnvironmentOptions.CompilerFilename;
    if FOptionsReader.ReadAndParseOptions <> mrOK then
      ShowMessage(FOptionsReader.ErrorMsg);
    RenderAndFilterOptions;
    edOptionsFilter.Enabled := True;
  finally
    Screen.Cursor:=crDefault;
  end;
  //sbAllOptions.Anchors := [];
  //sbAllOptions.Anchors := [akLeft,akTop, akRight, akBottom];
end;

procedure TfrmAllCompilerOptions.RenderAndFilterOptions;
const
  LeftEdit = 120;
  LeftDescrEdit = 230;
  LeftDescrBoolean = 150;
var
  Opt: TCompilerOpt;
  yLoc: Integer;
  Container: TCustomControl;

  function MakeHeaderLabel: TControl;
  begin
    Result := TLabel.Create(Nil); // Container
    Result.Parent := Container;
    Result.Top := yLoc;
    Result.Left := Opt.Indentation*4;
    Result.Caption := Opt.Option+#9#9+Opt.Description;
    FGeneratedControls.Add(Result);
  end;

  function MakeOptionCntrl(aCntrlClass: TControlClass; aTopOffs: integer=0): TControl;
  begin
    Result := aCntrlClass.Create(Nil);
    Result.Parent := Container;
    Result.Top := yLoc+aTopOffs;
    Result.Left := Opt.Indentation*4;
    Result.Caption := Opt.Option;
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
    Result.Anchors := [akLeft,akTop];
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
      if not Opt.Visible then Continue;         // Maybe filtered out
      case Opt.EditKind of
        oeNone: begin                           // Label
          Cntrl := MakeHeaderLabel;
        end;
        oeBoolean: begin                        // CheckBox
          Cntrl := MakeOptionCntrl(TCheckBox);
          if Length(Opt.Option) > 10 then
            NewLeft := LeftDescrBoolean + (Length(Opt.Option)-10)*8
          else
            NewLeft := LeftDescrBoolean;
          MakeDescrLabel(Cntrl, NewLeft);
        end;
        oeSetElem: begin                        // Sub-item for set, CheckBox
          Cntrl := MakeOptionCntrl(TCheckBox);
        end;
        oeNumber, oeText, oeSetNumber: begin    // Edit
          Lbl := MakeOptionCntrl(TLabel, 3);
          Cntrl := MakeEditCntrl(Lbl, TEdit);
          MakeDescrLabel(Cntrl, LeftDescrEdit);
        end;
        oeList: begin                           // ComboBox
          Lbl := MakeOptionCntrl(TLabel, 3);
          Cntrl := MakeEditCntrl(Lbl, TComboBox);
          cb := TComboBox(Cntrl);
          cb.Style := csDropDownList;
          case Opt.Option of
            '-Ca<x>':     AddChoices(cb, 'ABI targets:');
            '-Cf<x>':     AddChoices(cb, 'FPU instruction sets:');
            '-Cp<x>':     AddChoices(cb, 'CPU instruction sets:');
            '-Oo[NO]<x>': AddChoices(cb, 'Optimizations:');
            '-Op<x>':     AddChoices(cb, 'CPU instruction sets:');
            '-OW<x>':     AddChoices(cb, 'Whole Program Optimizations:');
            '-Ow<x>':     AddChoices(cb, 'Whole Program Optimizations:');
            else
              raise Exception.Create('AddChoices: Unknown option ' + Opt.Option);
          end;
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
  if FEffectiveFilter = edOptionsFilter.Text then Exit;
  Container := sbAllOptions;
  Container.DisableAutoSizing;
  try
    // First filter and set Visible flag.
    FOptionsReader.FilterOptions(edOptionsFilter.Text);
    // Then create and place new controls in GUI
    FGeneratedControls.Clear;
    yLoc := 0;
    RenderOneLevel(FOptionsReader.RootOptGroup);
    FEffectiveFilter:=edOptionsFilter.Text;
  finally
    Container.EnableAutoSizing;
    Container.Invalidate;
  end;
end;

end.

