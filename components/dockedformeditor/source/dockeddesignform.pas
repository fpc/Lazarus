{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Maciej Izak
          Michael W. Vogel

}

unit DockedDesignForm;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  // RTL
  Classes, SysUtils, fgl,
  // LCL
  LMessages, Forms, LCLType, LCLIntf, Controls,
  // LazUtils
  LazLoggerBase,
  // IDEIntf
  SrcEditorIntf,
  // DockedFormEditor
  DockedFormAccesses, DockedBasicAnchorDesigner;

const
  WM_SETNOFRAME            = WM_USER;
  WM_BOUNDTODESIGNTABSHEET = WM_USER + 1;

type
  TDesignForms = class;

  { TDesignForm }

  TDesignForm = class(TDesignFormIDE)
  private
    FContainer: TDesignForms;
    FHiding: Boolean;
    FOnAdjustPageNeeded: TNotifyEvent;
    FWndMethod: TWndMethod;
    class var FNewForm: TCustomForm;
    procedure FixF12_ActiveEditor;
    procedure FormChangeBounds(Sender: TObject);
    procedure WndMethod(var Msg: TLMessage);
  public
    constructor Create(AForm: TCustomForm); override;
    destructor Destroy; override;
    class procedure Screen_NewFormCreated(Sender: TObject; AForm: TCustomForm);
  public
    property Hiding: Boolean read FHiding write FHiding;
    property OnAdjustPageNeeded: TNotifyEvent read FOnAdjustPageNeeded write FOnAdjustPageNeeded;
  end;

  { TDesignForms }

  TDesignForms = class(specialize TFPGList<TDesignForm>)
  public
    destructor Destroy; override;
    function Add(const Item: TDesignForm): Integer;
    procedure DeleteDesignForm(AIndex: Integer);
    function Find(AForm: TCustomForm): TDesignForm; overload;
    function Find(ADesigner: TIDesigner): TDesignForm; overload;
    function IndexOf(AForm: TCustomForm): Integer; overload;
    procedure Remove(AForm: TCustomForm); overload;
    procedure RemoveAllAnchorDesigner;
  end;

var
  DesignForms: TDesignForms;

implementation

{ TDesignForm }

class procedure TDesignForm.Screen_NewFormCreated(Sender: TObject; AForm: TCustomForm);
begin
  FNewForm := AForm;
  //DebugLn(['TDesignForm.Screen_NewFormCreated! Self=', Self,
  //  ', FNewForm=', FNewForm.Name,':',FNewForm.ClassName]);
end;

procedure TDesignForm.FixF12_ActiveEditor;
var
  i: Integer;
begin
  //DebugLn(['TDesignForm.FixF12_ActiveEditor: Self=', Self, ', FNewForm=', FNewForm, ', Form=', Form]);
  if FNewForm = Nil then exit;
  FNewForm := Nil;
  //DebugLn([' TDesignForm.FixF12_ActiveEditor: Passed']);

  // Without this, button F12 don't work after creating a new form.
  if Designer <> SourceEditorManagerIntf.ActiveEditor.GetDesigner(True) then Exit;
  SourceEditorManagerIntf.ActiveEditor := nil;
  for i := 0 to SourceEditorManagerIntf.UniqueSourceEditorCount - 1 do
    if Designer = SourceEditorManagerIntf.UniqueSourceEditors[i].GetDesigner(True) then
    begin
      SourceEditorManagerIntf.ActiveEditor := SourceEditorManagerIntf.UniqueSourceEditors[i];
      Break;
    end;
end;

procedure TDesignForm.FormChangeBounds(Sender: TObject);
begin
  if not Update then
    SetTimer(Form.Handle, WM_BOUNDTODESIGNTABSHEET, 10, nil);
end;

procedure TDesignForm.WndMethod(var Msg: TLMessage);
var
  Timer: TLMTimer;
begin
  case Msg.msg of
    LM_TIMER:
      begin
        Timer := TLMTimer(Msg);
        case Timer.TimerID of
          WM_SETNOFRAME:
            begin
              KillTimer(Form.Handle, WM_SETNOFRAME);
              LCLIntf.ShowWindow(Form.Handle, SW_HIDE);
              FHiding := False;
              FixF12_ActiveEditor;
              Exit;
            end;
          WM_BOUNDTODESIGNTABSHEET:
            begin
              KillTimer(Form.Handle, WM_BOUNDTODESIGNTABSHEET);
              if Assigned(FOnAdjustPageNeeded) then
                FOnAdjustPageNeeded(Self);
              Exit;
            end;
        end;
      end;
    {$IFDEF LCLWin32}
    // we need to correct ActiveEditor to right form
    // this code works correctly on Windows platform
    // (is necessery for selecting controls after form resizing).
    // in Linux platforms below code brings problems with QT (inactive form)
    LM_LBUTTONDOWN, LM_RBUTTONDOWN, LM_MBUTTONDOWN, LM_XBUTTONDOWN:
      if LastActiveSourceWindow <> nil then
      begin
        SourceEditorManagerIntf.ActiveSourceWindow := LastActiveSourceWindow;
        SourceEditorManagerIntf.ActiveEditor := LastActiveSourceWindow.ActiveEditor;
      end;
    {$ENDIF}
    // Prevent usage of parent hint (SourceEditorWindow), see issue #39217
    CM_PARENTSHOWHINTCHANGED:
      begin
        Msg.Result := 0;
        Exit;
      end;
  end;

  FWndMethod(Msg);
end;

constructor TDesignForm.Create(AForm: TCustomForm);
begin
  inherited Create(AForm);
  AForm.AddHandlerOnChangeBounds(@FormChangeBounds);
  FWndMethod := Form.WindowProc;
  Form.WindowProc := @WndMethod;
end;

destructor TDesignForm.Destroy;
begin
  Form.WindowProc := FWndMethod; // ! important risky point :P
  Form.RemoveHandlerOnChangeBounds(@FormChangeBounds);
  inherited Destroy;
end;

{ TDesignForms }

destructor TDesignForms.Destroy;
begin
  while Count > 0 do
    DeleteDesignForm(0);
  inherited Destroy;
end;

function TDesignForms.Add(const Item: TDesignForm): Integer;
begin
  Item.FContainer := Self;
  Result := inherited Add(Item);
end;

procedure TDesignForms.DeleteDesignForm(AIndex: Integer);
var
  LDesignForm: TDesignForm;
begin
  LDesignForm := Self[AIndex];
  LDesignForm.Free;
  Delete(AIndex);
end;

function TDesignForms.Find(AForm: TCustomForm): TDesignForm;
var
  LDesignForm: TDesignForm;
begin
  // sometimes at some level of initialization form can not contain TIDesigner
  // (during ide run and when is oppened default project with some TForm1)
  Result := nil;
  if AForm = nil then Exit;
  for LDesignForm in Self do
    if LDesignForm.Form = AForm then
      Exit(LDesignForm);
end;

function TDesignForms.Find(ADesigner: TIDesigner): TDesignForm;
var
  LDesignForm: TDesignForm;
begin
  Result := nil;
  if ADesigner = nil then Exit;
  for LDesignForm in Self do
    if LDesignForm.Designer = ADesigner then
      Exit(LDesignForm);
end;

function TDesignForms.IndexOf(AForm: TCustomForm): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].Form = AForm then
      Exit(i);
end;

procedure TDesignForms.Remove(AForm: TCustomForm);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AForm);
  if LIndex < 0 then Exit;
  Items[LIndex].Free;
  Delete(LIndex);
end;

procedure TDesignForms.RemoveAllAnchorDesigner;
var
  LDesignForm: TDesignForm;
  LAnchorDesigner: TBasicAnchorDesigner;
begin
  for LDesignForm in Self do
  begin
    LAnchorDesigner := LDesignForm.AnchorDesigner;
    if not Assigned(LAnchorDesigner) then Continue;
    LAnchorDesigner.Free;
    LDesignForm.AnchorDesigner := nil;
  end;
end;

end.

