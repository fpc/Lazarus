unit sparta_MultiplyResizer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, LMessages,
{$IF FPC_FULLVERSION>=30200}
  Generics.Collections,
{$ELSE}
  sparta_Generics.Collections,
{$ENDIF}
  sparta_AbstractResizer, sparta_InterfacesMDI, sparta_BasicResizeFrame;

type

  { TMultiplyResizer }

  { TResizerRec }

  TResizerRec = class
  public
    Frame: TBasicResizeFrame;
    Idx: Integer;
    constructor Create(AFrame: TBasicResizeFrame);
    destructor Destroy; override;
  end;

  TMultiplyResizer = class(TAbstractResizer)
  private class var
    FAllForms: TDictionary<IDesignedForm, TMultiplyResizer>;

    class constructor Create;
    class destructor Destroy;

    class procedure OnUserInputHandler(Sender: TObject; Msg: Cardinal);
  private
    FFormsStack: TList<IDesignedForm>;
    FForms: TObjectDictionary<IDesignedForm, TResizerRec>;
  protected
    // only allow to set prevously added DesignedForms by AddDesignedForm
    //procedure SetDesignedForm(const AValue: IDesignedForm); override;

    procedure RemoveFormEvent(Sender: TObject; Form: TCustomForm);
  protected { IResizer }
    //procedure TryBoundSizerToDesignedForm(Sender: TObject); override;
    function GetActiveResizeFrame: IResizeFrame; override;
    function GetActiveDesignedForm: IDesignedForm; override;
  public
    constructor Create(AParent: TWinControl; AResizerFrameClass: TResizerFrameClass); override;
    destructor Destroy; override;

    procedure AddDesignedForm(const AForm: IDesignedForm);
  end;

implementation

{ TResizerRec }

constructor TResizerRec.Create(AFrame: TBasicResizeFrame);
begin
  Frame := AFrame;
end;

destructor TResizerRec.Destroy;
begin
  //Frame.Free; // free by owner
  inherited Destroy;
end;

{ TMultiplyResizer }

class constructor TMultiplyResizer.Create;
begin
  Application.AddOnUserInputHandler(OnUserInputHandler);
  FAllForms := TDictionary<IDesignedForm, TMultiplyResizer>.Create;
end;

class destructor TMultiplyResizer.Destroy;
begin
  Application.RemoveOnUserInputHandler(OnUserInputHandler);
  FAllForms.Free;
end;

class procedure TMultiplyResizer.OnUserInputHandler(Sender: TObject;
  Msg: Cardinal);
var
  LCtrl: TControl;
  LActiveFrame: TBasicResizeFrame = nil;
  LResizer: TMultiplyResizer = nil;
  LResizerRec, LLastResizerRec: TResizerRec;
  tmp: Integer;
begin
  if (Msg = LM_LBUTTONDOWN) or (Msg = LM_RBUTTONDOWN) or (Msg = LM_MBUTTONDOWN) then
  begin
    LCtrl := FindDragTarget(Mouse.CursorPos, True);

    // find dedicated TMultiplyResizer and Frame
    if LCtrl <> nil then
    repeat
      if LCtrl is TBasicResizeFrame then
        LActiveFrame := TBasicResizeFrame(LCtrl);

      LCtrl := LCtrl.Parent;
      if (LCtrl <> nil) and (LCtrl.Owner is TMultiplyResizer) then
      begin
        LResizer := TMultiplyResizer(LCtrl.Owner);
        Break;
      end;
    until (LCtrl = nil);

    // frame to activate
    if Assigned(LActiveFrame) and Assigned(LResizer) then
    begin
      LResizerRec := LResizer.FForms[LActiveFrame.DesignedForm];
      LLastResizerRec := LResizer.FForms[LResizer.FFormsStack.Last];
      // already on top
      if LResizerRec = LLastResizerRec then
        Exit;

      LResizer.FFormsStack.Exchange(LResizerRec.Idx, LLastResizerRec.Idx);
      tmp := LLastResizerRec.Idx;
      LLastResizerRec.Idx := LResizerRec.Idx;
      LResizerRec.Idx := tmp;
      // show!
      LActiveFrame.BringToFront;
    end;
  end;
end;

procedure TMultiplyResizer.RemoveFormEvent(Sender: TObject; Form: TCustomForm);
var
  LForm: IDesignedForm;
begin
  if Supports(Form, IDesignedForm, LForm) then
  begin
    FFormsStack.Remove(LForm);
    FForms.Remove(LForm);
  end;
end;

function TMultiplyResizer.GetActiveResizeFrame: IResizeFrame;
var
  LForm: IDesignedForm; 
begin
  LForm := GetActiveDesignedForm;
  if LForm = nil then
    Result := nil
  else
    Result := FForms[LForm].Frame;
end;

function TMultiplyResizer.GetActiveDesignedForm: IDesignedForm;
begin
  if FFormsStack.Count = 0 then
    Result := nil
  else
    Result := FFormsStack.Last;
end;

constructor TMultiplyResizer.Create(AParent: TWinControl;
  AResizerFrameClass: TResizerFrameClass);
begin
  inherited Create(AParent, AResizerFrameClass);
  FForms := TObjectDictionary<IDesignedForm, TResizerRec>.Create([doOwnsValues]);
  FFormsStack := TList<IDesignedForm>.Create;
end;

destructor TMultiplyResizer.Destroy;
begin
  FFormsStack.Free;
  FForms.Free;
  inherited Destroy;
end;

procedure TMultiplyResizer.AddDesignedForm(const AForm: IDesignedForm);
var
  LFrame: TBasicResizeFrame;
  LResizerRec: TResizerRec;
begin
  if AForm = nil then
    Exit;

  LFrame := CreateResizeFrame;

  AForm.BeginUpdate;

  AForm.Form.Parent := LFrame.pClient;
  {$IFNDEF WINDOWS}
  AForm.Form.BorderStyle := bsNone;
  {$ENDIF}
  // for big forms (bigger than screen resolution) we need to refresh Real* values
  AForm.RealWidth := AForm.Width;
  AForm.RealHeight := AForm.Height;

  AForm.EndUpdate;
  AForm.OnChangeHackedBounds := TryBoundSizerToDesignedForm;

  LFrame.DesignedForm := AForm;

  LResizerRec := TResizerRec.Create(LFrame);
  FForms.Add(AForm, LResizerRec);
  LResizerRec.Idx := FFormsStack.Add(AForm);

  // when form is removed we need to remove all handlers located in FFormsStack
  // and FForms
  Screen.AddHandlerRemoveForm(RemoveFormEvent);
end;

end.

