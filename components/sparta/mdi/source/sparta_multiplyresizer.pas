unit sparta_MultiplyResizer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Generics.Collections, sparta_AbstractResizer, sparta_InterfacesMDI,
  sparta_BasicResizeFrame;

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
  private
    FFormsStack: TList<IDesignedForm>;
    FForms: TObjectDictionary<IDesignedForm, TResizerRec>;
  protected
    // only allow to set prevously added DesignedForms by AddDesignedForm
    //procedure SetDesignedForm(const AValue: IDesignedForm); override;
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
  FAllForms := TDictionary<IDesignedForm, TMultiplyResizer>.Create;
end;

class destructor TMultiplyResizer.Destroy;
begin
  FAllForms.Free;
end;

function TMultiplyResizer.GetActiveResizeFrame: IResizeFrame;
begin
  Result := FForms[GetActiveDesignedForm].Frame;
end;

function TMultiplyResizer.GetActiveDesignedForm: IDesignedForm;
begin
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

  FForms.Add(AForm, TResizerRec.Create(LFrame));
  FFormsStack.Add(AForm);
end;

end.

