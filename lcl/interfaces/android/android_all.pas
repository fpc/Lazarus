unit android_all;

interface

uses javalang, androidpipescomm;

type

  { Forward declaration of classes }

  TDisplayMetrics = class;
  TDisplay = class;
  TWindowManager = class;
  TViewGroup_LayoutParams = class;
  TView = class;
  TViewGroup = class;
  TLinearLayout = class;
  TAbsoluteLayout = class;
  TAbsoluteLayout_LayoutParams = class;
  TTextView = class;
  TEditText = class;
  TButton = class;
  TFrameLayout = class;
  TTimePicker = class;
  TScrollView = class;
  TCompoundButton = class;
  TCheckBox = class;
  TAdapterView = class;
  TAbsSpinner = class;
  TSpinner = class;

  { Types }

  TOnClickListener = procedure (v: TView) of object;

  { Classes }

  TDisplayMetrics = class(TJavaObject)
  public
    constructor Create();
    function density(): Single;
    function densityDpi(): Integer;
    function heightPixels(): Integer;
    function scaledDensity(): Single;
    function widthPixels(): Integer;
    function xdpi(): Single;
    function ydpi(): Single;
  end;

  TDisplay = class(TJavaObject)
  public
    procedure getMetrics(var outMetrics: TDisplayMetrics);
  end;

  TWindowManager = class(TJavaObject)
  public
    function getDefaultDisplay(): TDisplay;
  end;

  TViewGroup_LayoutParams = class(TJavaObject)
  public
    constructor Create(width: Integer; height: Integer);
  end;

  TView = class(TJavaObject)
  public
    procedure setLayoutParams(var params: TViewGroup_LayoutParams);
    procedure setVisibility(visibility: Integer);
  end;

  TViewGroup = class(TView)
  public
    procedure addView(var child: TView; var params: TViewGroup_LayoutParams);
  end;

  TLinearLayout = class(TViewGroup)
  public
    constructor Create();
    procedure setOrientation(orientation: Integer);
  end;

  TAbsoluteLayout = class(TViewGroup)
  public
    constructor Create();
  end;

  TAbsoluteLayout_LayoutParams = class(TViewGroup_LayoutParams)
  public
    constructor Create(param_width: Integer; param_height: Integer; param_x: Integer; param_y: Integer);
  end;

  TTextView = class(TView)
  public
    constructor Create();
    procedure setText(var AText: string);
  public
    OnClickListener: TOnClickListener;
    procedure setOnClickListener(ACallback: TOnClickListener);
    procedure callOnClickListener();
  public
    procedure setTextSize(unit_: Integer; size: Single);
  end;

  TEditText = class(TTextView)
  public
    constructor Create();
    procedure setText(var AText: string);
  end;

  TButton = class(TTextView)
  public
    constructor Create();
    procedure setText(var AText: string);
  end;

  TFrameLayout = class(TViewGroup)
  public
  end;

  TTimePicker = class(TFrameLayout)
  public
    constructor Create();
    function getCurrentHour(): Integer;
    procedure setCurrentHour(currentHour: Integer);
    function getCurrentMinute(): Integer;
    procedure setCurrentMinute(currentMinute: Integer);
    function is24HourView(): Boolean;
    procedure setIs24HourView(AIs24HourView: Boolean);
  end;

  TScrollView = class(TFrameLayout)
  public
    constructor Create();
  end;

  TCompoundButton = class(TButton)
  public
    function isChecked(): Boolean;
    function performClick(): Boolean;
    procedure setChecked(checked: Boolean);
    procedure toggle();
  end;

  TCheckBox = class(TCompoundButton)
  public
    constructor Create();
  end;

  TAdapterView = class(TViewGroup)
  public
  end;

  TAbsSpinner = class(TAdapterView)
  public
  end;

  TSpinner = class(TAbsSpinner)
  public
    constructor Create();
  end;

const
  { Constants }
  { TDisplayMetrics }
  { TDisplay }
  { TWindowManager }
  { TViewGroup_LayoutParams }
  FILL_PARENT = $FFFFFFFF;
  MATCH_PARENT = $FFFFFFFF;
  WRAP_CONTENT = $FFFFFFFE;
  { TView }
  VISIBLE = 0;
  INVISIBLE = 4;
  GONE = 8;
  { TViewGroup }
  { TLinearLayout }
  HORIZONTAL = 0;
  VERTICAL = 1;
  { TAbsoluteLayout }
  { TAbsoluteLayout_LayoutParams }
  { TTextView }
  { TEditText }
  { TButton }
  { TFrameLayout }
  { TTimePicker }
  { TScrollView }
  { TCompoundButton }
  { TCheckBox }
  { TAdapterView }
  { TAbsSpinner }
  { TSpinner }

function HandleMessage(AFirstInt: Integer): Boolean;

implementation

const
  { IDs }

  // TDisplayMetrics
  amkUI_TDisplayMetrics_Create = $00101000;
  amkUI_TDisplayMetrics_density = $00101001;
  amkUI_TDisplayMetrics_densityDpi = $00101002;
  amkUI_TDisplayMetrics_heightPixels = $00101003;
  amkUI_TDisplayMetrics_scaledDensity = $00101004;
  amkUI_TDisplayMetrics_widthPixels = $00101005;
  amkUI_TDisplayMetrics_xdpi = $00101006;
  amkUI_TDisplayMetrics_ydpi = $00101007;
  // TDisplay
  amkUI_TDisplay_getMetrics = $00102000;
  // TWindowManager
  amkUI_TWindowManager_getDefaultDisplay = $00103000;
  // TViewGroup_LayoutParams
  amkUI_TViewGroup_LayoutParams_Create = $00104000;
  // TView
  amkUI_TView_setLayoutParams = $00105000;
  amkUI_TView_setVisibility = $00105001;
  // TViewGroup
  amkUI_TViewGroup_addView = $00106000;
  // TLinearLayout
  amkUI_TLinearLayout_Create = $00107000;
  amkUI_TLinearLayout_setOrientation = $00107001;
  // TAbsoluteLayout
  amkUI_TAbsoluteLayout_Create = $00108000;
  // TAbsoluteLayout_LayoutParams
  amkUI_TAbsoluteLayout_LayoutParams_Create = $00109000;
  // TTextView
  amkUI_TTextView_Create = $0010A000;
  amkUI_TTextView_setText = $0010A001;
  amkUI_TTextView_setOnClickListener = $0010A002;
  amkUI_TTextView_OnClickListener_Start = $0010A003;
  amkUI_TTextView_OnClickListener_Finished = $0010A004;
  amkUI_TTextView_setTextSize = $0010A005;
  // TEditText
  amkUI_TEditText_Create = $0010B000;
  amkUI_TEditText_setText = $0010B001;
  // TButton
  amkUI_TButton_Create = $0010C000;
  amkUI_TButton_setText = $0010C001;
  // TFrameLayout
  // TTimePicker
  amkUI_TTimePicker_Create = $0010E000;
  amkUI_TTimePicker_getCurrentHour = $0010E001;
  amkUI_TTimePicker_setCurrentHour = $0010E002;
  amkUI_TTimePicker_getCurrentMinute = $0010E003;
  amkUI_TTimePicker_setCurrentMinute = $0010E004;
  amkUI_TTimePicker_is24HourView = $0010E005;
  amkUI_TTimePicker_setIs24HourView = $0010E006;
  // TScrollView
  amkUI_TScrollView_Create = $0010F000;
  // TCompoundButton
  amkUI_TCompoundButton_isChecked = $00110000;
  amkUI_TCompoundButton_performClick = $00110001;
  amkUI_TCompoundButton_setChecked = $00110002;
  amkUI_TCompoundButton_toggle = $00110003;
  // TCheckBox
  amkUI_TCheckBox_Create = $00111000;
  // TAdapterView
  // TAbsSpinner
  // TSpinner
  amkUI_TSpinner_Create = $00114000;

{ Implementation of Classes }

constructor TDisplayMetrics.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TDisplayMetrics.density(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_density);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.densityDpi(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_densityDpi);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.heightPixels(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_heightPixels);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.scaledDensity(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_scaledDensity);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.widthPixels(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_widthPixels);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.xdpi(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_xdpi);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.ydpi(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_ydpi);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TDisplay.getMetrics(var outMetrics: TDisplayMetrics);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplay_getMetrics);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(outMetrics.Index));
  vAndroidPipesComm.WaitForReturn();
end;

function TWindowManager.getDefaultDisplay(): TDisplay;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TWindowManager_getDefaultDisplay);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := TDisplay(vAndroidPipesComm.WaitForIntReturn());
end;

constructor TViewGroup_LayoutParams.Create(width: Integer; height: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_LayoutParams_Create);
  vAndroidPipesComm.SendInt(Integer(width));
  vAndroidPipesComm.SendInt(Integer(height));
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TView.setLayoutParams(var params: TViewGroup_LayoutParams);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TView_setLayoutParams);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(params.Index));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TView.setVisibility(visibility: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TView_setVisibility);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(visibility));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TViewGroup.addView(var child: TView; var params: TViewGroup_LayoutParams);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_addView);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(child.Index));
  vAndroidPipesComm.SendInt(Integer(params.Index));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TLinearLayout.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TLinearLayout_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TLinearLayout.setOrientation(orientation: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TLinearLayout_setOrientation);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(orientation));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TAbsoluteLayout.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAbsoluteLayout_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TAbsoluteLayout_LayoutParams.Create(param_width: Integer; param_height: Integer; param_x: Integer; param_y: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAbsoluteLayout_LayoutParams_Create);
  vAndroidPipesComm.SendInt(Integer(param_width));
  vAndroidPipesComm.SendInt(Integer(param_height));
  vAndroidPipesComm.SendInt(Integer(param_x));
  vAndroidPipesComm.SendInt(Integer(param_y));
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TTextView.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TTextView.setText(var AText: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(AText);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_setText);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

procedure TTextView.setOnClickListener(ACallback: TOnClickListener);
begin
  OnClickListener := ACallback;
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_setOnClickListener);
  vAndroidPipesComm.SendInt(Index); // Self, Java Index
  vAndroidPipesComm.SendInt(PtrInt(Self)); // Self, Pascal pointer
  vAndroidPipesComm.WaitForReturn();
end;

procedure TTextView.callOnClickListener();
begin
  if Assigned(OnClickListener) then OnClickListener(Self);
end;
procedure TTextView.setTextSize(unit_: Integer; size: Single);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_setTextSize);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(unit_));
  vAndroidPipesComm.SendInt(Integer(size));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TEditText.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TEditText_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TEditText.setText(var AText: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(AText);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TEditText_setText);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

constructor TButton.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TButton_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TButton.setText(var AText: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(AText);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TButton_setText);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

constructor TTimePicker.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TTimePicker.getCurrentHour(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_getCurrentHour);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TTimePicker.setCurrentHour(currentHour: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_setCurrentHour);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(currentHour));
  vAndroidPipesComm.WaitForReturn();
end;

function TTimePicker.getCurrentMinute(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_getCurrentMinute);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TTimePicker.setCurrentMinute(currentMinute: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_setCurrentMinute);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(currentMinute));
  vAndroidPipesComm.WaitForReturn();
end;

function TTimePicker.is24HourView(): Boolean;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_is24HourView);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Boolean(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TTimePicker.setIs24HourView(AIs24HourView: Boolean);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_setIs24HourView);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(AIs24HourView));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TScrollView.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TScrollView_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TCompoundButton.isChecked(): Boolean;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_isChecked);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Boolean(vAndroidPipesComm.WaitForIntReturn());
end;

function TCompoundButton.performClick(): Boolean;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_performClick);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Boolean(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TCompoundButton.setChecked(checked: Boolean);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_setChecked);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(checked));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TCompoundButton.toggle();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_toggle);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.WaitForReturn();
end;

constructor TCheckBox.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCheckBox_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TSpinner.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TSpinner_Create);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;

{ Message Handling }

function HandleMessage(AFirstInt: Integer): Boolean;
var
  lInt: Integer;
  lPascalPointer: PtrInt = -1;
begin
  case AFirstInt of
  amkUI_TTextView_OnClickListener_Start:
  begin
    lPascalPointer := vAndroidPipesComm.ReadInt();
    TTextView(lPascalPointer).callOnClickListener();
    vAndroidPipesComm.SendMessage(amkUICommand, amkUI_TTextView_OnClickListener_Finished);
  end;
  end;
end;

end.
