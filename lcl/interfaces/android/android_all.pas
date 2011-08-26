unit android_all;

interface

uses SysUtils, javalang, androidpipescomm;

{$INTERFACES CORBA}

type

  { Forward declaration of classes }

  TDisplayMetrics = class;
  TDialogInterface = interface;
  TActivity = class;
  TDisplay = class;
  TWindowManager = class;
  TDialog = class;
  TAlertDialog = class;
  TAlertDialog_Builder = class;
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
  TFilterable = interface;
  TAdapter = interface;
  TListAdapter = interface;
  TSpinnerAdapter = interface;
  TBaseAdapter = class;
  TArrayAdapter_String_ = class;
  Tlayout = class;

  { Types }

  TDialogInterface_OnClickListener = procedure () of object;
  TView_OnClickListener = procedure () of object;

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

  TDialogInterface = interface(IJavaInterface)
  end;

  TActivity = class(TJavaObject)
  public
    procedure setTitle(title: string);
    function getTitle(): string;
  end;

  TDisplay = class(TJavaObject)
  public
    procedure getMetrics(outMetrics: TDisplayMetrics);
  end;

  TWindowManager = class(TJavaObject)
  public
    function getDefaultDisplay(): TDisplay;
  end;

  TDialog = class(TJavaObject)
  public
    procedure show();
  end;

  TAlertDialog = class(TDialog)
  public
  public
    Button: TDialogInterface_OnClickListener;
    procedure setButton(whichButton: Integer; text: string; ACallback: TDialogInterface_OnClickListener);
    procedure callButton();
  public
    procedure setMessage(message: string);
    procedure setTitle(title: string);
    procedure setView(view: TView);
  end;

  TAlertDialog_Builder = class(TJavaObject)
  public
    constructor Create();
    function create(): TAlertDialog;
    function setMessage(message: string): TAlertDialog_Builder;
    function setTitle(title: string): TAlertDialog_Builder;
    function setView(view: TView): TAlertDialog_Builder;
    function show(): TAlertDialog;
  end;

  TViewGroup_LayoutParams = class(TJavaObject)
  public
    constructor Create(width: Integer; height: Integer);
  end;

  TView = class(TJavaObject)
  public
    procedure setLayoutParams(params: TViewGroup_LayoutParams);
    procedure setVisibility(visibility: Integer);
  end;

  TViewGroup = class(TView)
  public
    procedure addView(child: TView; aindex: Integer; params: TViewGroup_LayoutParams); overload;
    procedure addView(child: TView; params: TViewGroup_LayoutParams); overload;
    procedure addView(child: TView; aindex: Integer); overload;
    procedure addView(child: TView); overload;
    procedure addView(child: TView; width: Integer; height: Integer); overload;
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
    procedure setText(AText: string);
  public
    OnClickListener: TView_OnClickListener;
    procedure setOnClickListener(ACallback: TView_OnClickListener);
    procedure callOnClickListener();
  public
    procedure setTextSize(unit_: Integer; size: Single);
    function getText(): string;
  end;

  TEditText = class(TTextView)
  public
    constructor Create();
  end;

  TButton = class(TTextView)
  public
    constructor Create();
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
    function getSelectedItemPosition(): Integer;
  end;

  TAbsSpinner = class(TAdapterView)
  public
    function getCount(): Integer;
    procedure setAdapter(adapter: TSpinnerAdapter);
    procedure setSelection(position: Integer); overload;
  end;

  TSpinner = class(TAbsSpinner)
  public
    constructor Create();
  end;

  TFilterable = interface(IJavaInterface)
  end;

  TAdapter = interface(IJavaInterface)
  end;

  TListAdapter = interface(TAdapter)
  end;

  TSpinnerAdapter = interface(TAdapter)
  end;

  TBaseAdapter = class(TJavaObject, TListAdapter, TSpinnerAdapter)
  public
  end;

  TArrayAdapter_String_ = class(TBaseAdapter, TFilterable)
  public
    constructor Create(textViewResourceId: Integer);
    procedure add(aobject: string);
    procedure clear();
    procedure insert(aobject: string; aindex: Integer);
    procedure remove(aobject: string);
  end;

  Tlayout = class(TJavaObject)
  public
  end;

const
  { Constants }
  { TDisplayMetrics }
  { TDialogInterface }
  { TActivity }
  { TDisplay }
  { TWindowManager }
  { TDialog }
  { TAlertDialog }
  THEME_HOLO_DARK = $00000002;
  THEME_HOLO_LIGHT = $00000003;
  THEME_TRADITIONAL = $00000001;
  { TAlertDialog_Builder }
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
  { TFilterable }
  { TAdapter }
  { TListAdapter }
  { TSpinnerAdapter }
  { TBaseAdapter }
  { TArrayAdapter_String_ }
  { Tlayout }
  activity_list_item = $01090000;
  browser_link_context_header = $0109000e;
  expandable_list_content = $01090001;
  list_content = $01090014;
  preference_category = $01090002;
  select_dialog_item = $01090011;
  select_dialog_multichoice = $01090013;
  select_dialog_singlechoice = $01090012;
  simple_dropdown_item_1line = $0109000a;
  simple_expandable_list_item_1 = $01090006;
  simple_expandable_list_item_2 = $01090007;
  simple_gallery_item = $0109000b;
  simple_list_item_1 = $01090003;
  simple_list_item_2 = $01090004;
  simple_list_item_activated_1 = $01090016;
  simple_list_item_activated_2 = $01090017;
  simple_list_item_checked = $01090005;
  simple_list_item_multiple_choice = $01090010;
  simple_list_item_single_choice = $0109000f;
  simple_selectable_list_item = $01090015;
  simple_spinner_dropdown_item = $01090009;
  simple_spinner_item = $01090008;
  test_list_item = $0109000c;
  two_line_list_item = $0109000d;

function HandleMessage(AFirstInt: Integer): Boolean;

var
  activity: TActivity;

implementation

const
  { IDs }

  // TDisplayMetrics
  amkUI_TDisplayMetrics_Create_0 = $00101000;
  amkUI_TDisplayMetrics_density_1 = $00101001;
  amkUI_TDisplayMetrics_densityDpi_2 = $00101002;
  amkUI_TDisplayMetrics_heightPixels_3 = $00101003;
  amkUI_TDisplayMetrics_scaledDensity_4 = $00101004;
  amkUI_TDisplayMetrics_widthPixels_5 = $00101005;
  amkUI_TDisplayMetrics_xdpi_6 = $00101006;
  amkUI_TDisplayMetrics_ydpi_7 = $00101007;
  // TDialogInterface
  // TActivity
  amkUI_TActivity_setTitle_0 = $00103000;
  amkUI_TActivity_getTitle_1 = $00103001;
  // TDisplay
  amkUI_TDisplay_getMetrics_0 = $00104000;
  // TWindowManager
  amkUI_TWindowManager_getDefaultDisplay_0 = $00105000;
  // TDialog
  amkUI_TDialog_show_0 = $00106000;
  // TAlertDialog
  amkUI_TAlertDialog_setButton_0 = $00107000;
  amkUI_TAlertDialog_Button_Start_1 = $00107001;
  amkUI_TAlertDialog_Button_Finished_2 = $00107002;
  amkUI_TAlertDialog_setMessage_3 = $00107003;
  amkUI_TAlertDialog_setTitle_4 = $00107004;
  amkUI_TAlertDialog_setView_5 = $00107005;
  // TAlertDialog_Builder
  amkUI_TAlertDialog_Builder_Create_0 = $00108000;
  amkUI_TAlertDialog_Builder_create_1 = $00108001;
  amkUI_TAlertDialog_Builder_setMessage_2 = $00108002;
  amkUI_TAlertDialog_Builder_setTitle_3 = $00108003;
  amkUI_TAlertDialog_Builder_setView_4 = $00108004;
  amkUI_TAlertDialog_Builder_show_5 = $00108005;
  // TViewGroup_LayoutParams
  amkUI_TViewGroup_LayoutParams_Create_0 = $00109000;
  // TView
  amkUI_TView_setLayoutParams_0 = $0010A000;
  amkUI_TView_setVisibility_1 = $0010A001;
  // TViewGroup
  amkUI_TViewGroup_addView_0 = $0010B000;
  amkUI_TViewGroup_addView_1 = $0010B001;
  amkUI_TViewGroup_addView_2 = $0010B002;
  amkUI_TViewGroup_addView_3 = $0010B003;
  amkUI_TViewGroup_addView_4 = $0010B004;
  // TLinearLayout
  amkUI_TLinearLayout_Create_0 = $0010C000;
  amkUI_TLinearLayout_setOrientation_1 = $0010C001;
  // TAbsoluteLayout
  amkUI_TAbsoluteLayout_Create_0 = $0010D000;
  // TAbsoluteLayout_LayoutParams
  amkUI_TAbsoluteLayout_LayoutParams_Create_0 = $0010E000;
  // TTextView
  amkUI_TTextView_Create_0 = $0010F000;
  amkUI_TTextView_setText_1 = $0010F001;
  amkUI_TTextView_setOnClickListener_2 = $0010F002;
  amkUI_TTextView_OnClickListener_Start_3 = $0010F003;
  amkUI_TTextView_OnClickListener_Finished_4 = $0010F004;
  amkUI_TTextView_setTextSize_5 = $0010F005;
  amkUI_TTextView_getText_6 = $0010F006;
  // TEditText
  amkUI_TEditText_Create_0 = $00110000;
  // TButton
  amkUI_TButton_Create_0 = $00111000;
  // TFrameLayout
  // TTimePicker
  amkUI_TTimePicker_Create_0 = $00113000;
  amkUI_TTimePicker_getCurrentHour_1 = $00113001;
  amkUI_TTimePicker_setCurrentHour_2 = $00113002;
  amkUI_TTimePicker_getCurrentMinute_3 = $00113003;
  amkUI_TTimePicker_setCurrentMinute_4 = $00113004;
  amkUI_TTimePicker_is24HourView_5 = $00113005;
  amkUI_TTimePicker_setIs24HourView_6 = $00113006;
  // TScrollView
  amkUI_TScrollView_Create_0 = $00114000;
  // TCompoundButton
  amkUI_TCompoundButton_isChecked_0 = $00115000;
  amkUI_TCompoundButton_performClick_1 = $00115001;
  amkUI_TCompoundButton_setChecked_2 = $00115002;
  amkUI_TCompoundButton_toggle_3 = $00115003;
  // TCheckBox
  amkUI_TCheckBox_Create_0 = $00116000;
  // TAdapterView
  amkUI_TAdapterView_getSelectedItemPosition_0 = $00117000;
  // TAbsSpinner
  amkUI_TAbsSpinner_getCount_0 = $00118000;
  amkUI_TAbsSpinner_setAdapter_1 = $00118001;
  amkUI_TAbsSpinner_setSelection_2 = $00118002;
  // TSpinner
  amkUI_TSpinner_Create_0 = $00119000;
  // TFilterable
  // TAdapter
  // TListAdapter
  // TSpinnerAdapter
  // TBaseAdapter
  // TArrayAdapter_String_
  amkUI_TArrayAdapter_String__Create_0 = $0011F000;
  amkUI_TArrayAdapter_String__add_1 = $0011F001;
  amkUI_TArrayAdapter_String__clear_2 = $0011F002;
  amkUI_TArrayAdapter_String__insert_3 = $0011F003;
  amkUI_TArrayAdapter_String__remove_4 = $0011F004;
  // Tlayout

{ Implementation of Classes }

constructor TDisplayMetrics.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TDisplayMetrics.density(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_density_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.densityDpi(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_densityDpi_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.heightPixels(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_heightPixels_3);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.scaledDensity(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_scaledDensity_4);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.widthPixels(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_widthPixels_5);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.xdpi(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_xdpi_6);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

function TDisplayMetrics.ydpi(): Single;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplayMetrics_ydpi_7);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Single(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TActivity.setTitle(title: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(title);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TActivity_setTitle_0);
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

function TActivity.getTitle(): string;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TActivity_getTitle_1);
  Result := vAndroidPipesComm.WaitForStringReturn();
end;

procedure TDisplay.getMetrics(outMetrics: TDisplayMetrics);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDisplay_getMetrics_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(outMetrics.Index));
  vAndroidPipesComm.WaitForReturn();
end;

function TWindowManager.getDefaultDisplay(): TDisplay;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TWindowManager_getDefaultDisplay_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := TDisplay(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TDialog.show();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TDialog_show_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.WaitForReturn();
end;

procedure TAlertDialog.setButton(whichButton: Integer; text: string; ACallback: TDialogInterface_OnClickListener);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(text);
  Button := ACallback;
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_setButton_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Index
  vAndroidPipesComm.SendInt(PtrInt(Self)); // Self, Pascal pointer
  vAndroidPipesComm.SendInt(Integer(whichButton));
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  lString_1.Free;
  vAndroidPipesComm.WaitForReturn();
end;

procedure TAlertDialog.callButton();
begin
  if Assigned(Button) then Button();
end;
procedure TAlertDialog.setMessage(message: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(message);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_setMessage_3);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

procedure TAlertDialog.setTitle(title: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(title);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_setTitle_4);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

procedure TAlertDialog.setView(view: TView);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_setView_5);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(view.Index));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TAlertDialog_Builder.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_Builder_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TAlertDialog_Builder.create(): TAlertDialog;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_Builder_create_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := TAlertDialog(vAndroidPipesComm.WaitForIntReturn());
end;

function TAlertDialog_Builder.setMessage(message: string): TAlertDialog_Builder;
var
  lString_1: TString;
begin
  lString_1 := TString.Create(message);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_Builder_setMessage_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  Result := TAlertDialog_Builder(vAndroidPipesComm.WaitForIntReturn());
  lString_1.Free;
end;

function TAlertDialog_Builder.setTitle(title: string): TAlertDialog_Builder;
var
  lString_1: TString;
begin
  lString_1 := TString.Create(title);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_Builder_setTitle_3);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  Result := TAlertDialog_Builder(vAndroidPipesComm.WaitForIntReturn());
  lString_1.Free;
end;

function TAlertDialog_Builder.setView(view: TView): TAlertDialog_Builder;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_Builder_setView_4);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(view.Index));
  Result := TAlertDialog_Builder(vAndroidPipesComm.WaitForIntReturn());
end;

function TAlertDialog_Builder.show(): TAlertDialog;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAlertDialog_Builder_show_5);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := TAlertDialog(vAndroidPipesComm.WaitForIntReturn());
end;

constructor TViewGroup_LayoutParams.Create(width: Integer; height: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_LayoutParams_Create_0);
  vAndroidPipesComm.SendInt(Integer(width));
  vAndroidPipesComm.SendInt(Integer(height));
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TView.setLayoutParams(params: TViewGroup_LayoutParams);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TView_setLayoutParams_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(params.Index));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TView.setVisibility(visibility: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TView_setVisibility_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(visibility));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TViewGroup.addView(child: TView; aindex: Integer; params: TViewGroup_LayoutParams);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_addView_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(child.Index));
  vAndroidPipesComm.SendInt(Integer(aindex));
  vAndroidPipesComm.SendInt(Integer(params.Index));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TViewGroup.addView(child: TView; params: TViewGroup_LayoutParams);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_addView_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(child.Index));
  vAndroidPipesComm.SendInt(Integer(params.Index));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TViewGroup.addView(child: TView; aindex: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_addView_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(child.Index));
  vAndroidPipesComm.SendInt(Integer(aindex));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TViewGroup.addView(child: TView);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_addView_3);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(child.Index));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TViewGroup.addView(child: TView; width: Integer; height: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TViewGroup_addView_4);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(child.Index));
  vAndroidPipesComm.SendInt(Integer(width));
  vAndroidPipesComm.SendInt(Integer(height));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TLinearLayout.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TLinearLayout_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TLinearLayout.setOrientation(orientation: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TLinearLayout_setOrientation_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(orientation));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TAbsoluteLayout.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAbsoluteLayout_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TAbsoluteLayout_LayoutParams.Create(param_width: Integer; param_height: Integer; param_x: Integer; param_y: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAbsoluteLayout_LayoutParams_Create_0);
  vAndroidPipesComm.SendInt(Integer(param_width));
  vAndroidPipesComm.SendInt(Integer(param_height));
  vAndroidPipesComm.SendInt(Integer(param_x));
  vAndroidPipesComm.SendInt(Integer(param_y));
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TTextView.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TTextView.setText(AText: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(AText);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_setText_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

procedure TTextView.setOnClickListener(ACallback: TView_OnClickListener);
begin
  OnClickListener := ACallback;
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_setOnClickListener_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Index
  vAndroidPipesComm.SendInt(PtrInt(Self)); // Self, Pascal pointer
  vAndroidPipesComm.WaitForReturn();
end;

procedure TTextView.callOnClickListener();
begin
  if Assigned(OnClickListener) then OnClickListener();
end;
procedure TTextView.setTextSize(unit_: Integer; size: Single);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_setTextSize_5);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(unit_));
  vAndroidPipesComm.SendInt(Integer(size));
  vAndroidPipesComm.WaitForReturn();
end;

function TTextView.getText(): string;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTextView_getText_6);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := vAndroidPipesComm.WaitForStringReturn();
end;

constructor TEditText.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TEditText_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TButton.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TButton_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TTimePicker.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TTimePicker.getCurrentHour(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_getCurrentHour_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TTimePicker.setCurrentHour(currentHour: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_setCurrentHour_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(currentHour));
  vAndroidPipesComm.WaitForReturn();
end;

function TTimePicker.getCurrentMinute(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_getCurrentMinute_3);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TTimePicker.setCurrentMinute(currentMinute: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_setCurrentMinute_4);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(currentMinute));
  vAndroidPipesComm.WaitForReturn();
end;

function TTimePicker.is24HourView(): Boolean;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_is24HourView_5);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Boolean(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TTimePicker.setIs24HourView(AIs24HourView: Boolean);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TTimePicker_setIs24HourView_6);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(AIs24HourView));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TScrollView.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TScrollView_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TCompoundButton.isChecked(): Boolean;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_isChecked_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Boolean(vAndroidPipesComm.WaitForIntReturn());
end;

function TCompoundButton.performClick(): Boolean;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_performClick_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Boolean(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TCompoundButton.setChecked(checked: Boolean);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_setChecked_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(checked));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TCompoundButton.toggle();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCompoundButton_toggle_3);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.WaitForReturn();
end;

constructor TCheckBox.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TCheckBox_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
function TAdapterView.getSelectedItemPosition(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAdapterView_getSelectedItemPosition_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

function TAbsSpinner.getCount(): Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAbsSpinner_getCount_0);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  Result := Integer(vAndroidPipesComm.WaitForIntReturn());
end;

procedure TAbsSpinner.setAdapter(adapter: TSpinnerAdapter);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAbsSpinner_setAdapter_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(adapter.Index));
  vAndroidPipesComm.WaitForReturn();
end;

procedure TAbsSpinner.setSelection(position: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TAbsSpinner_setSelection_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(Integer(position));
  vAndroidPipesComm.WaitForReturn();
end;

constructor TSpinner.Create();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TSpinner_Create_0);
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
constructor TArrayAdapter_String_.Create(textViewResourceId: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TArrayAdapter_String__Create_0);
  vAndroidPipesComm.SendInt(Integer(textViewResourceId));
  Index := vAndroidPipesComm.WaitForIntReturn();
end;
procedure TArrayAdapter_String_.add(aobject: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(aobject);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TArrayAdapter_String__add_1);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

procedure TArrayAdapter_String_.clear();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TArrayAdapter_String__clear_2);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.WaitForReturn();
end;

procedure TArrayAdapter_String_.insert(aobject: string; aindex: Integer);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(aobject);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TArrayAdapter_String__insert_3);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.SendInt(Integer(aindex));
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;

procedure TArrayAdapter_String_.remove(aobject: string);
var
  lString_1: TString;
begin
  lString_1 := TString.Create(aobject);
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_TArrayAdapter_String__remove_4);
  vAndroidPipesComm.SendInt(Index); // Self, Java Pointer
  vAndroidPipesComm.SendInt(lString_1.Index); // text
  vAndroidPipesComm.WaitForReturn();
  lString_1.Free;
end;


{ Message Handling }

function HandleMessage(AFirstInt: Integer): Boolean;
var
  lInt: Integer;
  lPascalPointer: PtrInt = -1;
begin
  case AFirstInt of
  amkUI_TAlertDialog_Button_Start_1:
  begin
    lPascalPointer := vAndroidPipesComm.ReadInt();
    TAlertDialog(lPascalPointer).callButton();
    vAndroidPipesComm.SendMessage(amkUICommand, amkUI_TAlertDialog_Button_Finished_2);
  end;
  amkUI_TTextView_OnClickListener_Start_3:
  begin
    lPascalPointer := vAndroidPipesComm.ReadInt();
    TTextView(lPascalPointer).callOnClickListener();
    vAndroidPipesComm.SendMessage(amkUICommand, amkUI_TTextView_OnClickListener_Finished_4);
  end;
  end;
end;

initialization

  activity := TActivity.Create;

end.
