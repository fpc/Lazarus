unit ;

interface

type
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

implementation

const
  // TCompoundButton
  amkUI_TCompoundButton_isChecked = $00101000;
  amkUI_TCompoundButton_performClick = $00101001;
  amkUI_TCompoundButton_setChecked = $00101002;
  amkUI_TCompoundButton_toggle = $00101003;
  // TCheckBox
  amkUI_TCheckBox_Create = $00102000;

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

end.
