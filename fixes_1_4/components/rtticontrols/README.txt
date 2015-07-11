RTTI Controls for LCL


What is RTTI?

RTTI stands for the FreePascal Run Time Type Information.
The RTTI controls can be connected to published properties of TPersistant
objects (e.g. any LCL or FCL component). For example you can connect a
TTICheckBox to the Visible property of a TButton.
The IDE uses the RTTI for the Object Inspector and defines a lot of property
editors. The RTTI controls use these property editors, so they can edit anything
the Object Inspector can edit. But contrary to the Inspector the RTTI controls
are not limited to TEdit and TComboBox.


Usage:

Install the package runtimetypeinfocontrols.lpk in the Lazarus IDE. The new
controls will be installed in the new group "RTTI".

Each RTTI control has a property "Link".
There you can set the TIObject and the TIPropertyName.
For example:
 TIEdit.Link.TIObject:=Button1;
 TIEdit.Link.TIPropertyName:="Visible";
Or shorter:
 TIEdit.Link.SetObjectAndProperty(Button1,"Visible");
 
Make sure to unlink before you destroy the connected TIObject:
 TIEdit.Link.TObject:=nil;
Otherwise you can get access violations, when the RTTI control tries to access
it. For TComponent descendents like TButton the unlinking is done automatically.
 

Updating:

The RTTI has no callbacks, so if a property changes, the RTTI control does not
recognize this. You can either call
  TIEdit.Link.LoadFromProperty;
or you can set ploReadOnIdle in TIEdit.Link.Options:
  TIEdit.Link.Options:=TIEdit.Link.Options+[ploReadOnIdle];
Then the RTTI control will call LoadFromProperty everytime the Application gets
idle.

BEWARE:
The on idle update will not load the property, if the control is focused.
You can use this:
    // update focused control
    GetPropertyLinkOfComponent(ActiveControl).LoadFromProperty;

