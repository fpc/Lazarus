This package is an example how to to use the lazarus IDE form designer to
design non LCL widgets like web pages, UML diagrams or other widgetsets like
fpgui.

Installation:
Install the package examples/designnonlcl/notlcldesigner.lpk and restart the IDE.
Then open the example project examples/designnonlcl/project/NonLCL1.lpi.
Open the form of unit1.pas and you will see some non LCL controls in the
designer as red rectangles.

Overview:
The unit mywidgetset.pas contains the class TMyWidget which is a TComponent
and has Left, Top, Width, Height and Parent to define a simple widgetset.
This widgetset is only for demonstration and does not contain any paint
routines, so you can not do anything useful with this widgetset.

The unit mywidgetdesigner.pas contains the designer mediator TMyWidgetMediator
which is registered in the Register procedure:
  FormEditingHook.RegisterDesignerMediator(TMyWidgetMediator);
When the package is installed in the IDE, the mediator allows to design
TMyWidget components visually. Without the mediator the IDE would show them as
icons, like TOpenDialog or TDataSource.

