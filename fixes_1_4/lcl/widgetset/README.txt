This directory contains all skeleton widget set component 
classes. These classes will never get instantiated and may 
only contain class functions.
  
=========  
IMPORTANT
=========  

Derivation and inheritance of classes is different then one
might be used to. It will be explained by the following 
examples.

Suppose the following LCL class hierarchy:

 TLCLComponent
     |
  TControl
     |
 TWinControl
 
the corresponding WS skeleton would be

 TWSLCLComponent
      |
  TWSControl
      |
 TWSWinControl


When method X of TWSControl gets implemented by 
widgetset Q the hierarchy looks like

 TWSLCLComponent
      |
  TWSControl.X --> TQWSControl.X
      |
 TWSWinControl


Calling TWSWinControl.X doesn't call TQWSControl.X since
it's parent is TWSControl. This problem is solved by 
modifying the class hierarchy at runtime.
When a component class is registered by RegisterWSComponent,
the class is copied and the vmt entries are adjusted so 
that the hierarchy looks like:


 TWSLCLComponent
      |
  TWSControl.X --> TQWSControl.X
                        |
                   TWSWinControl

In this case, calling TWSWinControl.X will call the overridden 
TQWSControl.X. The only thing which doesn't get handled is the 
inherited statement. Suppose there is also a TQWSWinControl.X 
which implements a few extra steps. In a normal situation one 
would have called "inherited". The call to inherited is
resolved at compile time and would in this example to a call to
TWSControl.X. That is not what we want.
To get around this, call the parent yourself:
  TWSWinControlClass(ClassParent).X
