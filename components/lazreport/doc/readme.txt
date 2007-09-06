+-----------------------------+
!       LINUX/WIN32           !
+-----------------------------+

Required : 
  - FPC 2.0.0 Last CVS version
  - LAZARUS Last CVS version
  - Pakage Printer4Lazarus installed

Known Bugs
  - On change zoom, that kill X server. I noticed that that occurs when resize one Font.            [Fixed]
  - since the end of October, there is an error "Division by zero" if ShowProgress:=True            [Fixed]
  12-abr-2005:
  - Dragging a band containing objects, hides the first object of the list                          [Fixed]	
  - Windows: report designer doesn't start with a new page, laz win32 problem, patch already sent.  [Fixed 21/04/05 add OnShow(self) in onResize() of designer]
  - Windows: preview or print doesn't work correctly if ShowProgress:=true
  - Linux: report designer scrollbars doesn't work
  - Linux & Windows: The font list is not filled with list of fonts (use OI to change fonts)
  20-apr-2005
  - Cross tab report not work                                                                        [Fixed]

