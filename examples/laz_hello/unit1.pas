unit Unit1;          // Declares the unit name. Each Pascal unit must have a unique name.

{$mode objfpc}{$H+}  // Use the Free Pascal Object Pascal syntax and enable long strings (AnsiString).

interface            // Start of the interface section — defines what’s visible to other units.

uses
            // These are standard units that provide core functionality:
  Classes,  // - Classes & SysUtils: core data types and utilities.
  SysUtils,
  Forms,    // - Forms: GUI form definitions.
  Controls, // - Controls: inner functionality of controls
  Graphics, // - Graphics: drawing and colors.
  Dialogs,  // - Dialogs: message dialogs.
  StdCtrls; // - StdCtrls: standard controls like TButton, TMemo, TLabel.

type

    { TForm1 }

    TForm1 = class(TForm)      // This declares a form class named TForm1, inheriting from TForm (a window).
        ButtonHello: TButton;  // A button named ButtonHello — clicking this will show “Hello World” in the memo.
        ButtonClose: TButton;  // A button named ButtonClose — clicking this will close the form.
        Memo1: TMemo;          // A TMemo component named Memo1 — used to display and edit multiple lines of text.
        procedure ButtonCloseClick(Sender: TObject);  // This procedure handles clicks on the Close button.
        procedure ButtonHelloClick(Sender: TObject);  // This procedure handles clicks on the Hello button.

    private
        // Private section for internal fields or methods (not used here).

    public
        // Public section for fields/methods accessible by other units (not used here).

    end;

var
    Form1: TForm1;    // This declares the global instance of the form.

implementation        // Implementation section starts here — contains the actual code of the methods.

{$R *.lfm}            // This links the form's design file (.lfm) into the unit at compile time.

{ TForm1 }            // This just tells you: "Hey! The following procedures belong to the TForm1 form."

// This is the handler of the OnClick event of the ButtonHello.
// "Event handler" means: The associated code is executed whenever the
// corresponding "event" is triggered, in this case, when the button is clicked.
procedure TForm1.ButtonHelloClick(Sender: TObject);
begin
    Memo1.Append('Hello World');                    // Appends the string 'Hello World' as a new line at the end of the memo.
end;

// This is the handler of the OnClick event of the ButtonClose.
// The event fires when the ButtonClose is clicked.
procedure TForm1.ButtonCloseClick(Sender: TObject);
begin
    Close;    // Closes the form (and ends the application if it's the main form).
end;

end.

