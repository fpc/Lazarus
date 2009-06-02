This directory contains demo applications for the FP/Lazarus web
package lazweb

echo demo:
==========

This is a very simple demo, it just echoes the request back to the
webbrowser. The standard webutil unit contains a 'dumprequest'
routine which can be used to dump the request.

hello world demo:
============
The simplest "Hello World" example using fcl-web (fpweb) as cgi and Apache module. For more information see README.txt in helloword directory. 

session demo:
=============

The session directory contains a session demonstration CGI app.
The DemoSession action variable controls what will happen:
DemoSession=NewSession - a new session is started
DemoSession=InSession (the default): the session variable is displayed
DemoSession=EndSession - the sesson is destroyed.

Sample URLs:

end session:
http://localhost/~michael/sessiondemo.cgi?DemoSession=EndSession

set session variable 'var':
http://localhost/~michael/sessiondemo.cgi?Var=SomeValue

start session:
http://localhost/~michael/sessiondemo.cgi?DemoSession=NewSession

image demo:
===========

You need to set some constants in this application, you must configure some
font/image paths and set a font name.

This demo shows how to send images. It has 2 actions:

Pie: creates a pie diagram on the fly. It uses the fpimage routines
for this. The text is shown using a font, for which the FreeType
font library is used.

A sample URL is:
http://localhost/~michael/imagedemo.cgi?action=pie

It can be configured with the Runs/Failed/Skipped/FontName query
variables. See the sources for more information.

The second action is image:
In this mode, the image is searched on disk through the 'FileName' query
variable. The directory is hardcoded in the program, see the constants in
the source. Only PNG images are sent.

Sample URL:
http://localhost/~michael/imagedemo.cgi?action=file&filename=menu_run

This will send the 'menu_run.png' file from the configured directory.

fptemplate demos:
==================
These examples are demonstrating some uses of templates (with FPTemplate) when 
generating HTML pages by CGI programs or Apache modules. For more information see the README.txt in the fptemplate directory.