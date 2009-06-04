Sessions stored in cookies, Autosession example
==========================
(Requires FPC versions past 4/30/2009 /revisions 13062 and later/)


Every visitor automatically gets a session ID (no login required) passed in a 
cookie to the web visitor's browser and stored on the web server until it 
expires.
This way for example the previously entered data can be carried forward to 
the following pages.


Setup:

CGI
---------
Usually it works if you put the templates next to the CGI executable file.
(autosession-template.html)

http://<WebServer>/cgi-bin/<CGIExecutableName>/gotonextpage should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/autosession.exe/gotonextpage

Note: You need to change the URLs in the templates if "cgi-bin" or 
"autosession.exe" changes (for example on Linux it is not autosession.exe).
(* see at the bottom)


Apache
---------
Usually it works if you put the templates into the Apache main directory (not 
the DocumentRoot, but the main Apache directory).
(autosession-template.html)

http://<WebServer>/<ApacheLocationName>/gotonextpage should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/myapache/gotonextpage
if in httpd.conf it was set up as:
LoadModule mod_autosession "<path_to_mod>/mod_autosession.dll"
<Location /myapache>
    SetHandler mod_autosession
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "myapache" changes. 
Also, for example on Linux the module can be mod_autosession.so and not 
mod_autosession.dll 
(* see at the bottom)

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation will fail because the file is in use (Apache
 modules stay in the memory). So first you always need to stop the server before
you recompile or before you copy over the new version of the created module.



(*)To use the same templates for both CGI programs and Apache modules the 
links can also be generated dynamically when the response html pages are 
generated. For example instead of putting

/cgi-bin/autosession.exe/gotonextpage
/myapache/gotonextpage

into the templates, a tag can be used to put the righ link there.
Ex:
<form action="{*SCRIPTPATH*}/gotonextpage" method="post" ...
Of course, the {*SCRIPTPATH*} template tag needs to be replaced with the 
correct string.