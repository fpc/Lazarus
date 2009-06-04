List records using template tags, example
================================
(Requires FPC versions past 4/30/2009 /revisions 13062 and later/)


Demonstrates how to list multiple records using templates and template tags.

Setup:

CGI
---------
Usually it works if you put the templates next to the CGI executable file.
(mytemplate3.html)

http://<WebServer>/cgi-bin/<CGIExecutableName>/func1call should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/listrecords.exe/func1call

Note: You need to change the URLs in the templates if "cgi-bin" or 
"listrecords.exe" changes (for example on Linux it is not listrecords.exe).
(* see at the bottom)


Apache
---------
Usually it works if you put the templates into the Apache main directory (not 
the DocumentRoot, but the main Apache directory).
(mytemplate3.html)

http://<WebServer>/<ApacheLocationName>/func1call should start the 
example if everything is set up properly.
(* see at the bottom)
ex: http://127.0.0.1:8080/myapache/func1call
if in httpd.conf it was set up as:
LoadModule mod_listrecords "<path_to_mod>/mod_listrecords.dll"
<Location /myapache>
    SetHandler mod_listrecords
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "myapache" changes. 
Also, for example on Linux the module can be mod_listrecords.so and not 
mod_listrecords.dll 

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation will fail because the file is in use (Apache
 modules stay in the memory). So first you always need to stop the server before
you recompile or before you copy over the new version of the created module.



(*)To use the same templates for both CGI programs and Apache modules the 
links can also be generated dynamically when the response html pages are 
generated. For example instead of putting

/cgi-bin/fileupload.exe/listfiles
/myapache/listfiles

into the templates, a tag can be used to put the right link there.
Ex:
<form action="{+SCRIPTPATH+}/func1call" method="post" ...
Of course, the {+SCRIPTPATH+} template tag needs to be replaced with the 
correct string.