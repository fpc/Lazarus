Sessions stored in the URLs with Login, example
======================================
(Requires FPC versions past 4/30/2009 /revisions 13062 and later/)


Every visitor needs to log in to get a session ID that is always passed in the 
URLs of the response pages as a query parameter and stored on the web server 
until it expires.
This way fully functional web sites can be built without mixing the different 
visitor's screens and without using cookies to keep the session separated.

Note: Because the session ID is stored dynamically in the generated pages, all
web browser tabs will have separate sessions after logging in with them.
The sample user logins are stored in userdb.txt for the example.


Setup:
The web server application needs read/write access to the file where the session
 informations will be stored with the login names (session-db.txt by default).

CGI
----------
Usually it works if you put the templates next to the CGI executable file.
(testurllogin.html, testurllogout.html, testurlwelcome.html, 
testurlsomepage.html and userdb.txt)

http://<WebServer>/cgi-bin/<CGIExecutableName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/urlsession.exe/login

Note: You need to change the URLs in the templates if "cgi-bin" or 
"urlsession.exe" changes (for example on Linux it is not urlsession.exe).
(* see at the bottom)


Apache
----------
Usually it works if you put the templates into the Apache main directory (not 
the DocumentRoot, but the main Apache directory).
(testurllogin.html, testurllogout.html, testurlwelcome.html, 
testurlsomepage.html and userdb.txt)

http://<WebServer>/<ApacheLocationName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/myapache/login
if in httpd.conf it was set up as:
LoadModule mod_urlsession "<path_to_mod>/mod_urlsession.dll"
<Location /myapache>
    SetHandler mod_urlsession
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "myapache" changes. 
Also, for example on Linux the module can be mod_urlsession.so and not 
mod_urlsession.dll 
(* see at the bottom)

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation will fail because the file is in use (Apache
 modules stay in the memory). So first you always need to stop the server before
you recompile or before you copy over the new version of the created module.



(*)To use the same templates for both CGI programs and Apache modules the 
links can also be generated dynamically when the response html pages are 
generated. For example instead of putting

/cgi-bin/urlsession.exe/login
/myapache/login

into the templates, a tag can be used to put the righ link there.
Ex:
<form action="{*SCRIPTPATH*}/login" method="post" ...
