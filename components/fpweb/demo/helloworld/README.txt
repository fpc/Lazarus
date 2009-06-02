The simplest "Hello World" example using fcl-web (fpweb)


CGI
----------

http://<WebServer>/cgi-bin/<CGIExecutableName>/func1call should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/helloworld.exe/func1call

Note: You need to change the URL if "cgi-bin" or "helloworld.exe" changes 
(for example on Linux it is not helloworld.exe).


Apache
----------

http://<WebServer>/<ApacheLocationName>/func1call should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/myapache/func1call
if in httpd.conf it was set up as:
LoadModule mod_helloworld "<path_to_mod>/mod_helloworld.dll"
<Location /myapache>
    SetHandler mod_helloworld
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URL if "myapache" changes.
Also, for example on Linux the module can be mod_helloworld.so and not 
mod_helloworld.dll 

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation will fail because the file is in use (Apache
 modules stay in the memory). So first you always need to stop the server before
you recompile or before you copy over the new version of the created module.
