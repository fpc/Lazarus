Name: fpcsrc
Version: 1.0.7
Release: 1
Copyright: GPL
Group: Development/Languages
Source: %{name}-%{version}.tgz
Summary: FreePascal sources 1.0.7
Packager: Mattias Gaertner (gaertner@informatik.uni-koeln.de)
URL: http://www.freepascal.org/
BuildRoot: %{_tmppath}/fpcsrc-build

%description
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked. Basic Delphi support is already implemented (classes,
exceptions, ansistrings, RTTI). This package contains the sources for the
commandline compiler and utils. Provided units are the runtime library (RTL),
free component library (FCL), gtk, ncurses, zlib, mysql, postgres, ibase
bindings and many more.

%prep

%setup -c

%build

%install
if [ %{buildroot} != "/" ]; then
	rm -rf %{buildroot}
fi
mkdir -p %{buildroot}%{_datadir}/fpcsrc
cp -a fpc/* %{buildroot}%{_datadir}/fpcsrc/

%clean
if [ %{buildroot} != "/" ]; then
	rm -rf %{buildroot}
fi

%files
%defattr(-,root,root)
%{_datadir}/fpcsrc

%changelog


