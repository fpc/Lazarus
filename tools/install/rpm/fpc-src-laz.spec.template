Name: fpc-src-laz
Version: LAZVERSION
Release: LAZRELEASE
License: LGPL2
Group: Development/Languages
Source: %{name}-%{version}-%{release}.source.tar.gz
Summary: FreePascal sources
Packager: Mattias Gaertner (mattias@freepascal.org)
URL: http://www.freepascal.org/
BuildRoot: %{_tmppath}/%{name}-build%{version}
Conflicts: fpc-src

%global debug_package %{nil}
%define fpcsrcdir %{_datadir}/fpcsrc
%define destdir %{buildroot}%{fpcsrcdir}/%{version}
%define _source_payload w9.bzdio
%define _binary_payload w9.bzdio

# The normal redhat rpm scripts tests every installed file for requirements.
# We install only sources, so we don't need the requirements.
AutoReq: 0

# The normal redhat rpm scripts do not recognize properly, what files to strip
# Hook our own strip command
%define __strip LAZSCRIPTDIR/smart_strip.sh

%description
Free Pascal is a mature, versatile, open source Pascal compiler.
It can target many processor architectures: Intel x86 (16 and 32 bit),
AMD64/x86-64, PowerPC, PowerPC64, SPARC, SPARC64, ARM, AArch64, MIPS,
Motorola 68k, AVR, and the JVM. Supported operating systems include
Windows (16/32/64 bit, CE, and native NT), Linux,
Mac OS X/iOS/iPhoneSimulator/Darwin, FreeBSD and other BSD flavors,
DOS (16 bit, or 32 bit DPMI), OS/2, AIX, Android, Haiku,
Nintendo GBA/DS/Wii, AmigaOS, MorphOS, AROS, Atari TOS,
and various embedded platforms. Additionally, support for RISC-V (32/64),
Xtensa, and Z80 architectures, and for the LLVM compiler infrastructure
is available in the development version.

%prep

%setup -c

%build

%install
if [ %{buildroot} != "/" ]; then
  rm -rf %{buildroot}
fi
mkdir -p %{destdir}
cp -a fpc/* %{destdir}/
find %{destdir} -name '*.o' -delete
find %{destdir} -name '*.a' -delete

%clean
if [ %{buildroot} != "/" ]; then
  rm -rf %{buildroot}
fi

%files
%defattr(-,root,root)
%{fpcsrcdir}

%changelog

