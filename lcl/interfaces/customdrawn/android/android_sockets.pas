{*
 * This file joins together the Android NDK sockets related headers
 *}
{#include <stdio.h>      /* for printf() and fprintf() */
#include <sys/socket.h> /* for socket(), connect(), send(), and recv() */
#include <arpa/inet.h>  /* for sockaddr_in and inet_addr() */
#include <unistd.h>     /* for close() */}
unit android_sockets;

{$mode delphi}
{$packrecords c}

interface

uses
  ctypes;

{$linklib c}

const
  libname='libc.so';

{$include sockets_linux_in.inc}
{$include sockets_netinet_in.inc}
{$include sockets_arpa_inet.inc}
{$include sockets_sys_socket.inc}
{$include sockets_linux_socket.inc}

implementation

end.

