{%MainUnit android_sockets.pas}
{****************************************************************************
 ****************************************************************************
 ***
 ***   This header was automatically generated from a Linux kernel header
 ***   of the same name, to make information necessary for userspace to
 ***   call into the kernel available to libc.  It contains only constants,
 ***   structures, and macros generated from the original header, and thus,
 ***   contains no copyrightable information.
 ***
 ****************************************************************************
 ****************************************************************************}
//#ifndef _LINUX_SOCKET_H
//#define _LINUX_SOCKET_H

const
 _K_SS_MAXSIZE = 128;
//#define _K_SS_ALIGNSIZE (__alignof__ (struct sockaddr *))

{struct __kernel_sockaddr_storage = record
 unsigned short ss_family;

 char __data[_K_SS_MAXSIZE - sizeof(unsigned short)];

end;// __attribute__ ((aligned(_K_SS_ALIGNSIZE)));}

//#if !defined(__GLIBC__) || __GLIBC__ < 2

(*#include <asm/socket.h>
#include <linux/sockios.h>  
#include <linux/uio.h>  
#include <linux/types.h>  
#include <linux/compiler.h>  

typedef unsigned short sa_family_t;

struct sockaddr {
 sa_family_t sa_family;
 char sa_data[14];
};

struct linger {
 int l_onoff;
 int l_linger;
};

#define sockaddr_storage __kernel_sockaddr_storage

struct msghdr {
 void * msg_name;
 int msg_namelen;
 struct iovec * msg_iov;
 __kernel_size_t msg_iovlen;
 void * msg_control;
 __kernel_size_t msg_controllen;
 unsigned msg_flags;
};

struct cmsghdr {
 __kernel_size_t cmsg_len;
 int cmsg_level;
 int cmsg_type;
};

#define __CMSG_NXTHDR(ctl, len, cmsg) __cmsg_nxthdr((ctl),(len),(cmsg))
#define CMSG_NXTHDR(mhdr, cmsg) cmsg_nxthdr((mhdr), (cmsg))

#define CMSG_ALIGN(len) ( ((len)+sizeof(long)-1) & ~(sizeof(long)-1) )

#define CMSG_DATA(cmsg) ((void * )((char * )(cmsg) + CMSG_ALIGN(sizeof(struct cmsghdr))))
#define CMSG_SPACE(len) (CMSG_ALIGN(sizeof(struct cmsghdr)) + CMSG_ALIGN(len))
#define CMSG_LEN(len) (CMSG_ALIGN(sizeof(struct cmsghdr)) + (len))

#define __CMSG_FIRSTHDR(ctl,len) ((len) >= sizeof(struct cmsghdr) ?   (struct cmsghdr * )(ctl) :   (struct cmsghdr * )NULL)
#define CMSG_FIRSTHDR(msg) __CMSG_FIRSTHDR((msg)->msg_control, (msg)->msg_controllen)
#define CMSG_OK(mhdr, cmsg) ((cmsg)->cmsg_len >= sizeof(struct cmsghdr) &&   (cmsg)->cmsg_len <= (unsigned long)   ((mhdr)->msg_controllen -   ((char * )(cmsg) - (char * )(mhdr)->msg_control)))

#ifdef __GNUC__
#define __KINLINE static __inline__
#elif defined(__cplusplus)
#define __KINLINE static inline
#else
#define __KINLINE static
#endif

__KINLINE struct cmsghdr * __cmsg_nxthdr(void *__ctl, __kernel_size_t __size,
 struct cmsghdr *__cmsg)
{
 struct cmsghdr * __ptr;*)
{
 __ptr = (struct cmsghdr*)(((unsigned char *) __cmsg) + CMSG_ALIGN(__cmsg->cmsg_len));
 if ((unsigned long)((char*)(__ptr+1) - (char *) __ctl) > __size)
 return (struct cmsghdr *)0;

 return __ptr;
}

//__KINLINE struct cmsghdr * cmsg_nxthdr (struct msghdr *__msg, struct cmsghdr *__cmsg)
{
 return __cmsg_nxthdr(__msg->msg_control, __msg->msg_controllen, __cmsg);
}

{#define SCM_RIGHTS 0x01
#define SCM_CREDENTIALS 0x02  
#define SCM_SECURITY 0x03  

struct ucred = record
 __u32 pid;
 __u32 uid;
 __u32 gid;
end;}

const
  AF_UNSPEC =0;
  AF_UNIX =1;
  AF_LOCAL =1;
  AF_INET =2;
  AF_AX25 =3;
  AF_IPX =4;
  AF_APPLETALK =5;
  AF_NETROM =6;
  AF_BRIDGE =7;
  AF_ATMPVC =8;
  AF_X25 =9;
  AF_INET6 =10;
  AF_ROSE =11;
  AF_DECnet =12;
  AF_NETBEUI =13;
  AF_SECURITY =14;
  AF_KEY =15;
  AF_NETLINK =16;
  AF_ROUTE =AF_NETLINK;
  AF_PACKET =17;
  AF_ASH =18;
  AF_ECONET =19;
  AF_ATMSVC =20;
  AF_SNA =22;
  AF_IRDA =23;
  AF_PPPOX =24;
  AF_WANPIPE =25;
  AF_LLC =26;
  AF_TIPC =30;
  AF_BLUETOOTH =31;
  AF_MAX =32;

  PF_UNSPEC =AF_UNSPEC;
  PF_UNIX =AF_UNIX;
  PF_LOCAL =AF_LOCAL;
  PF_INET =AF_INET;
  PF_AX25 =AF_AX25;
  PF_IPX =AF_IPX;
  PF_APPLETALK =AF_APPLETALK;
  PF_NETROM =AF_NETROM;
  PF_BRIDGE =AF_BRIDGE;
  PF_ATMPVC =AF_ATMPVC;
  PF_X25 =AF_X25;
  PF_INET6 =AF_INET6;
  PF_ROSE =AF_ROSE;
  PF_DECnet =AF_DECnet;
  PF_NETBEUI =AF_NETBEUI;
  PF_SECURITY =AF_SECURITY;
  PF_KEY =AF_KEY;
  PF_NETLINK =AF_NETLINK;
  PF_ROUTE =AF_ROUTE;
  PF_PACKET =AF_PACKET;
  PF_ASH =AF_ASH;
  PF_ECONET =AF_ECONET;
  PF_ATMSVC =AF_ATMSVC;
  PF_SNA =AF_SNA;
  PF_IRDA =AF_IRDA;
  PF_PPPOX =AF_PPPOX;
  PF_WANPIPE =AF_WANPIPE;
  PF_LLC =AF_LLC;
  PF_TIPC =AF_TIPC;
  PF_BLUETOOTH =AF_BLUETOOTH;
  PF_MAX =AF_MAX;

  SOMAXCONN =128;

  MSG_OOB =1;
  MSG_PEEK =2;
  MSG_DONTROUTE =4;
  MSG_TRYHARD =4;
  MSG_CTRUNC =8;
  MSG_PROBE =$10;
  MSG_TRUNC =$20;
  MSG_DONTWAIT =$40;
  MSG_EOR =$80;
  MSG_WAITALL =$100;
  MSG_FIN =$200;
  MSG_SYN =$400;
  MSG_CONFIRM =$800;
  MSG_RST =$1000;
  MSG_ERRQUEUE =$2000;
  MSG_NOSIGNAL =$4000;
  MSG_MORE =$8000;

  MSG_EOF =MSG_FIN;

  MSG_CMSG_COMPAT =0;

  SOL_IP =0;

  SOL_TCP =6;
  SOL_UDP =17;
  SOL_IPV6 =41;
  SOL_ICMPV6 =58;
  SOL_SCTP =132;
  SOL_RAW =255;
  SOL_IPX =256;
  SOL_AX25 =257;
  SOL_ATALK =258;
  SOL_NETROM =259;
  SOL_ROSE =260;
  SOL_DECNET =261;
  SOL_X25 =262;
  SOL_PACKET =263;
  SOL_ATM =264;
  SOL_AAL =265;
  SOL_IRDA =266;
  SOL_NETBEUI =267;
  SOL_LLC =268;
  SOL_DCCP =269;
  SOL_NETLINK =270;
  SOL_TIPC =271;

  IPX_TYPE =1;

//#endif
///#endif

