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
//#ifndef _LINUX_IN_H
//#define _LINUX_IN_H

//#include <linux/types.h>
//#include <linux/socket.h>

const
 IPPROTO_IP = 0;
 IPPROTO_ICMP = 1;
 IPPROTO_IGMP = 2;
 IPPROTO_IPIP = 4;
 IPPROTO_TCP = 6;
 IPPROTO_EGP = 8;
 IPPROTO_PUP = 12;
 IPPROTO_UDP = 17;
 IPPROTO_IDP = 22;
 IPPROTO_DCCP = 33;
 IPPROTO_RSVP = 46;
 IPPROTO_GRE = 47;

 IPPROTO_IPV6 = 41;

 IPPROTO_ESP = 50;
 IPPROTO_AH = 51;
 IPPROTO_PIM = 103;

 IPPROTO_COMP = 108;
 IPPROTO_SCTP = 132;

 IPPROTO_RAW = 255;
 IPPROTO_MAX = 256;

type
  Tin_addr = record
//    s_addr: __u32;
  end;
  Pin_addr = ^Tin_addr;

const
  IP_TOS =1;
  IP_TTL =2;
  IP_HDRINCL =3;
  IP_OPTIONS =4;
  IP_ROUTER_ALERT =5;
  IP_RECVOPTS =6;
  IP_RETOPTS =7;
  IP_PKTINFO =8;
  IP_PKTOPTIONS =9;
  IP_MTU_DISCOVER =10;
  IP_RECVERR =11;
  IP_RECVTTL =12;
  IP_RECVTOS =13;
  IP_MTU =14;
  IP_FREEBIND =15;
  IP_IPSEC_POLICY =16;
  IP_XFRM_POLICY =17;
  IP_PASSSEC =18;

  IP_RECVRETOPTS =IP_RETOPTS;

  IP_PMTUDISC_DONT =0;
  IP_PMTUDISC_WANT =1;
  IP_PMTUDISC_DO =2;

  IP_MULTICAST_IF =32;
  IP_MULTICAST_TTL =33;
  IP_MULTICAST_LOOP =34;
  IP_ADD_MEMBERSHIP =35;
  IP_DROP_MEMBERSHIP =36;
  IP_UNBLOCK_SOURCE =37;
  IP_BLOCK_SOURCE =38;
  IP_ADD_SOURCE_MEMBERSHIP =39;
  IP_DROP_SOURCE_MEMBERSHIP =40;
  IP_MSFILTER =41;
  MCAST_JOIN_GROUP =42;
  MCAST_BLOCK_SOURCE =43;
  MCAST_UNBLOCK_SOURCE =44;
  MCAST_LEAVE_GROUP =45;
  MCAST_JOIN_SOURCE_GROUP =46;
  MCAST_LEAVE_SOURCE_GROUP =47;
  MCAST_MSFILTER =48;

  MCAST_EXCLUDE =0;
  MCAST_INCLUDE =1;

  IP_DEFAULT_MULTICAST_TTL =1;
  IP_DEFAULT_MULTICAST_LOOP =1;

type
  Tip_mreq = record
    imr_multiaddr: Tin_addr;
    imr_interface: Tin_addr;
  end;

  Tip_mreqn = record
    imr_multiaddr: Tin_addr;
    imr_address: Tin_addr;
    imr_ifindex: cint;
  end;

  Tip_mreq_source = record
{    imr_multiaddr: __u32;
    imr_interface: __u32;
    imr_sourceaddr: __u32;}
  end;

  Tip_msfilter = record
{    imsf_multiaddr: __u32;
    imsf_interface: __u32;
    imsf_fmode: __u32;
    imsf_numsrc: __u32;
    imsf_slist: array[0..0] of __u32;}
  end;

//#define IP_MSFILTER_SIZE(numsrc)   (sizeof(struct ip_msfilter) - sizeof(__u32)   + (numsrc) * sizeof(__u32))

  Tgroup_req = record
{    gr_interface: __u32;
    gr_group: __kernel_sockaddr_storage;}
  end;

  Tgroup_source_req = record
{    gsr_interface: __u32;
    gsr_group: __kernel_sockaddr_storage;
    gsr_source: __kernel_sockaddr_storage;}
  end;

{  group_filter = record
 __u32 gf_interface;
 struct __kernel_sockaddr_storage gf_group;
 __u32 gf_fmode;
 __u32 gf_numsrc;
 struct __kernel_sockaddr_storage gf_slist[1];
  end;}

//#define GROUP_FILTER_SIZE(numsrc)   (sizeof(struct group_filter) - sizeof(struct __kernel_sockaddr_storage)   + (numsrc) * sizeof(struct __kernel_sockaddr_storage))

  Tin_pktinfo = record
    ipi_ifindex: cint;
    ipi_spec_dst: Tin_addr;
    ipi_addr: Tin_addr;
  end;

const
  __SOCK_SIZE__ = 16;

type
  Tsockaddr_in = record
//    sin_family: sa_family_t;
//    sin_port: cushortint; // unsigned short int
//    sin_addr: in_addr;

// unsigned char __pad[__SOCK_SIZE__ - sizeof(short int) -
// sizeof(unsigned short int) - sizeof(struct in_addr)];
  end;

//#define sin_zero __pad

//#define IN_CLASSA(a) ((((long int) (a)) & 0x80000000) == 0)
//#define IN_CLASSA_NET 0xff000000
//#define IN_CLASSA_NSHIFT 24
//#define IN_CLASSA_HOST (0xffffffff & ~IN_CLASSA_NET)
//#define IN_CLASSA_MAX 128

//#define IN_CLASSB(a) ((((long int) (a)) & 0xc0000000) == 0x80000000)
//#define IN_CLASSB_NET 0xffff0000
//#define IN_CLASSB_NSHIFT 16
//#define IN_CLASSB_HOST (0xffffffff & ~IN_CLASSB_NET)
//#define IN_CLASSB_MAX 65536

//#define IN_CLASSC(a) ((((long int) (a)) & 0xe0000000) == 0xc0000000)
//#define IN_CLASSC_NET 0xffffff00
//#define IN_CLASSC_NSHIFT 8
//#define IN_CLASSC_HOST (0xffffffff & ~IN_CLASSC_NET)

//#define IN_CLASSD(a) ((((long int) (a)) & 0xf0000000) == 0xe0000000)
//#define IN_MULTICAST(a) IN_CLASSD(a)
//#define IN_MULTICAST_NET 0xF0000000

//#define IN_EXPERIMENTAL(a) ((((long int) (a)) & 0xf0000000) == 0xf0000000)
//#define IN_BADCLASS(a) IN_EXPERIMENTAL((a))

//#define INADDR_ANY ((unsigned long int) 0x00000000)

//#define INADDR_BROADCAST ((unsigned long int) 0xffffffff)

//  INADDR_NONE ((unsigned long int) 0xffffffff)

const
  IN_LOOPBACKNET =127;

  INADDR_LOOPBACK =$7f000001;
//#define IN_LOOPBACK(a) ((((long int) (a)) & 0xff000000) == 0x7f000000)

  INADDR_UNSPEC_GROUP =$e0000000;
  INADDR_ALLHOSTS_GROUP =$e0000001;
  INADDR_ALLRTRS_GROUP =$e0000002;
  INADDR_MAX_LOCAL_GROUP =$e00000ff;

//#include <asm/byteorder.h>

//#endif

