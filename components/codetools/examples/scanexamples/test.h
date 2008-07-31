// The macro __TEST_H is an example for a common trick in c header files
// Undefine it. The h2p simplifier will remove it.
#undef __TEST_H

/*
  Comment
  */
#ifndef __TEST_H
#define __TEST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

#ifndef SOME_FLAG1
#define SOME_FLAG1   31
#define SOME_FLAG2  SOME_FLAG1
#endif

#define constant 1
#define macro1 1

// empty macro
#define MPI_FILE_DEFINED  // double definition, this one will be removed
#define MPI_FILE_DEFINED
// null pointer
#define MPI_BOTTOM      (void *)0

/* An anonymous enum */
enum {
        TEST_ENUM1 = 1, /* Enum starts at 1 */
        TEST_ENUM2,
        TEST_ENUM3
};

enum e1{dark, light};
enum e2{a=3, b=9};

/* Byte order conversions */
#if __BYTE_ORDER == __LITTLE_ENDIAN
#define htobs(d)  (d)
#define htobl(d)  (d)
#define btohs(d)  (d)
#define btohl(d)  (d)
#elif __BYTE_ORDER == __BIG_ENDIAN
#define htobs(d)  bswap_16(d)
#define htobl(d)  bswap_32(d)
#define btohs(d)  bswap_16(d)
#define btohl(d)  bswap_32(d)
#else
#error "Unknown byte order"
#endif

/* complex macro function */
#define test_get_unaligned(ptr)                 \
({                                              \
        struct __attribute__((packed)) {        \
                typeof(*(ptr)) __v;             \
        } *__p = (void *) (ptr);                \
        __p->__v;                               \
})

/* named struct with macro and implicit type */
typedef struct {
        uint8_t b[6]; // implicit type
} __attribute__((packed)) bdaddr_t;

void baswap(bdaddr_t *dst, const bdaddr_t *src);
bdaddr_t *strtoba(const char *str);
int baprintf(const char *format, ...);
int bafprintf(FILE *stream, const char *format, ...);
int basprintf(char *str, const char *format, ...);
int basnprintf(char *str, size_t size, const char *format, ...);
int hci_send_req(int dd, struct hci_request *req, int timeout); // implicit struct
int hci_for_each_dev(int flag, int(*func)(int dd2, int dev_id, long arg1), long arg2); // implicit function type

void *bt_malloc(size_t size);
void bt_free(void *ptr);

#define HIDPCONNADD     _IOW('H', 200, int)

struct hidp_connadd_req {
        int ctrl_sock;          /* Connected control socket */
        int intr_sock;          /* Connected interrupt socket */
        uint16_t parser;        /* Parser version */
        uint16_t rd_size;       /* Report descriptor size */
        uint8_t *rd_data;       /* Report descriptor data */
        uint8_t  country;
        uint8_t  subclass;
        uint16_t vendor;
        uint16_t product;
        uint16_t version;
        uint32_t flags;
        uint32_t idle_to;
        char name[128];         /* Device name */
};

struct hidp_connlist_req {
        uint32_t cnum;
        struct hidp_conninfo *ci;
};

#define SDP_UNIX_PATH "/var/run/sdp"
#define SDP_PSM                 0x0001
#define SDP_PRIMARY_LANG_BASE           0x0100
#define SDP_ATTR_SVCNAME_PRIMARY        0x0000 + SDP_PRIMARY_LANG_BASE
#define AnOctal                 0001

typedef struct {
        uint8_t type; // invalid pascal name
        union {
                uint16_t  uuid16;
                uint32_t  uuid32;
                uint128_t uuid128;
        } value;
} uuid_t;


#define SDP_IS_UUID(x) ((x) == SDP_UUID16 || (x) == SDP_UUID32 || (x) ==SDP_UUID128)

typedef struct struct1 struct2;
struct struct1 {
        struct2 *next;
        void *data;
};

typedef void(*procedure_type)(void *, void *);

complex operator+(complex, complex);

int y = 7;
float internalfunc(int){};
int dim2[][3];
bool b1 = a==b;
char c = 'a';
short signed int ssi_octal = 0123;
long unsigned int lui = sizeof(char);

int *pi; // pointer to int
char ** ppc; // pointer to pointer to char
int* ap[15]; // array of 15 pointers to ints
int (*fp)(char*); // pointer to function taking a char* argument; returns an int
int * func1(char*); // function taking a char* argument; returns a pointer to int
int func2(int=3); // function taking an int argument or no argument; returns an int
unsigned short unsigned_short;
unsigned long long unsigned_long_long;


#define MACRO_CONCATENATION(a,b) a##b

// const or not const
const char a;           // A constant character
char const b;           // A constant character (the same)
char *const c;          // A constant pointer to a character
const char *const d;    // A constant pointer to a constant character
const char *e;          // A pointer to a constant character. The pointer may be modified.

/* Copy, swap, convert BD Address */
static inline int bacmp(const bdaddr_t *ba1, const bdaddr_t *ba2)
{
        return memcmp(ba1, ba2, sizeof(bdaddr_t));
}

typedef unsigned short sa_family_t;
int hci_inquiry(int dev_id, int len, int num_rsp, const uint8_t *lap, inquiry_info **ii, long flags);

const char* (*item_name)(void* ctx);
const struct AVOption *option;
#if LIBAVUTIL_VERSION_INT < (50<<16)
void av_log(void*, int level, const char *fmt, ...) __attribute__ ((__format__ (__printf__, 3, 4)));
struct SwsContext; // SwsContext = record end;


#ifdef __cplusplus
}
#endif

#endif

