#include "Rts.h"
#include "util.h"


////////////////////////////////////////////////////////////////////////
//
// kprintf and friends
//

/* From gprintf.c */
int general_printf(int (*output_function)(void *, int), void *output_pointer,
  const char *control_string, const int *argument_pointer);

/* From video.c */
#ifdef BOCHS_VIDEO
static void putch(unsigned c)
{
  /* This IO port is used by Bochs to put a simple character on stdout. */
  outb(0xe9, c & 0xFF);
}
#else
void putch(unsigned c);
void init_video(void);
#endif

static int kprintf_helper(void *ptr, int c)
{
  putch(c);
  return 0;
}

void kprintf(char const * fmt, ...)
{
  (void) general_printf(kprintf_helper, NULL, fmt, (int *)(&fmt + 1));
}

void * stderr = NULL;
void * stdout = NULL;

/* RTS-DEBUG */
void fprintf(void * dummy, char const * fmt, ...)
{
  (void) general_printf(kprintf_helper, NULL, fmt, (int *)(&fmt + 1));
}

void vfprintf(void * dummy, char const * fmt, void * ap)
{
  (void) general_printf(kprintf_helper, NULL, fmt, ap);
}

static int fill_string(void *p, int c)
{
  *(*(char **)p)++ = c;
  return 0;
}
 
int sprintf(char *s, char const *control, ...)
{
  int n = general_printf(fill_string, &s, control, (int *)(&control+1));
  *s = 0;
  return n;
}

void fwrite(void const * ptr, size_t size, size_t nmemb, void * dummy)
{
  if (size == 1) {
    while (nmemb--) {
      putch(*((char *) ptr++));
    }
  }
}

ssize_t write(int fd, const void *buf, size_t count)
{
  if (1 == fd || 2 == fd) {
    while (count--) {
      putch(*((char *) buf++));
    }
    return count;
  } else {
    return 0;
  }
}

void fputc(int c, void * dummy)
{
  putch(c);
}

void fputs(char const * str, void * dummy)
{
  kprintf("%s", str);
}

void fflush(void * dummy)
{
  return;
}

char * getenv(char const * dummy)
{
  return NULL;
}

void * fopen(char const * path, char const * mode)
{
  return NULL;
}

void c_print(char const * str)
{
  kprintf("%s", str);
}

void exit(int n) {
  while (1)
    __asm__("hlt");
}

void abort() {
  while (1)
    __asm__("hlt");
}

void perror(const char *s) {
  kprintf("%s", s);
  abort();
}


