#ifndef __CTCP__
#define __CTCP__

#if defined(mingw32_HOST_OS)
#include <winsock.h>

typedef int DNSHdl;

struct DNSInfo
	{	struct DNSInfo	*next;
		HANDLE	dnsHdl;
		union	{	struct hostent	Hostent;
					char			freeSpace[MAXGETHOSTSTRUCT];
				}
				junion;
	};
typedef struct DNSInfo DNSInfo;

// the dictionary items
struct dictitem
	{	SOCKET			endpointRef;
		struct dictitem	*next;
		char			availByte;
		unsigned		availByteValid		: 1;
		unsigned		referenceCount		: 2;
		unsigned		hasReceiveNotifier	: 1;
			// three kinds of receivers: receivers for established connections,
			// receivers for dns requests, receivers for asynchronous connect
		unsigned		hasSendableNotifier	: 1;
		unsigned		aborted				: 1;
		unsigned		disconnected		: 1;
	};
typedef struct dictitem dictitem;

#define IE_CONNECTREQUEST		0x0001
#define IE_RECEIVED				0x0004
#define IE_EOM					0x0010
#define IE_SENDABLE				0x0100
#define IE_DISCONNECTED			0x0011
#define IE_IPADDRESSFOUND		0x2000000F
#define IE_IPADDRESSNOTFOUND	0x20000010
#define IE_ASYNCCONNECTCOMPLETE	0x0002
#define IE_ASYNCCONNECTFAILED	0x0003

#define ListenerReceiver	0
#define RChanReceiver		1
#define SChanReceiver		2
#define DNSReceiver			3
#define ConnectReceiver		4

/* PA: InitSockets has no definition.
void InitSockets();
*/
extern dictitem* lookup(SOCKET endpointRef);
#endif

#endif
