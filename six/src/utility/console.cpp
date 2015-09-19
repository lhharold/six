#include "core.h"
#include "console.h"

namespace six {
	Console::Console() 
		: mConsole(NULL) 
	{
#if OS_PLATFORM == OS_PLATFORM_WIN32
		AllocConsole();
        mConsole = (void*)freopen("CONOUT$","w+t",stdout);

		char buf[MAX_PATH];
		GetConsoleTitle(buf, MAX_PATH);   
		HWND hwnd = ::FindWindow(NULL, buf);   
		HMENU hmenu = ::GetSystemMenu(hwnd, FALSE);   
		if (hwnd) {
			::RemoveMenu(hmenu, SC_CLOSE, MF_BYCOMMAND);
			::SendMessage(hwnd, WM_SYSCOMMAND, SC_MINIMIZE, 0);
		}
#elif OS_PLATFORM == OS_PLATFORM_LINUX || OS_PLATFORM == OS_PLATFORM_MAC
        mConsole = (void*)freopen("/dev/tty","w+t",stdout);
#endif
		ASSERT(mConsole != NULL);
	}

	Console::~Console() {
#if OS_PLATFORM == OS_PLATFORM_WIN32
		FCLOSE((FILE*)mConsole);
		FreeConsole();
#elif OS_PLATFORM == OS_PLATFORM_LINUX || OS_PLATFORM == OS_PLATFORM_MAC
		FCLOSE((FILE*)mConsole);
#endif
	}
}