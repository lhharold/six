#ifndef __SIX_WIN32EVENT_H_INCLUDE__
#define __SIX_WIN32EVENT_H_INCLUDE__

namespace six {
	class WindowEvent {
	public:
    static void messagePump();
#if OS_PLATFORM == OS_PLATFORM_WIN32
    static LRESULT CALLBACK _WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
#endif
	};
}

#endif //__SIX_WIN32EVENT_H_INCLUDE__
