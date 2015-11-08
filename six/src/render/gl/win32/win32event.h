#ifndef __SIX_WIN32EVENT_H_INCLUDE__
#define __SIX_WIN32EVENT_H_INCLUDE__

namespace six {
	class Win32Event {
	public:
    static LRESULT CALLBACK _WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	};
}

#endif //__SIX_WIN32EVENT_H_INCLUDE__
