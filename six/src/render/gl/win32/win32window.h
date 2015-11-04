#ifndef __SIX_WIN32WINDOW_H_INCLUDE__
#define __SIX_WIN32WINDOW_H_INCLUDE__

namespace six {
	class Win32Window : public RenderWindow {
	public:
		Win32Window(GLWin32Support* support);
		virtual ~Win32Window();
	};
}

#endif