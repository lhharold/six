#ifndef __SIX_WIN32WINDOW_H_INCLUDE__
#define __SIX_WIN32WINDOW_H_INCLUDE__

namespace six {
	class Win32Window : public RenderWindow {
	public:
		Win32Window(GLWin32Support* support);
		virtual ~Win32Window();

    virtual void create(const char* name, u32 width, u32 height, bool fullScreen);
  protected:
    GLWin32Support* mSupport;
    Win32Context* mContext;
    HWND mHwnd;
    HDC mHDC;
    HGLRC	mGlrc;
    bool mClosed;
    bool mExternal;
	};
}

#endif //__SIX_WIN32WINDOW_H_INCLUDE__
