#ifndef __SIX_WIN32WINDOW_H_INCLUDE__
#define __SIX_WIN32WINDOW_H_INCLUDE__

namespace six {
  class Win32Context;
  class GLWin32Support;
	class Win32Window : public RenderWindow {
	public:
		Win32Window(GLWin32Support* support);
		virtual ~Win32Window();

    virtual void getUserData(const char* name, void* data);

    virtual void create(const char* name, u32 width, u32 height, bool fullScreen);
    virtual void destroy();
		virtual void windowMovedOrResized();

    void setHidden(bool hidden);
    HWND getWindowHandle() {return mHwnd;}
  protected:
    GLWin32Support* mSupport;
    Win32Context* mContext;
    HWND mHwnd;
    HDC mHDC;
    HGLRC	mGlrc;
    bool mClosed;
    bool mExternal;
    bool mHidden;
	};
}

#endif //__SIX_WIN32WINDOW_H_INCLUDE__
