#include "core.h"
#include "win32window.h"
#include "win32event.h"

namespace six {
  Win32Window::Win32Window(GLWin32Support* support)
    : mSupport(support)
    , mContext(NULL)
    , mHwnd(NULL)
    , mHDC(NULL)
    , mGlrc(NULL)
    , mClosed(false)
    , mExternal(false)
  {
  }

  Win32Window::~Win32Window() {
    
  }

  void Win32Window::create(const char* name, u32 width, u32 height, bool fullScreen) {
    if(mHwnd != NULL)
      destroy();
    mHwnd = NULL;
    DWORD dwStyleEx = 0;
    HWND hParent = NULL;
    HMENU hMenu = NULL;
#if STATIC_LIB
    HINSTANCE hInst = NULL;
#else
    HINSTANCE hInst = GetModuleHandle("RenderSysGL.dll");
#endif

    mName = name;
    mWidth = width;
    mHeight = height;
    mFullScreen = fullScreen;

    if(fullscreen)
      dwStyleEx |= WS_EX_TOPMOST;

    WNDCLASSEX wcex;
    wcex.cbSize        = sizeof(WNDCLASSEX);
    wcex.style         = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc   = Win32Event::_WndProc;
    wcex.cbClsExtra		 = 0;
    wcex.cbWndExtra		 = 0;
    wcex.hInstance		 = hInst;
    wcex.hIcon			   = LoadIcon(NULL, IDI_APPLICATION);
    wcex.hCursor		   = LoadCursor(NULL, IDC_ARROW);
    wcex.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wcex.lpszMenuName  = NULL;
    wcex.lpszClassName = "OpenGLWin32Window";
    wcex.hIconSm       = NULL;
    RegisterClass(&wcex);
    mHwnd = CreateWindowEx(dwStyleEx, "OpenGLWin32Window", name, WS_OVERLAPPED, mLeft, mTop,
                           mWidth, mHeight, hParent, hMenu, hInst, this);

    HDC oldHdc wglGetCurrentDC();
    HGLRC oldContext = wglGetCurrentContext();
    RECT rc;
    GetWindowRect(mHwnd, &rc);
    mLeft = rc.left;
    mTop = rc.top;
    GetClientRect(mHwnd, &rc);
    mWidth = rc.right;
    mHeight = rc.bottom;

    mHDC = GetDC(mHwnd);
    mGlrc = wglCreateContext(mHDC);
    if(!mGlrc) {
      //translateWGLError();
      ASSERT(0 && "Win32Window::create - wglCreateContext() failed.");
    }
    if(oldContext && oldContext != mGlrc) {
      if(!wglShareLists(oldContext, mGlrc))
        ASSERT(0 && "Win32Window::create - wglShareLists() failed.");
    }
    if(!wglMakeCurrent(mHDC, mGlrc))
      ASSERT(0 && "Win32Window::create - wglMakeCurrent() failed.");

    if(oldContext && oldContext != mGlrc) {
      if(!wglMakeCurrent(oldHdc, oldContext))
        ASSERT(0 && "Win32Window::create - wglMakeCurrent() failed.");
    }

    mContext = new Win32Context(mHDC, mGlrc);
    mActive = true;
    setHidden(false);
  }

  void Win32Window::setHidden(bool hidden) {
		mHidden = hidden;
		if (!mExternal) {
      if (hidden)
        ShowWindow(mHWnd, SW_HIDE);
      else
        ShowWindow(mHWnd, SW_SHOWNORMAL);
    }
	}

  void Win32Window::destroy() {
    if(mHwnd == NULL)
      return;
    SAFE_DEL(mContext);
    if(mGlrc) {
      wglDeleteContext(mGlrc);
      mGlrc = 0;
    }
    if(!mFullScreen) {
      //ChangeDisplaySettingsEx()
    }
    DestroyWindow(mHwnd);
    //if the wnd is external remember to ReleaseDC(mHWnd, mHDC);

    mActive = false;
    mClosed = true;
    mHDC = 0;
    mHwnd = 0;
  }
}
