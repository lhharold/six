#include "glcore.h"
#include "win32window.h"
#include "windowevent.h"
#include "glwin32support.h"
#include "win32context.h"

namespace six {
  Win32Window::Win32Window(GLWin32Support* support)
    : mSupport(support)
    , mContext(NULL)
    , mHwnd(NULL)
    , mInst(NULL)
    , mHDC(NULL)
    , mGlrc(NULL)
    , mClosed(false)
    , mExternal(false)
    , mHidden(false)
  {
  }

  Win32Window::~Win32Window() {
    
  }

  void Win32Window::getUserData(const char* name, void* data) {
    static const String strGLContext = "GLCONTEXT";
    static const String strWindow = "WINDOW";
    if(strGLContext == name) {
      *static_cast<GLContext**>(data) = mContext;
    } else if(strWindow == name) {
      HWND* pHwnd = (HWND*)data;
      *pHwnd = getWindowHandle();
    }
  }

  void Win32Window::create(const char* name, u32 width, u32 height, bool fullScreen) {
    if(mHwnd != NULL)
      destroy();

    DWORD dwStyle = WS_VISIBLE | WS_CLIPCHILDREN | WS_OVERLAPPEDWINDOW;
    DWORD dwStyleEx = 0;
    HWND hParent = NULL;
    HMENU hMenu = NULL;
#ifdef _STATIC_LIB
    mInst = GetModuleHandle(NULL);
#else
    mInst = GetModuleHandle("RenderSysGL.dll");
#endif

    mName = name;
    mWidth = width;
    mHeight = height;
    mFullScreen = fullScreen;

    if(fullScreen)
      dwStyleEx |= WS_EX_TOPMOST;
#if 0
    WNDCLASSEX wcex;
    wcex.cbSize        = sizeof(WNDCLASSEX);
    wcex.style         = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc   = WindowEvent::_WndProc;
    wcex.cbClsExtra		 = 0;
    wcex.cbWndExtra		 = 0;
    wcex.hInstance		 = mInst;
    wcex.hIcon			   = LoadIcon(NULL, IDI_APPLICATION);
    wcex.hCursor		   = LoadCursor(NULL, IDC_ARROW);
    wcex.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wcex.lpszMenuName  = NULL;
    wcex.lpszClassName = "GLWin32Window";
    wcex.hIconSm       = NULL;
    RegisterClassEx(&wcex);
    mHwnd = CreateWindowEx(dwStyleEx, "OpenGLWin32Window", name, dwStyle, mLeft, mTop,
                           mWidth, mHeight, hParent, hMenu, mInst, this);
#else
    WNDCLASS wc = {CS_HREDRAW | CS_VREDRAW | CS_OWNDC, WindowEvent::_WndProc, 0, 0, mInst,
      LoadIcon(NULL, IDI_APPLICATION), LoadCursor(NULL, IDC_ARROW),
      (HBRUSH)GetStockObject(BLACK_BRUSH), NULL, "GLWin32Window" };
    RegisterClass(&wc);

#if 1
    RECT wndRect;
    wndRect.left = (long)200;
    wndRect.right = wndRect.left + (long)mWidth;
    wndRect.top = (long)100;
    wndRect.bottom = wndRect.top + (long)mHeight;
    AdjustWindowRectEx(&wndRect, dwStyle, FALSE, dwStyleEx);
    mLeft = wndRect.left;
    mTop = wndRect.top;
    mWidth = wndRect.right - wndRect.left;
    mWidth = wndRect.bottom - wndRect.top;
#endif
    mHwnd = CreateWindowEx(dwStyleEx, "GLWin32Window", name,
      dwStyle, mLeft, mTop, mWidth, mHeight, hParent, hMenu, mInst, this);
#endif

#if 0
    GetWindowRect(mHwnd, &rc);
    mLeft = rc.left;
    mTop = rc.top;
    GetClientRect(mHwnd, &rc);
    mWidth = rc.right;
    mHeight = rc.bottom;
#endif

    HDC oldHdc = wglGetCurrentDC();
    HGLRC oldContext = wglGetCurrentContext();
    mHDC = GetDC(mHwnd);
#if 1
    PIXELFORMATDESCRIPTOR pfd = {
      sizeof(PIXELFORMATDESCRIPTOR),	// Size Of This Pixel Format Descriptor
      1,				// Version Number
      PFD_DRAW_TO_WINDOW |		// Format Must Support Window
      PFD_SUPPORT_OPENGL |		// Format Must Support OpenGL
      PFD_DOUBLEBUFFER,		// Must Support Double Buffering
      PFD_TYPE_RGBA,			// Request An RGBA Format
      32,						// Select Our Color Depth
      0, 0, 0, 0, 0, 0,		// Color Bits Ignored
      0,				// No Alpha Buffer
      0,				// Shift Bit Ignored
      0,				// No Accumulation Buffer
      0, 0, 0, 0,			// Accumulation Bits Ignored
      24,				// Z-Buffer (Depth Buffer)
      1,		// Stencil Buffer Depth
      0,				// No Auxiliary Buffer
      PFD_MAIN_PLANE,			// Main Drawing Layer
      0,				// Reserved
      0, 0, 0				// Layer Masks Ignored
    };

    GLuint PixelFormat = ChoosePixelFormat(mHDC, &pfd);
    // choose pixelformat
    if (PixelFormat == 0) {
      ASSERT(0);
    }

    if (!SetPixelFormat(mHDC, PixelFormat, &pfd)) {
      ASSERT(0);
    }
#endif

    mGlrc = wglCreateContext(mHDC);
    if(!mGlrc) {
      translateWGLError();
      ASSERT(0 && "Win32Window::create - wglCreateContext() failed.");
    }
    if(oldContext && oldContext != mGlrc) {
      if(!wglShareLists(oldContext, mGlrc))
        ASSERT(0 && "Win32Window::create - wglShareLists() failed.");
    }
    if(!wglMakeCurrent(mHDC, mGlrc)) {
      translateWGLError();
      ASSERT(0 && "Win32Window::create - wglMakeCurrent() failed.");
    }

    if(oldContext && oldContext != mGlrc) {
      if(!wglMakeCurrent(oldHdc, oldContext))
        ASSERT(0 && "Win32Window::create - wglMakeCurrent() failed.");
    }

    mContext = NEW Win32Context(mHDC, mGlrc);
    mActive = true;
    setHidden(false);
    SetForegroundWindow(mHwnd);
    SetFocus(mHwnd);
  }

  void Win32Window::setHidden(bool hidden) {
		mHidden = hidden;
		if (!mExternal) {
      if (hidden)
        ShowWindow(mHwnd, SW_HIDE);
      else
        ShowWindow(mHwnd, SW_SHOWNORMAL);
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
    //if the wnd is external remember to ReleaseDC(mHWnd, mHDC);
    ReleaseDC(mHwnd, mHDC);

    DestroyWindow(mHwnd);

		UnregisterClass("GLWin32Window", mInst);

    mActive = false;
    mClosed = true;
    mHDC = NULL;
    mHwnd = NULL;
    mInst = NULL;
  }

  void Win32Window::windowMovedOrResized() {
    if(mHwnd == NULL || IsIconic(mHwnd))
      return;

    RECT rc;
    GetWindowRect(mHwnd, &rc);
    mTop = rc.top;
    mLeft = rc.left;
    GetClientRect(mHwnd, &rc);
    if(mWidth == rc.right && mHeight == rc.bottom)
      return;
    mWidth = rc.right - rc.left;
    mHeight = rc.bottom - rc.top;

    for(ViewportList::iterator i = mViewportList.begin(), iend = mViewportList.end(); i != iend; ++i) {
      i->second->updateDimensions();
    }
  }

  void Win32Window::swapBuffers() {
    if(!mExternal)
      SwapBuffers(mHDC);
  }
}
