#include "glcore.h"
#include "glwin32support.h"
#include "win32window.h"
#include "../glrendersystem.h"

namespace six {
	GLWin32Support::GLWin32Support() : mInitWindow(NULL) {
		initializeWGL();
	}
	GLWin32Support::~GLWin32Support() {
	}
	RenderWindow* GLWin32Support::createWindow(GLRenderSystem* renderSystem, bool autoWindow, const char* windowName){
		if(autoWindow) {
			int width = 800;
			int height = 600;
			bool fullScreen = false;
			return renderSystem->createWindow(windowName, width, height, fullScreen);
		}
		return NULL;
	}
	RenderWindow* GLWin32Support::newWindow(const char* windowName, u32 width, u32 height, bool fullScreen) {
		Win32Window* window = NEW Win32Window(this);
		window->create(windowName, width, height, fullScreen);
		if(mInitWindow == NULL)
			mInitWindow = window;
		return window;
	}
	void GLWin32Support::start() {
	}

	void GLWin32Support::stop() {
		mInitWindow = NULL;
	}
	void GLWin32Support::initializeWGL() {
#if 0
		LPCSTR dummyText = "WglDummy";
#ifdef _STATIC_LIB
		HINSTANCE hinst = GetModuleHandle(NULL);
#else
		HINSTANCE hinst = GetModuleHandle("RenderSystem_GL.dll");
#endif
		WNDCLASS dummyClass;
		memset(&dummyClass, 0, sizeof(WNDCLASS));
		dummyClass.style = CS_OWNDC;
		dummyClass.hInstance = hinst;
		dummyClass.lpfnWndProc = dummyWndProc;
		dummyClass.lpszClassName = dummyText;
		RegisterClass(&dummyClass);
		
		HWND hwnd = CreateWindow(dummyText, dummyText,
			WS_POPUP | WS_CLIPCHILDREN,
			0, 0, 32, 32, 0, 0, hinst, 0);
		if (hwnd == NULL)
			ASSERT(0 && "CreateWindow() failed", "GLWin32Support::initializeWGL");
		HDC hdc = GetDC(hwnd);

#if 0
		PIXELFORMATDESCRIPTOR pfd;
		memset(&pfd, 0, sizeof(PIXELFORMATDESCRIPTOR));
		pfd.nSize = sizeof(PIXELFORMATDESCRIPTOR);
		pfd.nVersion = 1;
		pfd.cColorBits = 16;
		pfd.cDepthBits = 15;
		pfd.dwFlags = PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_DOUBLEBUFFER;
		pfd.iPixelType = PFD_TYPE_RGBA;
#else
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
			/*stencilBuffer ? 1 :*/ 0,		// Stencil Buffer Depth
			0,				// No Auxiliary Buffer
			PFD_MAIN_PLANE,			// Main Drawing Layer
			0,				// Reserved
			0, 0, 0				// Layer Masks Ignored
		};
#endif

		int format;
		if ((format = ChoosePixelFormat(hdc, &pfd)) != 0)
			SetPixelFormat(hdc, format, &pfd);

		HGLRC hrc = wglCreateContext(hdc);
		if (hrc) {
			HGLRC oldrc = wglGetCurrentContext();
			HDC oldhdc = wglGetCurrentDC();
			// if wglMakeCurrent fails, wglGetProcAddress will return null
			wglMakeCurrent(hdc, hrc);
			
			wglMakeCurrent(oldhdc, oldrc);
			wglDeleteContext(hrc);
		}

		// clean up our dummy window and class
		DestroyWindow(hwnd);
		UnregisterClass(dummyText, hinst);
#endif
	}
	LRESULT GLWin32Support::dummyWndProc(HWND hwnd, UINT umsg, WPARAM wp, LPARAM lp) {
		return DefWindowProc(hwnd, umsg, wp, lp);
	}
	

	String translateWGLError() {
		int winError = GetLastError();
		int i;
		char errDesc[255];
    i = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, winError, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) errDesc, 255, NULL);
		return String(errDesc);
	}
}