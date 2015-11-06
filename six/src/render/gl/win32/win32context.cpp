#include "core.h"
#include "win32context.h"

namespace six {
  Win32Context::Win32Context(HDC hdc, HGLRC glrc) : mHDC(hdc), mGlrc(glrc){
    
  }

  Win32Context::~Win32Context() {
		GLRenderSystem *rs = static_cast<GLRenderSystem*>(Root::get().getRenderSystem());
		rs->_unregisterContext(this);
  }

  void Win32Context::setContext() {
    wglMakeCurrent(mHDC, mGlrc);
  }
  void Win32Context::endCurrent() {
    wglMakeCurrent(NULL, NULL);
  }
  GLContext* Win32Context::clone() const {
    HGLRC newGlrc = wglCreateContext(mHDC);
    if(!newGlrc)
      ASSERT(0 && "Win32Context::clone - wglCreateContext failed.");
    HGLRC oldGlrc = wglGetCurrentContext();
    HDC = oldHdc = wglGetCurrentDC();
    if(!wglShareLists(mGlrc, newGlrc)) {
      wglDeleteContext(newGlrc);
      ASSERT(0 && "Win32Context::clone - wglShareLists failed.");
    }
    wglMakeCurrent(oldHdc, oldGlrc);
    return NEW Win32Context(mHDC, newGlrc);
  }
  void Win32Context::releaseContext() {
    if(mGlrc != NULL) {
      wglDeleteContext(mGlrc);
      mGlrc = 0;
      mHDC = 0;
    }
  }
}
