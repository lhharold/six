#ifndef __SIX_WIN32CONTEXT_H_INCLUDE__
#define __SIX_WIN32CONTEXT_H_INCLUDE__

#include "../glcontext.h"

namespace six {
	class Win32Context : public GLContext {
	public:
    Win32Context(HDC hdc, HGLRC glrc);
    virtual ~Win32Context();

    virtual void setCurrent();
    virtual void endCurrent();
    virtual GLContext* clone() const;
    virtual void releaseContext();
  protected:
    HDC mHDC;
    HGLRC mGlrc;
	};
}

#endif //__SIX_WIN32CONTEXT_H_INCLUDE__
