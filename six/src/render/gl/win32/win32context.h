#yfndef __SIX_WIN32CONTEXT_H_INCLUDE__
#define __SIX_WIN32CONTEXT_H_INCLUDE__

#include "glcontext.h"

namespace six {
	class Win32Context : public GLContext {
	public:
    Win32Context(HDC hdc, HGLRC glrc);
    virtual ~Win32Context();

    virtual void setContext();
    virtual void endCurrent();
    GLContext* clone() const;
    virtual void releaseContext();
  protected:
    HDC mHDC;
    HGLRC mGlrc;
	};
}

#endif //__SIX_WIN32CONTEXT_H_INCLUDE__
