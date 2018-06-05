#ifndef __SIX_GLCONTEXT_H_INCLUDE__
#define __SIX_GLCONTEXT_H_INCLUDE__

namespace six {
	class GLContext {
	public:
		GLContext() : mInit(false) {}
		virtual ~GLContext() {}
    virtual void setCurrent() = 0;
    virtual void endCurrent() = 0;
    virtual void releaseContext() = 0;
    virtual GLContext* clone() const = 0;
    bool getInitialized() {return mInit;}
    void setInitialized() {mInit = true;}
  protected:
    bool mInit;
	};
}
#endif //__SIX_GLCONTEXT_H_INCLUDE__
