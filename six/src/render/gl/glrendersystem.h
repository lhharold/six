#ifndef __SIX_GLRENDERSYSTEM_H_INCLUDE__
#define __SIX_GLRENDERSYSTEM_H_INCLUDE__

#include "rendersystem.h"
#include "glsupport.h"

namespace six {
	class GLContext;
	class GLRenderSystem : public RenderSystem {
	public:
		GLRenderSystem();
		virtual ~GLRenderSystem();

		virtual RenderWindow* startup(bool autoWindow, const char* windowName);
		virtual RenderWindow* createWindow(const char* windowName, u32 width, u32 height, bool fullScreen);
		virtual void shutdown();
    virtual void setViewport(Viewport* viewport);
    virtual void clearFrameBuffer(u32 buffers, const Color& color = Color::Black, f32 depth = 1.f, u16 stencil = 0);
    virtual void _setColourBufferWriteEnabled(bool r, bool g, bool b, bool a);
    virtual void setRenderTarget(RenderTarget* target);

    void unregisterContext(GLContext *context);
	protected:
		void initialiseContext(RenderWindow* window);
    void switchContext(GLContext* context);
    void setColourBufferWriteEnabled(bool r, bool g, bool b, bool a);
		GLSupport* mGLSupport;
		GLContext* mMainContext;
		GLContext* mActiveContext;

		union {
			struct {
				u32 mGLInitialised : 1;
			};
			u32 mFlags;
		};
    union {
      struct {
        u8 mColorWriteR : 1;
        u8 mColorWriteG : 1;
        u8 mColorWriteB : 1;
        u8 mColorWriteA : 1;
      };
      u8 mColorWrite;
    };
    bool mDepthWrite;
    bool mStencilMask;
	};
}
#endif //__SIX_GLRENDERSYSTEM_H_INCLUDE__